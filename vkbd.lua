
local ffi = require("ffi")
local bit = require("bit")

local math = require("math")

local KB = require("layouts.")

local assert = assert
local ipairs = ipairs
local pairs = pairs
local tonumber = tonumber
local type = type

----------

-- KEEPINSYNC grabscreen.lua
-- Default screen dimensions on the reMarkable.
local ScreenWidth_rM = 1404
local ScreenHeight_rM = 1872
-- Guesstimate for the side length of the touch-active region around the rM "eye".
local EyeSize_rM = 128

-- On-screen keyboard dimensions
local OriginY = 1080 + EyeSize_rM
local OriginX = 0
local KeyHeight = 132
local KeyWidth = KeyHeight
local RowCount = 5
local ColumnCount = 10
local FullHeight = RowCount*KeyHeight
local FullWidth = ColumnCount*KeyWidth

assert(KeyHeight % 4 == 0)
assert(OriginY + FullHeight == ScreenHeight_rM - 4)
assert(OriginX + FullWidth == ScreenWidth_rM - 84)

----------

local api = {
    OriginY = OriginY
}

function api.drawGrid(drawHline, drawVline)
    assert(type(drawHline) == "function")
    assert(type(drawVline) == "function")

    -- Horizontal lines
    for j = 0, RowCount-1 do
        drawHline(OriginX, OriginY + j*KeyHeight, FullWidth)
    end

    -- Vertical lines (inside: first four rows only)
    for i = 1, ColumnCount do
        local r = (i == ColumnCount) and RowCount or RowCount-1
        drawVline(OriginX + i*KeyWidth, OriginY, r*KeyHeight)
    end

    -- Vertical lines (last row)
    --  Ctrl | Alt |  Space  | Tux | Enter
    local oy = OriginY + (RowCount-1)*KeyHeight
    for _, iFrac in ipairs{ 1.5, 2.5, ColumnCount-2.5, ColumnCount-1.5 } do
        drawVline(OriginX + iFrac*KeyWidth, oy, KeyHeight)
    end
end

local mainKbLayoutName = KB.layoutNames[1]
assert(type(mainKbLayoutName) == "string")
local mainLayout = KB.layouts[mainKbLayoutName]
assert(type(mainLayout) == "table")

do
    -- Validate referential consistency: each key mnemonic must map to a code point.
    for k, mnemonic in pairs(mainLayout) do
        assert(type(k) == "number")
        assert(type(mnemonic) == "string")

        local codePt = KB.codepoints[mnemonic]
        assert(type(codePt) == "number")

        -- Upper (> 16th) bits encode the X KeySym value.
        assert(codePt >= 0x10000)
    end

    -- Check presence of data for used fixed-function mnemonics, needed for KeySym values.
    assert(KB.codepoints["BackSpace"] ~= nil)
end

local function getCodePointAndKeySym(row, col)
    local k = 100*(row-1) + 10*col

    local mnemonic =
        (row == 3 and col == ColumnCount) and "BackSpace" or
        mainLayout[k]
    if (mnemonic == nil) then
        return nil, nil  -- key is special and not yet handled
    end

    local u64pv = 0ULL + KB.codepoints[mnemonic]  -- packed value
    local codePt = tonumber(bit.band(u64pv, 0xffff))
    local keySym = tonumber(bit.rshift(u64pv, 16))

    return (codePt > 0) and codePt or nil, keySym
end

local function getOrigin(row, col)
    return OriginX + (col-1)*KeyWidth, OriginY + (row-1)*KeyHeight
end

local function drawKey(row, col, drawChar)
    assert(type(row) == "number")
    assert(type(col) == "number")

    assert(row >= 1 and row <= RowCount - 1)
    assert(col >= 1 and col <= ColumnCount)

    local codePt = getCodePointAndKeySym(row, col)

    if (codePt ~= nil) then
        local ox, oy = getOrigin(row, col)
        drawChar(ox + KeyWidth/2, oy + math.floor(2*KeyHeight/3), codePt)
    end
end

function api.drawAllKeys(drawChar)
    for row = 1, RowCount - 1 do
        for col = 1, ColumnCount do
            drawKey(row, col, drawChar)
        end
    end
end

local InactiveMargin = 18  -- approx. 2 mm
local key_spec_t = ffi.typeof[[const struct{
    uint8_t r, c;
}]]

-- Returns:
--  key_spec_t: x/y represent the active region of an on-screen keyboard key
--  nil: otherwise
function api.checkCoords(x, y)
    assert(type(x) == "number")
    assert(type(y) == "number")

    if (not (x >= OriginX and x < OriginX + FullWidth)) then
        return nil
    elseif (not (y >= OriginY and y < OriginY + FullHeight)) then
        return nil
    end

    local dx, dy = x - OriginX, y - OriginY
    local r = math.floor(dy / KeyHeight)
    local c = math.floor(dx / KeyWidth)

    local row, col = r + 1, c + 1
    assert(row >= 1 and row <= RowCount and col >= 1 and col <= ColumnCount)

    if (row == RowCount) then
        return nil  -- TODO: handle
    elseif (row == RowCount - 1 and col == 1) then
        -- Shift key.
        return nil  -- TODO: handle
    end

    local remx, remy = dx % KeyWidth, dy % KeyHeight
    -- Only the inside area of a key rectangle is active.
    if (not (remx >= InactiveMargin and remx < KeyWidth - InactiveMargin)) then
        return nil
    elseif (not (remy >= InactiveMargin and remy < KeyHeight - InactiveMargin)) then
        return nil
    end

    return key_spec_t(r, c)
end

function api.blinkKey(keySpec, flashingRefresh)
    assert(ffi.istype(key_spec_t, keySpec))

    local x = OriginX + KeyWidth*keySpec.c + 1
    local y = OriginY + KeyHeight*keySpec.r + 1

    flashingRefresh(x, y, KeyWidth - 1, KeyHeight - 1)
end

function api.getKeySym(keySpec)
    assert(ffi.istype(key_spec_t, keySpec))
    local _, keySym = getCodePointAndKeySym(keySpec.r + 1, keySpec.c + 1)
    return keySym
end

-- Done!
return api
