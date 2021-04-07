
local math = require("math")

local KB = require("layouts.")

local assert = assert
local ipairs = ipairs
local pairs = pairs
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

assert(KeyHeight % 4 == 0)
assert(OriginY + RowCount*KeyHeight == ScreenHeight_rM - 4)
assert(OriginX + ColumnCount*KeyWidth == ScreenWidth_rM - 84)

----------

local api = {}

local function doRefresh(refresh)
    refresh(OriginX, OriginY, ColumnCount*KeyWidth + 1, RowCount*KeyHeight)
end

function api.drawGrid(drawHline, drawVline, refresh)
    assert(type(drawHline) == "function")
    assert(type(drawVline) == "function")
    assert(type(refresh) == "function")

    -- Horizontal lines
    for j = 0, RowCount-1 do
        drawHline(OriginX, OriginY + j*KeyHeight, ColumnCount*KeyWidth)
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

    doRefresh(refresh)
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
    end
end

local function getCodePoint(row, col)
    local k = 100*(row-1) + 10*col
    local mnemonic = mainLayout[k]
    return (mnemonic ~= nil) and
        assert(KB.codepoints[mnemonic]) or
        nil  -- key is special
end

local function getOrigin(row, col)
    return OriginX + (col-1)*KeyWidth, OriginY + (row-1)*KeyHeight
end

local function drawKey(row, col, drawChar)
    assert(type(row) == "number")
    assert(type(col) == "number")

    assert(row >= 1 and row <= RowCount - 1)
    assert(col >= 1 and col <= ColumnCount)

    local codePt = getCodePoint(row, col)

    if (codePt ~= nil) then
        local ox, oy = getOrigin(row, col)
        drawChar(ox + KeyWidth/2, oy + math.floor(2*KeyHeight/3), codePt)
    end
end

function api.drawAllKeys(drawChar, refresh)
    for r = 1, RowCount - 1 do
        for c = 1, ColumnCount do
            drawKey(r, c, drawChar)
        end
    end

    doRefresh(refresh)
end

-- Done!
return api
