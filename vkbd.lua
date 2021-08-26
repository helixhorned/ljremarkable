-- SPDX-License-Identifier: GPL-3.0-or-later

-- Copyright (C) 2019-2021 Philipp Kutin

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
local LastRowKeyCount = 5
local HalfKeyWidth = KeyWidth / 2

assert(KeyHeight % 4 == 0)
assert(OriginY + FullHeight == ScreenHeight_rM - 4)
assert(OriginX + FullWidth == ScreenWidth_rM - 84)

local LayoutCount = #KB.layoutNames

local g_currentLayoutIdx = 1

----------

local api = {
    OriginY = OriginY,
    KeyHeight = KeyHeight,
    LayoutCount = LayoutCount,
    RightBorder = OriginX + FullWidth,
    RowCount = RowCount,
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

local function validateLayout(layout)
    -- Validate referential consistency: each key mnemonic must map to a code point.
    for k, mnemonic in pairs(layout) do
        assert(type(k) == "number")
        assert(type(mnemonic) == "string")

        local codePt = KB.codepoints[mnemonic]
        assert(type(codePt) == "number")

        -- Upper (> 16th) bits encode the X KeySym value.
        assert(codePt >= 0x10000)
    end

    -- Check presence of data for used fixed-function mnemonics, needed for KeySym values.
    assert(KB.codepoints["BackSpace"] ~= nil)
    assert(KB.codepoints["Delete"] ~= nil)
    assert(KB.codepoints["space"] ~= nil)
    assert(KB.codepoints["Tab"] ~= nil)
    assert(KB.codepoints["Return"] ~= nil)
end

do
    for i = 1, LayoutCount do
        local layoutName = KB.layoutNames[i]
        assert(type(layoutName) == "string")
        assert(type(KB.layouts[layoutName]) == "table")
    end

    for _, layout in pairs(KB.layouts) do
        validateLayout(layout)
    end
end

local function getLayout(layoutIdx)
    assert(layoutIdx >= 1 and layoutIdx <= LayoutCount)
    return KB.layouts[KB.layoutNames[layoutIdx]]
end

local function getCodePointAndKeySym(layoutIdx, row, col, level)
    assert(layoutIdx >= 1 and layoutIdx <= LayoutCount)
    assert(level == 0 or level == 1 or level == 2)

    local k = 100*(row-1) + 10*col + level
    local layout = getLayout(layoutIdx)
    local mnemonic =
        (row == 3 and col == ColumnCount) and (
            level == 0 and "BackSpace" or "Delete") or
        (row == RowCount and level == 0) and (
            col == 3 and (level == 0 and "space" or "Tab") or
            col == 5 and level == 0 and "Return" or
            nil) or
        layout[k]
    if (mnemonic == nil) then
        return nil, nil  -- key has no symbol associated
    end

    local u64pv = 0ULL + KB.codepoints[mnemonic]  -- packed value
    local codePt = tonumber(bit.band(u64pv, 0xffff))
    local keySym = tonumber(bit.rshift(u64pv, 16))

    return (codePt > 0) and codePt or nil, keySym
end

local function getOrigin(row, col)
    return OriginX + (col-1)*KeyWidth, OriginY + (row-1)*KeyHeight
end

local function drawKey(layoutIdx, row, col, drawChar)
    assert(type(row) == "number")
    assert(type(col) == "number")

    assert(row >= 1 and row <= RowCount - 1)
    assert(col >= 1 and col <= ColumnCount)

    local codePt = getCodePointAndKeySym(layoutIdx, row, col, 0)

    if (codePt ~= nil) then
        local ox, oy = getOrigin(row, col)
        drawChar(ox + KeyWidth/2, oy + math.floor(2*KeyHeight/3), codePt, false)

        if (row == 1 or (row == 4 and col >= ColumnCount - 1)) then
            local shiftCodePt = getCodePointAndKeySym(layoutIdx, row, col, 1)
            if (shiftCodePt ~= nil) then
                drawChar(ox + 3*KeyWidth/4, oy + math.floor(KeyHeight/3), shiftCodePt, true)
            end
        end
    end
end

function api.drawAllKeys(drawChar)
    for row = 1, RowCount - 1 do
        for col = 1, ColumnCount do
            drawKey(g_currentLayoutIdx, row, col, drawChar)
        end
    end
end

local key_spec_t = ffi.typeof[[const struct{
    int8_t r, c;
}]]

-- In terms of half-columns.
local LastRowKeyBorders = {
    [0] = 0,
    3,
    5,
    (2*ColumnCount - 5),
    (2*ColumnCount - 3),
    2*ColumnCount
}

-- Returns: zero-based, "nominal" column index.
local function GetColumnForLastRow(hc)
    for i = 1, LastRowKeyCount do
        if (hc < LastRowKeyBorders[i]) then
            return i - 1
        end
    end

    assert(false)
end

local function GetXBoundsOfLastRowKey(c)
    assert(OriginX == 0)
    assert(c >= 0 and c < LastRowKeyCount)

    return
        HalfKeyWidth*LastRowKeyBorders[c],
        HalfKeyWidth*LastRowKeyBorders[c+1]
end

-- Only the inside area of a key rectangle is active.
local InactiveMargin = 18  -- approx. 2 mm

local function IsInsideKeyExcludingMargin(isLastRow, dx, dy, c)
    local remy = dy % KeyHeight
    local ok = (remy >= InactiveMargin and remy < KeyHeight - InactiveMargin)

    if (not isLastRow) then
        local remx = dx % KeyWidth
        ok = ok and (remx >= InactiveMargin and remx < KeyWidth - InactiveMargin)
    else
        local x1, x2 = GetXBoundsOfLastRowKey(c)
        ok = ok and (dx >= x1 + InactiveMargin and dx <= x2 - InactiveMargin)
    end

    return ok
end

-- Returns:
--  key_spec_t: x/y represent the active region of an on-screen keyboard key
--    or that of an imagined key in the row above the topmost one (<return>.r == -1)
--  nil: otherwise
function api.checkCoords(x, y)
    assert(type(x) == "number")
    assert(type(y) == "number")

    if (not (x >= OriginX and x < OriginX + FullWidth)) then
        return nil
    elseif (not (y >= OriginY - KeyHeight and y < OriginY + FullHeight)) then
        return nil
    end

    local dx, dy = x - OriginX, y - OriginY
    local r = math.floor(dy / KeyHeight)
    local c = math.floor(dx / KeyWidth)

    local row, col = r + 1, c + 1
    assert(row >= 0 and row <= RowCount and col >= 1 and col <= ColumnCount)

    local isLastRow = (row == RowCount)

    if (isLastRow) then
        local hc = math.floor(dx / HalfKeyWidth)
        assert(hc >= 0 and hc < 2*ColumnCount)
        c = GetColumnForLastRow(hc)
    end

    if (not IsInsideKeyExcludingMargin(isLastRow, dx, dy, c)) then
        return nil
    end

    return key_spec_t(r, c)
end

function api.blinkKey(keySpec, flashingRefresh)
    assert(ffi.istype(key_spec_t, keySpec))

    local x = OriginX + KeyWidth*keySpec.c + 1
    local y = OriginY + KeyHeight*keySpec.r + 1
    local w = KeyWidth

    if (keySpec.r + 1 == RowCount) then
        local x1, x2 = GetXBoundsOfLastRowKey(keySpec.c)
        if (not (keySpec.c == 2 or keySpec.c == 4)) then
            -- Only Space and Return are handled for now.
            return
        end

        x = x1
        w = x2 - x1
    end

    flashingRefresh(x, y, w - 1, KeyHeight - 1)
end

function api.changeLayout(layoutIdx)
    assert(layoutIdx >= 1 and layoutIdx <= LayoutCount)
    g_currentLayoutIdx = layoutIdx
end

function api.getKeySym(keySpec, level)
    assert(ffi.istype(key_spec_t, keySpec))
    local _, keySym = getCodePointAndKeySym(g_currentLayoutIdx, keySpec.r + 1, keySpec.c + 1, level)
    return keySym
end

-- Done!
return api
