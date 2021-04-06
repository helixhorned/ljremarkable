
local assert = assert
local ipairs = ipairs
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

assert(OriginY + RowCount*KeyHeight == ScreenHeight_rM - 4)
assert(OriginX + ColumnCount*KeyWidth == ScreenWidth_rM - 84)

----------

local api = {}

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

    refresh(OriginX, OriginY, ColumnCount*KeyWidth + 1, RowCount*KeyHeight)
end

-- Done!
return api
