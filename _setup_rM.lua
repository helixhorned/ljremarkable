#!/bin/false

-- Debugging setup, to be loaded as Lua module on the LuaJIT command line on the rM.

-- NOTE: will assign to global environment at the end.
local _G = _G

local useRealFB = (os.getenv("LJREMARKABLE_SETUP_rM_USE_DUMMY_FB") ~= "1")

local bit=require'bit'
local ffi=require'ffi'
local FB=require'framebuffer'
local fb = useRealFB and FB.FrameBuffer(0, true) or nil
local map = useRealFB and fb:getMapping() or nil

local RM=require'remarkable'
local rM = useRealFB and RM.Remarkable() or nil

local charpics = require'charpics'

local math = require("math")
local os = require("os")

local assert = assert
local require = require
local unpack = unpack

local arg = arg

----------

local MaxColorComponentVal = { r=31, g=63, b=31 }

local function rgb(r, g, b)
    assert(r>=0 and r <= MaxColorComponentVal.r)
    assert(g>=0 and g <= MaxColorComponentVal.g)
    assert(b>=0 and b <= MaxColorComponentVal.b)

    return b + 32*g + 32*64*r
end

-- Fill+update.
local function fup(x, y, w, h, val)
    map:fill(x, y, w, h, val)
    rM:requestRefresh(RM.xywh(x,y,w,h), 123)
end

local cpr
local function drawstr(x, yForBaseline, interCharAdvanceX, str, codePtOffset)
    cpr = cpr or charpics.Renderer(".charpics", map)
    local endX, topY, botY = cpr:drawString(x, yForBaseline, interCharAdvanceX, str, codePtOffset)
    if (endX > x and botY > topY) then
        rM:requestRefresh(RM.xywh(x, topY, endX - x, botY - topY))
    end
end

-- NOTE: zero-based for more convenient '%'-ing.
local ColorComponentIdx = { r=0, g=1, b=2 }
local ColorComponentName = { [0]='r', 'g', 'b' }

local function completeRgbTab(protoTab)
    for compIdx = 0, 2 do
        protoTab[compIdx] = protoTab[compIdx] or 0
    end
    return protoTab
end

local function unpackRgbTab(rgbTab)
    return unpack(rgbTab, 0)
end

local TotalSideLen = 1024
local LeftOffset = 192
local TopOffset = 128

local function clear(r, g, b)
    fup(LeftOffset, TopOffset, TotalSideLen, TotalSideLen, rgb(r, g, b))
end

local function gradient(fixedCompName, fixedVal)
    local fixedComp = ColorComponentIdx[fixedCompName]
    assert(fixedComp ~= nil)
    -- Just for the early bounds check on 'fixedVal':
    rgb(unpackRgbTab(completeRgbTab{[fixedComp]=fixedVal}))

    local c1 = (fixedComp - 1) % 3
    local c2 = (fixedComp + 1) % 3
    local firstComp = math.min(c1, c2)
    local secondComp = math.max(c1, c2)
    local maxFirst = MaxColorComponentVal[ColorComponentName[firstComp]]
    local maxSecond = MaxColorComponentVal[ColorComponentName[secondComp]]

    local firstSideLen = TotalSideLen / (maxFirst + 1)
    local secondSideLen = TotalSideLen / (maxSecond + 1)

    for i = 0, maxFirst do
        local x = LeftOffset + firstSideLen*i
        for j = 0, maxSecond do
            local y = TopOffset + secondSideLen*j
            local rgbTab = { [firstComp]=i, [secondComp]=j, [fixedComp]=fixedVal }
            map:fill(x, y, firstSideLen, secondSideLen, rgb(unpackRgbTab(rgbTab)))
        end
    end

    rM:requestRefresh(RM.xywh(LeftOffset, TopOffset, TotalSideLen, TotalSideLen), 124)
end

_G.ffi = ffi
_G.FB = FB
_G.fb = fb
_G.map = map
_G.RM = RM
_G.rM = rM
_G.charpics = charpics

_G.rgb = rgb
_G.fup = fup
_G.drawstr = drawstr
_G.clear = clear
_G.gradient = gradient

_G.input = require'input'
_G.MTC = _G.input.MultiTouchCode
_G.evd = _G.input.EventDevice(1)

if (arg[1] == '-Q' and #arg == 1) then
    -- Making .app.lua
    os.exit(0)
end
