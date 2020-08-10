#!/bin/false

-- Debugging setup, to be loaded as Lua module on the LuaJIT command line on the rM.

-- NOTE: will assign to global environment at the end.
local _G = _G

local ffi=require'ffi'
local FB=require'framebuffer'
local fb=FB.FrameBuffer(0, true)
local map=fb:getMapping()

local RM=require'remarkable'
local rM=RM.Remarkable()

local math = require("math")
local os = require("os")

local assert = assert
local require = require

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

_G.ffi = ffi
_G.FB = FB
_G.fb = fb
_G.map = map
_G.RM = RM
_G.rM = rM

_G.rgb = rgb
_G.fup = fup

_G.input = require'input'
_G.MTC = _G.input.MultiTouchCode
_G.evd = _G.input.EventDevice(1)

if (arg[1] == '-Q' and #arg == 1) then
    -- Making .app.lua
    os.exit(0)
end
