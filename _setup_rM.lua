#!/bin/false

-- Debugging setup, to be loaded as Lua module on the LuaJIT command line on the rM.
-- NOTE: hence, deliberately assigning in global environment.

ffi=require'ffi'
FB=require'framebuffer'
fb=FB.FrameBuffer(0, true)
map=fb:getMapping()

RM=require'remarkable'
rM=RM.Remarkable()

local assert = assert

function rgb(r, g, b)
    assert(r>=0 and r <= 31)
    assert(g>=0 and g <= 63)
    assert(b>=0 and b <= 31)

    return b + 32*g + 32*64*r
end

-- Fill+update.
function fup(x, y, w, h, val)
    map:fill(x, y, w, h, val)
    rM:requestRefresh(RM.xywh(x,y,w,h), 123)
end

input=require'input'
MTC=input.MultiTouchCode
evd=input.EventDevice(1)

if (arg[1] == '-Q' and #arg == 1) then
    -- Making .app.lua
    os.exit(0)
end
