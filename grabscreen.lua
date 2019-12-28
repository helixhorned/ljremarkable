#!/usr/bin/env luajit

local ffi = require("ffi")

local bit = require("bit")
local io = require("io")
local os = require("os")

local class = require("class").class
local FrameBuffer = require("framebuffer").FrameBuffer
local posix = require("posix")

local assert = assert
local print = print
local tonumber = tonumber

local arg = arg
local stderr = io.stderr

----------

local function currentTimeMs()
    local ts = posix.clock_gettime()
    return 1000 * tonumber(ts.sec) + tonumber(ts.nsec) / 1000000
end

----------

local fb = FrameBuffer(0, false)
local map = fb:getMapping()
local unpackPixel = map:getUnpackPixelFunc()
local fbPtr = map:getBasePointer()

if (map:getPixelSize() ~= 4) then
    stderr:write("ERROR: Unsupported pixel size.\n")
    os.exit(1)
end

local PixelArray = ffi.typeof("$ [?]", map:getPixelType())
local NarrowArray = ffi.typeof("uint16_t [?]")

local size = map:getSize()
local tempBuf = PixelArray(size)
local narrowBuf = NarrowArray(size)

local shl, shr = bit.lshift, bit.rshift
local band, bor = bit.band, bit.bor

-- Convert to RGB565 represented as integer.
local function narrow(px)
    local r, g, b = unpackPixel(px)
    return bor(shr(r, 3), shl(shr(g, 2), 5), shl(shr(b, 3), 11))
end

-- Initial copy of the screen contents.
ffi.copy(tempBuf, fbPtr, size * map:getPixelSize())

for i = 0, size - 1 do
    narrowBuf[i] = narrow(tempBuf[i])
end

---------- Sampling and comparison ----------

local SideLen = 8
local HalfSideLen = SideLen / 2
local SquareSize = SideLen * SideLen  -- in pixels
assert(map.xres % SideLen == 0)
assert(map.yres % SideLen == 0)

local SBuf = ffi.typeof[[struct {
    static const int Current = 0;
    static const int Other = 1;
}]]

local Sampler = class
{
    function()
        assert(size % SquareSize == 0)
        local sampleCount = size / SquareSize

        return {
            fbIndexes = {},
            sampleCount = sampleCount,
            currentBufIdx = 0,
            sampleBufs = {
                [0] = PixelArray(sampleCount),
                [1] = PixelArray(sampleCount),
            }
        }
    end,

    generate = function(self)
        local idxs = {}

        for y = 0, map.yres - 1, SideLen do
            for x = 0, map.xres - 1, SideLen do
                -- TODO: randomly perturb sample positions? But, can only do that for the
                --  tiles that changed. Otherwise, we'd consider the whole screen changed in
                --  the next iteration, even if only a small part was changed.
                local xoff = HalfSideLen
                local yoff = HalfSideLen

                idxs[#idxs + 1] = map:getLinearIndex(x + xoff, y + yoff)
            end
        end

        assert(#idxs == self.sampleCount)

        self.fbIndexes = idxs
    end,

    sample = function(self)
        assert(#self.fbIndexes == self.sampleCount)

        self:switchBuffer()
        local sampleBuf = self:getBuffer(SBuf.Current)

        for i = 1, self.sampleCount do
            sampleBuf[i] = fbPtr[self.fbIndexes[i]]
        end
    end,

    compare = function(self)
        local currentBuf = self:getBuffer(SBuf.Current)
        local otherBuf = self:getBuffer(SBuf.Other)

        local diffCount = 0

        for i = 0, self.sampleCount - 1 do
            diffCount = diffCount + ((currentBuf[i] ~= otherBuf[i]) and 1 or 0)
        end

        return diffCount
    end,

-- private:
    getBuffer = function(self, which)
        local bufIdx = (which == SBuf.Current)
            and self.currentBufIdx
            or 1 - self.currentBufIdx
        return self.sampleBufs[bufIdx]
    end,

    switchBuffer = function(self)
        self.currentBufIdx = 1 - self.currentBufIdx
    end,
}

local sampler = Sampler()
sampler:generate()

local function sampleAndCompare()
    sampler:sample()
    local diffCount = sampler:compare()

    if (diffCount > 0) then
        stderr:write("changed, tiles differing: "..diffCount.."\n")
    end

    posix.clock_nanosleep(250e6)
end

----------

while (true) do
    local startMs = currentTimeMs()
    sampleAndCompare()
    stderr:write(("%.0f ms\n"):format(currentTimeMs() - startMs))
end
