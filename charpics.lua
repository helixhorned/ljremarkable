-- Copyright (C) 2019-2020 Philipp Kutin

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

local io = require("io")
local math = require("math")
local table = require("table")

local art_table = require("art_table")

local assert = assert
local error = error
local ipairs = ipairs
local pairs = pairs
local type = type

----------

local api = {}

local MAGIC = "\222rMp\237cs"  -- 7 bytes, "ÞrMpícs"
local VERSION = 1  -- 1 byte

local MAXCHARCODE = 0x10ffff
-- We use a byte for tile xlen/ylen. (The high bit is reserved for future use.)
local MAXSIDELEN = 127
-- We use 6 bits of a byte for the length of a run.
-- The two high bits are: isRLE, {uncovered|fully-covered}
local MAXRUNLEN = 63

local PIXSHIFT = 3
-- Pixel value for maximum coverage (black, if painting with white..black).
local MAXPIXVAL = bit.rshift(255, PIXSHIFT)
-- Non-RLE pixel values must not stomp on the two RLE high bits.
assert(MAXPIXVAL <= MAXRUNLEN)

-- TODO: require pre-shifted tile as input?
local function ShiftPixel(value)
    return bit.rshift(value, PIXSHIFT)
end

local Bits = {
    IsRLE = 128,
    IsMaxCov = 64,
}

assert(Bits.IsMaxCov > MAXPIXVAL)

local ValidateOnWrite = true

local Header_t = ffi.typeof[[struct {
    char magic[7];
    uint8_t version;
    uint32_t fileSize;
    uint32_t entryCount;
}]]

-- Negative value: alternative (e.g. small) rendering of the codepoint @ the absolute value.
-- NOTE: This means that there can be no alternative for #0.
local code_point_t = ffi.typeof("int32_t")
assert(code_point_t(-MAXCHARCODE) == -MAXCHARCODE)
assert(code_point_t(MAXCHARCODE) == MAXCHARCODE)

local code_point_array_t = ffi.typeof("$ [?]", code_point_t)
local uint8_t = ffi.typeof("uint8_t")
local uint8_array_t = ffi.typeof("$ [?]", uint8_t)

local EntryDesc_t = ffi.typeof[[struct {
    uint8_t w;
    uint8_t h;
    uint16_t comprSize;
}]]

local EntryDesc_array_t = ffi.typeof("$ [?]", EntryDesc_t)

local function FindFirstNonZero(ptr, len)
    for i = 0, len - 1 do
        if (ShiftPixel(ptr[i]) ~= 0) then
            return i
        end
    end
    return len  -- "one past end"
end

local function FindFirstNonZeroFromRight(ptr, len)
    for i = len - 1, 0, -1 do
        if (ShiftPixel(ptr[i]) ~= 0) then
            return i
        end
    end
    return -1  -- "one before beginning"!
end

local function GetSpanSpec(ptr, len)
    assert(len > 0 and len <= MAXRUNLEN)

    local value = ShiftPixel(ptr[0])
    assert(value >= 0 and value <= MAXPIXVAL)
    local isMaxCov = (value == MAXPIXVAL)

    if (not (value == 0 or isMaxCov)) then
        -- Gray pixel. No RLE.
        return value, 1
    end

    local ii = len

    -- Run-length encoding (on the range-compressed pixel values).
    for i = 1, len - 1 do
        if (ShiftPixel(ptr[i]) ~= value) then
            ii = i
            break
        end
    end

    return bit.bor(Bits.IsRLE, isMaxCov and Bits.IsMaxCov or 0, ii), ii
end

-- Returns: number of pixel span specs.
local function EncodeLine(srcPtr, srcLen, dstPtr, dstLen)
    assert(srcPtr ~= 0 and dstPtr ~= 0)
    if (srcLen <= 0) then
        return 0
    end

    assert(srcPtr[0] ~= 0)
    assert(dstLen > 0)

    local d, s = 0, 0

    while (s < srcLen) do
        -- NOTE: MAXRUNLEN < MAXSIDELEN, so one run may need more than one spec.
        local len = math.min(MAXRUNLEN, srcLen - s)
        local spec, step = GetSpanSpec(srcPtr + s, len)
        assert(spec >= 0 and spec <= 255)
        assert(step >= 1 and step <= len)
        assert(d < dstLen)
        dstPtr[d] = spec
        s = s + step
        d = d + 1
    end

    assert(s == srcLen)

    return d
end

-- Worst case for a line:
--  offset=0, specCount=MAXSIDELEN, then MAXSIDELEN*alternating grays (i.e. no RLE)
local ScratchBuf = uint8_array_t((MAXSIDELEN + 2)*MAXSIDELEN)
local ScratchBufEnd = ScratchBuf + ffi.sizeof(ScratchBuf)

local function Encode(data, width, height)
    assert(width >= 1 and width <= MAXSIDELEN)
    assert(height >= 1 and height <= MAXSIDELEN)
    assert(type(data) == "cdata" and ffi.sizeof(data) == width*height)

    local ii = 0
    local srcPtr = data
    local dstPtr = ScratchBuf

    for row = 0, height-1 do
        local minCol = FindFirstNonZero(srcPtr, width)
        local maxCol = FindFirstNonZeroFromRight(srcPtr, width)
        assert(0 <= minCol and minCol <= width)
        assert(-1 <= maxCol and maxCol < width)

        local isBlankLine = (minCol == width)
        assert(isBlankLine == (maxCol == -1))
        assert((not isBlankLine) == (minCol <= maxCol))

        dstPtr[0] = minCol  -- offset
        local numSpecs = EncodeLine(srcPtr + minCol, maxCol - minCol + 1,
                                    dstPtr + 2, ScratchBufEnd - (dstPtr + 2))
        assert(isBlankLine == (numSpecs == 0))
        dstPtr[1] = numSpecs

        srcPtr = srcPtr + width
        dstPtr = dstPtr + 2 + numSpecs
    end

    local comprSize = dstPtr - ScratchBuf
    local comprData = uint8_array_t(comprSize)
    ffi.copy(comprData, ScratchBuf, comprSize)

    return comprData, comprSize
end

local function ReplaySpec(spec, ptr, remainingLen)
    assert(remainingLen >= 1)

    if (bit.band(spec, Bits.IsRLE) == 0) then
        ptr[0] = spec
        return 1
    end

    local length = bit.band(spec, MAXRUNLEN)
    assert(length <= remainingLen)

    -- Only write non-zero pixels.
    -- TODO on demand: blending.
    if (bit.band(spec, Bits.IsMaxCov) ~= 0) then
        for i = 0, length - 1 do
            ptr[i] = MAXPIXVAL
        end
    end

    return length
end

-- NOTE: does not fill 'dstPtr'. The caller prepares it as it likes.
-- TODO: pass back errors due to improper input data instead of asserting.
local function Decode(srcPtr, srcLen, width, height, dstPtr)
    assert(type(srcPtr) == "cdata" and type(dstPtr) == "cdata")
    assert(srcLen > 0)
    assert(width > 0 and height > 0)

    local srcEnd = srcPtr + srcLen

    for row = 0, height - 1 do
        assert(srcEnd - srcPtr >= 2)
        local offset, specCount = srcPtr[0], srcPtr[1]
        assert((offset == width) == (specCount == 0))

        srcPtr = srcPtr + 2
        assert(srcEnd - srcPtr >= specCount)

        local remainingLen = width - offset
        local dstPtrOffset = offset

        for s = 0, specCount - 1 do
            local step = ReplaySpec(srcPtr[s], dstPtr + dstPtrOffset, remainingLen)
            assert(step <= remainingLen)
            dstPtrOffset = dstPtrOffset + step
            remainingLen = remainingLen - step
        end

        -- NOTE: may have pixels left over at the right border (which were omitted from
        --  being encoded in the first place because they had value 0).
        assert(remainingLen >= 0)

        srcPtr = srcPtr + specCount
        dstPtr = dstPtr + width
    end

    assert(srcPtr == srcEnd)
end

-- 'artTab': [codePt] = { w=<width>, h=<height>, data=<cdata> }
function api.write(fileName, artTab)
    assert(type(fileName) == "string", "argument #1 must be a string")
    assert(type(artTab) == "table", "argument #2 must be a table")

    do
        local minTileNum, maxTileNum = art_table.validate(artTab)
        assert(minTileNum >= -MAXCHARCODE and maxTileNum <= MAXCHARCODE,
               "Empty code point table, or some code points outside the allowed range")
        assert(minTileNum <= maxTileNum)
    end

    local codePoints = {}
    for codePt, tileTab in pairs(artTab) do
        local sx, sy = tileTab.w, tileTab.h
        assert(sx <= MAXSIDELEN and sy <= MAXSIDELEN, "Tile dimensions out of range")
        codePoints[#codePoints + 1] = codePt
    end
    table.sort(codePoints)

    local entryCount = #codePoints

    --== Write out the file!

    local f, msg = io.open(fileName, "w")
    if (f == nil) then
        return nil, msg
    end

    local function write(cdata)
        f:write(ffi.string(cdata, ffi.sizeof(cdata)))
    end

    -- Will overwrite at the end, fill varying members with zeros for now.
    local header = Header_t(MAGIC, VERSION)
    write(header)

    write(code_point_array_t(entryCount, codePoints))

    local entryDescs = EntryDesc_array_t(entryCount)
    local comprEntries = {}

    for i = 1, entryCount do
        local descRef = entryDescs[i - 1]
        local tileTab = artTab[codePoints[i]]
        local w, h = tileTab.w, tileTab.h
        descRef.w, descRef.h = w, h

        local comprEntry, comprSize = Encode(tileTab.data, w, h)
        comprEntries[i], descRef.comprSize = comprEntry, comprSize

        if (ValidateOnWrite) then
            local size = w * h
            local tempBuf = uint8_array_t(size)
            Decode(comprEntry, comprSize, w, h, tempBuf)

            for k = 0, size - 1 do
                local pix, ref = tempBuf[k], tileTab.data[k]
                if (pix ~= ShiftPixel(ref)) then
                    local fmt = "RLE validation: codept %d, pix offset %d (w=%d); ref=%d>>%d but pix=%d"
                    error(("INTERNAL: "..fmt):format(codePoints[i], k, w, ref, PIXSHIFT, pix))
                end
            end
        end
    end

    write(entryDescs)

    for _, comprEntry in ipairs(comprEntries) do
        write(comprEntry)
    end

    -- Trailing magic for consistency checking when loading.
    f:write(MAGIC)

    -- Fill the header with proper data.
    local fileSize, msg = f:seek("cur")
    if (fileSize == nil) then
        f:close()
        return nil, msg
    end

    header.fileSize = fileSize
    header.entryCount = entryCount

    -- Seek to the beginning and properly write the header.
    local offset = f:seek("set")
    -- The Lua docs do not document a failure case.
    assert(offset == 0)
    write(header)

    f:close()
end

-- Done!
return api
