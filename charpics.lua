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

local io = require("io")
local math = require("math")
local table = require("table")

local art_table = require("moonglow.art_table")
local class = require("class").class
local error_util = require("error_util")
local posix = require("posix")

local checktype = error_util.checktype
local check = error_util.check

local assert = assert
local error = error
local ipairs = ipairs
local pairs = pairs
local type = type

----------

local api = {}

local MAGIC = "\222rMp\237cs"  -- 7 bytes, "ÞrMpícs"
local VERSION = 3  -- 1 byte

-- We use a uint32_t for code points. Their interpretation is up to the user.
local MAXCHARCODE = 0xffffffff
-- We use a byte for tile xlen/ylen. (The high bit is reserved for future use.)
local MAXSIDELEN = 127
-- Use a signed 16-bit integer for the baseline in pixels:
local MINBASELINE = -32768
local MAXBASELINE = 32767
-- We use 6 bits of a byte for the length of a run.
-- The two high bits are: isRLE, {uncovered|fully-covered}
local MAXRUNLEN = 63

local PIXSHIFT = 3
-- Pixel value for maximum coverage (black, if painting with white..black).
local MAXPIXVAL = bit.rshift(255, PIXSHIFT)
-- Non-RLE pixel values must not stomp on the two RLE high bits.
assert(MAXPIXVAL <= MAXRUNLEN)

-- Code point value to use when a glyph is missing.
-- May e.g. be rendered as a question mark in a box.
local MissingCharCode = 1

api.MaxSideLength = MAXSIDELEN
api.CoverageValueShift = PIXSHIFT

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

local code_point_t = ffi.typeof("uint32_t")
assert(code_point_t(MAXCHARCODE) == MAXCHARCODE)

local code_point_array_t = ffi.typeof("$ [?]", code_point_t)
local code_point_ptr_t = ffi.typeof("const $ *", code_point_t)
local uint8_t = ffi.typeof("uint8_t")
local uint8_array_t = ffi.typeof("$ [?]", uint8_t)
local uint8_ptr_t = ffi.typeof("const $ *", uint8_t)

local EntryDesc_t = ffi.typeof[[struct {
    uint8_t w;
    uint8_t h;
    int16_t baseline;
    uint16_t comprSize;
}]]

local EntryDesc_array_t = ffi.typeof("$ [?]", EntryDesc_t)
local EntryDesc_ptr_t = ffi.typeof("const $ *", EntryDesc_t)

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

local function ReplaySpec(spec, ptr, remainingLen, pixFunc)
    assert(remainingLen >= 1)

    if (bit.band(spec, Bits.IsRLE) == 0) then
        ptr[0] = pixFunc(spec)
        return 1
    end

    local length = bit.band(spec, MAXRUNLEN)
    assert(length <= remainingLen)

    -- Only write non-zero pixels.
    -- TODO on demand: blending.
    if (bit.band(spec, Bits.IsMaxCov) ~= 0) then
        for i = 0, length - 1 do
            ptr[i] = pixFunc(MAXPIXVAL)
        end
    end

    return length
end

-- NOTE: does not fill 'dstPtr'. The caller prepares it as it likes.
-- TODO: pass back errors due to improper input data instead of asserting.
local function Decode(srcPtr, srcLen, width, height, dstPtr, dstStride, pixFunc)
    assert(type(srcPtr) == "cdata" and type(dstPtr) == "cdata")
    assert(srcLen > 0)
    assert(width > 0 and height > 0)
    assert(dstStride >= width)
    assert(pixFunc == nil or type(pixFunc) == "function")

    pixFunc = pixFunc or function(p) return p end

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
            local step = ReplaySpec(srcPtr[s], dstPtr + dstPtrOffset, remainingLen, pixFunc)
            assert(step <= remainingLen)
            dstPtrOffset = dstPtrOffset + step
            remainingLen = remainingLen - step
        end

        -- NOTE: may have pixels left over at the right border (which were omitted from
        --  being encoded in the first place because they had value 0).
        assert(remainingLen >= 0)

        srcPtr = srcPtr + specCount
        dstPtr = dstPtr + dstStride
    end

    assert(srcPtr == srcEnd)
end

local MagicSize = #MAGIC
local HeaderSize = ffi.sizeof(Header_t)
local Header_ptr_t = ffi.typeof("const $ *", Header_t)

local CharPicsFile = class
{
    function(memMapPtr, memMapSize)
        assert(memMapPtr ~= nil)
        local maxBound = memMapSize - MagicSize
        local uMemMapPtr = ffi.cast(uint8_ptr_t, memMapPtr)

        local header = ffi.cast(Header_ptr_t, memMapPtr)
        local entryCount = header.entryCount

        local function byteIncremented(ptrCType, ptr, inc)
            local uptr = ffi.cast(uint8_ptr_t, ptr) + inc
            return ffi.cast(ptrCType, uptr), uptr - uMemMapPtr
        end

        -- TODO: instead of data-dependent asserts, pass back errors.

        local function incremented(resultCType, ptr, elementCType)
            local inc = entryCount
            local uptr = ffi.cast(uint8_ptr_t, ptr)
            local offset = uptr - uMemMapPtr

            local byteInc = ffi.sizeof(elementCType) * inc
            local newOffset = offset + byteInc

            assert(newOffset < maxBound)
            return ffi.cast(resultCType, uMemMapPtr + newOffset), newOffset
        end

        local codePoints = byteIncremented(code_point_ptr_t, memMapPtr, HeaderSize)
        local entryDescs = incremented(EntryDesc_ptr_t, codePoints, code_point_t)
        local comprData, startOffset = incremented(uint8_ptr_t, entryDescs, EntryDesc_t)

        local comprDataPtrs = {}  -- code point -> pointer into this memory map: compressed tile
        local entryDescRefs = {}  -- code point -> reference into this memory map: entry description
        local currentOffset = startOffset

        for i = 0, entryCount-1 do
            local desc = entryDescs[i]
            local comprSize = desc.comprSize
            assert(desc.w > 0 and desc.h > 0 and comprSize > 0)
            assert(desc.w <= MAXSIDELEN and desc.h <= MAXSIDELEN)

            assert(currentOffset + comprSize <= maxBound)
            assert((currentOffset + comprSize < maxBound) == (i ~= entryCount - 1))

            local codePt = codePoints[i]
            assert(codePt >= 0 and codePt <= MAXCHARCODE)
            comprDataPtrs[codePt] = ffi.cast(uint8_ptr_t, uMemMapPtr + currentOffset)
            entryDescRefs[codePt] = desc

            currentOffset = currentOffset + comprSize
        end

        assert(ffi.string(uMemMapPtr + currentOffset, MagicSize) == MAGIC)

        return {
            -- Must be held on to prevent GC -> munmap:
            ptr_ = memMapPtr,

            comprDataPtrs = comprDataPtrs,
            entryDescRefs = entryDescRefs,
        }
    end,

    renderUnsafe = function(self, codePoint, ptr, stride, pixFunc)
        local desc, data = self:descAndData(codePoint)
        assert(desc ~= nil, "no tile for given code point")
        Decode(data, desc.comprSize, desc.w, desc.h, ptr, stride, pixFunc)
        return desc.w
    end,

-- private:
    descAndData = function(self, codePoint)
        assert(type(codePoint) == "number", "argument #1 must be a number")

        local entryDescRef = self.entryDescRefs[codePoint]
        if (entryDescRef == nil) then
            return nil
        end

        local comprDataPtr = self.comprDataPtrs[codePoint]
        assert(comprDataPtr ~= nil)

        return entryDescRef, comprDataPtr
    end,
}

local function RGB565(r, g, b)
    return b + 32*g + 32*64*r
end

-- NOTE: this really belongs to the site where class 'Renderer' is used, but for
--  convenience, have it here for now.
local function CoverageToRGB565(cov)
    local gray = 31 - cov
    return RGB565(gray, 2*gray, gray)
end

api.Renderer = class
{
    function(fileName, fbMapping)
        checktype(fileName, 1, "string", 2)
        check(type(fbMapping) == "table" and type(fbMapping.yres_original) == "number",
              "argument #1 must be a Mapping object", 2)
        --     ^ ... as returned by framebuffer.lua's FrameBuffer:getMapping().

        local cpFile, msg = api.load(fileName)
        if (cpFile == nil) then
            error(msg)
        end

        check(cpFile:descAndData(MissingCharCode) ~= nil, "missing fallback tile", 2)
        check(fbMapping:getPixelSize() == 2, "only pixel size 2 supported (assumed to be RGB565)", 2)

        return {
            pics = cpFile,
            map = fbMapping,
        }
    end,

    drawChar = function(self, x, yForBaseline, proCodePoint)
        -- The passed coordinates have to be inside bounds.
        self.map:getPixelPointer(x, yForBaseline)

        checktype(proCodePoint, 3, "number", 2)

        local proDesc, proData = self.pics:descAndData(proCodePoint)

        local function getDescAndData()
            if (proDesc ~= nil) then
                return proDesc, proData
            else
                return self.pics:descAndData(MissingCharCode)
            end
        end

        local desc, data = getDescAndData()
        -- NOTE: we checked for the presence of the fallback tile in the constructor:
        assert(desc ~= nil and data ~= nil)

        local topY = yForBaseline - desc.baseline
        local botY = topY + desc.h
        local endX = x + desc.w

        if (not (x >= 0 and endX < self.map.xres and topY >= 0 and botY < self.map.yres)) then
            -- TODO: allow partial rendering
            return x, yForBaseline, yForBaseline
        end

        local ptr = self.map:getPixelPointer(x, topY)
        Decode(data, desc.comprSize, desc.w, desc.h, ptr, self.map.xres_virtual, CoverageToRGB565)

        return endX, topY, botY
    end,

    drawString = function(self, x, yForBaseline, interCharAdvanceX, str, codePtOffset)
        -- The passed coordinates have to be inside bounds.
        self.map:getPixelPointer(x, yForBaseline)

        checktype(interCharAdvanceX, 3, "number", 2)
        check(interCharAdvanceX >= 0 and interCharAdvanceX <= 100,
              "argument #3 must be in [0, 100]")
        checktype(str, 4, "string", 2)
        codePtOffset = (codePtOffset ~= nil) and codePtOffset or 0
        checktype(codePtOffset, 5, "number", 2)

        local curX = x
        local upperY, lowerY = math.huge, -math.huge

        for i = 1, #str do
            local ch = str:byte(i)
            local xx = (i == 1) and curX or curX + interCharAdvanceX

            local nextX, topY, botY = self:drawChar(xx, yForBaseline, ch + codePtOffset)

            upperY = math.min(upperY, topY)
            lowerY = math.max(lowerY, botY)
            curX = nextX
        end

        return curX, upperY, lowerY
    end,
}

local function GetMemoryMappedPtr(fileName, fileSize)
    local MAP, PROT = posix.MAP, posix.PROT

    local fd = ffi.C.open(fileName, posix.O.RDONLY)
    if (fd == -1) then
        return nil, "failed opening file: "..posix.getErrnoString()
    end

    local ptr = posix.mmap(nil, fileSize, PROT.READ, MAP.PRIVATE, fd, 0)
    ffi.C.close(fd)
    return ptr
end

function api.load(fileName)
    assert(type(fileName) == "string", "argument #1 must be a string")

    local f, msg = io.open(fileName)
    if (f == nil) then
        return nil, msg
    end

    local function failure(msg)
        f:close()
        return nil, msg
    end

    --== Read the header

    local headerStr = f:read(HeaderSize)
    if (headerStr == nil or #headerStr ~= HeaderSize) then
        return failure("failed reading header or it is too short")
    end

    local header = Header_t()
    ffi.copy(header, headerStr, HeaderSize)

    --== Check the header

    if (ffi.string(header.magic, MagicSize) ~= MAGIC) then
        return failure("leading magic marker does not match")
    elseif (header.version ~= VERSION) then
        return failure("version does not match")
    elseif (header.fileSize < HeaderSize + MagicSize) then
        -- TODO on demand: be even stricter. We could.
        return failure("file size in file is too small")
    end

    local offset = f:seek("end", -MagicSize)
    if (offset == nil) then
        -- The Lua docs do not document a failure case, but check regardless.
        -- (What if the file happened to be a pipe?)
        return failure("failed seeking to the end")
    elseif (offset ~= header.fileSize - MagicSize) then
        return failure("file size in file does not match actual one")
    end

    local tailMagic = f:read(MagicSize)
    if (tailMagic == nil) then
        return failure("failed reading trailing magic marker")
    elseif (ffi.string(tailMagic, MagicSize) ~= MAGIC) then
        return failure("trailing magic marker does not match")
    end

    --== All OK so far, let the class take over.

    f:close()

    local ptr = GetMemoryMappedPtr(fileName, header.fileSize)
    if (ptr == nil) then
        return failure("failed memory mapping file")
    end

    return CharPicsFile(ptr, header.fileSize)
end

-- 'artTab': [codePt] = { w=<width>, h=<height>, data=<cdata> }
-- Returns: sequence table with indexes into 'artTab' with overlarge dimensions.
function api.write(fileName, artTab)
    assert(type(fileName) == "string", "argument #1 must be a string")
    assert(type(artTab) == "table", "argument #2 must be a table")

    do
        local minTileNum, maxTileNum = art_table.validate(artTab)
        assert(minTileNum >= 0 and maxTileNum <= MAXCHARCODE,
               "Empty code point table, or some code points outside the allowed range")
        assert(minTileNum <= maxTileNum)
    end

    local codePoints = {}
    local droppedCodePoints = {}

    for codePt, tileTab in pairs(artTab) do
        local sx, sy, baseline = tileTab.w, tileTab.h, tileTab.baseline
        local isSideLenOk = (sx >= 0 and sx <= MAXSIDELEN and sy >= 0 and sy <= MAXSIDELEN)
        local isBaselineOk = (baseline >= MINBASELINE and baseline <= MAXBASELINE)
        local tabRef = (isSideLenOk and isBaselineOk) and codePoints
            or droppedCodePoints
        tabRef[#tabRef + 1] = codePt
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
        descRef.baseline = tileTab.baseline

        local comprEntry, comprSize = Encode(tileTab.data, w, h)
        comprEntries[i], descRef.comprSize = comprEntry, comprSize

        if (ValidateOnWrite) then
            local size = w * h
            local tempBuf = uint8_array_t(size)
            Decode(comprEntry, comprSize, w, h, tempBuf, w)

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

    return droppedCodePoints
end

-- Done!
return api
