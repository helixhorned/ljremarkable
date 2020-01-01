#!/usr/bin/env luajit

local ffi = require("ffi")

local bit = require("bit")
local io = require("io")
local math = require("math")
local os = require("os")

local class = require("class").class
local inet = require("inet")
local FrameBuffer = require("framebuffer").FrameBuffer
local posix = require("posix")

local assert = assert
local ipairs = ipairs
local print = print
local tonumber = tonumber
local type = type

local arg = arg
local stderr = io.stderr

local isClient = (arg[1] == "c")
local isServer = (arg[1] == "s")

-- TODO: auto-detect?
if (not (isClient or isServer)) then
    stderr:write("Usage:\n")
    -- TODO: rename this application to something more suitable than 'grabscreen'.
    stderr:write((" %s c  # on the Raspberry Pi\n"):format(arg[0]))
    stderr:write((" %s s  # on the reMarkable\n"):format(arg[0]))
    os.exit(1)
end

----------

-- TODO_MOVE
ffi.cdef[[
int memcmp(const void *s1, const void *s2, size_t n);
]]

local function currentTimeMs()
    local ts = posix.clock_gettime()
    return 1000 * tonumber(ts.sec) + tonumber(ts.nsec) / 1000000
end

----------

local fb = FrameBuffer(0, isServer)
local map = fb:getMapping()
local unpackPixel = map:getUnpackPixelFunc()
local fbPtr = map:getBasePointer()

local SourcePixelSize = ffi.sizeof("uint32_t")
local DestPixelSize = ffi.sizeof("uint16_t")
local HalfwordSize = ffi.sizeof("uint16_t")

-- Are we running the server on something other than the reMarkable?
local isDebugging = (isServer and map:getPixelSize() == SourcePixelSize)

do
    local expectedPixelSize = (isClient and SourcePixelSize or DestPixelSize)

    if (not isDebugging and map:getPixelSize() ~= expectedPixelSize) then
        stderr:write("ERROR: Unsupported pixel size.\n")
        os.exit(1)
    end
end

local PixelArray = ffi.typeof("$ [?]", map:getPixelType())
local NarrowArray = ffi.typeof("uint16_t [?]")

local size = map:getSize()

local shl, shr = bit.lshift, bit.rshift
local band, bor = bit.band, bit.bor

-- Convert to RGB565 represented as integer.
local function narrow(px)
    local r, g, b = unpackPixel(px)
    return bor(shr(r, 3), shl(shr(g, 2), 5), shl(shr(b, 3), 11))
end

---------- Sampling and comparison ----------

-- Default screen dimensions on the reMarkable.
local ScreenWidth_rM = 1404
local ScreenHeight_rM = 1872

-- Tiling on the source:
local SideLen = 8
local SquareSize = SideLen * SideLen  -- in pixels

-- Tiling on the target:
local BigSideLen = 2 * SideLen
local BigSquareSize = BigSideLen * BigSideLen

-- Encoding / decoding.
local IsRLE_Bit = bit.lshift(1, 15)
local IsDirect_Bit = bit.lshift(1, 14)
local EncodingType_Mask = IsRLE_Bit + IsDirect_Bit
local Count_Mask = IsDirect_Bit - 1
-- Encoding is per big tile, with the 16-bit header having the format (bit | count).
assert(BigSquareSize <= Count_Mask)

local uint32_t = ffi.typeof("uint32_t")

local function RoundToTarget(pixelLength)
    return tonumber(BigSideLen * uint32_t(pixelLength / BigSideLen))
end

local isRealServer = (isServer and not isDebugging)

if (isRealServer) then
    if (map.xres ~= ScreenWidth_rM or map.yres ~= ScreenHeight_rM) then
        stderr:write("ERROR: Unexpected framebuffer dimensions.\n")
        os.exit(1)
    end
end

-- TODO: rotate.
local targetXres = math.min(RoundToTarget(map.xres), RoundToTarget(ScreenWidth_rM))
local targetYres = math.min(RoundToTarget(map.yres), RoundToTarget(ScreenHeight_rM))
local targetSize = targetXres * targetYres

local srcTileCountX = targetXres / SideLen
local srcTileCountY = targetYres / SideLen
local destTileCountX = targetXres / BigSideLen
local destTileCountY = targetYres / BigSideLen
local totalDestTileCount = destTileCountX * destTileCountY

print(("INFO: rounded %spicture dimensions: %d x %d"):format(
      isRealServer and "" or "target ", targetXres, targetYres))
print(("INFO: Tiled dimensions:"))
if (not isRealServer) then
    print(("INFO:  source:      %3d x %3d = %5d tiles (side length %d)"):format(
          srcTileCountX, srcTileCountY, (targetXres * targetYres) / SquareSize, SideLen))
end
print(("INFO: %s%3d x %3d = %5d tiles (side length %d)"):format(
      not isRealServer and " destination: " or "",
      destTileCountX, destTileCountY, totalDestTileCount, BigSideLen))

local SBuf = ffi.typeof[[struct {
    static const int Current = 0;
    static const int Other = 1;
}]]

local Sampler = class
{
    function()
        assert(targetSize % SquareSize == 0)
        local sampleCount = targetSize / SquareSize

        local firstBuffer = PixelArray(sampleCount)
        -- Fill the first buffer with a value that initially will very likely invalidate all
        -- tiles. (NOTE: 0xff does not!)
        ffi.fill(firstBuffer, ffi.sizeof(firstBuffer), 0xfe)

        return {
            fbIndexes = {},
            sampleCount = sampleCount,
            currentBufIdx = 0,
            sampleBufs = {
                [0] = firstBuffer,
                [1] = PixelArray(sampleCount),
            }
        }
    end,

    generate = function(self)
        local idxs = {}

        -- NOTE: y first!
        for y = 0, targetYres - 1, SideLen do
            for x = 0, targetXres - 1, SideLen do
                -- TODO: randomly perturb sample positions? But, can only do that for the
                --  tiles that changed. Otherwise, we'd consider the whole screen changed in
                --  the next iteration, even if only a small part was changed.
                local xoff = SideLen / 2
                local yoff = SideLen / 2

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
            sampleBuf[i - 1] = fbPtr[self.fbIndexes[i]]
        end
    end,

    compare = function(self)
        local currentBuf = self:getBuffer(SBuf.Current)
        local otherBuf = self:getBuffer(SBuf.Other)

        local destTileCoords = {}

        -- NOTE: y first!
        for y = 0, destTileCountY - 1 do
            for x = 0, destTileCountX - 1 do
                -- NOTE: assumes that BigSideLen == 2 * SideLen:
                local sx, sy = 2*x, 2*y

                local srcSampleIdxs = {
                    srcTileCountX * sy + sx,
                    srcTileCountX * sy + sx + 1,
                    srcTileCountX * (sy + 1) + sx,
                    srcTileCountX * (sy + 1) + sx + 1,
                }

                local destTileChanged = false

                for _, si in ipairs(srcSampleIdxs) do
                    destTileChanged = destTileChanged or (currentBuf[si] ~= otherBuf[si])
                end

                if (destTileChanged) then
                    destTileCoords[#destTileCoords + 1] = { x=x, y=y }
                end
            end
        end

        return destTileCoords
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

local function UnpackDestTileCoord(coord)
    local x, y = coord.x, coord.y
    assert(x >= 0 and x < destTileCountX)
    assert(y >= 0 and y < destTileCountY)
    return x, y
end

local function CopyBigTileFromScreen(destPtr, coord)
    local tx, ty = UnpackDestTileCoord(coord)
    local sx, sy = BigSideLen * tx, BigSideLen * ty

    for i = 0, BigSideLen - 1 do
        local srcOffset = map:getLinearIndex(sx, sy + i)
        ffi.copy(destPtr + BigSideLen * i, fbPtr + srcOffset,
                 BigSideLen * SourcePixelSize)
    end
end

local function EncodeBigTile(tilePtr, codedBuf, offset)
    local codedBufSize = ffi.sizeof(codedBuf) / DestPixelSize
    local oldOffset = offset

    -- Check that we have room for one worst-case scenario.
    assert(offset + 1 + BigSquareSize <= codedBufSize)

    local isDirect = false  -- force new header on first encounter of direct case
    local curHeaderOffset = -1  -- direct case only

    local lastPixIdx = 0
    local curValue = tilePtr[lastPixIdx]

    -- For every pixel of the big tile...
    for i = 1, BigSquareSize do
        if (i == BigSquareSize or tilePtr[i] ~= curValue) then
            local runLength = i - lastPixIdx

            assert(runLength <= Count_Mask)

            if (runLength >= 4) then
                -- Run-length encoding.
                assert(offset + 2 <= codedBufSize)
                -- Always have a separate header.
                codedBuf[offset + 0] = IsRLE_Bit + runLength
                codedBuf[offset + 1] = curValue
                offset = offset + 2

                isDirect = false
            else
                -- Direct encoding.
                if (not isDirect) then
                    assert(offset < codedBufSize)
                    -- Need a new header.
                    codedBuf[offset] = IsDirect_Bit + runLength
                    curHeaderOffset = offset
                    offset = offset + 1
                else
                    -- Update the current header.
                    local hdrOff = curHeaderOffset
                    assert(hdrOff >= 0 and hdrOff < offset)
                    local headerValue = codedBuf[hdrOff]
                    assert(bit.band(headerValue, EncodingType_Mask) == IsDirect_Bit)
                    local oldCount = bit.band(headerValue, Count_Mask)
                    assert(oldCount + runLength <= Count_Mask)
                    codedBuf[hdrOff] = IsDirect_Bit + (oldCount + runLength)
                end

                assert(offset + runLength <= codedBufSize)

                for pi = 0, runLength - 1 do
                    codedBuf[offset + pi] = curValue
                end

                offset = offset + runLength
                isDirect = true
            end

            lastPixIdx = i
            curValue = tilePtr[i]
        end
    end

    -- Check that we added not more than the worst case worth of data.
    assert(offset - oldOffset <= 1 + BigSquareSize)

    return offset
end

local function checkData(cond, errMsg)
    assert(type(cond) == "boolean")
    assert(type(errMsg) == "string")

    if (not cond) then
        stderr:write(("Data validation error: %s\n"):format(errMsg))
        os.exit(1)
    end
end

local function DecodeUpdates(inBuf, length, outBuf)
    local inBufSize = ffi.sizeof(inBuf) / DestPixelSize
    local outBufSize = ffi.sizeof(outBuf) / DestPixelSize

    checkData(length > 0 and length <= inBufSize, "invalid length")

    local srcOff, dstOff = 0, 0

    while (srcOff < length) do
        local header = inBuf[srcOff]

        local encodingType = bit.band(header, EncodingType_Mask)
        checkData(encodingType == IsRLE_Bit or encodingType == IsDirect_Bit,
                  "invalid header")

        local count = bit.band(header, Count_Mask)
        checkData(count > 0 and dstOff + count <= outBufSize, "invalid count 1")

        if (encodingType == IsDirect_Bit) then
            -- Direct encoding.
            checkData(srcOff + 1 + count <= inBufSize, "invalid count 2")

            for i = 0, count - 1 do
                outBuf[dstOff + i] = inBuf[srcOff + 1 + i]
            end

            srcOff = srcOff + 1 + count
        else
            -- Run-length encoding.
            checkData(srcOff + 1 < inBufSize, "invalid count 3")

            local value = inBuf[srcOff + 1]

            for i = 0, count - 1 do
                outBuf[dstOff + i] = value
            end

            srcOff = srcOff + 1 + 1
        end

        dstOff = dstOff + count
    end

    checkData(srcOff == length, "invalid decoding 1")
    checkData(dstOff > 0 and dstOff % BigSquareSize == 0, "invalid decoding 2")

    return dstOff / BigSquareSize
end

---------- Connection-related ----------

-- Wired reMarkable IP address.
-- Mnemonic for the port number is "P2R" ("Pi to reMarkable").
local Port = 16218
local AddrAndPort = {10,11,99,1; Port}

local Magic = "UpDatE"

assert(totalDestTileCount <= 65536, "too high screen resolution")
-- NOTE: what matters is just that they are the same on both ends,
--  since we do not do any endianness conversions.
assert(ffi.abi("le"), "unexpected architecture")

local Header_t = ffi.typeof[[struct {
    char magic[6];
    uint16_t changedTileCount;
    uint32_t encodingLength;
}]]

local coord_t = ffi.typeof[[struct {
    uint16_t x, y;
}]]

local coord_array_t = ffi.typeof("$ [?]", coord_t)

----------

local function AttemptConnectTo(socket, addrAndPort, displayName)
    local connFd, errMsg = socket:initiateConnection(AddrAndPort)

    if (connFd == nil) then
        stderr:write(("INFO: failed connecting to the %s: %s\n"):format(
                         displayName, errMsg))
    else
        stderr:write(("INFO: connected to the %s\n"):format(displayName))
    end

    return connFd
end

local Client = class
{
    function()
        local sampler = Sampler()
        sampler:generate()

        local socket = inet.Socket()
        local connFd = AttemptConnectTo(socket, AddrAndPort, "reMarkable") or
            AttemptConnectTo(socket, {127,0,0,1; Port}, "local host")

        return {
            sampler = sampler,

            tempBuf = PixelArray(targetSize),

            -- Updates in raw form:
            updateBuf = NarrowArray(targetSize),

            -- Run-length encoded updates. Encoding is per big tile.
            -- The format is sequence of items made up of halfword (16-bit) values:
            --
            --  [0] = (bit | count)  // header
            --
            --  if bit == IsDirect_Bit:
            --      itemCount := count
            --      [1 .. itemCount] = pixelValue
            --- if bit == IsRLE_Bit:
            --      repeatCount := count
            --      [1] = pixelValue
            --
            --  A run of pixel values is only encoded if <repeatCount> is at least 4, so
            --  that encoding always produces less data than directly writing out the pixel
            --  values and an additional header.
            --
            -- Worst cases:
            --  * uncompressible -> 1 + BigSquareSize halfwords
            --  * any combination of non-RLE and RLE sequences: not worse than uncompressed
            --    by construction
            --
            codedBuf = NarrowArray(totalDestTileCount * (1 + BigSquareSize)),

            connFd = connFd,

            -- For debugging (non-connected run) only:
            decodedBuf = NarrowArray(targetSize),
        }
    end,

    step = function(self)
        self.sampler:sample()
        local destTileCoords = self.sampler:compare()

        if (#destTileCoords > 0) then
            self:captureUpdates(destTileCoords)
            local encodingLength = self:encodeUpdates(#destTileCoords)

            if (self.connFd ~= nil) then
                self:sendUpdates(destTileCoords, encodingLength)
            else
                local fmt = "changed, #destTiles=%d, encoded=%d halfwords (factor = %.03f)\n"
                stderr:write(fmt:format(#destTileCoords, encodingLength,
                                        #destTileCoords * BigSquareSize / encodingLength))

                local tileCount = DecodeUpdates(self.codedBuf, encodingLength, self.decodedBuf)
                -- Check correctness of decoding.
                assert(tileCount == #destTileCoords)
                assert(ffi.C.memcmp(self.updateBuf, self.decodedBuf,
                                    tileCount * BigSquareSize * DestPixelSize) == 0)
            end
        end

        posix.clock_nanosleep(250e6)
    end,

-- private:
    captureUpdates = function(self, destTileCoords)
        local updatedTileCount = #destTileCoords
        assert(updatedTileCount <= totalDestTileCount)

        for i, coord in ipairs(destTileCoords) do
            assert(i > 0 and i <= updatedTileCount)
            CopyBigTileFromScreen(self.tempBuf + BigSquareSize * (i - 1), coord)
        end

        local updatedPixelCount = updatedTileCount * BigSquareSize

        for i = 0, updatedPixelCount - 1 do
            self.updateBuf[i] = narrow(self.tempBuf[i])
        end
    end,

    encodeUpdates = function(self, updatedTileCount)
        assert(updatedTileCount <= totalDestTileCount)

        local updateBuf, codedBuf = self.updateBuf, self.codedBuf
        local offset = 0

        for tileIdx = 0, updatedTileCount - 1 do
            offset = EncodeBigTile(updateBuf + BigSquareSize * tileIdx,
                                   codedBuf, offset)
        end

        return offset
    end,

    sendUpdates = function(self, destTileCoords, encodingLength)
        local connFd = self.connFd
        assert(connFd ~= nil)

        local changedTileCount = #destTileCoords
        local header = Header_t(Magic, changedTileCount, encodingLength)

        connFd:writeFull(header)

        -- TODO: store 'destTileCoords' in an FFI array in the first place.
        local coords = coord_array_t(changedTileCount)

        for i, coord in ipairs(destTileCoords) do
            coords[i - 1] = coord_t(coord.x, coord.y)
        end

        connFd:writeFull(coords)

        connFd:writeFull(self.codedBuf, encodingLength * HalfwordSize)
    end,
}

local Server = class
{
    function()
        local connFd, errMsg = inet.Socket():expectConnection(Port)
        if (connFd == nil) then
            stderr:write(("ERROR: failed connecting: %s\n"):format(errMsg))
            os.exit(1)
        end

        return {
            connFd = connFd,

            decodedBuf = NarrowArray(targetSize),
        }
    end,

    step = function(self)
        self:receiveUpdates()
    end,

-- private:
    receiveUpdates = function(self)
        local connFd = self.connFd

        local header = connFd:readInto(Header_t(), false)
        checkData(ffi.string(header.magic, #Magic) == Magic, "magic bytes mismatch")

        -- TODO: preallocate the two arrays?

        local changedTileCount = header.changedTileCount
        local coords = connFd:readInto(coord_array_t(changedTileCount), false)

        local encodingLength = header.encodingLength
        local encodedData = connFd:readInto(NarrowArray(header.encodingLength), false)

        local tileCount = DecodeUpdates(encodedData, encodingLength, self.decodedBuf)
        checkData(tileCount == changedTileCount, "corrupt encoding: tile count mismatch")

        -- TODO: apply etc.
    end,
}

local app = isClient and Client() or Server()

----------

while (true) do
    local startMs = currentTimeMs()
    app:step()
    io.stdout:write(("%.0f ms\n"):format(currentTimeMs() - startMs))
end
