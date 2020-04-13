#!/usr/bin/env luajit

local ffi = require("ffi")

local bit = require("bit")
local io = require("io")
local math = require("math")
local os = require("os")

local class = require("class").class
local inet = require("inet")
local input = require("input")
local FrameBuffer = require("framebuffer").FrameBuffer
local JKissRng = require("jkiss_rng").JKissRng
local posix = require("posix")

local EV = input.EV
local POLL = posix.POLL

local assert = assert
local error = error
local ipairs = ipairs
local pairs = pairs
local pcall = pcall
local print = print
local require = require
local tonumber = tonumber
local tostring = tostring
local type = type
local unpack = unpack

local arg = arg
local stderr = io.stderr

local isClient = (arg[1] == "c")
local isServer = (arg[1] == "s")
local hostNameOrAddr = arg[2]

local function errprintfAndExit(fmt, ...)
    stderr:write((fmt.."\n"):format(...))
    os.exit(1)
end

if (not ((isClient and hostNameOrAddr ~= nil) or (isServer and hostNameOrAddr == nil))) then
    errprintfAndExit([[
Usage:
  %s c <host name or IPv4 address>  # on the Raspberry Pi
  %s s                              # on the reMarkable

A passed host name is resolved by reading /etc/hosts.
]], arg[0], arg[0])
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
        errprintfAndExit("ERROR: Unsupported pixel size.")
    end

    local vi = fb:getVarInfo()

    if (not (vi.blue.offset < vi.green.offset and vi.green.offset < vi.red.offset)) then
        errprintfAndExit("ERROR: Unexpected pixel component order.")
    end
end

local PixelArray = ffi.typeof("$ [?]", map:getPixelType())
local NarrowArray = ffi.typeof("uint16_t [?]")
local UInt32Array = ffi.typeof("uint32_t [?]")

local shl, shr = bit.lshift, bit.rshift
local band, bor = bit.band, bit.bor

-- Convert to RGB565 represented as integer.
-- NOTE: The order R-G-B is to be understood as from most to least significant bit.
--  (Thus, it is reverse to the order in terms of bit shift counts.)
local function narrow(px)
    local r, g, b = unpackPixel(px)
    return bor(shr(b, 3), shl(shr(g, 2), 5), shl(shr(r, 3), 11))
end

---------- Sampling and comparison ----------

-- Default screen dimensions on the reMarkable.
local ScreenWidth_rM = 1404
local ScreenHeight_rM = 1872
-- Guesstimate for the side length of the touch-active region around the rM "eye".
local EyeSize_rM = 128

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
        errprintfAndExit("ERROR: Unexpected framebuffer dimensions.")
    end
end

local targetXres = math.min(RoundToTarget(map.xres), RoundToTarget(ScreenWidth_rM))
local targetYres = math.min(RoundToTarget(map.yres), RoundToTarget(ScreenHeight_rM))
local targetSize = targetXres * targetYres

-- We're OK with a portion of the screen clipped on the top, but want it whole on the bottom
-- where the menu bar is located.
local globalSrcYOffset = -map.yres % BigSideLen
assert(globalSrcYOffset >= 0 and globalSrcYOffset < BigSideLen)
-- Make sure we do not overwrite the "eye" on the reMarkable.
local DestYPixelOffset = EyeSize_rM
assert(DestYPixelOffset % BigSideLen == 0)  -- see usage for why

-- X position of the rectangle signaling certain status conditions.
local StatusRectPosX = {
    OnError = EyeSize_rM,
    OnLockedUp = ScreenWidth_rM - EyeSize_rM,
}

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

-- Throttling: Destination tiles for which updates happen more frequently than this are
-- tentatively held back...
local MinSlowChangeTime = 100
-- ... until at most this much time has passed between last sending an update and now:
local MaxHoldBackTime = 1000

local Sampler = class
{
    function()
        assert(targetSize % SquareSize == 0)
        local sampleCount = targetSize / SquareSize

        local screenCopyBuf = PixelArray(map:getRawSize())
        -- Fill with a value that initially will very likely invalidate all
        -- tiles. (NOTE: 0xff does not!)
        ffi.fill(screenCopyBuf, ffi.sizeof(screenCopyBuf), 0xfe)

        return {
            rng = JKissRng(),
            sampleCount = sampleCount,
            sampleBufs = {
                PixelArray(sampleCount),
                PixelArray(sampleCount),
            },

            lastChangedTimes = UInt32Array(totalDestTileCount),
            lastSentTimes = UInt32Array(totalDestTileCount),

            -- exposed to Client
            screenCopyBuf = screenCopyBuf,
        }
    end,

    sampleAndCompare = function(self)
        local fbSampleBuf = self.sampleBufs[1]
        local scSampleBuf = self.sampleBufs[2]
        local screenCopyBuf = self.screenCopyBuf

        -- Generate sample indexes.
        local fbIndexes = self:generate()

        -- Sample.
        for i = 1, self.sampleCount do
            fbSampleBuf[i - 1] = fbPtr[fbIndexes[i]]
            scSampleBuf[i - 1] = screenCopyBuf[fbIndexes[i]]
        end

        -- Compare sample pixel values with current state!

        local currentTime = currentTimeMs()
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

                local destTileIdx = destTileCountX * y + x
                local destTileChanged = false

                for _, si in ipairs(srcSampleIdxs) do
                    destTileChanged = destTileChanged or (fbSampleBuf[si] ~= scSampleBuf[si])
                end

                local lastChangedTime = self.lastChangedTimes[destTileIdx]
                local lastSentTime = self.lastSentTimes[destTileIdx]

                local isOutdated = destTileChanged or (lastSentTime < lastChangedTime)
                local wantHoldBack = (currentTime - lastChangedTime < MinSlowChangeTime)
                local canHoldBack = wantHoldBack and (currentTime - lastSentTime <= MaxHoldBackTime)
                local shouldSend = isOutdated and not canHoldBack

                if (destTileChanged) then
                    self.lastChangedTimes[destTileIdx] = currentTime
                end

                if (shouldSend) then
                    self.lastSentTimes[destTileIdx] = currentTime

                    local lastCoords = destTileCoords[#destTileCoords]
                    if (lastCoords ~= nil and lastCoords.y == y) then
                        for xx = lastCoords.x + 1, x - 1 do
                            destTileCoords[#destTileCoords + 1] = { x=xx, y=y }
                        end
                    end

                    destTileCoords[#destTileCoords + 1] = { x=x, y=y }
                end
            end
        end

        return destTileCoords
    end,

-- private:
    generate = function(self)
        local idxs = {}

        -- NOTE: y first!
        for y = 0, targetYres - 1, SideLen do
            for x = 0, targetXres - 1, SideLen do
                -- Randomly perturb sample positions.
                local xoff = self.rng:getu32() % SideLen
                local yoff = globalSrcYOffset + self.rng:getu32() % SideLen

                idxs[#idxs + 1] = map:getLinearIndex(x + xoff, y + yoff)
            end
        end

        assert(#idxs == self.sampleCount)
        return idxs
    end,
}

local function UnpackDestTileCoord(coord)
    local x, y = coord.x, coord.y
    assert(x >= 0 and x < destTileCountX)
    assert(y >= 0 and y < destTileCountY)
    return x, y
end

local CopyFormat = ffi.typeof[[struct {
    static const int Same = 1;
    static const int Packed = 2;
}]]

local function CopyBigTile(srcPtr, destPtr, coord, copyFormat)
    local keepFormat = (copyFormat == CopyFormat.Same)
    assert(keepFormat or copyFormat == CopyFormat.Packed)

    local tx, ty = UnpackDestTileCoord(coord)
    local sx = BigSideLen * tx
    local sy = BigSideLen * ty + globalSrcYOffset

    for i = 0, BigSideLen - 1 do
        local srcOffset = map:getLinearIndex(sx, sy + i)
        local destOffset = keepFormat and srcOffset or BigSideLen * i
        ffi.copy(destPtr + destOffset, srcPtr + srcOffset,
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
        error(("Data validation error: %s\n"):format(errMsg))
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

-- Mnemonic for the port number is "P2R" ("Pi to reMarkable").
local Port = 16218

-- Client -> server
local UpdateMagic = "UpDatE"

assert(totalDestTileCount <= 65536, "too high screen resolution")
-- NOTE: what matters is just that they are the same on both ends,
--  since we do not do any endianness conversions.
assert(ffi.abi("le"), "unexpected architecture")

-- Server -> client
local Cmd = {
    Length = 4,

    Enable = "Enbl",
    Disable = "Dsbl",
    Input = "Inpt",
    Ok = "_Ok_",
}

local UpdateHeader_t = ffi.typeof[[struct {
    char magic[6];
    uint16_t changedTileCount;
    uint32_t encodingLength;
}]]

local coord_t = ffi.typeof[[struct {
    uint16_t x, y;
}]]

local coord_array_t = ffi.typeof("$ [?]", coord_t)

local OurEventType = {
    SingleClick = 1,
    Drag = 2,
}

-- The amount of y travelled for sending one wheel up/down (9: 1mm).
local SingleWheelRollDistance = 81

local Button = {
    -- KEEPINSYNC with 'xdotool click' mouse button numbers:
    Left = 1,
    Middle = 2,
    Right = 3,
    WheelUp = 4,
    WheelDown = 5,

    -- These are for ourselves, 'xdotool' does not know them:
    VerticalDrag = 254,
    GenericDrag = 255
}

local OurEventDesc = {
    [OurEventType.SingleClick] = "single click",
    [OurEventType.Drag] = "drag",
}

local OurEvent_t = ffi.typeof[[struct {
    uint8_t ourType;
    uint8_t button;
    uint16_t x, y;
    uint16_t nx, ny;  // Drag only
}]]

----------

local function ConnectTo(socket, addrAndPort, displayName)
    local connFd, errMsg = socket:initiateConnection(addrAndPort)

    if (connFd == nil) then
        errprintfAndExit("ERROR: failed connecting to host %s: %s",
                         displayName, errMsg)
    end

    stderr:write(("INFO: connected to host %s\n"):format(displayName))

    return connFd
end

local function CheckCmdLength(length)
    if (length == 0) then
        errprintfAndExit("Connection to peer closed.")
    end

    assert(length == Cmd.Length, "unexpected command length")
end

local BackoffStepCount = 5
local MaxSleepTime = 500e3 -- microseconds
assert(MaxSleepTime < 1e6)  -- see usage for why

local timeval_t = ffi.typeof("struct timeval")

-- INPUT_EVENT_COORD_CONVERSION step 3
-- NOTE: Must be called on the client...
local function ConvertScreenToClient(sx, sy)
    local cx = sx
    local cy = sy - DestYPixelOffset
        + globalSrcYOffset  -- ...because of this!

    return cx, cy
end

local function InvokeXDoTool(commands)
    local whoami = posix.fork()

    if (whoami == "child") then
        assert(type(commands) == "table")
        posix.exec("/usr/bin/xdotool", commands)
    end
end


local MaxInputEvents = 1024
local sizeof_input_event = ffi.sizeof("struct input_event")
local input_event_array_t = ffi.typeof("struct input_event [?]")

local function ReadEvents(evd, inputBuf, maxAllowedSynValue)
    local events, bytesRead = evd.fd:readInto(inputBuf, true)
    assert(bytesRead % sizeof_input_event == 0)
    local eventCount = tonumber(bytesRead) / sizeof_input_event
    assert(eventCount > 0, "unexpected empty read")
    assert(eventCount < MaxInputEvents, "input event buffer overflow")

    local lastEvent = events[eventCount - 1]
    assert(lastEvent.type == EV.SYN and lastEvent.code == 0,
           "last read event is unexpectedly not a SYN_REPORT")
    assert(lastEvent.value <= maxAllowedSynValue,
           "unexpected value for final SYN_REPORT")

    return events, eventCount
end

local function GetAddress(nameOrQuad)
    assert(type(nameOrQuad) == "string")
    local IPAddressPattern = "([0-9]+)%.([0-9]+)%.([0-9]+)%.([0-9]+)"

    local a, b, c, d = nameOrQuad:match('^'..IPAddressPattern..'$')
    if (a ~= nil) then
        if (bit.bor(a, b, c, d) >= 256) then
            errprintfAndExit("ERROR: Invalid IPv4 address")
        end

        return {tonumber(a), tonumber(b), tonumber(c), tonumber(d)}
    end

    local HostNamePattern = "([a-z0-9][a-z0-9-]*)"

    if (nameOrQuad:match('^'..HostNamePattern..'$') == nil) then
        -- See 'man 7 hostname'.
        errprintfAndExit("ERROR: Invalid host name, must match "..HostNamePattern:sub(2, -2))
    end

    for line in io.lines("/etc/hosts") do
        local a, b, c, d, name = line:match('^'..IPAddressPattern.."\t"..HostNamePattern..'$')
        if (a ~= nil and name == nameOrQuad) then
            return {tonumber(a), tonumber(b), tonumber(c), tonumber(d)}
        end
    end

    errprintfAndExit("ERROR: Host name not found in /etc/hosts")
end

local function FindKeyboardDevFile()
    local Directory = "/dev/input/by-path"
    local dir = posix.Dir(Directory)

    while (true) do
        local fileName = dir:read()
        if (fileName == nil) then
            -- TODO: make optional
            errprintfAndExit("ERROR: no keyboard found")
        elseif (fileName:sub(-10,-1) == "-event-kbd") then
            return Directory.."/"..fileName
        end
    end
end

local Client = class
{
    function()
        local address = GetAddress(hostNameOrAddr)
        address[#address + 1] = Port

        local kbEvDevice = input.EventDevice(FindKeyboardDevFile())
        local connFd = ConnectTo(inet.Socket(), address, hostNameOrAddr)

        return {
            enabled = false,
            sampler = nil,  -- Sampler
            emptyStepCount = 0,

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

            kbEvDevice = kbEvDevice,
            inputBuf = input_event_array_t(MaxInputEvents),

            -- For debugging (non-connected run) only:
            decodedBuf_ = (connFd == nil) and NarrowArray(targetSize) or nil,
        }
    end,

    step = function(self)
        self:enable()

        local destTileCoords = self.sampler:sampleAndCompare()

        if (#destTileCoords > 0) then
            self:captureUpdates(destTileCoords, self.sampler.screenCopyBuf)
            local encodingLength = self:encodeUpdates(#destTileCoords)

            if (self.connFd ~= nil) then
                self:sendUpdates(destTileCoords, encodingLength)
            else
                local fmt = "changed, #destTiles=%d, encoded=%d halfwords (factor = %.03f)\n"
                stderr:write(fmt:format(#destTileCoords, encodingLength,
                                        #destTileCoords * BigSquareSize / encodingLength))
                local tileCount = DecodeUpdates(self.codedBuf, encodingLength, self.decodedBuf_)
                -- Check correctness of decoding.
                assert(tileCount == #destTileCoords)
                assert(ffi.C.memcmp(self.updateBuf, self.decodedBuf_,
                                    tileCount * BigSquareSize * DestPixelSize) == 0)
            end

            self.emptyStepCount = 0
        else
            self.emptyStepCount = self.emptyStepCount + 1
        end

        -- Now, wait for the first of:
        --  * data ready to be read from the socket connected to the server, or
        --  * a timeout expiration.

        local sleepFactor =
            (2 ^ math.min(self.emptyStepCount, BackoffStepCount) - 1) /
            (2 ^ BackoffStepCount)

        local sleepTime = sleepFactor * MaxSleepTime
        local kbEvDevFd = self.kbEvDevice:getRawFd()

        local waitFds = {
            self.connFd and self.connFd.fd or -1,
            kbEvDevFd,
        }

        local fdSet = posix.fd_set_t()

        for _, fd in ipairs(waitFds) do
            if (fd >= 0) then
                fdSet:set(fd)
            end
        end

        local readyFdCount = ffi.C.select(math.max(unpack(waitFds)) + 1, fdSet, nil, nil,
                                          timeval_t(0, sleepTime))
        assert(readyFdCount >= 0, "unexpected system call error")

        if (readyFdCount > 0) then
            assert(readyFdCount <= #waitFds)

            if (fdSet:isSet(kbEvDevFd)) then
                ReadEvents(self.kbEvDevice, self.inputBuf, 1)
            end

            if (self.connFd and fdSet:isSet(self.connFd.fd)) then
                -- TODO DISABLE_VIA_SERVER_INPUT: also allow Cmd.Disable
                self:receiveFromServerAndHandle(Cmd.Input)
            end
        end
    end,

-- private:
    captureUpdates = function(self, destTileCoords, screenCopyBuf)
        local updatedTileCount = #destTileCoords
        assert(updatedTileCount <= totalDestTileCount)

        for i, coord in ipairs(destTileCoords) do
            assert(i > 0 and i <= updatedTileCount)
            CopyBigTile(fbPtr, screenCopyBuf, coord, CopyFormat.Same)
            local destPtr = self.tempBuf + BigSquareSize * (i - 1)
            CopyBigTile(screenCopyBuf, destPtr, coord, CopyFormat.Packed)
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
        local header = UpdateHeader_t(UpdateMagic, changedTileCount, encodingLength)

        connFd:writeFull(header)

        -- TODO: store 'destTileCoords' in an FFI array in the first place.
        local coords = coord_array_t(changedTileCount)

        for i, coord in ipairs(destTileCoords) do
            coords[i - 1] = coord_t(coord.x, coord.y)
        end

        connFd:writeFull(coords)

        connFd:writeFull(self.codedBuf, encodingLength * HalfwordSize)

        self:receiveFromServerAndHandle()
    end,

    enable = function(self)
        if (not self.enabled) then
            -- Wait for the server to allow sending it screen updates.
            local cmd = self:readCommand()
            checkData(cmd == Cmd.Enable, "unexpected command")

            self.enabled = true
            self.sampler = Sampler()
        end
    end,

    receiveFromServerAndHandle = function(self, singleAllowedCommand)
        assert(self.enabled)

        while (true) do
            local cmd = self:readCommand()

            checkData(singleAllowedCommand == nil or cmd == singleAllowedCommand,
                      "unexpected command")

            if (cmd == Cmd.Input) then
                -- NOTE SEND_INPUT_FIRST: sent by the server before a Disable or Ok!
                local ourEvent = self.connFd:readInto(OurEvent_t(), false)
                local cx, cy = ConvertScreenToClient(ourEvent.x, ourEvent.y)
                local cnx, cny = ConvertScreenToClient(ourEvent.nx, ourEvent.ny)

                local function isInScreenBounds(x, y)
                    return x >= 0 and x < targetXres and
                        y >= globalSrcYOffset and y < targetYres
                end

                if (cy >= targetYres and cny < globalSrcYOffset) then
                    -- Drag across the Pi screen from below it to above it: resend picture.
                    if (ourEvent.ourType == OurEventType.Drag) then
                        if (self.sampler ~= nil) then
                            self.sampler = Sampler()
                        end
                    end
                elseif (isInScreenBounds(cx, cy)) then
                    if (ourEvent.ourType == OurEventType.SingleClick) then
                        checkData(ourEvent.button >= Button.Left and ourEvent.button <= Button.Right,
                                  "unexpected server input event: unexpected button")

                        InvokeXDoTool{
                            "mousemove", tostring(cx), tostring(cy),
                            "click", tostring(ourEvent.button),
                            -- NOTE: to have feedback, deliberately no 'mousemove restore'.
                        }
                    elseif (ourEvent.ourType == OurEventType.Drag) then
                        if (ourEvent.button == Button.VerticalDrag) then
                            -- Emulate "dragging the page" like on a tablet by sending mouse
                            -- wheel up/down. Ideally, we would somehow issue the source and
                            -- destination coordinates directly to X. But there does not seem to
                            -- be a way to tell graphical programs to process them the way we
                            -- want generically (i.e. across a wide range of programs). Issuing
                            -- wheel up/down seems like a reasonable lowest common denominator.
                            local button = (cny > cy) and Button.WheelUp or Button.WheelDown
                            local repeatCount = math.floor(math.abs(cny - cy) / SingleWheelRollDistance)

                            if (repeatCount >= 1) then
                                InvokeXDoTool{
                                    "mousemove", tostring(cx), tostring(cy),
                                    "click", "--repeat", tostring(repeatCount), "--delay", "1", tostring(button),
                                    "mousemove", "restore"
                                }
                            end
                        elseif (ourEvent.button == Button.GenericDrag) then
                            if (isInScreenBounds(cnx, cny)) then
                                -- Emulate a drag with the left mouse button clicked.
                                InvokeXDoTool{
                                    "mousemove", tostring(cx), tostring(cy),
                                    "mousedown", tostring(Button.Left),
                                    "mousemove", tostring(cnx), tostring(cny),
                                    "mouseup", tostring(Button.Left)
                                }
                            end
                        else
                            checkData("unexpected server input event: unexpected drag type")
                        end
                    else
                        checkData(false, "unexpected server input event: invalid type")
                    end
                end

                if (singleAllowedCommand) then
                    -- We do not know if there is another event coming (most likely no)
                    -- and would block if no, so return.
                    -- TODO: make this (distinction of the two usage cases) prettier.
                    return
                end
            elseif (cmd == Cmd.Disable) then
                self.enabled = false
                self.sampler = nil
                return
            elseif (cmd == Cmd.Ok) then
                return
            else
                checkData(false, "unexpected command")
            end
        end
    end,

    readCommand = function(self)
        if (self.connFd == nil) then
            -- Just get the simulation going.
            return Cmd.Enable
        end

        -- NOTE: partial reads may happen, but so far observed only when peer is gone.
        --  (And then presumably with return value 0.)
        local cmd = self.connFd:read(Cmd.Length)
        CheckCmdLength(#cmd)
        return cmd
    end,
}

local EyeData = nil

if (isRealServer) then
    local fd = ffi.C.open("rM_ul_eye_menu_hidden_46-28.dat", posix.O.RDONLY)
    if (fd == -1) then
        errprintfAndExit("ERROR: failed opening 'eye' screenshot data.")
    end

    fd = posix.Fd(fd)

    local pixelCount = 28 * 28
    EyeData = ffi.new("uint16_t [?]", pixelCount)
    fd:readInto(EyeData, false)
end

local function IsScreenDesired()
    assert(isServer)

    if (isRealServer) then
        local array = map:readRect(46, 46, 28, 28)
        assert(ffi.sizeof(array) == ffi.sizeof(EyeData))
        -- "Quick mode": we only wish that the Raspberry Pi screen is displayed if the main
        --  toolbar on the left hand side (when the rM is vertical) is hidden. Check this by
        --  comparing the pixels of the "eye".
        -- NOTE: we must take care not to compare too much around the "eye" since it becomes
        --  translucent from a certain radius onward.
        -- TODO: implement "marker document" mode.
        return (ffi.C.memcmp(array, EyeData, ffi.sizeof(EyeData)) == 0)
    else
        local Period = 5000
        return (uint32_t(currentTimeMs() / Period) % 2 == 0)
    end
end

local RM = isRealServer and require("remarkable") or nil
local xywh_t = RM and RM.xywh or nil

local RectSet = class
{
    function()
        return {
            rects = {}
        }
    end,

    add = function(self, rect)
        assert(ffi.istype(xywh_t, rect))

        local rects = self.rects
        rects[#rects + 1] = self.MergeHorizontally(rects[#rects], rect)
    end,

    finish = function(self)
        local oldRects = self.rects
        local newRects = {}

        for _, rect in ipairs(oldRects) do
            newRects[#newRects + 1] = self.MergeVertically(newRects[#newRects], rect)
        end

        self.rects = newRects
    end,

    getRects = function(self)
        return self.rects
    end,

-- static private:
    MergeHorizontally = function(lastRect, rect)
        if (lastRect ~= nil) then
            if (lastRect.y == rect.y and lastRect.h == rect.h and
                    rect.x == lastRect.x + lastRect.w) then
                -- Merge last update rect with incoming adjacent (next right) one.
                lastRect.w = lastRect.w + rect.w
                return nil
            end
        end

        return xywh_t(rect)
    end,

    MergeVertically = function(lastRect, rect)
        if (lastRect ~= nil) then
            if (lastRect.x == rect.x and lastRect.w == rect.w and
                    rect.y == lastRect.y + lastRect.h) then
                -- Merge last update rect with incoming adjacent (next below) one.
                lastRect.h = lastRect.h + rect.h
                return nil
            end
        end

        return xywh_t(rect)
    end,
}

--== All times in milliseconds.
local MaxSingleClickDuration = 500
-- Time that an initial tap must be held to commence generic dragging:
local GenericDragTapWaitTime = 500
-- Time after which input is considered to be "locked up":
local LockedUpDuration = 10000

local MaxSingleClickDeviation = 9  -- in delta x/y rM screen coordinates (9: 1 mm)

local Stage = {
    None = 0,
    Prefix = 1,
    Finished = 2,
}

-- Resolution of multitouch device.
local MtRes = {
    w = 0,
    h = 0,
}

-- Computed together with 'MtRes'.
local MultiTouchScreenUnitRatio

-- INPUT_EVENT_COORD_CONVERSION step 1
local function ObtainMultiTouchCoordRange(evd)
    local xRange = evd:ioctl(input.EVIOC.GABS.X)
    local yRange = evd:ioctl(input.EVIOC.GABS.Y)

    assert(xRange.minimum == 0 and yRange.minimum == 0,
           "unexpected multitouch device range x or y minimum")
    -- Check for something more than just 0 for extended sanity.
    -- NOTE: the ioctls report the both-sided *inclusive* range.
    assert(xRange.maximum >= 299 and yRange.maximum >= 399,
           "unexpected multitouch device range x or y maximum")

    MtRes.w = xRange.maximum + 1
    MtRes.h = yRange.maximum + 1

    MultiTouchScreenUnitRatio = MtRes.h / ScreenHeight_rM
end

-- INPUT_EVENT_COORD_CONVERSION step 2
local function ConvertMtToScreen(devx, devy)
    local isx = devx * ScreenWidth_rM / MtRes.w
    local isy = devy * ScreenHeight_rM / MtRes.h

    -- The MT device coordinates are flipped with respect to the
    -- framebuffer coordinates in both dimensions.
    local sx = ScreenWidth_rM - isx - 1
    local sy = ScreenHeight_rM - isy - 1

    -- NOTE: we may return fractional values here, but that's OK.
    assert(sx >= 0 and sx < ScreenWidth_rM)
    assert(sy >= 0 and sy < ScreenHeight_rM)

    return sx, sy
end

local function MakeEventToSend(ourEventType, ourData)
    local button = ourData.button
    local sx, sy = ConvertMtToScreen(ourData.x, ourData.y)
    assert(button ~= nil and sx ~= nil and sy ~= nil)

    local snx, sny = ConvertMtToScreen(ourData.nx, ourData.ny)
    assert(snx ~= nil and sny ~= nil)

    return OurEvent_t(ourEventType, button, sx, sy, snx, sny)
end

local MTC = input.MultiTouchCode

local InputState = class
{
    function()
        return {
            pressedCount = 0,
            -- The last time of the transition "no finger touches" -> one.
            -- (or infinity of not currently touching).
            lastFirstPressedTime = math.huge,

            stage = Stage.None,
            ourEventType = nil,
            ourData = nil,
        }
    end,

    handleEventFrame = function(self, events, eventCount, outputTab,
                                lockedUpTab)
        local oldStage = self.stage
        local didFinallyRelease = self:handlePressOrRelease(events, eventCount)
        local hadProgress = (self.stage > oldStage)

        if (not hadProgress) then
            if (self.stage == Stage.Prefix) then
                local dragStartEventIdx = 0

                if (self.ourEventType == OurEventType.SingleClick) then
                    dragStartEventIdx = self:handleSingleClick(events, eventCount)
                end
                -- NOTE: not 'else if' because a single click can become a drag.
                if (self.ourEventType == OurEventType.Drag) then
                    self:handleDrag(events, dragStartEventIdx, eventCount)
                end
            end
        elseif (self.stage == Stage.Finished) then
            outputTab[1] = MakeEventToSend(self.ourEventType, self.ourData)
            self:reset()
        end

        if (didFinallyRelease) then
            self.lastFirstPressedTime = math.huge
        end

        -- Indicate a locked up state.
        -- TODO: remove?
        lockedUpTab[1] = (currentTimeMs() >= self.lastFirstPressedTime + LockedUpDuration)
    end,

-- private:
    handleSingleClick = function(self, events, eventCount)
        local MaxDeviation = MaxSingleClickDeviation * MultiTouchScreenUnitRatio

        for i = 0, eventCount - 1 do
            local delta = self:updateNewPos(events[i])
            if (delta == nil) then
                self:reset()
                return eventCount
            end
            -- Allow a small deviation from the initially tapped point for
            -- "single click". If it is exceeded, consider it as starting...
            if (math.abs(delta) > MaxDeviation) then
                self.ourEventType = OurEventType.Drag  -- <- ...this.
                assert(self.lastFirstPressedTime ~= math.huge)
                local msSinceTap = currentTimeMs() - self.lastFirstPressedTime
                self.onlyVerticalDrag = (msSinceTap < GenericDragTapWaitTime)
                return i + 1
            end
        end

        return eventCount
    end,

    handleDrag = function(self, events, startEventIdx, eventCount)
        local MinSlope = 3
        local TriangRegionCheckXOffset = SingleWheelRollDistance * MultiTouchScreenUnitRatio

        for i = startEventIdx, eventCount - 1 do
            if (self:updateNewPos(events[i]) == nil) then
                return self:reset()
            end

            local data = self.ourData
            local ody = data.ny - data.y

            if (self.onlyVerticalDrag) then
                local dx, dy = data.nx - data.x, data.ny - data.y
                -- After the initial single click tolerance, vertical dragging
                -- has to proceed (1) up or down consistently, and (2) in an
                -- allowed truncated-triangular region.
                local isInconsistentUpDown =
                    ody ~= 0 and (dy/ody < 0 or math.abs(dy) < math.abs(ody))
                local isOutsideTriangle =
                    (math.abs(dx) > TriangRegionCheckXOffset and
                     math.abs(dy) / (math.abs(dx) - TriangRegionCheckXOffset) < MinSlope)

                if (isInconsistentUpDown or isOutsideTriangle) then
                    return self:reset()
                end
            end
        end
    end,

    updateNewPos = function(self, event)  --> delta
        local MemberTab = { [MTC.POSX]='x', [MTC.POSY]='y' }
        local m = MemberTab[event.code]
        if (m == nil) then
            return nil
        end
        local newValue = event.value
        self.ourData['n'..m] = newValue  -- update 'new' coordinate
        return newValue - self.ourData[m]
    end,

    handlePressOrRelease = function(self, events, eventCount)  --> "did finally release?"
        local pressedCountDelta = self:getPressedCountDelta(events, eventCount)
        if (pressedCountDelta == 0) then
            return false
        end

        local oldPressedCount = self.pressedCount

        self.pressedCount = oldPressedCount + pressedCountDelta
        -- FIXME: can fail on hiding rM menu after having connected when it was visible.
        assert(self.pressedCount >= 0, "more touch release than press events")

        local haveInitiallyPressed = (oldPressedCount == 0 and pressedCountDelta > 0)
        local haveFinallyReleased = (oldPressedCount > 0 and self.pressedCount == 0)

        if (haveInitiallyPressed) then
            self.lastFirstPressedTime = currentTimeMs()
        end

        if (self.pressedCount > 1) then
            -- We do not currently allow multi-finger gestures.
            self:reset()
        elseif (self.stage == Stage.None and haveInitiallyPressed) then
            self:handleInitialPress(events, eventCount)
        elseif (self.stage == Stage.Prefix and haveFinallyReleased) then
            self:handleFinalRelease(eventCount)
        end

        return haveFinallyReleased
    end,

    handleInitialPress = function(self, events, eventCount)
        -- First finger down: begin single click, but only we get all coordinates with the
        -- first input frame.
        if (eventCount >= 3 and
                events[1].code == MTC.POSX and
                events[2].code == MTC.POSY) then
            self.stage = Stage.Prefix
            self.ourEventType = OurEventType.SingleClick
            self.ourData = {
                x = events[1].value,
                y = events[2].value,
                -- For Drag -- the destination ("new") coordinates.
                nx = events[1].value,
                ny = events[2].value
            }
        end
    end,

    handleFinalRelease = function(self, eventCount)
        -- Last finger up: May finish a gesture in progress, but only if there are no
        -- additional events in the input frame.
        if (eventCount ~= 1) then
            self:reset()
        elseif (self.ourEventType == OurEventType.SingleClick) then
            self.ourData.button =
                self:timedOut(MaxSingleClickDuration) and Button.Right or Button.Left
            self.stage = Stage.Finished
        elseif (self.ourEventType == OurEventType.Drag) then
            self.ourData.button =
                self.onlyVerticalDrag and Button.VerticalDrag or Button.GenericDrag
            self.stage = Stage.Finished
        end
    end,

    timedOut = function(self, maxDuration)
        return currentTimeMs() >= self.lastFirstPressedTime + maxDuration
    end,

    getPressedCountDelta = function(self, events, eventCount)
        assert(eventCount > 0)

        local totalDelta = 0

        for i = 0, eventCount - 1 do
            local ev = events[i]
            assert(ev.type == EV.ABS, "unexpected event type")

            if (ev.code == input.MultiTouchCode.TRACKING_ID) then
                local delta = (ev.value >= 0) and 1 or -1

                totalDelta = totalDelta + delta
                -- We do not expect a frame to have both presses and releases.
                assert((totalDelta > 0) == (delta > 0))
            end
        end

        return totalDelta
    end,

    reset = function(self)
        self.stage = Stage.None
        self.ourEventType = nil
        self.ourData = nil
    end,
}

local Server = class
{
    function()
        local connFd, errMsg = inet.Socket():expectConnection(Port)
        if (connFd == nil) then
            errprintfAndExit("ERROR: failed connecting: %s", errMsg)
        end

        return {
            connFd = connFd,
            enabled = false,

            rM = RM.Remarkable(fb),
            evd = nil,  -- input.EventDevice
            inputBuf = input_event_array_t(MaxInputEvents),
            inputState = nil,  -- InputState
            inputLockedUpTab = { false },

            decodedBuf = NarrowArray(targetSize),
        }
    end,

    step = function(self)
        self:mainLoopStep()
    end,

    drawStatusRect = function(self, x)
        assert(type(x) == "number")
        local y, w = 32, 64
        map:fill(x, y, w, w, 15 + 32*31 + 32*64*15)
        self.rM:requestRefresh(xywh_t(x, y, w, w))
    end,

-- private:
    mainLoopStep = function(self)
        self:enable()

        -- Wait for input on either the multitouch device or from the network.

        local connFd, evdFd = self.connFd.fd, self.evd:getRawFd()

        local pollfds = posix.poll{ events=POLL.IN, connFd, evdFd }
        assert(#pollfds == 1 or #pollfds == 2)

        local haveFd = {}
        for _, pollfd in ipairs(pollfds) do
            haveFd[pollfd.fd] = true
        end

        assert(haveFd[connFd] or haveFd[evdFd])

        -- Handle received input(s) -- from the network (client) or the multitouch device.
        local updateData = haveFd[connFd] and self:receiveUpdates() or nil
        local inputData = haveFd[evdFd] and self:getInput() or nil

        -- Carry out should-disable check only when we have received updates.
        -- TODO [DISABLE_VIA_SERVER_INPUT]: also allow Cmd.Disable
        if (updateData ~= nil) then
            self:maybeDisable()

            if (not self.enabled) then
                -- NOTE: it may happen that we omit sending a mouse-up event and leave the
                --  client in an odd state.
                return
            end
        end

        -- Apply results.

        if (inputData ~= nil) then
            -- NOTE [SEND_INPUT_FIRST]: Send input first so that the Ok from the application
            --  of the updates (if present) arrives last.
            self:sendInput(inputData)
        end

        if (updateData ~= nil) then
            self:applyUpdates(updateData[1], updateData[2], self.decodedBuf)
        end

        if (self.inputLockedUpTab[1]) then
            self:drawStatusRect(StatusRectPosX.OnLockedUp)
        end
    end,

    getInput = function(self)
        local events, eventCount = ReadEvents(self.evd, self.inputBuf, 0)
        local eventToSend = {}
        local lastIdx = 0

        for i = 0, eventCount - 1 do
            local ev = events[i]

            if (ev.type == EV.SYN) then
                assert(i > lastIdx, "unexpected empty event frame")
                self.inputState:handleEventFrame(events + lastIdx, i - lastIdx, eventToSend,
                                                 self.inputLockedUpTab)
                lastIdx = i + 1
            end
        end

        return eventToSend[1]
    end,

    sendInput = function(self, data)
        local connFd = self.connFd
        assert(ffi.istype(OurEvent_t, data))

        local bytesWritten = connFd:write(Cmd.Input)
        assert(bytesWritten == Cmd.Length, "FIXME: partial write")

        connFd:writeFull(data)
    end,

    receiveUpdates = function(self)
        local connFd = self.connFd

        local header = connFd:readInto(UpdateHeader_t(), false)
        checkData(ffi.string(header.magic, #UpdateMagic) == UpdateMagic,
                  "magic bytes mismatch")

        -- TODO: preallocate the two arrays?

        local changedTileCount = header.changedTileCount
        local coords = connFd:readInto(coord_array_t(changedTileCount), false)

        local encodingLength = header.encodingLength
        local encodedData = connFd:readInto(NarrowArray(header.encodingLength), false)

        local tileCount = DecodeUpdates(encodedData, encodingLength, self.decodedBuf)
        checkData(tileCount == changedTileCount, "corrupt encoding: tile count mismatch")

        return { tileCount, coords }
    end,

    applyUpdates = function(self, tileCount, tileCoords, tileBuf)
        checkData(tileCount * BigSquareSize <= ffi.sizeof(tileBuf) / DestPixelSize,
                  "too many updated tiles")

        local yTileOffset = DestYPixelOffset / BigSideLen
        local updateRectSet = RectSet()

        for i = 0, tileCount - 1 do
            local tx, ty = tileCoords[i].x, tileCoords[i].y
            checkData(tx < destTileCountX and ty < destTileCountY,
                      "tile coordinates out of bounds")
            ty = ty + yTileOffset

            if (ty < destTileCountY) then
                local rect = xywh_t(BigSideLen * tx, BigSideLen * (ty),
                                    BigSideLen, BigSideLen)
                updateRectSet:add(rect)

                -- Write updated tile to the framebuffer.
                map:writeRect(rect.x, rect.y, rect.w, rect.h, tileBuf + BigSquareSize * i)
            end
        end

        updateRectSet:finish()
        local updateRects = updateRectSet:getRects()

        -- Request update of the changed screen portions.
        for i, rect in ipairs(updateRects) do
            self.rM:requestRefresh(rect, i)
        end

        -- Wait for the updates to finish.
        for i, _ in ipairs(updateRects) do
            self.rM:waitForCompletion(i)
        end

        local bytesWritten = self.connFd:write(Cmd.Ok)
        assert(bytesWritten == Cmd.Length, "FIXME: partial write")
    end,

    enable = function(self)
        if (not self.enabled) then
            while (not IsScreenDesired()) do
                posix.clock_nanosleep(500e6)
            end

            local bytesWritten = self.connFd:write(Cmd.Enable)
            assert(bytesWritten == Cmd.Length, "FIXME: partial write")
            self.enabled = true

            self.evd = self.rM:openEventDevice()
            ObtainMultiTouchCoordRange(self.evd)
            self.inputState = InputState()
        end
    end,

    maybeDisable = function(self)
        assert(self.enabled)
        self.enabled = IsScreenDesired()

        if (not self.enabled) then
            local bytesWritten = self.connFd:write(Cmd.Disable)
            assert(bytesWritten == Cmd.Length, "FIXME: partial write")

            self.rM:closeEventDevice()
            self.evd = nil

            -- TODO: clear? (E.g. if in pressed state, send as many release events.)
            self.inputState = nil
        end
    end,
}

local app = isClient and Client() or Server()

----------

local function main()
    while (true) do
        local startMs = currentTimeMs()
        app:step()

        if (not (isClient or isRealServer)) then
            io.stdout:write(("%.0f ms\n"):format(currentTimeMs() - startMs))
        end
    end
end

local ok, errMsg = pcall(main)
if (not ok) then
    if (io.open(".ljrM-enable-error-log") ~= nil) then
        local f = io.open("ljrM-error.log", "a+")
        if (f ~= nil) then
            local Sep = ('='):rep(30)
            local d = os.date("*t")
            local dateStr = ("%d-%02d-%02d %02d:%02d:%02d"):format(
                d.year, d.month, d.day, d.hour, d.min, d.sec)
            f:write(("\n%s %s %s\n"):format(Sep, dateStr, Sep))
            f:write(errMsg..'\n')
            f:close()
        end
    end

    if (isRealServer) then
        app:drawStatusRect(StatusRectPosX.OnError)
    end

    error(errMsg)
end
