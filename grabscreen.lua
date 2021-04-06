#!/usr/bin/env luajit

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
local os = require("os")

local charpics = require("charpics")
local class = require("class").class
local inet = require("inet")
local input = require("input")
local FrameBuffer = require("framebuffer").FrameBuffer
local JKissRng = require("jkiss_rng").JKissRng
local posix = require("posix")
local vkbd = require("vkbd")
-- NOTE [XLIB_LUA_CONDITIONAL_REQUIRE]: we want to be functional for setups without X
--  installed.
local xlib = (os.getenv("DISPLAY") ~= nil) and require("xlib") or nil

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

local alwaysOn = (arg[1] == "--always-on")
if (alwaysOn) then
    table.remove(arg, 1)
end

local doFork = (arg[1] == "--fork")
if (doFork) then
    table.remove(arg, 1)
end

local function ParseClientServerArgument(firstArg)
    if (firstArg == nil) then
        return false, false, 0
    end
    local cORs, numStr = firstArg:match("^([cs])%+([0-9][0-9]?)$")
    if (numStr == nil) then
        return (firstArg == 'c'), (firstArg == 's'), 0
    end
    return (cORs == 'c'), (cORs == 's'), tonumber(numStr)
end

local isClient, isServer, portOffset = ParseClientServerArgument(arg[1])
local hostNameOrAddr = isClient and arg[2] or nil
local acceptTimeoutMsArg = isServer and arg[2] or nil

local isAcceptTimeoutOk = not isServer or
    acceptTimeoutMsArg == nil or
    acceptTimeoutMsArg:match("^[1-9][0-9]*$")
local acceptTimeoutMs = isAcceptTimeoutOk and
    tonumber(acceptTimeoutMsArg) or nil

local function errprintf(fmt, ...)
    stderr:write((fmt.."\n"):format(...))
end

local function errprintfAndExit(fmt, ...)
    errprintf(fmt, ...)
    os.exit(1)
end

if (not ((isClient and hostNameOrAddr ~= nil) or (isServer and isAcceptTimeoutOk))) then
    if (not isAcceptTimeoutOk) then
        assert(isServer)
        errprintf("ERROR: timeout must be a non-negative integer.\n")
    end
    errprintf([[
Usage:
  %s [--fork] c[+<portOffset>] <host name or IPv4 address>               # on the Raspberry Pi
  %s [--always-on][--fork] s[+<portOffset>] [<connection timeout (ms)>]  # on the reMarkable

If provided, the port offset must consist of one or two decimal digits.

When forking is requested, it is done:
 - for the client, after having connected.
 - for the server, just before the first accept() call.
In that case, the parent will exit with code zero to signal success.

A passed host name is resolved by reading and parsing /etc/hosts.
]], arg[0], arg[0])

    local quiet = (arg[1] == '-Q' and arg[2] == nil)
    if (quiet) then
        -- For the application unity file.
        require("remarkable")
    end
    os.exit(quiet and 0 or 1)
end

----------

if (ffi.C.geteuid() == 0) then
    errprintfAndExit("ERROR: Refusing to run as root.")
end

ffi.cdef[[
int memcmp(const void *s1, const void *s2, size_t n);
]]

local function currentTimeMs()
    local ts = posix.clock_gettime()
    return 1000 * tonumber(ts.sec) + tonumber(ts.nsec) / 1000000
end

----------

-- Tiling on the source:
local SideLen = 8
local SquareSize = SideLen * SideLen  -- in pixels
-- Tiling on the target:
local BigSideLen = 2 * SideLen
local BigSquareSize = BigSideLen * BigSideLen

local fb = FrameBuffer(0, isServer)
local map = fb:getMapping(BigSideLen)
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
        errprintfAndExit("ERROR: Unsupported pixel size (%d bytes, expected %d).",
                         map:getPixelSize(), expectedPixelSize)
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

-- KEEPINSYNC vkbd.lua
-- Default screen dimensions on the reMarkable.
local ScreenWidth_rM = 1404
local ScreenHeight_rM = 1872
local MenuWidth_rM = 120
-- Guesstimate for the side length of the touch-active region around the rM "eye".
local EyeSize_rM = 128

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
local targetYres = math.min(RoundToTarget(map.yres + BigSideLen-1), RoundToTarget(ScreenHeight_rM))
local targetSize = targetXres * targetYres

-- Make sure we do not overwrite the "eye" on the reMarkable.
local DestYPixelOffset = EyeSize_rM
assert(DestYPixelOffset % BigSideLen == 0)  -- see usage for why

local StatusRectWidth = 64
-- X position of the rectangle signaling certain status conditions.
local StatusRectPosX = {
    OnError = EyeSize_rM,

    RightMarker = ScreenWidth_rM - 2 * EyeSize_rM,
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

-- For emulating input:
local display = (xlib ~= nil and isClient) and xlib.Display() or nil

-- Throttling: Destination tiles for which changes were present within the last '...Window'
-- updates are tentatively held back until the sequence number is evenly divisible by the
-- period. This ensures that all updates for formerly held-back tiles are synchronized.
-- Note that in general this does not mean a fixed period in terms of wall-clock time since
-- we operate in varying time steps due to exponential backoff. However, for a video playing
-- whose content changes often enough, this will be approximately the case.
local FastFrameHoldBackWindow = 3
local FastFrameSendPeriod = 40

local isFrameThrottlingEnabled = true

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

            currentSeqNum = FastFrameHoldBackWindow + 1,
            lastChangedSeqNums = UInt32Array(totalDestTileCount),
            lastSentSeqNums = UInt32Array(totalDestTileCount),

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

        local currentSeqNum = self.currentSeqNum
        local blockedTileCount = 0
        local destTileCoords = {}

        -- NOTE: y first!
        for y = 0, destTileCountY - 1 do
            local xL, xlBlocked, xrBlocked, xR

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

                local lastChangedSeqNum = self.lastChangedSeqNums[destTileIdx]
                local lastSentSeqNum = self.lastSentSeqNums[destTileIdx]

                local isOutdated = destTileChanged or (lastSentSeqNum < lastChangedSeqNum)
                local wantHoldBack = destTileChanged and
                    (currentSeqNum <= lastChangedSeqNum + FastFrameHoldBackWindow)
                local canHoldBack = isFrameThrottlingEnabled and
                    wantHoldBack and (currentSeqNum % FastFrameSendPeriod ~= 0)
                local shouldSend = isOutdated and not canHoldBack

                xlBlocked = xlBlocked or (canHoldBack and x or nil)
                xrBlocked = (canHoldBack and x or nil) or xrBlocked

                if (destTileChanged) then
                    self.lastChangedSeqNums[destTileIdx] = currentSeqNum
                end

                if (shouldSend) then
                    self.lastSentSeqNums[destTileIdx] = currentSeqNum
                    xL = xL or x
                    xR = x
                end
            end

            if (xL ~= nil) then
                for xx = xL, xR do
                    local isBlocked =
                        (xlBlocked ~= nil and xx >= xlBlocked and xx <= xrBlocked)

                    blockedTileCount = blockedTileCount + (isBlocked and 1 or 0)
                    destTileCoords[#destTileCoords + 1] = {
                        x=xx, y=y, blocked=isBlocked
                    }
                end
            end
        end

        self.currentSeqNum = self.currentSeqNum + 1

        return destTileCoords, blockedTileCount
    end,

-- private:
    generate = function(self)
        local idxs = {}

        -- NOTE: y first!
        for y = 0, targetYres - 1, SideLen do
            for x = 0, targetXres - 1, SideLen do
                -- Randomly perturb sample positions.
                local xoff = self.rng:getu32() % SideLen
                local yoff = self.rng:getu32() % SideLen

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
    local sy = BigSideLen * ty

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

-- Mnemonic for the base port number is "P2R" ("Pi to reMarkable").
local Port = 16218 + portOffset

-- Client -> server
local UpdateMagic = "UpDat3_"

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
    char magic[7];
    bool isFullScreen;
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
    int16_t x, y;
    int16_t nx, ny;  // Drag only
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
-- NOTE: called on the client for legacy reasons.
local function ConvertScreenToClient(sx, sy)
    local cx = sx
    local cy = sy - DestYPixelOffset
    return cx, cy
end

local MaxInputEvents = 1024
local sizeof_input_event = ffi.sizeof("struct input_event")
local input_event_array_t = ffi.typeof("struct input_event [?]")

local function ReadEvents(evd, inputBuf, maxAllowedSynValue, errnoAllowTab)
    local function readEventsIntoInputBuffer()
        if (errnoAllowTab == nil) then
            return evd.fd:readInto(inputBuf, true)
        else
            return evd.fd:readIntoAllowing(inputBuf, true, errnoAllowTab)
        end
    end

    local events, bytesRead = readEventsIntoInputBuffer()
    if (events == nil) then
        assert(errnoAllowTab ~= nil)
        -- NOTE: closing also sets evd.fd to -1. This is important for proper functioning
        --  after the keyboard has been unplugged, see INVALID_KB_EVD_FD.
        evd:close()
        return
    end

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
        local a, b, c, d, name = line:match(
            '^[ \t]*'..IPAddressPattern.."[ \t]+"..HostNamePattern..'[ \t]*$')
        if (a ~= nil and name == nameOrQuad) then
            return {tonumber(a), tonumber(b), tonumber(c), tonumber(d)}
        end
    end

    errprintfAndExit("ERROR: Host name not found in /etc/hosts")
end

local function FindKeyboardDevFile()
    local Directory = "/dev/input/by-path"
    if (io.open(Directory) == nil) then
        return nil
    end

    local dir = posix.Dir(Directory)

    while (true) do
        local fileName = dir:read()
        if (fileName == nil) then
            return nil
        elseif (fileName:sub(-10,-1) == "-event-kbd") then
            return Directory.."/"..fileName
        end
    end
end

local function MaybeForkAndExit()
    if (doFork) then
        local whoami, pid = posix.fork()
        if (whoami == "parent") then
            print(("Child process ID: %d"):format(pid))
            os.exit(0)
        end
    end
end

local DummyKbEventDevice = class
{
    function()
        return {}
    end,

    getRawFd = function()
        return -1
    end,
}

local function GetKbEventDevice()
    local kbDevFileName = FindKeyboardDevFile()
    if (kbDevFileName == nil) then
        return DummyKbEventDevice()
    end

    local evDev = input.EventDevice(kbDevFileName)
    if (evDev.errMsg ~= nil) then
        io.stderr:write(("WARNING: failed opening keyboard event device file: %s\n")
                :format(evDev.errMsg))
        io.stderr:write(("INFO:    %s\n"):format(kbDevFileName))
        io.stderr:write("INFO:    Will not react to keyboard input.\n")
    end
    return evDev
end

local Client = class
{
    function()
        local address = GetAddress(hostNameOrAddr)
        address[#address + 1] = Port

        local kbEvDevice = GetKbEventDevice()
        local connFd = ConnectTo(inet.Socket(), address, hostNameOrAddr)

        MaybeForkAndExit()

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

        local destTileCoords, blockedTileCount = self.sampler:sampleAndCompare()
        local updatedTileCount = #destTileCoords

        local packedTileCount = (updatedTileCount > 0) and
            self:captureUpdates(destTileCoords, self.sampler.screenCopyBuf) or 0
        assert(packedTileCount == updatedTileCount - blockedTileCount)

        if (packedTileCount > 0) then
            local encodingLength = self:encodeUpdates(packedTileCount)

            if (self.connFd ~= nil) then
                self:sendUpdates(destTileCoords, packedTileCount, encodingLength)
            else
                local fmt = "changed, #packedTiles=%d, encoded=%d halfwords (factor = %.03f)\n"
                stderr:write(fmt:format(packedTileCount, encodingLength,
                                        packedTileCount * BigSquareSize / encodingLength))
                local tileCount = DecodeUpdates(self.codedBuf, encodingLength, self.decodedBuf_)
                -- Check correctness of decoding.
                assert(tileCount == packedTileCount)
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

            if (kbEvDevFd >= 0 and fdSet:isSet(kbEvDevFd)) then
                ReadEvents(self.kbEvDevice, self.inputBuf, 1, { [posix.E.NODEV]=true })
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
        local packedTileCount = 0

        for i, coord in ipairs(destTileCoords) do
            assert(i > 0 and i <= updatedTileCount)
            CopyBigTile(fbPtr, screenCopyBuf, coord, CopyFormat.Same)
            if (not coord.blocked) then
                local destPtr = self.tempBuf + BigSquareSize*packedTileCount
                CopyBigTile(screenCopyBuf, destPtr, coord, CopyFormat.Packed)
                packedTileCount = packedTileCount + 1
            end
        end

        local packedPixelCount = packedTileCount * BigSquareSize

        for i = 0, packedPixelCount - 1 do
            -- TODO: 'updated' in the top local does not mean the same as 'update' here.
            self.updateBuf[i] = narrow(self.tempBuf[i])
        end

        return packedTileCount
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

    sendUpdates = function(self, destTileCoords, changedTileCount, encodingLength)
        local connFd = self.connFd
        assert(connFd ~= nil)

        local header = UpdateHeader_t(
            UpdateMagic, changedTileCount == totalDestTileCount,
            changedTileCount, encodingLength)
        connFd:writeFull(header)

        local coords = coord_array_t(changedTileCount)
        local clipBottomY = map.yres - map.yres_original
        assert(clipBottomY >= 0 and clipBottomY < BigSideLen)
        local highBits = bit.lshift(clipBottomY, 12)

        local i = 0
        for _, coord in ipairs(destTileCoords) do
            if (not coord.blocked) then
                assert(bit.band(highBits, coord.y) == 0)
                local usedHighBits = (coord.y == destTileCountY - 1) and highBits or 0
                coords[i] = coord_t(coord.x, usedHighBits + coord.y)
                i = i + 1
            end
        end

        assert(i == changedTileCount)

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
                    return x >= 0 and x < targetXres and y >= 0 and y < targetYres
                end

                if (cy >= targetYres and cny < 0) then
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

                        if (display ~= nil) then
                            display:moveMouse(cx, cy)
                            display:clickMouse(ourEvent.button)
                            -- NOTE: to have feedback, deliberately no restoration of the
                            --  mouse pointer position.
                        end
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
                                if (display ~= nil) then
                                    local oldX, oldY = display:getMousePos()
                                    if (oldX ~= nil and oldY ~= nil) then
                                        display:moveMouse(cx, cy)
                                        display:clickMouseMultiple(button, repeatCount, 3)
                                        display:moveMouse(oldX, oldY)
                                    end
                                end
                            end
                        elseif (ourEvent.button == Button.GenericDrag) then
                            if (isInScreenBounds(cnx, cny)) then
                                -- Emulate a drag with the left mouse button clicked.
                                if (display ~= nil) then
                                    -- Delay needs to be this large for web-based maps,
                                    -- unfortunately (already 50 ms may make dragging not work
                                    -- properly). Note that the total is five times the number.
                                    --
                                    -- TODO: see if *some* of the delays on the constituent
                                    --  operations can be lowered, though.
                                    local InterOpDelayMs = 100
                                    display:moveMouseClicked(cx,cy, cnx,cny, InterOpDelayMs)
                                end
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

local ServerRequest = {
    Shutdown = 2,
}

local Server  -- class "forward-declaration"
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

    if (alwaysOn) then
        return true
    elseif  (isRealServer) then
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

local UpdateRectManager = class
{
    function()
        return {
            x1 = math.huge,
            y1 = math.huge,
            x2 = -math.huge,
            y2 = -math.huge,
        }
    end,

    add = function(self, rect)
        assert(ffi.istype(xywh_t, rect))

        self.x1 = math.min(self.x1, rect.x)
        self.y1 = math.min(self.y1, rect.y)
        self.x2 = math.max(self.x2, rect.x + rect.w)
        self.y2 = math.max(self.y2, rect.y + rect.h)
    end,

    getRects = function(self)
        return {
            xywh_t(self.x1, self.y1,
                   self.x2 - self.x1, self.y2 - self.y1)
        }
    end,
}

--== All times in milliseconds.
local Duration = {
    MaxLeftClick = 300,
    MaxRightClick = 2000,
    -- Time that an initial tap must be held to commence generic (as opposed to
    -- vertical-only) dragging:
    GenericDragTapWait = 300,
}

local SecondFingerDragMultiplier = 3
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

local TouchState = class
{
    function(x, y)
        return {
            x = x,
            y = y,
            -- For Drag -- the destination ("new") coordinates.
            nx = x,
            ny = y,

            button = nil,  -- Button.* constants
        }
    end,
}

local function MakeEventToSend(ourEventType, touchState)
    local button = touchState.button
    local sx, sy = ConvertMtToScreen(touchState.x, touchState.y)
    assert(button ~= nil and sx ~= nil and sy ~= nil)

    local snx, sny = ConvertMtToScreen(touchState.nx, touchState.ny)
    assert(snx ~= nil and sny ~= nil)

    return OurEvent_t(ourEventType, button, sx, sy, snx, sny)
end

local function MergeEventToSend(eventToSend, dummyEvent)
    -- Multi-finger drag.
    local dyFirst = eventToSend.ny - eventToSend.y
    local dySecond = dummyEvent.ny - dummyEvent.y

    if ((dySecond < 0) ~= (dyFirst < 0)) then
        return nil
    end

    eventToSend.ny = eventToSend.ny + SecondFingerDragMultiplier * dySecond

    return eventToSend
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
            slot = nil,  -- set on first touch
            ourData = nil,  -- { [<slot>]=TouchState }
            -- Set when starting dragging:
            onlyVerticalDrag = nil,
            isShutdownGesture = nil,
            -- Set to 1 on the first touch of the second finger, to 2 when accounted for:
            multiFingerDragStage = 0,
        }
    end,

    handleEventFrame = function(self, events, eventCount, outputTab,
                                specialRqTab)
        local oldStage = self.stage
        local didFinallyRelease = self:handlePressOrRelease(events, eventCount, specialRqTab)
        local hadProgress = (self.stage > oldStage)

        if (self.stage == Stage.Prefix) then
            local dragStartEventIdx = 0

            if (self.ourEventType == OurEventType.SingleClick) then
                dragStartEventIdx = self:handleSingleClick(events, eventCount)
            end
            -- NOTE: not 'else if' because a single click can become a drag.
            if (self.ourEventType == OurEventType.Drag) then
                self:handleDrag(events, dragStartEventIdx, eventCount)
            end
        elseif (hadProgress and self.stage == Stage.Finished) then
            local eventToSend = MakeEventToSend(self.ourEventType, self.ourData[0])
            if (self.ourData[1] ~= nil) then
                self.ourData[1].button = self.ourData[0].button
                local dummyEvent = MakeEventToSend(self.ourEventType, self.ourData[1])
                eventToSend = MergeEventToSend(eventToSend, dummyEvent)
            end

            if (eventToSend ~= nil) then
                outputTab[1] = eventToSend
            end
            self:reset()
        end

        if (didFinallyRelease) then
            self.lastFirstPressedTime = math.huge
        end
    end,

-- private:
    setupMultiFingerDrag = function(self)
        self.multiFingerDragStage = 2
        self.ourEventType = OurEventType.Drag
        self.isShutdownGesture = false
        self.onlyVerticalDrag = true
    end,

    isConsistentWithMultiDrag = function(self)
        assert(self.multiFingerDragStage == 2)
        assert(self.ourEventType == OurEventType.Drag)
        return self.onlyVerticalDrag and not self.isShutdownGesture
    end,

    isMultiFingerDragRequested = function(self)
        return (self.multiFingerDragStage == 1)
    end,

    handleSingleClick = function(self, events, eventCount)
        local MaxDeviation = MaxSingleClickDeviation * MultiTouchScreenUnitRatio

        local isMultiDragRequested = self:isMultiFingerDragRequested()
        assert(self.multiFingerDragStage == 0 or isMultiDragRequested)

        if (isMultiDragRequested) then
            self:setupMultiFingerDrag()
            return 0
        end

        for i = 0, eventCount - 1 do
            local ev = events[i]
            -- For SLOT events with value > 0, we should be in the multi-finger touch path.
            assert(ev.code ~= MTC.SLOT or (ev.value == 0 and self.slot == 0))

            local doSkip = (ev.code == MTC.TRACKING_ID or ev.code == MTC.SLOT)
            local delta = doSkip and 0 or self:updateNewPos(events[i])

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
                self.isShutdownGesture = self:isStartShutdownGesturePos()
                self.onlyVerticalDrag = (msSinceTap < Duration.GenericDragTapWait) or
                    self.isShutdownGesture
                return i + 1
            end
        end

        return eventCount
    end,

    isStartShutdownGesturePos = function(self)
        local sx, sy = ConvertMtToScreen(self.ourData[0].x, self.ourData[0].y)
        return (sx >= ScreenWidth_rM - EyeSize_rM and sy < EyeSize_rM)
    end,

    isEndShutdownGesturePos = function(self)
        local sx, sy = ConvertMtToScreen(self.ourData[0].nx, self.ourData[0].ny)
        return (sx >= ScreenWidth_rM - EyeSize_rM and sy >= ScreenHeight_rM - EyeSize_rM)
    end,

    initOurData = function(self, eventTriple)
        local evID, evX, evY = eventTriple[0], eventTriple[1], eventTriple[2]

        assert(evID.code == MTC.TRACKING_ID)
        assert(evID.value >= 0)

        assert(evX.code == MTC.POSX)
        assert(evY.code == MTC.POSY)

        self.ourData[self.slot] = TouchState(evX.value, evY.value)
    end,

    handleDrag = function(self, events, startEventIdx, eventCount)
        local MinSlope = 3
        local TriangRegionCheckXOffset = SingleWheelRollDistance * MultiTouchScreenUnitRatio

        if (self:isMultiFingerDragRequested()) then
            self.multiFingerDragStage = 2
        end

        if (self.multiFingerDragStage == 2 and not self:isConsistentWithMultiDrag()) then
            return self:reset()
        end

        local dysPreLoop = {
            [0] = self.ourData[0].ny - self.ourData[0].y,
            [1] = self.ourData[1] and (self.ourData[1].ny - self.ourData[1].y) or 0,
        }

        for i = startEventIdx, eventCount - 1 do
            local ev = events[i]
            assert(ev.type == EV.ABS, "unexpected event type")

            if (ev.code == MTC.SLOT) then
                assert(self.multiFingerDragStage == 2)
                self.slot = ev.value

                if (self.ourData[self.slot] == nil) then
                    assert(eventCount - i >= 4)  -- assuming SLOT, TRACKING_ID, POSX, POSY
                    self:initOurData(events + i+1)
                    -- NOTE: events [i+1 .. i+3] are handled to ultimately no effect.
                end
            elseif (ev.code == MTC.TRACKING_ID) then
                -- Do nothing. Note that ev.value may be negative! (Touch released.)
                --  This is OK: it has been accounted for in handlePressOrRelease().
            elseif (self:updateNewPos(ev) == nil) then
                self:reset()
                return
            elseif (self.onlyVerticalDrag) then
                local data = self.ourData[self.slot]
                local ody = dysPreLoop[self.slot]
                assert(ody ~= nil)
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
                    self:reset()
                    return
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
        local touchState = self.ourData[self.slot]
        assert(touchState ~= nil)
        local newValue = event.value
        touchState['n'..m] = newValue  -- update 'new' coordinate
        return newValue - touchState[m]
    end,

    handlePressOrRelease = function(self, events, eventCount, specialRqTab)  --> "did finally release?"
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

        if (self.stage == Stage.None and haveInitiallyPressed) then
            self:handleInitialPress(events, eventCount)
        end

        if (self.pressedCount > 1) then
            -- Multi-finger gesture.
            -- NOTE: currently, allows at most two fingers.
            if (self.pressedCount == 2 and
                (self.stage == Stage.Prefix and self.multiFingerDragStage == 0)) then
                self.multiFingerDragStage = 1
            else
                self:reset()
            end
        end

        if (self.stage == Stage.Prefix and haveFinallyReleased) then
            self:handleFinalRelease(events, eventCount, specialRqTab)
        end

        return haveFinallyReleased
    end,

    handleInitialPress = function(self, events, eventCount)
        -- First finger down: begin single click, but only we get all coordinates with the
        -- first input frame. Usually there is no SLOT event (slot 0 is implied) but when
        -- the frame contains events from the touch of more than one finger, there is one
        -- (in which case the value 0 is *expected*, and asserted).
        --
        -- Note that the event frame may contain the sequence of events for the touchdown of
        -- another finger. This is not handled here, but we must make sure that it is
        -- accounted for subsequently!
        assert(eventCount > 0)
        local offset = (events[0].code == MTC.SLOT) and 1 or 0
        assert(offset == 0 or events[0].value == 0)

        if (eventCount >= 3 + offset) then
            local evX = events[offset + 1]
            local evY = events[offset + 2]

            if (evX.code == MTC.POSX and evY.code == MTC.POSY) then
                self.stage = Stage.Prefix
                self.ourEventType = OurEventType.SingleClick

                self.slot = 0
                self.ourData = { [self.slot] = TouchState(evX.value, evY.value) }
            end
        end
    end,

    handleFinalRelease = function(self, events, eventCount, specialRqTab)
        -- Last finger up: May finish a gesture in progress, but only if there are no
        -- additional events that are inconsistent with purely a (potentially multi-)finger
        -- release in the input frame.

        for i = 0, eventCount-1 do
            local ev = events[i]
            local isReleaseMarker = (ev.code == MTC.TRACKING_ID and ev.value == -1)
            if (not (isReleaseMarker or ev.code == MTC.SLOT)) then
                self:reset()
                return
            end
        end

        if (self.ourEventType == OurEventType.SingleClick) then
            self.ourData[0].button =
                self:timedOut(Duration.MaxRightClick) and Button.Middle or
                self:timedOut(Duration.MaxLeftClick) and Button.Right or
                Button.Left
            self.stage = Stage.Finished
        elseif (self.ourEventType == OurEventType.Drag) then
            if (self.isShutdownGesture) then
                if (self:isEndShutdownGesturePos()) then
                    specialRqTab[1] = ServerRequest.Shutdown
                end
                self:reset()
            else
                self.ourData[0].button =
                    self.onlyVerticalDrag and Button.VerticalDrag or Button.GenericDrag
                self.stage = Stage.Finished
            end
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

            if (ev.code == MTC.TRACKING_ID) then
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
        self.slot = nil
        self.ourData = nil
        self.onlyVerticalDrag = nil
        self.isShutdownGesture = nil
        self.multiFingerDragStage = 0
    end,
}

local function RGB565(r, g, b)
    return b + 32*g + 32*64*r
end

-- KEEPINSYNC mkcharpics.lua
local CODEPOINT_PLANE_STRIDE = 0x200000

Server = class
{
    function()
        local connFd, errMsg = inet.Socket():expectConnection(
            Port, acceptTimeoutMs, MaybeForkAndExit)
        if (connFd == nil) then
            errprintfAndExit("ERROR: failed awaiting connection: %s", errMsg)
        end

        return {
            connFd = connFd,
            enabled = false,

            rM = RM.Remarkable(fb),
            evd = nil,  -- input.EventDevice or DummyKbEventDevice
            inputBuf = input_event_array_t(MaxInputEvents),
            inputState = nil,  -- InputState
            specialRequestTab = { nil },  -- can be one of the 'ServerRequest' constants
            charRenderer = charpics.Renderer(".charpics", map),

            decodedBuf = NarrowArray(targetSize),
        }
    end,

    step = function(self)
        self:mainLoopStep()
    end,

    clearUpperArea = function(self)
        local x, y = MenuWidth_rM, 0
        local w, h = ScreenWidth_rM - MenuWidth_rM, DestYPixelOffset
        map:fill(x, y, w, h, 0xffff)
        self.rM:requestRefresh(xywh_t(x, y, w, h))
    end,

    displayMessage = function(self, x, message)
        assert(type(x) == "number")
        assert(x >= MenuWidth_rM and x < ScreenWidth_rM)
        local BaselineOffset = 32
        local InterCharAdvance = 6
        local endX, topY, botY = self.charRenderer:drawString(
            x, DestYPixelOffset - BaselineOffset, InterCharAdvance, message,
            -- Request small characters:
            CODEPOINT_PLANE_STRIDE)
        if (endX > x and botY > topY) then
            self.rM:requestRefresh(xywh_t(x, topY, endX - x, botY - topY))
        end
        return endX + InterCharAdvance
    end,

    drawStatusRect = function(self, x)
        assert(type(x) == "number")
        local y, w = 32, StatusRectWidth
        map:fill(x, y, w, w, RGB565(15, 31, 15))
        self.rM:requestRefresh(xywh_t(x, y, w, w))
    end,

    drawKeyboardGrid = function(self)
        local PixValue = RGB565(2, 4, 2)

        local function drawHline(x, y, width)
            map:fill(x, y, width, 1, PixValue)
        end

        local function drawVline(x, y, height)
            map:fill(x, y, 1, height, PixValue)
        end

        local function refresh(x, y, w, h)
            self.rM:requestRefresh(xywh_t(x, y, w, h))
        end

        vkbd.drawGrid(drawHline, drawVline, refresh)
    end,

    shutDownAndExit = function(self, exitCode)
        -- NOTE: do not disable, need DISABLE_VIA_SERVER_INPUT for that.

        -- TODO: in charpics.Renderer:drawString(), handle space specially.
        local x = self:displayMessage(300, "Shutting\xf7down...\xf7")
        self.connFd:shutdown(posix.SHUT.RDWR)

        repeat
            local str = self.connFd:read(256)
        until (#str == 0)

        self:displayMessage(x, "success.")
        os.exit(exitCode)
    end,

-- private:
    mainLoopStep = function(self)
        self:enable()

        -- Wait for input on either the multitouch device or from the network.

        -- NOTE [INVALID_KB_EVD_FD]: 'evdFd' may be -1, either because we were started with
        --  no keyboard to begin with, or because we had one when we started, but it was
        --  unplugged later.
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
            self:applyUpdates(updateData[1], updateData[2], self.decodedBuf, updateData[3])
        end

        do
            -- Handle special requests.
            local specialRequest = self.specialRequestTab[1]

            if (specialRequest == ServerRequest.Shutdown) then
                self:shutDownAndExit(0)
            end
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
                                                 self.specialRequestTab)
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

        return { tileCount, coords, header.isFullScreen }
    end,

    applyUpdates = function(self, tileCount, tileCoords, tileBuf, isFullScreen)
        checkData(tileCount * BigSquareSize <= ffi.sizeof(tileBuf) / DestPixelSize,
                  "too many updated tiles")

        local yTileOffset = DestYPixelOffset / BigSideLen
        local updateRectMgr = UpdateRectManager()

        for i = 0, tileCount - 1 do
            local tx, ty = tileCoords[i].x, bit.band(tileCoords[i].y, 0x0fff)
            checkData(tx < destTileCountX and ty < destTileCountY,
                      "tile coordinates out of bounds")
            local clipBottomY = bit.rshift(bit.band(tileCoords[i].y, 0xf000), 12)

            ty = ty + yTileOffset

            if (ty < destTileCountY) then
                local rect = xywh_t(BigSideLen * tx, BigSideLen * ty,
                                    BigSideLen, BigSideLen - clipBottomY)
                updateRectMgr:add(rect)

                -- Write updated tile to the framebuffer.
                map:writeRect(rect.x, rect.y, rect.w, rect.h, tileBuf + BigSquareSize * i)
            end
        end

        assert(tileCount >= 1)
        local updateRects = updateRectMgr:getRects()

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

        if (isFullScreen) then
            -- Client (claims to have) sent tiles for its whole screen.
            --  TODO: handle the situation "client screen height > 1080".
            --   Currently, we would overwrite a portion of the keyboard.
            -- On that occasion:
            self:drawKeyboardGrid()
        end
    end,

    enable = function(self)
        if (not self.enabled) then
            while (not IsScreenDesired()) do
                posix.clock_nanosleep(500e6)
            end

            if (isRealServer) then
                self:clearUpperArea()
                -- TODO: draw the rM "cross in circle"?
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
        stderr:write(errMsg..'\n')
        app:shutDownAndExit(101)
    end

    error(errMsg)
end
