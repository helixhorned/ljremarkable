
local ffi = require("ffi")
local bit = require("bit")

local error_util = require("error_util")

local class = require("class").class
local ioctl = require("framebuffer").ioctl  -- TODO: pull out to separate file
local posix = require("posix")
local util = require("util")

local linux_decls = require("linux_decls")
local ABS = linux_decls.ABS
local EV = linux_decls.EV

local check = error_util.check
local checktype = error_util.checktype

local assert = assert
local error = error
local ipairs = ipairs
local tonumber = tonumber
local type = type

----------

-- From <linux/input.h> with __u16/__s32 -> uint16_t/int32_t etc. changes.
ffi.cdef[[
struct input_event {
    struct timeval time;
    uint16_t type;
    uint16_t code;
    int32_t value;
};

struct input_absinfo {
    int32_t value;
    int32_t minimum;
    int32_t maximum;
    int32_t fuzz;
    int32_t flat;
    int32_t resolution;
};

struct input_mask {
    uint32_t type;
    uint32_t codes_size;
    uint64_t codes_ptr;
};
]]

local input_event_t = class
{
    ffi.typeof("struct input_event"),

    getMs = function(self)
        -- TODO: ensure monotonic clock.
        return 1000 * tonumber(self.time.sec) + tonumber(self.time.usec) / 1000
    end,
}

local input_absinfo_t = ffi.typeof("struct input_absinfo")
local input_mask_t = ffi.typeof("struct input_mask")

-- IOCTL marcos in Lua.
-- See /usr/include/asm-generic/ioctl.h

local _IOC_BITS = {
    NR = 8,
    TYPE = 8,
    SIZE = 14,
    DIR = 2,
}

local _IOC_SHIFT = {
    NR = 0,
    TYPE = _IOC_BITS.NR,
    SIZE = _IOC_BITS.NR + _IOC_BITS.TYPE,
    DIR = _IOC_BITS.NR + _IOC_BITS.TYPE + _IOC_BITS.SIZE,
}

local function _IOC(dir, typ, nr, size)
    local shl = bit.lshift

    assert(type(typ) == "string" and #typ == 1)
    assert(type(nr) == "number" and nr >= 0 and nr < 2^_IOC_BITS.NR)

    return bit.bor(
        shl(dir, _IOC_SHIFT.DIR),
        shl(typ:byte(), _IOC_SHIFT.TYPE),
        shl(nr, _IOC_SHIFT.NR),
        shl(size, _IOC_SHIFT.SIZE))
end

local function _IOR(typ, nr, ctype)
    return _IOC(2, typ, nr, ffi.sizeof(ctype))
end

local function _IOW(typ, nr, ctype)
    return _IOC(1, typ, nr, ffi.sizeof(ctype))
end

-- <linux/event.h> ioctls

local device_name_buf_t = ffi.typeof("char [256]")

local EVIOCGNAME = _IOR('E', 0x06, device_name_buf_t)  -- get device name
local EVIOCGABS = {  -- get abs value/limits
    -- TODO: generalize to other ABS values?
    X = _IOR('E', 0x40 + ABS.MT_POSITION_X, input_absinfo_t),
    Y = _IOR('E', 0x40 + ABS.MT_POSITION_Y, input_absinfo_t),
}

local EVIOCSMASK =_IOW('E', 0x93, input_mask_t)  -- Set event-masks

local IoctlTypes = {
    [EVIOCGNAME] = device_name_buf_t,
    [EVIOCGABS.X] = input_absinfo_t,
    [EVIOCGABS.Y] = input_absinfo_t,

    [EVIOCSMASK] = function(typ, codesToSet)
        checktype(typ, 1, "number", 2)
        checktype(codesToSet, 2, "table", 2)

        local NumCodes = 256
        local bitMask = ffi.new("uint8_t [?]", NumCodes / 8)

        local shl, shr, band, bor = bit.lshift, bit.rshift, bit.band, bit.bor

        for _, code in ipairs(codesToSet) do
            check(type(code) == "number", "every value in argument #1 must be a number", 2)
            check(code >= 0 and code < NumCodes, "code value out of bounds", 2)

            bitMask[shr(code, 3)] = bor(bitMask[shr(code, 3)],
                                        shl(1, band(code, 7)))
        end

        local ptrAsInt = ffi.cast("uint64_t", bitMask)
        return input_mask_t(typ, ffi.sizeof(bitMask), ptrAsInt), bitMask
    end,
}

local Identity = function(object)
    return object
end

local IoctlReturn = {
    [EVIOCGNAME] = function(deviceNameBuf)
        deviceNameBuf[ffi.sizeof(deviceNameBuf) - 1] = 0
        return ffi.string(deviceNameBuf)
    end,

    [EVIOCGABS.X] = Identity,
    [EVIOCGABS.Y] = Identity,
}

----------

local api = {
    EV = EV,

    EVIOC = {
        GNAME = EVIOCGNAME,
        GABS = {
            X = EVIOCGABS.X,
            Y = EVIOCGABS.Y,
        },

        SMASK = EVIOCSMASK,
    },
}

-- Originally adapted from:
--  https://github.com/canselcik/libremarkable/wiki/Reading-Parade-TrueTouch-Gen5-Multitouch-Input
api.MultiTouchCode = util.MakeBimap
{
    { "POSX", ABS.MT_POSITION_X },
    { "POSY", ABS.MT_POSITION_Y },
    { "PRESSURE", ABS.MT_PRESSURE },
    { "TRACKING_ID", ABS.MT_TRACKING_ID },
    { "TOUCH_MAJOR", ABS.MT_TOUCH_MAJOR },
    { "TOUCH_MINOR", ABS.MT_TOUCH_MINOR },
    { "ORIENTATION", ABS.MT_ORIENTATION },
    { "TOOL_TYPE", ABS.MT_TOOL_TYPE },
    { "ABS_DISTANCE", ABS.MT_DISTANCE },
}

api.EventDevice = class
{
    function(device)
        check(type(device)  == "number" or type(device) == "string",
              "argument #1 must be a number or string", 2)
        local devFileName = (type(device) == "number") and
            ("/dev/input/event%d"):format(device) or
            device

        local fd = ffi.C.open(devFileName, posix.O.RDONLY)
        local errMsg = (fd == -1) and posix.getErrnoString() or nil

        return {
            fd = posix.Fd(fd),
            errMsg = errMsg,
        }
    end,

    getRawFd = function(self)
        return self.fd.fd
    end,

    close = function(self)
        self.fd:close()
    end,

    ioctl = function(self, request, ...)
        checktype(request, 1, "number", 2)
        local cType = IoctlTypes[request]
        check(cType ~= nil, "unsupported ioctl", 2)

        local data, anchor = cType(...)
        local ret, errMsg = ioctl(self.fd.fd, request, data)

        if (ret == nil) then
            return nil, errMsg
        end

        local finalizer = IoctlReturn[request]
        return (finalizer ~= nil) and finalizer(data) or nil
    end,

    readEvent = function(self)
        local event = input_event_t()
        return self.fd:readInto(event, false)
    end,
}

-- Done!
return api
