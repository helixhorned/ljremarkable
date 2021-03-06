local ffi = require("ffi")
local C = ffi.C

local bit = require("bit")

local class = require("class").class
local error_util = require("error_util")
local linux_decls = require("linux_decls")
local posix = require("posix")

local check = error_util.check
local checktype = error_util.checktype

local FBIO = linux_decls.FBIO
local MAP = posix.MAP
local O = posix.O
local PROT = posix.PROT
local FB_TYPE = linux_decls.FB_TYPE
local FB_VISUAL = linux_decls.FB_VISUAL

local assert = assert
local error = error
local unpack = unpack

----------

local fb_fix_screeninfo = ffi.typeof("struct fb_fix_screeninfo")
local fb_var_screeninfo = ffi.typeof("struct fb_var_screeninfo")

ffi.cdef[[
int ioctl(int fd, unsigned long request, ...);
]]

local function ioctl(fd, request, arg)
    -- NOTE: will error if 'arg' present and not convertible to pointer.
    local ptr = (arg ~= nil) and ffi.new("void *", arg) or nil
    local vargs = { ptr }

    local res = C.ioctl(fd, request, unpack(vargs))

    if (res == -1) then
        return nil, posix.getErrnoString()
    end

    -- NOTE: 'res' discarded (currently no use).
    return arg
end

----------

local api = {
    ioctl = ioctl,
}

local function GetPixelTypes(bitsPerPixel)
    local intTypeStr = "uint"..bitsPerPixel.."_t"
    return ffi.typeof(intTypeStr), ffi.typeof("const "..intTypeStr)
end

local function GetOffsets(vi)
    check(vi.red.msb_right == 0 and vi.green.msb_right == 0 and vi.blue.msb_right == 0,
          "Most-significant-bit-is-right unsupported", 5)

    return vi.red.offset, vi.green.offset, vi.blue.offset
end

local function GetLengths(vi)
    return vi.red.length, vi.green.length, vi.blue.length
end

local function ExtractComponent(px, off, len)
    assert(off >= 0)
    assert(len >= 1)
    local mask = bit.lshift(1, len) - 1
    return bit.band(bit.rshift(px, off), mask)
end

local function GetUnpackPixelFunc(vi)
    local oR, oG, oB  = GetOffsets(vi)
    local lR, lG, lB  = GetLengths(vi)

    return function(px)
        return
            ExtractComponent(px, oR, lR),
            ExtractComponent(px, oG, lG),
            ExtractComponent(px, oB, lB),
            0
    end
end

local Mapping = class
{
    function(fb)
        local vinfo = fb:getVarInfo()
        check(fb.type == FB_TYPE.PACKED_PIXELS, "Only packed pixels supported", 3)
        check(fb.visual == FB_VISUAL.TRUECOLOR, "Only truecolor supported", 3)

        check(vinfo.xoffset == 0 and vinfo.yoffset == 0,
              "Only offset-less format supported", 2)
        assert(vinfo.xres <= vinfo.xres_virtual and vinfo.yres <= vinfo.yres_virtual)

        -- NOTE: this will error if there is no uint<BPP>_t type.
        local pixelType, constPxType = GetPixelTypes(vinfo.bits_per_pixel)
        local pixelPtrType = ffi.typeof("$ *", fb.writable and pixelType or constPxType)
        local pixelArrayType = ffi.typeof("$ [?]", pixelType)

        local fbSize = fb.line_length * vinfo.yres_virtual
        check(fbSize > 0, "INTERNAL ERROR: framebuffer has size zero", 1)
        local voidPtr = posix.mmap(
            nil, fbSize,
            PROT.READ + (fb.writable and PROT.WRITE or 0),
            fb.writable and MAP.SHARED or MAP.PRIVATE,
            fb.fd, 0)

        local pixelSize = vinfo.bits_per_pixel / 8
        assert(fb.line_length == pixelSize * vinfo.xres_virtual)

        return {
            -- Anchor mmap()ed memory (which will unmap on garbage collection).
            voidPtr_ = voidPtr,

            ptr = pixelPtrType(voidPtr),
            pxType = pixelType,
            pxSize = pixelSize,
            pxPtrType = pixelPtrType,
            pxArrayType = pixelArrayType,
            unpackPxFunc = GetUnpackPixelFunc(vinfo),

            xres_virtual = vinfo.xres_virtual,

            -- public:
            xres = vinfo.xres,
            yres = vinfo.yres,
        }
    end,

    -- CAUTION!
    getBasePointer = function(self)
        return self.ptr
    end,

    getLinearIndex = function(self, x, y)
        self:checkCoords(x, y)
        return self.xres_virtual * y + x
    end,

    getPixelPointer = function(self, x, y)
        return self.ptr + self:getLinearIndex(x, y)
    end,

    getPixelSize = function(self)
        return self.pxSize
    end,

    getPixelType = function(self)
        return self.pxType
    end,

    getRawSize = function(self)
        return self.xres_virtual * self.yres
    end,

    --== Reading

    getUnpackPixelFunc = function(self)
        return self.unpackPxFunc
    end,

    readRect = function(self, xb, yb, xlen, ylen)
        self:checkRect(xb, yb, xlen, ylen)

        local destArray = self.pxArrayType(xlen * ylen)

        for i = 0, ylen - 1 do
            local srcPtr = self:getPixelPointer(xb, yb + i)
            local destPtr = destArray + i * xlen
            ffi.copy(destPtr, srcPtr, xlen * self:getPixelSize())
        end

        return destArray
    end,

    --== Writing

    -- CAUTION: no bounds check for 'srcPtr'!
    writeRect = function(self, xb, yb, xlen, ylen, srcPtr)
        self:checkRect(xb, yb, xlen, ylen)
        -- Check that pointer type matches.
        self.pxPtrType(srcPtr)

        local lineByteCount = xlen * self:getPixelSize()

        for i = 0, ylen - 1 do
            local destPtr = self:getPixelPointer(xb, yb + i)
            ffi.copy(destPtr, srcPtr + i * xlen, lineByteCount)
        end
    end,

    fill = function(self, xb, yb, xlen, ylen, pixValue)
        self:checkRect(xb, yb, xlen, ylen)
        checktype(pixValue, 5, "number", 2)

        for y = yb, yb + ylen - 1 do
            local ptr = self:getPixelPointer(xb, y)
            for x = 0, xlen - 1 do
                ptr[x] = pixValue
            end
        end
    end,

-- private:
    checkRect = function(self, xb, yb, xlen, ylen)
        self:checkCoords(xb, yb)
        check(xlen >= 1 and ylen >= 1, "must specify a non-empty rectangle", 3)
        self:checkCoords(xb + xlen - 1, yb + ylen - 1)
    end,

    checkCoords = function(self, x, y)
        assert(x >= 0 and x <= self.xres - 1, "x coordinate out of bounds")
        assert(y >= 0 and y <= self.yres - 1, "y coordinate out of bounds")
    end,
}

api.FrameBuffer = class
{
    function(fbIndex, writable)
        checktype(fbIndex, 1, "number", 2)
        checktype(writable, 2, "boolean", 2)

        local deviceFileName = "/dev/fb"..fbIndex
        local fd = C.open(deviceFileName, writable and O.RDWR or O.RDONLY)
        if (fd == -1) then
            error("Failed opening "..deviceFileName..": "..posix.getErrnoString())
        end

        local finfo, errMsg = ioctl(fd, FBIO.GET_FSCREENINFO, fb_fix_screeninfo())
        if (finfo == nil) then
            C.close(fd)
            error("Failed getting fixed framebuffer info: "..posix.getErrnoString())
        end

        return {
            fd = fd,
            writable = writable,

            type = finfo.type,
            visual = finfo.visual,
            line_length = finfo.line_length,
        }
    end,

    getVarInfo = function(self)
        local vinfo, errMsg = ioctl(self.fd, FBIO.GET_VSCREENINFO, fb_var_screeninfo())
        if (vinfo == nil) then
            error("Failed getting variable framebuffer info: "..posix.getErrnoString())
        end
        return vinfo
    end,

    getMapping = function(self)
        return Mapping(self)
    end,

    close = function(self)
        C.close(self.fd)
    end,
}

-- Done!
return api
