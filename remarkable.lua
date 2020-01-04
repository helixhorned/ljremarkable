
local ffi = require("ffi")
local C = ffi.C

local math = require("math")

local class = require("class").class
local error_util = require("error_util")
local check = error_util.check
local checktype = error_util.checktype
local input = require("input")

local FB = require("framebuffer")
local ioctl = FB.ioctl

local assert = assert
local type = type

require("remarkable_decls")

----------

local fb_var_screeninfo = ffi.typeof("struct fb_var_screeninfo")
local xywh_t = ffi.typeof("struct { uint32_t x, y, w, h; }")

-- NOTE: order is: top, left, width, height
local rect_t = ffi.typeof("mxcfb_rect")
local update_data_t = ffi.typeof("mxcfb_update_data")
local update_marker_data_t = ffi.typeof("mxcfb_update_marker_data")

-- The marker values appear to be system-global in the epdc_fb driver
-- (though note that we do pass a file descriptor),
-- so have an offset in the hope of avoiding collisions.
local MarkerOffset = 2^31

local function RequestRefresh(fd, vinfo,
                              rect, marker)
    assert(type(fd) == "number")
    assert(ffi.istype(fb_var_screeninfo, vinfo))

    check(ffi.istype(xywh_t, rect), "argument #1 must be a remarkable.xywh", 3)
    check(marker == nil or type(marker) == "number",
          "argument #2 must be nil or a number", 3)

    local mode = C.UPDATE_MODE_PARTIAL
    local waveform = C.WAVEFORM_MODE_GC16
    local displayTemp = C.TEMP_USE_REMARKABLE_DRAW

    local x, y = rect.x, rect.y

    -- NOTE: mxc_epdc_fb_send_single_update() errors on out-of-bounds values.
    check(x + rect.w <= vinfo.xres and y + rect.h <= vinfo.yres,
          "Update region out of bounds", 3)

    -- NOTE: Disallow empty update region. The driver does not like "odd" values.
    -- Observed in the kernel log when (0, 0, 0, 0) was passed:
    --
    --  Invalid drect width and height passed in
    --  imx_epdc_fb 20f4000.epdc: PxP operation failed due to timeout
    --  imx_epdc_fb 20f4000.epdc: Unable to complete PxP update task.
    --  pxp_dispatch_thread: task is timeout
    --
    --  WARNING: (...) at (...)/pxp_dma_v2.c:1496 pxp_alloc_chan_resources+0x5c/0x60
    check(rect.w > 0 and rect.h > 0, "Update region must not be empty", 3)

    local region = rect_t(y, x, rect.w, rect.h)

    local data = update_data_t(
        region, waveform, mode,
        (marker ~= nil) and MarkerOffset + marker or 0,
        displayTemp,
        0,  -- flags,
        0,  -- dither_mode (unused),
        0,  -- quant_bit (unused?)
        {}  -- alt_buffer_data, not used when flags == 0
    )

    ioctl(fd, C.MXCFB_SEND_UPDATE, data)
end

local function WaitForCompletion(fd,
                                 marker)
    checktype(marker, 1, "number", 3)

    local data = update_marker_data_t(MarkerOffset + marker, 0)
    ioctl(fd, C.MXCFB_WAIT_FOR_UPDATE_COMPLETE, data)
    return (data.collision_test ~= 0)
end

----------

local api = {
    xywh = xywh_t
}

local function MakeEventDevice()
    local MTC = input.MultiTouchCode
    local evd = input.EventDevice(1)
    evd:ioctl(input.EVIOC.SMASK, input.EV.ABS, {MTC.POSX, MTC.POSY, MTC.TRACKING_ID})
    return evd
end

api.Remarkable = class
{
    function(fb)
        if (fb == nil) then
            fb = FB.FrameBuffer(0, true)
        end

        -- NOTE: class type checking function would be nice.
        check(type(fb) == "table", "argument #1 must be nil or a FrameBuffer", 2)
        check(type(fb.line_length) == "number", "argument #1 must be nil or a FrameBuffer", 2)

        return {
            fb = fb,
            fd = assert(fb.fd),
            vinfo = fb:getVarInfo(),

            evd = nil,  -- input.EventDevice
        }
    end,

    getFrameBuffer = function(self)
        return self.fb
    end,

    openEventDevice = function(self)
        check(self.evd == nil, "must have no event device opened", 2)
        self.evd = MakeEventDevice()
    end,

    closeEventDevice = function(self)
        check(self.evd ~= nil, "must have the event device opened", 2)
        self.evd:close()
        self.evd = nil
    end,

    requestRefresh = function(self, ...)
        return RequestRefresh(self.fd, self.vinfo, ...)
    end,

    waitForCompletion = function(self, ...)
        return WaitForCompletion(self.fd, ...)
    end,
}

-- Done!
return api
