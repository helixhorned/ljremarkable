
local ffi = require("ffi")
local C = ffi.C

local math = require("math")

local class = require("class").class
local error_util = require("error_util")
local check = error_util.check
local checktype = error_util.checktype

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

local function RequestRefresh(fd, vinfo,
                              rect)
    assert(type(fd) == "number")
    assert(ffi.istype(fb_var_screeninfo, vinfo))

    check(ffi.istype(xywh_t, rect), "argument #1 must be a remarkable.xywh", 3)

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
        region, waveform, mode, 0, displayTemp,
        0,  -- flags,
        0,  -- dither_mode (unused),
        0,  -- quant_bit (unused?)
        {}  -- alt_buffer_data, not used when flags == 0
    )

    ioctl(fd, C.MXCFB_SEND_UPDATE, data)
end

----------

local api = {
    xywh = xywh_t
}

api.Remarkable = class
{
    function()
        local fb = FB.FrameBuffer(0, true)

        return {
            fb = fb,
            fd = assert(fb.fd),
            vinfo = fb:getVarInfo(),
        }
    end,

    getFrameBuffer = function(self)
        return self.fb
    end,

    requestRefresh = function(self, ...)
        return RequestRefresh(self.fd, self.vinfo, ...)
    end,
}

-- Done!
return api
