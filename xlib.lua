-- This is an extract of 'xdotool' functionality that ljremarkable's 'grabscreen'
-- application needs.

local ffi = require("ffi")

local X = ffi.load("X11")
local Xtst = ffi.load("Xtst")

local class = require("class").class
local error_util = require("error_util")
local posix = require("posix")

local check = error_util.check
local checktype = error_util.checktype

local assert = assert

----------

-- CARD32 type: see /usr/include/X11/Xmd.h
ffi.cdef[[
typedef uint32_t CARD32;
typedef CARD32 XID;
typedef int Bool;

typedef struct _XDisplay Display;
typedef XID Window;

Display *XOpenDisplay(const char *display_name);
int XCloseDisplay(Display *display);
Window XDefaultRootWindow(Display *display);
int XWarpPointer(Display *display, Window src_w, Window dest_w,
  int src_x, int src_y,
  unsigned int src_width, unsigned int src_height,
  int dest_x, int dest_y);
int XFlush(Display *display);
Bool XQueryPointer(Display *display, Window w,
  Window *root_ret, Window *child_ret,
  int *root_x_ret, int *root_y_ret,
  int *win_x_ret, int *win_y_ret,
  unsigned int *mask_ret);

Bool XTestQueryExtension(Display *display, int *, int *, int *, int *);
int XTestFakeButtonEvent(Display *display,
  unsigned int button, Bool is_press, unsigned long delay);
]]

----------

local api = {}

-- NOTE: In X.h, these are just numbered ('Button<i>'), not named.
local Button = ffi.new[[
struct {
    static const int Left = 1;
    static const int Middle = 2;
    static const int Right = 3;
    static const int WheelUp = 4;
    static const int WheelDown = 5;
}]]

api.Button = Button

local function msleep(ms)
    posix.clock_nanosleep(1000000 * ms)
end

-- Linearly interpolate with an implicit weight of 1 for the
-- second argument:
local function weigh(weight, a, b)
    return (weight*a + b) / (weight + 1)
end

api.Display = class
{
    function(displayName)
        if (displayName ~= nil) then
            checktype(displayName, 1, "string", 2)
        end

        local display = X.XOpenDisplay(displayName)
        assert(display ~= nil, "failed opening display")

        display = ffi.gc(display, function(d)
            X.XCloseDisplay(d)
        end)

        local dia = ffi.new("int [1]")  -- dummy int array
        local tab = {
            display = display,
            haveTestExt = (Xtst.XTestQueryExtension(display, dia, dia, dia, dia) ~= 0),
        }

        return tab
    end,

    getMousePos = function(self)
        local window = X.XDefaultRootWindow(self.display)
        local uAr = ffi.new("unsigned int [1]")
        local wAr = ffi.new("Window [1]")
        local iAr = ffi.new("int [1]")
        local pos = ffi.new("int [2]")

        local ret = X.XQueryPointer(
            self.display, window, wAr, wAr, pos+0, pos+1, iAr, iAr, uAr)

        if (ret == 0) then
            return nil
        end

        return pos[0], pos[1]
    end,

    moveMouse = function(self, x, y)
        checktype(x, 1, "number", 2)
        checktype(y, 2, "number", 2)

        local window = X.XDefaultRootWindow(self.display)
        local ret = X.XWarpPointer(self.display, 0, window, 0, 0, 0, 0, x, y)
        X.XFlush(self.display)

        return (ret == 0)
    end,

    haveTestExtension = function(self)
        return self.haveTestExt
    end,

    moveMouseClicked = function(self, srcX, srcY, dstX, dstY, opDelayMs)
        check(self:haveTestExtension(), "XTest extension must be present", 2)
        checktype(opDelayMs, 5, "number", 2)
        -- Limit the delay between constituent operations to something reasonably short.
        -- On the other hand, this needs to be not too strict (i.e. short): web-based maps
        -- can take a (comparatively) long time to process mouse drag events.
        check(opDelayMs >= 0 and opDelayMs <= 200,
              "invalid inter-operation delay in milliseconds, must be in [0, 200]", 2)

        local function sleep()
            if (opDelayMs > 0) then
                msleep(opDelayMs)
            end
        end

        self:moveMouse(srcX, srcY)
        sleep()

        self:_operateMouseButton(Button.Left, 1)
        sleep()

        -- Emulate moving the mouse "just a little bit" first. Otherwise, with web-based
        -- geographic maps, there may be only a very slight effect.
        self:moveMouse(weigh(31, srcX, dstX), weigh(31, srcY, dstY))
        sleep()

        -- For dragging windows in a desktop environment, there may be a minimum drag amount
        -- required. For example, the Openbox window manager has the configuration option
        -- 'openbox_config::mouse::dragThreshold'. So, move the mouse "almost to the
        -- destination" next.
        self:moveMouse(weigh(31, dstX, srcX), weigh(31, dstY, srcY))
        sleep()

        self:moveMouse(dstX, dstY)
        sleep()

        self:_operateMouseButton(Button.Left, 0)
    end,

    clickMouse = function(self, button)
        self:_operateMouseButton(button, 1)
        msleep(1)
        self:_operateMouseButton(button, 0)
    end,

    clickMouseMultiple = function(self, button, repeatCount, interClickDelayMs)
        checktype(repeatCount, 2, "number", 2)
        checktype(interClickDelayMs, 3, "number", 2)
        -- Be very strict (because we can):
        check(interClickDelayMs >= 0 and interClickDelayMs <= 10,
              "invalid inter-click delay in milliseconds, must be in [0, 10]", 2)

        for i = 1, repeatCount do
            self:clickMouse(button)
            msleep(interClickDelayMs)
        end
    end,

-- private:
    _operateMouseButton = function(self, button, downInt)
        checktype(button, 1, "number", 3)
        check(button >= Button.Left and button <= Button.WheelDown, "invalid button", 3)
        assert(downInt == 1 or downInt == 0)

        local CurrentTime = 0  -- see X.h
        -- TODO: what is the meaning of the return value?
        Xtst.XTestFakeButtonEvent(self.display, button, downInt, CurrentTime)
        X.XFlush(self.display)
    end,
}

-- Done!
return api
