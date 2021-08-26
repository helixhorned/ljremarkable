-- SPDX-License-Identifier: MIT

-- This is based on a subset of 'xdotool' functionality that ljremarkable's 'grabscreen'
-- application needs.

local ffi = require("ffi")
local bit = require("bit")

if (os.getenv("DISPLAY") == nil) then
    -- NOTE: this is only for running '.app.lua' unity files in an environment that does not
    --  come with the X libraries loaded below. Calling 'require("xlib")' conditionally
    --  won't save us since an '.app.lua' always executes the code for each module
    --  amalgamated into it.
    --
    -- TODO: address this in ljclang's mkapp.lua?
    return {}
end

-- NOTE: Debian includes the '.so' symlinks only with the '-dev' packages,
--  so explicitly specify the major version in order to not depend on them.
local X = ffi.load("X11.so.6")
local Xtst = ffi.load("Xtst.so.6")

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
typedef CARD32 Atom;
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
int XTestFakeKeyEvent(Display *display,
  unsigned int keycode, Bool is_press, unsigned long delay);
]]

ffi.cdef[[
typedef XID KeySym;
typedef unsigned char KeyCode;
]]

-- Structure definitions adapted from 'man 3 XkbFreeClientMap', 'man 3 XkbKeyType', or taken
-- over directly from '/usr/include/X11/extensions/XKBstr.h':
ffi.cdef[[
typedef struct {
    unsigned char mask;    /* effective mods */
    unsigned char real_mods;
    unsigned short vmods;
} XkbModsRec;

typedef struct {
    Bool active;
    unsigned char level;
    XkbModsRec mods;
} XkbKTMapEntryRec;

typedef struct {
    XkbModsRec        mods;         /* modifiers used to compute shift level */
    unsigned char     num_levels;   /* total # shift levels, do not modify directly */
    unsigned char     map_count;    /* # entries in map, preserve (if non-NULL) */
    XkbKTMapEntryRec *map;          /* vector of modifiers for each shift level */
    XkbModsRec *      preserve;     /* mods to preserve for corresponding map entry */
    Atom              name;         /* name of key type */
    Atom *            level_names;  /* array of names of each shift level */
} XkbKeyTypeRec;

typedef	struct {
    unsigned char     kt_index[4 /*Xkb.NumKbdGroups*/];
    unsigned char     group_info;
    unsigned char     width;
    unsigned short    offset;
} XkbSymMapRec;

typedef struct {
    unsigned char     size_types;   /* # occupied entries in types */
    unsigned char     num_types;    /* # entries in types */
    XkbKeyTypeRec *   types;        /* vector of key types used by this keymap */

    unsigned short    size_syms;    /* length of the syms array */
    unsigned short    num_syms;     /* # entries in syms */
    KeySym *          syms;         /* linear 2d tables of keysyms, 1 per key */

    XkbSymMapRec *    key_sym_map;  /* 1 per keycode, maps keycode to syms */
    unsigned char *   modmap;       /* 1 per keycode, real mods bound to key */
} XkbClientMapRec;

typedef struct {
    Display *display;  /* connection to X server */

    unsigned short    flags;        /* private to Xkb, do not modify */
    unsigned short    device_spec;  /* device of interest */
    KeyCode           min_key_code; /* minimum keycode for device */
    KeyCode           max_key_code; /* maximum keycode for device */

    void *ctrls;
    void *server;
    XkbClientMapRec *map;  /* client keymap */
    void *indicators;
    void *names;
    void *compat;
    void *geom;
} XkbDescRec;

XkbDescRec *XkbGetMap(Display *display, unsigned int which, unsigned int device_spec);
void XkbFreeClientMap(XkbDescRec *xkb, unsigned int which, Bool free_all);

KeySym XkbKeycodeToKeysym(Display *dpy, KeyCode kc, unsigned int group, unsigned int level);

Bool XkbLatchGroup(Display *display, unsigned int device_spec, unsigned int group);
Bool XkbLatchModifiers(Display *display, unsigned int device_spec, unsigned int affect, unsigned int values);
]]

local Xkb = ffi.new[[
struct {
    static const int NumKbdGroups = 4;

    static const int KeyTypesMask = (1<<0);
    static const int KeySymsMask = (1<<1);

    static const int UseCoreKbd = 0x0100;
}]]

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

local KeySymInfo = class
{
    function()
        return {
            map = {}
        }
    end,

    get = function(self, keySym)
        local pv = self.map[keySym]
        if (pv == nil) then
            return nil
        end

        return bit.band(pv, 255), bit.band(bit.rshift(pv, 8), 255), bit.rshift(pv, 16)
    end,

-- private:
    add = function(self, keySym, keyCode, group, modMask)
        self.map[keySym] = keyCode + bit.lshift(group, 8) + bit.lshift(modMask, 16)
    end,
}

local function GetModMask(keyType, level)
    local modMask = 0

    for i = 0, keyType.map_count - 1 do
        local map = keyType.map[i]
        if (map.active ~= 0 and map.level == level) then
            return map.mods.mask
        end
    end

    return modMask
end

-- Based on xdotool's '_xdo_populate_charcode_map()'.
local function GetKeySymInfo(display)
    local desc = X.XkbGetMap(display, Xkb.KeyTypesMask + Xkb.KeySymsMask, Xkb.UseCoreKbd)
    check(desc, "XkbGetMap() failed", 3)
    local clientMap = desc.map

    local res = KeySymInfo()

    for keyCode = desc.min_key_code, desc.max_key_code do
        local keySymMap = clientMap.key_sym_map[keyCode]
        local groupInfo = keySymMap.group_info
        local groupCount = bit.band(groupInfo, 0xf)
        assert(groupCount <= Xkb.NumKbdGroups)

        for group = 0, groupCount - 1 do
            local ktIdx = keySymMap.kt_index[group]
            local keyType = clientMap.types[ktIdx]

            for level = 0, keyType.num_levels - 1 do
                local keySym = X.XkbKeycodeToKeysym(display, keyCode, group, level)
                if (keySym ~= 0) then
                    res:add(keySym, keyCode, group, GetModMask(keyType, level))
                end
            end
        end
    end

    X.XkbFreeClientMap(desc, 0, true)

    return res
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
            keySymInfo = GetKeySymInfo(display)
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
        check(self:haveTestExtension(), "XTest extension must be present", 2)

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

    pressAndReleaseKey = function(self, keySym)
        check(self:haveTestExtension(), "XTest extension must be present", 2)

        local keyCode, group, modMask = self.keySymInfo:get(keySym)
        if (keyCode == nil) then
            -- TODO: handle
            return
        end

        -- For Backspace, do not latch the modifiers and group since otherwise,
        -- the effect is "clear to beginning of line".
        --
        -- FIXME: Tab. Is there an underlying problem common to the two?
        if (keySym ~= 0xff08) then
            self:_prepareSendKey(group, modMask)
        end

        self:_sendKey(keyCode, 1)
        msleep(1)
        self:_sendKey(keyCode, 0)
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

    _prepareSendKey = function(self, group, modMask)
        -- NOTE: the order of the two calls matters!
        X.XkbLatchModifiers(self.display, Xkb.UseCoreKbd, 255, modMask)
        X.XkbLatchGroup(self.display, Xkb.UseCoreKbd, group)
    end,

    _sendKey = function(self, keyCode, downInt)
        assert(keyCode >= 0 and keyCode <= 255)
        assert(downInt == 1 or downInt == 0)

        local CurrentTime = 0  -- see X.h
        Xtst.XTestFakeKeyEvent(self.display, keyCode, downInt, CurrentTime)
        X.XFlush(self.display)
    end,
}

-- Done!
return api
