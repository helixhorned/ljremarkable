
local ffi = require("ffi")

local FT = require("freetype_decls").FT
local ft = ffi.load("freetype")

local class = require("class").class
local error_util = require("error_util")
local checktype = error_util.checktype

local assert = assert
local error = error
local type = type

----------

local api = {}

local FT_Library = ffi.typeof("FT_Library")
local FT_Library_Array = ffi.typeof("$ [?]", FT_Library)
local FT_Face = ffi.typeof("FT_Face")
local FT_Face_Array = ffi.typeof("$ [?]", FT_Face)

local FT_GlyphSlotRec = ffi.typeof("FT_GlyphSlotRec")

local function DefaultZero(value)
    return value ~= nil and value or 0
end

local uint8_array_t = ffi.typeof("uint8_t [?]")

local Face = class
{
    function(face, parent)
        assert(ffi.istype(FT_Face, face))
        assert(type(parent) == "table")

        -- FIXME [GC_ORDER]: why do we have nondeterministic order between GC of FT_Library
        --  and FT_Face even when we link the latter to the former using '_parent'?
--        face = ffi.gc(face, ft.FT_Done_Face)

        -- Select the Unicode charmap. This seems to be the default, but let's be explicit
        -- so that we do not miss failure. From the documentation of the enum type
        -- 'FT_Encoding':
        --
        -- *   By default, FreeType enables a Unicode charmap and tags it with
        -- *   `FT_ENCODING_UNICODE` when it is either provided or can be generated
        -- *   from PostScript glyph name dictionaries in the font file.
        if (ft.FT_Select_Charmap(face, ffi.C.FT_ENCODING_UNICODE) ~= 0) then
            error("FT_Select_Charmap() with FT_ENCODING_UNICODE failed")
        end

        return {
            _face = face,
            _parent = parent
        }
    end,

    -- width and height: in points (1/72th of an inch)
    -- horizontal and vertical resolution: in dots per inch (dpi)
    setCharSize = function(self, width, height, hres, vres)
        width = DefaultZero(width)
        height = DefaultZero(height)
        hres = DefaultZero(hres)
        vres = DefaultZero(vres)

        checktype(width, 1, "number", 2)
        checktype(height, 2, "number", 2)
        checktype(hres, 3, "number", 2)
        checktype(vres, 4, "number", 2)

        local FACTOR = 64
        local err = ft.FT_Set_Char_Size(
            self._face, FACTOR*width, FACTOR*height, hres, vres)
        if (err ~= 0) then
            error("FT_Set_Char_Size() failed")
        end

        return self
    end,

    renderChar = function(self, codePoint)
        checktype(codePoint, 1, "number", 2)

        local err = ft.FT_Load_Char(self._face, codePoint, FT.LOAD_RENDER)
        if (err ~= 0) then
            error('FT_Load_Char() failed')
        end

        local slot = self._face.glyph
        assert(slot ~= nil)
        local bitmap = slot.bitmap
        assert(bitmap ~= nil)

        local size = bitmap.pitch * bitmap.rows
        assert(not (size == 0) or bitmap.pitch == 0, "Unexpected non-zero pitch for empty bitmap")
        assert(not (size ~= 0) or (bitmap.pitch > 0), "Unexpected non-down-flowing bitmap")
        assert(bitmap.pixel_mode == FT.PIXEL_MODE_GRAY, "Unexpected pixel mode")
        assert(bitmap.num_grays == 256, "Unexpected number of gray levels")

        local function getData()
            -- Create a copy of the bitmap data to pass outwards.
            local array = uint8_array_t(size)
            ffi.copy(array, bitmap.buffer, size)
            return array
        end

        -- TODO: information about offsets.

        return (size > 0) and {
            w = bitmap.pitch,
            h = bitmap.rows,
            data = getData()
        } or nil
    end,
}

api.Library = class
{
    function()
        local libAr = FT_Library_Array(1)
        local err = ft.FT_Init_FreeType(libAr)
        if (err ~= 0) then
            error("FT_Init_FreeType() failed")
        end
        assert(libAr[0] ~= nil)

        return {
            _lib = libAr[0]
            -- FIXME GC_ORDER:
--            _lib = ffi.gc(libAr[0], ft.FT_Done_FreeType)
        }
    end,

    face = function(self, fileName)
        checktype(fileName, 1, "string", 2)
        local faceAr = FT_Face_Array(1)
        local err = ft.FT_New_Face(self._lib, fileName, 0, faceAr)
        if (err ~= 0) then
            error("FT_New_Face() failed")
        end
        assert(faceAr[0] ~= nil)

        return Face(faceAr[0], self)
    end,
}

-- Done!
return api
