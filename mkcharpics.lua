#!/usr/bin/env luajit

local ffi = require("ffi")

local io = require("io")
local os = require("os")

local freetype = require("freetype")
local build = require("build")
local class = require("class").class
local kb_layout_util = require("kb_layout_util")
local parsecmdline = require("ljclang.parsecmdline_pk")

local assert = assert
local ipairs = ipairs
local loadfile = loadfile
local print = print
local tonumber = tonumber
local type = type

local arg = arg

----------

local function usage(hline)
    if (hline) then
        io.stderr:write("ERROR: "..hline..'\n')
    end
    print[[
Usage: mkcharpics.lua <mandatoty options...> <options...> [[<startCodePt>]:<endCodePt>]
]]
    if (hline == nil) then
        print[[
  Mandatory options:
    -f {<font-file>|<file>.fontdesc}
    -o <out-file>{.ART|.charpics}
  Options:
    -c <codepoints-file>

  The code point range argument and option -c are mutually exclusive.
  <startCodePt> defaults to 1.
]]
    end

    os.exit(1)
end

-- Meta-information about command-line options.
local opt_meta = { ['c']=true, ['f']=true, ['o']=true }
local opts, args = parsecmdline.getopts(opt_meta, arg, usage)

local codePtRangeStr = args[1]
local codePointsFileName = opts.c

do
    local haveAnyOpt = (opts.f or opts.o) ~= nil
    if (codePtRangeStr == nil and codePointsFileName == nil) then
        usage(haveAnyOpt and "Must pass code point range or option -c" or nil)
    elseif (codePtRangeStr ~= nil and codePointsFileName ~= nil) then
        usage("Must pass either code point range or option -c, but not both")
    elseif (codePointsFileName ~= nil) then
        -- TODO:
        usage("Option -c not yet implemented")
    elseif (not (opts.f and opts.o)) then
        usage(haveAnyOpt and "Must provide both -f and -o options" or nil)
    end
end

local fontFileOrMapName = opts.f
local outFileName = opts.o

if (not outFileName:match("%.ART$")) then
    -- TODO: .charpics
    usage("Output file name must end in '.ART'")
end

local isFontMap = fontFileOrMapName:match("%.fontmap")

local function GetCodePtRange()
    local startCodePtStr, endCodePtStr = codePtRangeStr:match("([^:]*):([^:]+)$")
    local startCodePt = (startCodePtStr == "") and 1 or tonumber(startCodePtStr)
    local endCodePt = (endCodePtStr ~= nil) and tonumber(endCodePtStr) or nil

    if (startCodePt == nil) then
        usage("Malformed code point range: <startCodePt> must be empty or a number")
    elseif (endCodePt == nil) then
        usage("Malformed code point range: <endCodePt> must be a number")
    end

    return { startCodePt, endCodePt }
end

local codePtRange = GetCodePtRange()

----------

local function errprintfAndExit(fmt, ...)
    io.stderr:write(("ERROR: "..fmt..'\n'):format(...))
    os.exit(2)
end

local function checkOrExit(cond, fmt, ...)
    if (not cond) then
        errprintfAndExit(fmt, ...)
    end
end

local function ReadFontMapDefaults(fontMapFileName)
    local tab, msg = kb_layout_util.get_table(loadfile(fontMapFileName))
    checkOrExit(tab ~= nil, "Failed reading %s as Lua table: %s\n",
                    fontMapFileName, msg)

    local function check(cond, fmt, ...)
        checkOrExit(cond, "%s: "..fmt, fontMapFileName, ...)
    end

    local defaults = tab.defaults
    check(type(defaults) == "table", ".defaults must be a table")

    local defaultsCount = #defaults
    for i = 1, defaultsCount do
        check(type(defaults[i]) == "string",
              ".defaults must be a contiguous sequence of strings")
    end

    -- NOTE: other named or integer fields of the table are reserved for future use.
    --  (Such as actually mapping characters / character ranges to fonts.)
    return defaults
end

-- TODO on demand: Extend as necessary, potentially with user-provided directories.
local FontSearchPath = {
    "/usr/share/fonts/truetype"
}

local function FindFontFile(fontName)
    local fontFileName = FontSearchPath[1]..'/'..fontName

    local f = io.open(fontFileName)
    if (f == nil) then
        errprintfAndExit("Font %s not found in font search path %s",
                         fontName, FontSearchPath[1])
    end

    f:close()
    return fontFileName
end

local function CreateFace(lib, fontFileName)
    return lib:face(fontFileName):setCharSize(nil, 120)
end

local FontRenderer = class
{
    function()
        local lib = freetype.Library()
        local faces = {
            not isFontMap and CreateFace(lib, fontFileOrMapName) or nil
        }

        if (isFontMap) then
            local defaults = ReadFontMapDefaults(fontFileOrMapName)
            for i = 1, #defaults do
                faces[i] = CreateFace(lib, FindFontFile(defaults[i]))
            end
        end

        return {
           lib_ = lib,
           faces_ = faces,
        }
    end,

    renderChar = function(self, ...)
        for _, face in ipairs(self.faces_) do
            local tileTab = face:renderChar(...)
            if (tileTab ~= nil) then
                return tileTab
            end
        end

        -- No font was able to render the character.
        return nil
    end,
}

local renderer = FontRenderer()

local uint8_array_t = ffi.typeof("uint8_t [?]")
local function transpose(srcData, sx, sy)
    local tData = uint8_array_t(sx*sy)

    for x = 0, sx-1 do
        for y = 0, sy-1 do
            tData[sy*x + y] = srcData[sx*y + x]
        end
    end

    return tData
end

local function convertForBUILD(tileTab)
    tileTab.data = transpose(tileTab.data, tileTab.w, tileTab.h)
    return tileTab
end

local artTab = {
    -- Make sure there is a tile 0 so that LunART renders it at the top left border instead
    -- of the first present tile. This is for alignment convenience. Ideally, LunART should
    -- handle it though.
    [0] = {
        w = 1,
        h = 1,
        data = uint8_array_t(1)
    }
}

for c = codePtRange[1], codePtRange[2] do
    -- NOTE: allow char 1 to render as 'not defined' / "[?]" glyph to have one instance of
    --  it somewhere.
    local tileTab = renderer:renderChar(c, {[1]=true})
    if (tileTab ~= nil) then
        -- TODO: keep this only for debugging.
        artTab[c] = convertForBUILD(tileTab)
    end
end

build.writeart(outFileName, artTab)
