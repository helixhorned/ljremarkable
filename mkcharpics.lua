#!/usr/bin/env luajit

local ffi = require("ffi")

local io = require("io")
local os = require("os")

local freetype = require("freetype")
local build = require("build")
local class = require("class").class
local parsecmdline = require("ljclang.parsecmdline_pk")

local assert = assert
local print = print
local tonumber = tonumber

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

-- TODO: extend to allow multiple fonts.
local fontFileOrMapName = opts.f
local outFileName = opts.o

if (not outFileName:match("%.ART$")) then
    -- TODO: .charpics
    usage("Output file name must end in '.ART'")
end

local isFontMap = fontFileOrMapName:match("%.fontmap")
if (isFontMap) then
    usage(".fontmap input not yet implemented")
end

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

-- TODO on demand: Extend as necessary, potentially with user-provided directories.
local FontSearchPath = {
    "/usr/share/fonts"
}

local function CreateFace(lib, fontFileName)
    return lib:face(fontFileName):setCharSize(nil, 120)
end

local FontRenderer = class
{
    function()
        assert(not isFontMap)

        local lib = freetype.Library()
        local faces = {
            not isFontMap and CreateFace(lib, fontFileOrMapName) or nil
        }

        return {
           lib_ = lib,
           faces_ = faces,
        }
    end,

    renderChar = function(self, ...)
        return self.faces_[1]:renderChar(...)
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
