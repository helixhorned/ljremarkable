#!/usr/bin/env luajit

local ffi = require("ffi")

local io = require("io")
local os = require("os")
local table = require("table")

local freetype = require("freetype")
local build = require("build")
local charpics = require("charpics")
local class = require("class").class
local kb_layout_util = require("kb_layout_util")
local parsecmdline = require("ljclang.parsecmdline_pk")

local assert = assert
local ipairs = ipairs
local loadfile = loadfile
local pairs = pairs
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
    elseif (not (opts.f and opts.o)) then
        usage(haveAnyOpt and "Must provide both -f and -o options" or nil)
    end
end

local fontFileOrMapName = opts.f
local outFileName = opts.o

local isART = outFileName:match("%.ART$")
if (not (isART or outFileName:match("%.charpics"))) then
    usage("Output file name must end in '.ART' or '.charpics'")
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

local codePtRange = (codePtRangeStr ~= nil) and GetCodePtRange() or nil

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

local function ReadCodePoints(codePtsFileName)
    local tab, msg = kb_layout_util.get_table(loadfile(codePtsFileName))
    checkOrExit(tab ~= nil, "Failed reading %s as Lua table: %s\n",
                    codePtsFileName, msg)

    local function check(cond, fmt, ...)
        checkOrExit(cond, "%s: "..fmt, codePtsFileName, ...)
    end

    local codePoints = {}

    for mnemonic, codePt in pairs(tab) do
        check(type(mnemonic) == "string", "table keys must be strings")
        local isNumber = (type(codePt) == "number")
        check(isNumber or codePt == true,
              "table values must be numbers or the boolean value true")
        if (isNumber) then
            codePoints[#codePoints + 1] = codePt
        end
    end

    table.sort(codePoints)
    return codePoints
end

local codePoints = (codePointsFileName ~= nil) and ReadCodePoints(codePointsFileName) or nil

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

local function iota(b, e)
    local t = {}
    for i = b, e do
        t[i - b + 1] = i
    end
    return t
end

assert((codePoints ~= nil) ~= (codePtRange ~= nil))
codePoints = codePoints or iota(codePtRange[1], codePtRange[2])

local missingCount = 0

for _, c in ipairs(codePoints) do
    -- NOTE: allow char 1 to render as 'not defined' / "[?]" glyph to have one instance of
    --  it somewhere.
    local tileTab = renderer:renderChar(c, {[1]=true})
    if (tileTab ~= nil) then
        artTab[c] = isART and convertForBUILD(tileTab) or tileTab
    else
        missingCount = missingCount + 1
    end
end

if (isART) then
    build.writeart(outFileName, artTab)
    -- When writing to ART, there is exploration / debugging intent,
    -- so signal error only if there was no character rendered at all.
    os.exit(missingCount == #codePoints and 1 or 0)
else
    -- When writing to .charpics, signal success only if all characters end up in the file.
    local droppedCodePoints = charpics.write(outFileName, artTab)
    local exitCode =
        (missingCount > 0 and 1 or 0) +
        (#droppedCodePoints > 0 and 2 or 0)

    if (missingCount > 0) then
        io.stderr:write(("WARNING: Failed rendering %d out of %d characters.\n")
                :format(missingCount, #codePoints))
    end
    if (#droppedCodePoints > 0) then
        io.stderr:write(("WARNING: Failed packing %d out of %d character tiles.\n")
                :format(#droppedCodePoints, #codePoints - missingCount))
    end

    os.exit(exitCode)
end
