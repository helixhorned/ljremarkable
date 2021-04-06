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
Usage: mkcharpics.lua <mandatory options...> <options...> [[<startCodePt>]:<endCodePt>]
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
if (not (isART or outFileName:match("%.charpics$"))) then
    usage("Output file name must end in '.ART' or '.charpics'")
end

local isFontMap = fontFileOrMapName:match("%.fontmap$")

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

    local haveCodePtOne = false
    local codePoints = {}

    for mnemonic, codePt in pairs(tab) do
        check(type(mnemonic) == "string", "table keys must be strings")
        local isNumber = (type(codePt) == "number")
        -- TODO: 'true' is probably obsolete (and wrong downstream), remove?
        check(isNumber or codePt == true,
              "table values must be numbers or the boolean value true")
        if (isNumber) then
            codePoints[#codePoints + 1] = codePt
            haveCodePtOne = haveCodePtOne or (codePt == 1)
        end
    end

    if (not haveCodePtOne) then
        -- Make sure that char 1, used as a placeholder for characters not placed into the
        -- charpics file, is present.
        codePoints[#codePoints + 1] = 1
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
    "./fonts",
    "/usr/share/fonts/truetype"
}

local function FindFontFile(fontName)
    for i, fontFilePrefix in ipairs(FontSearchPath) do
        local fontFileName = fontFilePrefix..'/'..fontName

        local f = io.open(fontFileName)
        if (f ~= nil) then
            f:close()
            return fontFileName
        end

        if (i == #FontSearchPath) then
            errprintfAndExit("Font %s not found in font search path %s",
                             fontName, table.concat(FontSearchPath, ':'))
        end
    end

    assert(false)
end

local function CreateFace(lib, fontFileName, charSize)
    return lib:face(fontFileName):setCharSize(nil, charSize)
end

local DefaultCharSize = 120
local SmallCharSize = 60

local FontRenderer = class
{
    function()
        local lib = freetype.Library()
        local faces = {
            not isFontMap and CreateFace(lib, fontFileOrMapName, DefaultCharSize) or nil
        }

        if (isFontMap) then
            local defaults = ReadFontMapDefaults(fontFileOrMapName)
            for i = 1, #defaults do
                local fontFile = FindFontFile(defaults[i])
                faces[i] = CreateFace(lib, fontFile, DefaultCharSize)
                faces[-i] = CreateFace(lib, fontFile, SmallCharSize)
            end
        end

        return {
           lib_ = lib,
           faces_ = faces,
        }
    end,

    renderChar = function(self, alsoSmall, ...)
        for i, face in ipairs(self.faces_) do
            local tileTab = face:renderChar(...)
            if (tileTab ~= nil) then
                local smallTileTab = alsoSmall and
                    self.faces_[-i]:renderChar(...) or nil
                return tileTab, smallTileTab
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
    local baseline = tileTab.baseline
    if (baseline >= 0 and baseline < tileTab.h) then
        -- Draw the baseline on top of the rendered glyph.
        ffi.fill(tileTab.data + baseline*tileTab.w, tileTab.w, 128)
    end

    tileTab.data = transpose(tileTab.data, tileTab.w, tileTab.h)
    return tileTab
end

local artTab = (not isART) and {} or {
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

local function PrependedWithPrintableASCII(tab)
    local FirstCodePt = 1
    local RangeLow = { b=33, e=126 }
    local RangeHigh = { b=161, e=254 }

    -- Expect table obtained from ReadCodePoints():
    assert(tab[1] == FirstCodePt)

    -- First: code point 1, printable 7-bit ASCII, printable ASCII with high bit set.
    local newTab = iota(RangeLow.b, RangeLow.e)
    table.insert(newTab, 1, FirstCodePt)
    for codePt = RangeHigh.b, RangeHigh.e do
        newTab[#newTab + 1] = codePt
    end

    local lastCodePt = 0
    local startIdx = nil

    for i, codePt in ipairs(tab) do
        if (codePt > RangeHigh.e) then
            assert(i > 1)
            startIdx = i
            break
        end

        checkOrExit(codePt == 1
                        or (codePt >= RangeLow.b and codePt <= RangeLow.e)
                        or (codePt >= RangeHigh.b and codePt <= RangeHigh.e),
                    "Non-printable ASCII characters present in code points file.")

        assert(codePt > lastCodePt)
        lastCodePt = codePt
    end

    -- Append the remaining code points, if applicable.
    if (startIdx ~= nil) then
        for i = startIdx, #tab do
            newTab[#newTab + 1] = tab[i]
        end
    end

    return newTab
end

assert((codePoints ~= nil) ~= (codePtRange ~= nil))
-- When invoked with a .codepoints file (and thus not with an explicit code point range),
-- make sure to render all printable ASCII characters. Note: the bundled Quicksand font
-- contains glyphs for all of them.
codePoints = codePoints and
    PrependedWithPrintableASCII(codePoints) or
    iota(codePtRange[1], codePtRange[2])

-- KEEPINSYNC grabscreen.lua
local CODEPOINT_PLANE_STRIDE = 0x200000
local MAX_UCS_CODE_POINT = 0x10ffff
assert(CODEPOINT_PLANE_STRIDE > MAX_UCS_CODE_POINT)

local missingCount = 0

for _, c in ipairs(codePoints) do
    assert(c >= 0 and c <= MAX_UCS_CODE_POINT, "unexpected code point value")

    -- NOTE: allow char 1 to render as 'not defined' / "[?]" glyph to have one instance of
    --  it somewhere.
    local tileTab, smallTileTab = renderer:renderChar(not isART, c, {[1]=true})
    if (tileTab ~= nil) then
        artTab[c] = isART and convertForBUILD(tileTab) or tileTab
        artTab[c + CODEPOINT_PLANE_STRIDE] = smallTileTab
    else
        missingCount = missingCount + 1

        local MaxDisplayedCount = 5

        if (missingCount <= MaxDisplayedCount) then
            io.stderr:write(("INFO: failed rendering character with code point %d (0x%x).\n"):format(c, c))
        elseif (missingCount == MaxDisplayedCount + 1) then
            io.stderr:write("INFO: stopping reporting failed character rendering.\n")
        end
    end
end

if (isART) then
    -- When writing to ART, there is exploration / debugging intent,
    -- so signal error only if there was no character rendered at all.
    if (missingCount == #codePoints) then
        io.stderr:write("ERROR: Did not render any character.\n")
        os.exit(1)
    end

    local ok = build.writeart(outFileName, artTab)
    if (not ok) then
        io.stderr:write("ERROR: Failed writing ART file.\n")
        os.exit(2)
    end

    os.exit(0)
else
    -- When writing to .charpics, signal success only if all characters end up in the file.
    local droppedCodePoints = charpics.write(outFileName, artTab)
    local exitCode =
        (missingCount > 0 and 1 or 0) +
        (#droppedCodePoints > 0 and 2 or 0)

    if (missingCount > 0) then
        io.stderr:write(("ERROR: Failed rendering %d out of %d characters.\n")
                :format(missingCount, #codePoints))
    end

    if (#droppedCodePoints > 0) then
        io.stderr:write(("ERROR: Failed packing %d out of %d character tiles.\n")
                :format(#droppedCodePoints, #codePoints - missingCount))
    end

    os.exit(exitCode)
end
