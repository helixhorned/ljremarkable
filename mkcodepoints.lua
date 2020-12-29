#!/usr/bin/env luajit

local io = require("io")
local os = require("os")
local string = require("string")
local table = require("table")

local kb_layout_util = require("kb_layout_util")

local assert = assert
local error = error
local ipairs = ipairs
local loadfile = loadfile
local print = print
local tonumber = tonumber
local type = type

local arg = arg

----------

if (arg[1] == nil) then
    print("Usage: mkcodepoints.lua <layoutFile1> [<layoutFile2> ...]")
    print(" (Layout files generated by mklayout.lua reside in layout/)")
    os.exit(1)
end

local function readMnemonicMap()
    local SymDefFileName = "/usr/include/X11/keysymdef.h"
    local codePts = {}

    local SP_ = "[ \t]"
    local SPp, SPz = SP_..'+', SP_..'*'
    local CommentStartPat = "/%*[%"..string.char(40).." ]"  -- ASCII 40: paren-open

    -- NOTE: the comment in the header file permits to match textually. We slightly
    --  tweak the pattern from the Perl regex there.
    local PrefixPattern = "^"..SPz.."#"..SPz.."define XK_([a-zA-Z_0-9]+)"..SPp.."0x[0-9a-fA-F]+(.*)"
    local SuffixPattern = SPz..CommentStartPat.."U%+([0-9a-fA-F]+)"

    local lineNum = 0
    local readCodePtCount = 0

    for line in io.lines(SymDefFileName) do
        lineNum = lineNum + 1
        local mnemonic, rest = line:match(PrefixPattern)

        if (codePts[mnemonic] ~= nil) then
            error(("%s:%d: Duplicate mnemonic '%s'"):format(
                    SymDefFileName, lineNum, mnemonic))
        end

        if (mnemonic ~= nil) then
            local codePtStr = rest:match(SuffixPattern)

            if (codePtStr == nil) then
                -- OK, mnemonic with a keysym but not UCS code point.
                codePts[mnemonic] = true
            else
                -- NOTE: on Raspbian Buster, there are always four hex digits after the
                --  'U+', even though the regex in the comment allows up to 6.
                if (#codePtStr ~= 4) then
                    error(("%s:%d: Unexpected length of code point string %d"):format(
                            SymDefFileName, lineNum, #codePtStr))
                end

                local codePt = tonumber("0x"..codePtStr)
                assert(codePt ~= nil)

                codePts[mnemonic] = codePt
                readCodePtCount = readCodePtCount + 1
            end
        end
    end

    if (readCodePtCount == 0) then
        error("Did not read any code points from "..SymDefFileName)
    end

    return codePts
end

-- [<mnemonic>] = true | <UCS code point>
local allCodePts = readMnemonicMap()

----------

local TotalDestKeyCount = 40
local KeyIdxFactor = 10
local MaxShift = 2

local mnemonics = {}  -- [<running index>] = mnemonic
local codePts = {}  -- sub-map of 'allCodePts'

local function isMnemonicUCS(mnemonic)
    -- Notes:
    --  * Match exactly four hex digits for now.
    --  * For more disjointness it should be only uppercase letters, but lowercase do appear
    --    as well. (Though less frequently.)
    return mnemonic:match("^U[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]$")
end

local function isSpecialMnemonic(mnemonic)
    return
        -- NOTE: do not exclude digit characters!
        -- FIXME: what was this about?
        mnemonic:match("^[0-9][0-9]+$") or
        mnemonic:match("^0x[0-9A-Fa-f]+$")
end

for _, layoutFileName in ipairs(arg) do
    local layoutTab, msg = kb_layout_util.get_table(loadfile(layoutFileName))
    if (layoutTab == nil) then
        io.stderr:write(("ERROR: failed loading '%s' as Lua table: %s\n")
                :format(layoutFileName, msg))
        os.exit(2)
    end

    for k = 1, TotalDestKeyCount do
        for s = 0, MaxShift do
            local ourKeyIdx = KeyIdxFactor*k + s
            local mnemonic = layoutTab[ourKeyIdx]

            if (mnemonic ~= nil and not isSpecialMnemonic(mnemonic)) then
                if (codePts[mnemonic] == nil) then
                    -- NOTE: it's redundant to enter the mapping for 'U<hexdigit>*'
                    --  mnemonics into the table for the mapping aspect, but since it will
                    --  also be used for validation of input from the server, go ahead.
                    local directCodePt = isMnemonicUCS(mnemonic) and tonumber("0x"..mnemonic:sub(2)) or nil
                    local mappedCodePt = allCodePts[mnemonic]
                    local codePt = directCodePt or mappedCodePt
                    if (codePt == nil) then
                        error(("%s: failed obtaining UCS codepoint for mnemonic '%s'")
                                :format(layoutFileName, mnemonic))
                    elseif (directCodePt and mappedCodePt) then
                        error(("%s: mnemonic '%s' is ambiguous")
                                :format(layoutFileName, mnemonic))
                    end

                    mnemonics[#mnemonics + 1] = mnemonic
                    codePts[mnemonic] = codePt
                end
            end
        end
    end
end

table.sort(mnemonics)

local strTab = {
    "#!/bin/false ljremarkable UCS code points",
    "-- -*-lua-*-",
    "return {"
}

for _, mnemonic in ipairs(mnemonics) do
    local codePt = codePts[mnemonic]
    assert(codePt)

    strTab[#strTab + 1] = ("[%q]=%s,"):format(mnemonic, codePt)
end

strTab[#strTab + 1] = "}\n"

io.stdout:write(table.concat(strTab, '\n'))
