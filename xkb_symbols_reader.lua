local io = require("io")
local string = require("string")
local table = require("table")

local kb_layout_util = require("kb_layout_util")

local assert = assert
local error = error
local loadfile = loadfile
local tonumber = tonumber
local type = type

----------

local api = {}

-- NOTE: /usr/include/X11/keysymdef.h calls these "mnemonic names", actually.
--  The keysyms are the numeric values written in hex.
local SYM_Pattern = '([^, ]+)'

local KeyDefLinePat = ('^key <KEY> { %[ SYM , SYM(.*)%] };$')
    :gsub(' ', ' ?')
    :gsub('KEY', '([^>]+)')
    :gsub('SYM', SYM_Pattern)

-- Number of keys in the mostly-user-customizable area.
-- Four rows of ten keys each.
-- Two of them are special though, see below.
local TotalSourceKeyCount = 39
-- NOTE [SHIFT_MOD]: The leftmost key of our fourth row is our Shift/mod:
local TotalDestKeyCount = TotalSourceKeyCount + 1
local KeyIdxFactor = 10

local us_basic_AZ_map
-- ^ obtained on demand
local function ourKeyIdxForLat(capitalChar)
    if (us_basic_AZ_map == nil) then
        us_basic_AZ_map = kb_layout_util.get_AZ_map("./layouts/us.basic")
    end

    local keyIdx = us_basic_AZ_map[capitalChar]

    if (keyIdx == nil or not (keyIdx >= 1 and keyIdx <= TotalDestKeyCount)) then
        error(("INTERNAL ERROR: unexpected value in us.basic A-Z map: %s -> %s")
                :format(capitalChar, keyIdx))
    end

    return KeyIdxFactor * keyIdx
end

-- TODO: layout merging?
--
--  For example, with a layout that has non-latin base characters, we may want to have
--  '1'-'9'+'0' and ',' and '.' as stroke-down (tertiary) characters.

local function ourIdxForKey(key)
    assert(type(key) == "string")

    local rowChar, colNumStr = key:match('^A([EDCB])([0-9]+)$')
    if (rowChar == nil or colNumStr == nil) then
        -- Try 'Lat*', present in the phonetic layouts.
        local capitalChar = key:match('^Lat([A-Z])$')
        return (capitalChar ~= nil) and ourKeyIdxForLat(capitalChar) or nil
    end

    local row = string.byte('E') - string.byte(rowChar)
    assert(row >= 0 and row <= 3)
    local col = tonumber(colNumStr, 10)

    local function isUserKey(tempKeyIdx)
        return (tempKeyIdx >= 1 and tempKeyIdx <= TotalSourceKeyCount
                -- Our backspace/DEL key:
                    and tempKeyIdx ~= 30)
    end

    local function mapKey(tempKeyIdx)
        -- SHIFT_MOD: so the affected key indexes are offset by one.
        return tempKeyIdx + (tempKeyIdx >= 31 and 1 or 0)
    end

    local tempKeyIdx = (col >= 1 and col <= 10) and 10*row + col or nil
    return (tempKeyIdx and isUserKey(tempKeyIdx)) and
        KeyIdxFactor * mapKey(tempKeyIdx) or nil
end

local parseSubLayoutLine  -- "forward-declare" function

-- NOTE: location on Raspbian, pull out when necessary.
local XkbSymbolsDir = "/usr/share/X11/xkb/symbols"

-- 'subLayout' == nil means to find the default sub-layout in the symbols file.
local function read_symbols(baseName, subLayout, result, quiet)
    assert(type(baseName) == "string")
    assert(subLayout == nil or type(subLayout) == "string")
    assert(result == nil or type(result) == "table")

    local fileName = XkbSymbolsDir.."/"..baseName

    local f, msg = io.open(fileName)
    if (f == nil) then
        return nil, msg
    end

    local isTopLevel = (result == nil)
    local result = isTopLevel and { count = 0, hadInclude = false, hadWarnings = false } or result
    local inSubLayout = false
    local previousLine
    local lineNum = 0

    -- NOTE: this parsing is very ad-hoc! Expect issues with xkb symbols files not formatted
    --  accoring to the expectations encoded here.
    --
    -- Examples which will not work:
    --  - closing brace not on separate line:
    --     xkb_symbols "lk44x"   { include "digital_vndr/us(pcxalga)" };
    --  - key definition not on a single line. (See 'parseSubLayoutLine()'.)
    while (true) do
        local line = f:read("*l")
        if (line == nil) then
            break
        end

        lineNum = lineNum + 1

        -- Remove comments.
        line = line:gsub("//.*", "")
        -- Trim.
        line = line:gsub("^[ \t]+", "")
        line = line:gsub("[ \t]+$", "")
        -- Normalize whitespace.
        line = line:gsub("[ \t]+", " ")

        local lineSubLayout = line:match'^xkb_symbols *"([^"]+)"'
        local isLineSubLayoutDefault = (previousLine ~= nil and previousLine:match("^default[ \t]") ~= nil)
        local isSubLayoutMatch =
            (subLayout ~= nil and lineSubLayout == subLayout) or
            (subLayout == nil and lineSubLayout ~= nil and isLineSubLayoutDefault)

        if (not inSubLayout and isSubLayoutMatch) then
            inSubLayout = true
        elseif (inSubLayout) then
            if (lineSubLayout ~= nil) then
                error(("%s:%d: encountered 'xkb_symbols' line before end of current sub-layout")
                        :format(fileName, lineNum))
            elseif (line == "};") then
                -- Done with this <file>.<sublayout>!
                inSubLayout = false
                break
            end

            parseSubLayoutLine(fileName, line, lineNum, result, quiet)
        end

        previousLine = line
    end

    f:close()

    if (result.count == 0) then
        return nil, ("Did not find sub-layout '%s' in %s"):format(subLayout, fileName)
    end

    return result
end

-- In: 'line'
-- Out: 'result' (modified in-place)
--
-- 'fileName' and 'lineNum' are purely for debugging purposes.
function parseSubLayoutLine(fileName, line, lineNum, result, quiet)
    local includedLayoutName = line:match('^include *"([^"]+)"')
    if (includedLayoutName ~= nil) then
        local PRIM = "([^%(]+)"
        local SUB = "%((.+)%)"
        local primaryLayout, rest = includedLayoutName:match("^"..PRIM.."(.*)")

        if (primaryLayout == nil) then
            error(("%s:%d: failed parsing 'include' directive"):format(fileName, lineNum))
        end

        local subLayout = rest:match(SUB.."$")
        if (not (rest == "" or subLayout ~= nil)) then
            error(("%s:%d: unexpected form of 'include' directive"):format(fileName, lineNum))
        end

        local res_, msg = read_symbols(primaryLayout, subLayout, result, quiet)
        if (res_ == nil) then
            local ourLayoutSpec = primaryLayout.."."..subLayout
            error(("in processing include %s: %s"):format(ourLayoutSpec, msg))
        end

        assert(res_ == result)
        result.hadInclude = true

        return
    end

    local key, sym1, sym2, rest = line:match(KeyDefLinePat)

    -- TODO:
    --  - Let pass only keys representing a printable character.
    --  - More generally: simulate passing to 'xdotool key' with a specially set up program?
    --    For an attempt at containment and for easy (direct) addressing: in a separate
    --    window running under a dummy (off-screen) X driver?
    local ourKeyIdx = (key ~= nil) and ourIdxForKey(key) or nil
    if (ourKeyIdx == nil) then
        return
    end

    local sym3 = rest:match(" ?, ?"..SYM_Pattern)

    if (result[ourKeyIdx] ~= nil or result[ourKeyIdx + 1] ~= nil or result[ourKeyIdx + 2] ~= nil) then
        if (not result.hadInclude) then
            -- NOTE: it is a bit inappropriate error due to unexpected input, but we are
            --  intended to be driven from the command line or scripts.
            error(("%s:%d: key %d encountered twice, not overwriting"):format(
                    fileName, lineNum, ourKeyIdx/KeyIdxFactor))
        end
    end

    local redefined = ""

    local function defineKey(oki, offset, sym)
        if (sym:match"^dead_") then
            if (not quiet) then
                io.stderr:write(("warning: %s:%d: blocked definition of key: %d[%d] to '%s'\n")
                        :format(fileName, lineNum, oki/KeyIdxFactor, offset, sym))
            end
            result.hadWarnings = true
            return 0
        else
            local oldSym = result[oki + offset]
            if (oldSym ~= nil and sym ~= oldSym) then
                redefined = redefined..tonumber(offset)
            end
            result[oki + offset] = sym
            return 1
        end
    end

    local n =
        -- Primary key.
        defineKey(ourKeyIdx, 0, sym1) +
        -- Secondary (shifted) key.
        defineKey(ourKeyIdx, 1, sym2)

    if (sym3 ~= nil and (sym3 ~= sym1 and sym3 ~= sym2)) then
        -- Tertiary key. (usually AltGr?)
        n = n + defineKey(ourKeyIdx, 2, sym3)
    end

    if (#redefined > 0) then
        if (not quiet) then
            io.stderr:write(("warning: %s:%d: redefined key %d[%s]\n"):format(
                    fileName, lineNum, ourKeyIdx/KeyIdxFactor, redefined))
        end
        result.hadWarnings = true
    end

    result.count = result.count + n
end

function api.as_lua(layout, quiet)
    assert(quiet == nil or type(quiet) == "boolean")

    local baseName, subLayout = layout:match("^([^%.]+)%.([^%.]+)$")
    if (baseName == nil or subLayout == nil) then
        return nil, "Layout must consist of two names separated by a dot."
    end

    local result, msg = read_symbols(baseName, subLayout, nil, quiet)
    if (result == nil) then
        return nil, msg
    end
--[[
    -- Hard-code special characters.
    -- TODO: allow customization and/or more general operations over the 'result' table.
    assert(result[300] == nil and result[301] == nil and result[310] == nil)
    result[300] = "U232B" -- ⌫ ERASE TO THE LEFT
    result[301] = "U2326" -- ⌦ ERASE TO THE RIGHT
    result[310] = "U2302" -- ⌂ HOUSE
--]]
    local strTab = {
        "#!/bin/false ljremarkable keyboard layout definition",
        "-- -*-lua-*-",
        "return {"
    }

    local getKeyDefLine = function(ourKeyIdx)
        local sym = result[ourKeyIdx]
        if (sym == nil and ourKeyIdx % 10 >= 2) then
            return nil
        end
        return (" [%d]=%s,"):format(
            ourKeyIdx,
            sym ~= nil and ("%q"):format(sym) or "nil")
    end

    local function getKeySym(k, o)
        return result[KeyIdxFactor*k + o]
    end

    for k = 1, TotalDestKeyCount do
        if (k > 1 and k % 10 == 1) then
            strTab[#strTab + 1] = ''
        end
        for o = 0, 2 do
            strTab[#strTab + 1] = getKeyDefLine(KeyIdxFactor*k + o)
        end
        strTab[#strTab + 1] = " --"
    end

    strTab[#strTab + 1] = "}\n"

    return table.concat(strTab, '\n'), (quiet and result.hadWarnings) and
        "INFO: one or more warnings not printed." or nil
end

-- Done!
return api
