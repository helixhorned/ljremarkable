local io = require("io")
local string = require("string")
local table = require("table")

local assert = assert
local error = error
local tonumber = tonumber
local type = type

----------

local api = {}

local KeyDefLinePat = ('^key <KEY> { %[ SYM , SYM.*%] };$')
    :gsub(' ', ' ?')
    :gsub('KEY', '([^>]+)')
    :gsub('SYM', '([^, ]+)')

-- Number of keys in the mostly-user-customizable area.
-- Four rows of ten keys each.
-- Two of them are special though, see below.
local TotalKeyCount = 40
local KeyIdxFactor = 10

local function ourIdxForKey(key)
    assert(type(key) == "string")
    -- TODO: 'Lat*' for the phonetic layouts.
    local rowChar, colNumStr = key:match('^A([EDCB])([0-9]+)$')
    if (rowChar == nil or colNumStr == nil) then
        return nil
    end

    local row = string.byte('E') - string.byte(rowChar)
    assert(row >= 0 and row <= 3)
    local col = tonumber(colNumStr, 10)

    local function isUserKey(tempKeyIdx)
        return (tempKeyIdx >= 1 and tempKeyIdx <= TotalKeyCount
                -- Our backspace/DEL + Shift/mod key:
                    and not (tempKeyIdx == 30 or tempKeyIdx == 31))
    end

    local tempKeyIdx = (col >= 1 and col <= 10) and 10*row + col or nil
    return (tempKeyIdx and isUserKey(tempKeyIdx)) and
        KeyIdxFactor*tempKeyIdx or nil
end

local read_symbols  -- "forward-declare" function

-- In: 'line'
-- Out: 'result' (modified in-place)
--
-- 'fileName' and 'lineNum' are purely for debugging purposes.
local function parseSubLayoutLine(fileName, line, lineNum, result)
    local includedLayoutName = line:match('^include *"([^"]+)"')
    if (includedLayoutName ~= nil) then
        local PRIM = "([^%(]+)"
        local SUB = "%((.+)%)"
        local primaryLayout, subLayout = includedLayoutName:match("^"..PRIM..SUB.."$")

        if (primaryLayout == nil or subLayout == nil) then
            error(("%s:%d: failed parsing 'include' directive"):format(fileName, lineNum))
        end

        -- TODO: restructure?
        local ourLayoutSpec = primaryLayout.."."..subLayout
        local res_, msg = read_symbols(ourLayoutSpec, result)
        if (res_ == nil) then
            error(("in processing include %s: %s"):format(ourLayoutSpec))
        end

        assert(res_ == result)
        result.hadInclude = true

        return
    end

    local key, sym1, sym2 = line:match(KeyDefLinePat)

    -- TODO:
    --  - Let pass only keys representing a printable character.
    --  - More generally: simulate passing to 'xdotool key' with a specially set up program?
    --    For an attempt at containment and for easy (direct) addressing: in a separate
    --    window running under a dummy (off-screen) X driver?
    local ourKeyIdx = (key ~= nil) and ourIdxForKey(key) or nil
    if (ourKeyIdx == nil) then
        return
    end

    if (result[ourKeyIdx] ~= nil or result[ourKeyIdx + 1] ~= nil) then
        if (not result.hadInclude) then
            -- NOTE: it is a bit inappropriate error due to unexpected input, but we are
            --  intended to be driven from the command line or scripts.
            error(("%s:%d: key %d encountered twice, not overwriting"):format(
                    fileName, lineNum, ourKeyIdx/KeyIdxFactor))
        elseif (result[ourKeyIdx] ~= sym1 or result[ourKeyIdx + 1] ~= sym2) then
            io.stderr:write(("warning: %s:%d: potentially redefining key %d\n"):format(
                    fileName, lineNum, ourKeyIdx/KeyIdxFactor))
        end
    end

    local function defineKey(oki, offset, sym)
        if (sym:match"^dead_") then
            io.stderr:write(("warning: %s:%d: blocked definition of key: %d[%d] to '%s'\n")
                    :format(fileName, lineNum, oki/KeyIdxFactor, offset, sym))
            return 0
        else
            result[oki + offset] = sym
            return 1
        end
    end

    local n =
        -- Primary key.
        defineKey(ourKeyIdx, 0, sym1) +
        -- Secondary (shifted) key.
        defineKey(ourKeyIdx, 1, sym2)

    result.count = result.count + n
end

-- NOTE: location on Raspbian, pull out when necessary.
local XkbSymbolsDir = "/usr/share/X11/xkb/symbols"

function read_symbols(layout, result)
    assert(type(layout) == "string")
    local baseName, subLayout = layout:match("^([^%.]+)%.([^%.]+)$")
    if (baseName == nil or subLayout == nil) then
        return nil, "Layout must consist of two names separated by a dot."
    end

    local fileName = XkbSymbolsDir.."/"..baseName

    local f, msg = io.open(fileName)
    if (f == nil) then
        return nil, msg
    end

    local result = result or { count = 0, hadInclude = false }
    local inSubLayout = false
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

        if (not inSubLayout and lineSubLayout == subLayout) then
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

            parseSubLayoutLine(fileName, line, lineNum, result)
        end
    end

    f:close()

    if (result.count == 0) then
        return nil, ("Did not find sub-layout '%s' in %s"):format(subLayout, fileName)
    end

    return result
end

function api.as_lua(layout)
    local result, msg = read_symbols(layout)
    if (result == nil) then
        return nil, msg
    end

    local strTab = {
        "#!/bin/false ljremarkable keyboard layout definition",
        "-- -*-lua-*-",
        "return {"
    }

    local getKeyDefLine = function(ourKeyIdx)
        local sym = result[ourKeyIdx]
        return (" [%d]=%s,"):format(
            ourKeyIdx,
            sym ~= nil and ("%q"):format(sym) or "nil")
    end

    for k = 1, TotalKeyCount do
        if (k > 1 and k % 10 == 1) then
            strTab[#strTab + 1] = ''
        end
        strTab[#strTab + 1] = getKeyDefLine(KeyIdxFactor*k + 0)
        strTab[#strTab + 1] = getKeyDefLine(KeyIdxFactor*k + 1)
    end

    strTab[#strTab + 1] = "}\n"

    return table.concat(strTab, '\n')
end

-- Done!
return api
