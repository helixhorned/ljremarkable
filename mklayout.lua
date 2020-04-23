#!/usr/bin/env luajit

local io = require("io")
local os = require("os")
local table = require("table")

local xkb_symbols_reader = require("xkb_symbols_reader")
local kb_layout_util = require("kb_layout_util")

local assert = assert
local error = error
local ipairs = ipairs
local loadstring = loadstring
local print = print
local type = type

local arg = arg

----------

if (arg[1] == nil) then
    print[[
Usage: mklayout.lua <layout1> [<layout2> ...]
 Each layout should be of the form <baseLayout>.<subLayout>,
 optionally prefixed by the destination directory 'layouts/'
 or './layouts/' which is stripped. Except for this special case,
 the layout name must not contain slashes.
]]
    os.exit(1)
end

local quiet = (arg[1] == "-q")
if (quiet) then
    table.remove(arg, 1)
end

local DestDir = "layouts"
local DestDirPat1 = "^"..DestDir.."/"
local DestDirPat2 = "^%./"..DestDir:sub(3).."/"

for _, layout in ipairs(arg) do
    layout = layout:gsub(DestDirPat1, ""):gsub(DestDirPat2, "")
    if (layout:match('/')) then
        io.stderr:write("ERROR: layout name must not contain slashes.\n")
        os.exit(1)
    end

    print("["..layout.."]")

    local layoutAsLua, readAsLuaMsg = xkb_symbols_reader.as_lua(layout, quiet)
    if (layoutAsLua == nil) then
        io.stderr:write(("ERROR: %s\n"):format(readAsLuaMsg))
        os.exit(1)
    end
    assert(type(layoutAsLua) == "string")

    -- Validate generated Lua code.
    do
        local tab, msg = kb_layout_util.get_table(
            -- NOTE: potentially more than one return value:
            loadstring(layoutAsLua, "<generated>"))
        if (tab == nil) then
            error("INTERNAL ERROR: "..msg)
        end
    end

    local layoutLuaFileName = (DestDir.."/%s"):format(layout)

    local f, msg = io.open(layoutLuaFileName, 'w')
    if (f == nil) then
        io.stderr:write("ERROR: Failed opening for writing "..msg..'\n')
        os.exit(1)
    end

    f:write(layoutAsLua)
    f:close()

    if (readAsLuaMsg ~= nil) then
        io.stderr:write(readAsLuaMsg..'\n')
    end
end
