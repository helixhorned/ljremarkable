#!/usr/bin/env luajit

local io = require("io")
local os = require("os")

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
    print("Usage: mklayout.lua <layout1> [<layout2> ...]")
    os.exit(1)
end

for _, layout in ipairs(arg) do
    print("["..layout.."]")

    local layoutAsLua, msg = xkb_symbols_reader.as_lua(layout)
    if (layoutAsLua == nil) then
        io.stderr:write(("ERROR: %s\n"):format(msg))
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

    local layoutLuaFileName = ("./layouts/%s"):format(layout)

    local f, msg = io.open(layoutLuaFileName, 'w')
    if (f == nil) then
        io.stderr:write("ERROR: Failed opening for writing "..msg..'\n')
        os.exit(1)
    end

    f:write(layoutAsLua)
    f:close()
end
