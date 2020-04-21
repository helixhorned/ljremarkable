#!/usr/bin/env luajit

local io = require("io")
local os = require("os")

local xkb_symbols_reader = require("xkb_symbols_reader")

local assert = assert
local error = error
local loadstring = loadstring
local pcall = pcall
local print = print
local setfenv = setfenv
local type = type

local arg = arg

----------

local layout = arg[1]

if (layout == nil) then
    print("Usage: mklayout.lua <layout>")
    os.exit(1)
end

local layoutAsLua, msg = xkb_symbols_reader.as_lua(layout)
if (layoutAsLua == nil) then
    io.stderr:write(("ERROR: %s\n"):format(msg))
    os.exit(1)
end
assert(type(layoutAsLua) == "string")

-- Validate generated Lua code.
do
    local func, loadErrMsg = loadstring(layoutAsLua, "<generated>")
    if (func == nil) then
        error("INTERNAL ERROR: not loadable as Lua code: "..loadErrMsg)
    end

    func = setfenv(func, {})
    local ok, callErrMsg = pcall(func)
    if (not ok) then
        error("INTERNAL ERROR: failed executing in an empty environment: "..callErrMsg)
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
