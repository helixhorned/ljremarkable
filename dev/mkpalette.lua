#!/usr/bin/env luajit

local ffi = require("ffi")

local io = require("io")
local os = require("os")

local fileName = arg[1]

local arg = arg

----------

if (fileName == nil) then
    io.stderr:write(("Usage: %s /path/to/out/<palette-fileName>.DAT\n"):format(arg[0]))
    io.stderr:write[[

 Creates a BUILD engine palette file suitable for viewing
 FreeType-rasterized glyphs.

]]
    os.exit(1)
end

local pal = ffi.new("uint8_t [3 * 256]")

for i = 0, 255 do
    local c = (255 - i) / 4
    for k = 0, 2 do
        pal[3 * i + k] = c
    end
end

local f, msg = io.open(fileName, 'w')
if (f == nil) then
    io.stderr:write(("Error opening %s: %s\n"):format(fileName, msg))
    os.exit(2)
end

f:write(ffi.string(pal, 3 * 256))

f:close()
