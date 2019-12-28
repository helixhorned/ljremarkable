#!/bin/bash

d=./ljclang

files="\
$d/class.lua \
$d/error_util.lua \
$d/posix.lua \
$d/posix_decls.lua \
$d/posix_types.lua \
\
framebuffer.lua \
linux_decls.lua \
remarkable_decls.lua \
remarkable.lua \
"

scp $files $USER@remarkable:~
