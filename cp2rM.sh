#!/bin/bash

d=./ljclang

files="\
$d/class.lua \
$d/error_util.lua \
$d/framebuffer.lua \
$d/linux_decls.lua \
$d/posix.lua \
$d/posix_decls.lua \
$d/posix_types.lua \
"

scp $files $USER@remarkable:~
