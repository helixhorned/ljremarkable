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
grabscreen.lua \
inet.lua \
jkiss_rng.lua \
linux_decls.lua \
remarkable_decls.lua \
remarkable.lua \
rM_ul_eye_menu_hidden_46-28.dat \
"

scp $files $USER@remarkable:~
scp $d/libljposix.so $USER@remarkable:~/lib
