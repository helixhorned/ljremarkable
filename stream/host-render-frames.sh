#!/bin/sh

# This script is supposed to run on the host, receiving the framebuffer dumps of
# 'rM-cat-fb.sh' running on the tablet.

width=1404
height=1872

ffplay \
	-loglevel error \
	-vcodec rawvideo \
	-f rawvideo \
	-pixel_format "bgra" \
	-video_size "$width,$height" \
	-vf "scale=$((width/2)):-1" \
	-i - \
	;
