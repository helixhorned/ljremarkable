#!/bin/bash

whose="$1"
outFile="$2"

function usage() {
    echo "Usage: $0 {local|rM} <outFile>.png"
    exit 1
}

if [[ x"$whose" != x"local" && x"$whose" != x"rM" ]]; then
    if [ -n "$whose" ]; then
        echo "ERROR: sub-command is unexpected." >&2
    fi
    usage
elif [ -z "$outFile" ]; then
    echo "ERROR: must specify an output file." >&2
    usage
fi

REMARKABLE_FB_SIZE=1408,1872

function do_ffmpeg() {
    pixfmt="$1"
    size="$2"

    # Command adapted from libremarkable wiki.
    ffmpeg -loglevel error \
        -vcodec rawvideo \
        -f rawvideo \
        -pix_fmt $pixfmt \
        -s $size \
        -i - \
        -vframes 1  \
        -f image2 \
        -vcodec png \
        "$outFile" && echo "$outFile"
}

if [ $whose == local ]; then
    fbInfoCmd="fbset --show -fb /dev/fb0"
    regex="^    geometry [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+"
    xres_virtual=$($fbInfoCmd | grep -E "$regex" | awk '{ print $4 }')
    yres=$($fbInfoCmd | grep -E "$regex" | awk '{ print $3 }')

    # Notes:
    #  - Do not parse pixel format, just assume the one under Raspbian.
    #  - Zero the alpha channel.
    cat /dev/fb0 | do_ffmpeg bgr0 "$xres_virtual,$yres"
else
    user="$LJREMARKABLE_TABLET_USER"
    if [ -z "$user" ]; then
        user="$USER"
    fi
    host="$LJREMARKABLE_TABLET_HOST"
    if [ -z "$host" ]; then
        host=remarkable
    fi

    ssh "$user"@"$host" "cat /dev/fb0" | do_ffmpeg rgb565le $REMARKABLE_FB_SIZE
fi
