#!/bin/bash

if [ x"$1" == x"-p" ]; then
    shift
fi

fileName="$1"

if [ -z "$fileName" ]; then
    echo "Usage: $0 <file.lua>"
    exit 1
fi

sed -E 's/\<([0-9]+)[uU]?[lL][lL]\>/\1/g' "$fileName" | luac -p -

if [ $? -eq 0 ]; then
    # Convention: 10 consecutive dashes mark the end of the "preamble"
    markerLineNo=`grep -o -m 1 -n '^----------' "$fileName" | sed 's/:.*//'`
    if [ -z "$markerLineNo" ]; then
        echo "No module marker"
        exit 0
    fi

    echo "Module marker at line $markerLineNo"
    # Prepare for matching the flymake-lua regexp, flagging only lines after the marker.
    listglobals.sh "$1" | awk -F ':' "\$1 > $markerLineNo" | sed "s^luac: $fileName:g"
fi
