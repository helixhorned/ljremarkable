#!/bin/bash
# SPDX-License-Identifier: MIT

set -e

codePointsFile="$1"
shift

if [ -z "$codePointsFile" ]; then
    echo "Usage: $0 <.codepoints-file> [<layout1> [<layout2> ...]]"
    exit 2
fi

function keepLuaTableOnly() {
    # - Strip '#!' and Emacs mode marker lines
    # - Strip 'return '
    sed '/^[#-]/d; s/^return //g' "$1"
}

# ----------

layoutFiles=("$@")

echo '-- ljremarkable single-file keyboard layout data'
echo '-- -*-lua-*-'
echo 'return {
codepoints='

keepLuaTableOnly "$codePointsFile"

echo ',layoutNames={'
for layoutFile in "${layoutFiles[@]}"; do
    layout=${layoutFile/.*\//}
    if [[ ! "$layout" =~ [a-z]+\.[a-z]+ ]]; then
        echo "ERROR: string validation failure for layout '$layout'"
        exit 3
    fi
    echo "\"$layout\","
done
echo '}  -- layoutNames'

echo ',layouts={'
for layoutFile in "${layoutFiles[@]}"; do
    layout=${layoutFile/.*\//}
    echo "[\"$layout\"]="
    keepLuaTableOnly "$layoutFile"
    echo ','
done
echo '}  -- layouts'

echo '}'
