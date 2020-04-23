
# Keyboard layouts to extract:
#  Source: /usr/share/X11/xkb/symbols/??
#  Destination: ./layouts/??.<sub-layout>
# See ./mklayout.lua
layouts := us.basic us.intl

luajit := luajit

LJREMARKABLE_TABLET_USER ?= $(USER)
LJREMARKABLE_TABLET_HOST ?= remarkable

# Will use this Markdown processor for .md -> .html if it is found:
MARKDOWN := cmark
