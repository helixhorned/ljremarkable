
# Directory to install the application.
BINDIR ?= $(HOME)/bin

## Keyboard layouts
#  Source: /usr/share/X11/xkb/symbols/??
#  Destination: ./layouts/??.<sub-layout>
# See ./mklayout.lua

# Fixed ones, committed to the repo.
fixed_layouts := us.basic us.intl
# User-defined.
LJREMARKABLE_LAYOUTS ?=
##########

luajit := luajit

LJREMARKABLE_TABLET_USER ?= $(USER)
LJREMARKABLE_TABLET_HOST ?= remarkable

# Will use this Markdown processor for .md -> .html if it is found:
MARKDOWN := cmark
