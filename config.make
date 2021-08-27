
# Directory to install the application.
BINDIR ?= $(HOME)/bin

# Allow font files that are not bundled?
LJREMARKABLE_ALLOW_EXT_FONTS ?= 0

## Keyboard layouts
#  Sources:
#   - ./scripts/<Script>  (ISO 15924 four-letter code)
#   - /usr/share/X11/xkb/symbols/<file>  (2-4 letters, e.g. 'us')
#  Destination: ./layouts/<layout>.<sub-layout>
# See ./mklayout.lua

# Fixed ones, committed to the repo.
fixed_layouts := Latn.ljrM Armn.ljrM Arab.ljrM
# User-defined, only used when LJREMARKABLE_ALLOW_EXT_FONTS is not 0
LJREMARKABLE_USER_LAYOUTS ?=

##########

luajit := luajit

LJREMARKABLE_TABLET_USER ?= $(USER)
LJREMARKABLE_TABLET_HOST ?= remarkable

# Will use this Markdown processor for .md -> .html if it is found:
MARKDOWN := cmark
