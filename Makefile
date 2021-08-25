
# User configuration
include config.make

ifeq ($(LJREMARKABLE_ALLOW_EXT_FONTS),0)
  allowExtFonts :=
else
  allowExtFonts := t
endif

layouts := $(fixed_layouts)
layouts += $(if $(allowExtFonts),$(LJREMARKABLE_USER_LAYOUTS))

last_release := v0.9.0
# Obtained using: git checkout <version-tag>; git ls-files -s <file>
last_release_app_blob := 999defb07

########## PROGRAMS ##########

THIS_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

markdown := $(shell which $(MARKDOWN))

extractrange := ./ljclang/extractrange.lua
# LJClang application invoked from the tree:
EXDECL_LDLIB_PATH := $(shell make --silent -C ljclang print-extractdecls-library-path)
EXDECL_ENV := LD_LIBRARY_PATH='$(EXDECL_LDLIB_PATH)' LUA_PATH='$(THIS_DIR)/ljclang/?.lua;;'
EXTRACTDECLS := $(THIS_DIR)/ljclang/extractdecls.lua
MKDECLS_ENV := $(EXDECL_ENV) LJCLANG_EXTRACTDECLS=$(EXTRACTDECLS)
extractdecls := $(EXDECL_ENV) $(EXTRACTDECLS)
MOONGLOW_ENV := MOONGLOW_EXTRACTDECLS="$(extractdecls)"

########## RULES ##########

.PHONY: all app check_extractdecls clean decls doc upload veryclean ljclang_deps ljclang_clean ljclang_veryclean
.PHONY: layouts codepoints moonglow_deps moonglow_clean committed-generated showtiles
.PHONY: checkout-last-release install-last-release capture-local-screen capture-rM-screen

linux_decls_lua := linux_decls.lua
linux_decls_lua_tmp := $(linux_decls_lua).tmp
remarkable_decls_lua := remarkable_decls.lua
remarkable_decls_lua_tmp := $(remarkable_decls_lua).tmp
freetype_decls_lua := freetype_decls.lua
freetype_decls_lua_tmp := $(freetype_decls_lua).tmp
app_name := grabscreen.app.lua

fixedLayoutFiles := $(fixed_layouts:%=./layouts/%)
layoutFiles := $(layouts:%=./layouts/%)

committed_generated_files := $(fixedLayoutFiles) $(linux_decls_lua) $(remarkable_decls_lua)

all: decls

app: $(app_name)

.SILENT: ensure_extractdecls
ensure_extractdecls:
	echo "Ensuring dependencies for extractdecls.lua"
	make --silent -C ljclang extractdecls_deps

clean: ljclang_clean moonglow_clean src_clean
	$(RM) $(remarkable_decls_lua) $(remarkable_decls_lua_tmp) \
		$(linux_decls_lua) $(linux_decls_lua_tmp) \
		$(freetype_decls_lua) $(freetype_decls_lua_tmp) \
		layouts/.codepoints layouts/.charpics layouts/.lua \
		grabscreen.app.lua _setup_rM-app.lua

committed-generated: $(committed_generated_files) ensure_extractdecls
	$(MAKE) -C ljclang committed-generated
	@echo "<MOONGLOW_ENV> $(MAKE) -C moonglow committed-generated"
	@$(MOONGLOW_ENV) $(MAKE) -C moonglow committed-generated

install: app
	install $(app_name) $(BINDIR)/$(app_name)
	install pi-rM-control.sh $(BINDIR)/pi-rM-control.sh

# NOTE: use 'cat-file' and not 'checkout' so as not to add the file to the Git staging area.
checkout-last-release:
	git cat-file blob $(last_release_app_blob) > $(app_name)
	touch --date=`git for-each-ref --format='%(creatordate:iso-strict)' refs/tags/$(last_release)` $(app_name)

# TODO: upload-last-release?
install-last-release: checkout-last-release
	@$(MAKE) --assume-old=$(app_name) install

veryclean: clean ljclang_veryclean
	$(RM) $(remarkable_decls_lua).reject $(linux_decls_lua).reject layouts/??.* layouts/???.* layouts/????.* layouts/.codepoints

# NOTE: prevent interpretation as layouts:
.PHONY: layouts/*.fontmap

layouts/%: mklayout.lua xkb_symbols_reader.lua
	@./mklayout.lua -q $@
layouts: $(layoutFiles)

layouts/.codepoints: mkcodepoints.lua $(layoutFiles)
	./mkcodepoints.lua $(layoutFiles) > $@ || ($(RM) $@ && false)
codepoints: layouts/.codepoints

layouts/.lua: ./make_layouts_as_single_lua.sh layouts/.codepoints $(layoutFiles)
	(./make_layouts_as_single_lua.sh layouts/.codepoints $(layoutFiles) > $@ && \
	 luajit -e "assert(require('kb_layout_util').get_table(loadfile('layouts/.lua')))") \
	|| ($(RM) $@ && false)

ljclang_clean:
	$(MAKE) -C ljclang clean

ljclang_veryclean:
	$(MAKE) -C ljclang veryclean

ljclang_deps:
	$(MAKE) -C ljclang all app_dependencies

moonglow_clean:
	$(MAKE) -C moonglow clean

moonglow_deps: ensure_extractdecls
	@echo "<MOONGLOW_ENV> $(MAKE) -C moonglow all"
	@$(MOONGLOW_ENV) $(MAKE) -C moonglow all

src_clean:
	$(MAKE) -C src clean

# Linux framebuffer interface exposed to us

CHECK_EXTRACTED_LINUX_CMD := $(luajit) -e "require'linux_decls'"

linux_fb_h ?= /usr/include/linux/fb.h
linux_input_h ?= /usr/include/linux/input.h
sed_replace_ints_cmds := s/__u16 /uint16_t /g; s/__u32 /uint32_t /g

$(linux_decls_lua): ensure_extractdecls $(linux_fb_h) Makefile
	@echo 'local ffi=require"ffi"' > $(linux_decls_lua_tmp)
	@echo 'ffi.cdef[[' >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_fix_screeninfo {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_bitfield {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_var_screeninfo {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@echo ']]' >> $(linux_decls_lua_tmp)
	@echo 'return { FBIO = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^FBIO.*SCREENINFO' -s '^FBIO' $(linux_fb_h) >> $(linux_decls_lua_tmp)
	@echo '}]],' >> $(linux_decls_lua_tmp)
	@echo 'EV = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^EV_' -s '^EV_' $(linux_input_h) >> $(linux_decls_lua_tmp)
	@echo '}]],' >> $(linux_decls_lua_tmp)
	@echo 'ABS = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^ABS_MT_' -s '^ABS_' $(linux_input_h) >> $(linux_decls_lua_tmp)
	@echo '}]],' >> $(linux_decls_lua_tmp)
	@echo 'FB_TYPE = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^FB_TYPE_' -s '^FB_TYPE_' $(linux_fb_h) >> $(linux_decls_lua_tmp)
	@echo '}]],' >> $(linux_decls_lua_tmp)
	@echo 'FB_VISUAL = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^FB_VISUAL_' -s '^FB_VISUAL_' $(linux_fb_h) >> $(linux_decls_lua_tmp)
	@echo '}]],' >> $(linux_decls_lua_tmp)
	@echo '}' >> $(linux_decls_lua_tmp)
	@mv $(linux_decls_lua_tmp) $@
	@($(CHECK_EXTRACTED_LINUX_CMD) && \
	    printf "* \033[1mGenerated $@\033[0m\n") \
	|| (printf "* \033[1;31mError\033[0m generating $@\n" && \
	    mv $@ $@.reject && false)

# reMarkable ioctl structures / enums exposed to us

remarkable_lib_h := ./libremarkable/legacy-c-impl/libremarkable/lib.h

# NOTE: inline the ioctl reMarkable prefix.
# From $(libremarkable_lib_h):
#  0x4048 is the Remarkable Prefix
#  'F' (0x46) is the namespace
$(remarkable_decls_lua): $(remarkable_lib_h) Makefile
	@echo 'local ffi=require"ffi"' > $(remarkable_decls_lua_tmp)
	@echo 'ffi.cdef[[' >> $(remarkable_decls_lua_tmp)
	@./ljclang/extractrange.lua $(remarkable_lib_h) \
		'^typedef enum _eink_ioctl_command {' \
		'^} eink_ioctl_command;' | \
		sed 's/= /= 0x40484600 | /g' >> $(remarkable_decls_lua_tmp)
	@echo ']]' >> $(remarkable_decls_lua_tmp)
	@echo 'ffi.cdef[[' >> $(remarkable_decls_lua_tmp)
	@./ljclang/extractrange.lua $(remarkable_lib_h) \
		'^typedef enum _auto_update_mode {' \
		'^} mxcfb_dithering_mode;' >> $(remarkable_decls_lua_tmp)
	@echo ']]' >> $(remarkable_decls_lua_tmp)
	@echo 'return {}' >> $(remarkable_decls_lua_tmp)
	@echo ''
	@mv $(remarkable_decls_lua_tmp) $@
	@($(luajit) -e "require 'remarkable_decls'" && \
	    printf "* \033[1mGenerated $@\033[0m\n") \
	|| (printf "* \033[1;31mError\033[0m generating $@\n" && \
	    mv $@ $@.reject && false)

$(freetype_decls_lua): ensure_extractdecls
$(freetype_decls_lua): ./dev/freetype_decls.lua.in Makefile
	@$(MKDECLS_ENV) ./ljclang/mkdecls.sh $< > $(freetype_decls_lua_tmp)
	@mv $(freetype_decls_lua_tmp) $@
	@($(luajit) -e "require 'freetype_decls'" && \
	    printf "* \033[1mGenerated $@\033[0m\n") \
	|| (printf "* \033[1;31mError\033[0m generating $@\n" && \
	    mv $@ $@.reject && false)

decls: $(linux_decls_lua) $(remarkable_decls_lua) $(freetype_decls_lua)

# Documentation
.SILENT: doc
doc: README.md
ifneq ($(markdown),)
	$(markdown) README.md > README.html \
	    && printf "* \033[1mGenerated README.html\033[0m\n"
else
	echo "* Did not generate README.html: '$(MARKDOWN)' not installed"
endif

MAKE_APP_ENV := LD_LIBRARY_PATH="./ljclang" LUA_PATH=";;./ljclang/?.lua"

_setup_rM-app.lua: _setup_rM.lua $(linux_decls_lua) $(remarkable_decls_lua) ljclang_deps
	$(MAKE_APP_ENV) $(luajit) -l ljclang.mkapp $< -Q >/dev/null 2>&1 && \
		test -e _setup_rM.app.lua && mv _setup_rM.app.lua $@

# Application unity file
# NOTE: XLIB_LUA_CONDITIONAL_REQUIRE.
grabscreen.app.lua: layouts/.lua
grabscreen.app.lua: grabscreen.lua $(linux_decls_lua) $(remarkable_decls_lua) ljclang_deps
	@if test -z "$$DISPLAY"; then if test x"$$LJREMARKABLE_ALLOW_NO_X_APP_LUA" != x'1'; then \
	echo "ERROR: $(app_name) would be built without X support, but no override specified."; false; fi; fi
	@test -n "$$DISPLAY" || echo "WARNING: $(app_name) created without X support."
	@$(MAKE_APP_ENV) $(luajit) -l ljclang.mkapp $< -Q >/dev/null 2>&1 && test -e $@ && \
		chmod +x $@ && printf "* \033[1mCreated $(app_name)\033[0m\n" || \
		(printf "* \033[1;31mError\033[0m creating $(app_name)\n" && false)

upload: grabscreen.app.lua rM_ul_eye_menu_hidden_46-28.dat layouts/.charpics
	@if [ x`uname -m` != x"armv7l" ]; then echo "ERROR: Cannot upload app to the rM: processor architecture does not match"; false; fi
	scp $^ "$(LJREMARKABLE_TABLET_USER)@$(LJREMARKABLE_TABLET_HOST):"

upload-debugging-setup: _setup_rM-app.lua layouts/.charpics
	@if [ x`uname -m` != x"armv7l" ]; then echo "ERROR: Cannot upload app to the rM: processor architecture does not match"; false; fi
	scp $^ "$(LJREMARKABLE_TABLET_USER)@$(LJREMARKABLE_TABLET_HOST):"

## Visual exploration / debugging

VIS_ENV := LUA_PATH=";;ljclang/?.lua;./moonglow/?.lua"

capture-local-screen:
	./dev/capture-fb.sh local screen-local-`date +%Y%m%d-%H%M%S`.png

capture-rM-screen:
	./dev/capture-fb.sh rM screen-rM-`date +%Y%m%d-%H%M%S`.png

capture-rM-offscreen:
	./dev/capture-fb.sh rM-offscreen screen-rM.off-`date +%Y%m%d-%H%M%S`.png

PALETTE.DAT: ./dev/mkpalette.lua
	$< $@

ifeq ($(allowExtFonts),)
  fontMapFile := ./layouts/bundled_only.fontmap
else
  fontMapFile := ./layouts/bundled_and_on_system.fontmap
endif

# Not debugging, but here because the invocation is the same as for TILES000.ART
layouts/.charpics: ./charpics.lua ./mkcharpics.lua $(fontMapFile) ./layouts/.codepoints $(freetype_decls_lua)
	$(VIS_ENV) ./mkcharpics.lua -f $(fontMapFile) -c ./layouts/.codepoints -o $@ || ($(RM) $@ && false)

TILES000.ART: ./charpics.lua ./mkcharpics.lua $(fontMapFile) ./layouts/.codepoints $(freetype_decls_lua)
	$(VIS_ENV) ./mkcharpics.lua -f $(fontMapFile) -c ./layouts/.codepoints -o $@ || ($(RM) $@ && false)

showtiles: TILES000.ART PALETTE.DAT moonglow_deps ./moonglow/lunart.lua
	@$(VIS_ENV) $(SHOW_TILES_ENV) ./moonglow/lunart.lua $<
