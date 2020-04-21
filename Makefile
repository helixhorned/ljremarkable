
# User configuration
include config.make

LJREMARKABLE_LAYOUTS ?=
layouts += $(LJREMARKABLE_LAYOUTS)

########## PROGRAMS ##########

markdown := $(shell which $(MARKDOWN))

extractrange := ./ljclang/extractrange.lua
# LJClang application that is expected to be installed:
EXTRACTDECLS := extractdecls
extractdecls := $(shell which $(EXTRACTDECLS))

########## RULES ##########

.PHONY: all app check_extractdecls clean decls doc upload veryclean ljclang_deps ljclang_clean ljclang_veryclean
.PHONY: docker-build docker-run layouts

linux_decls_lua := linux_decls.lua
linux_decls_lua_tmp := $(linux_decls_lua).tmp
remarkable_decls_lua := remarkable_decls.lua
remarkable_decls_lua_tmp := $(remarkable_decls_lua).tmp

all: decls

app: grabscreen.app.lua

check_extractdecls:
	@(test -n "$(extractdecls)" && test -x "$(extractdecls)") || \
		(echo "ERROR: '$(EXTRACTDECLS)' not found in PATH." && false)

clean: ljclang_clean
	$(RM) $(remarkable_decls_lua) $(remarkable_decls_lua_tmp) \
		$(linux_decls_lua) $(linux_decls_lua_tmp)

veryclean: clean ljclang_veryclean
	$(RM) $(remarkable_decls_lua).reject $(linux_decls_lua).reject layouts/??.*

layouts: mklayout.lua xkb_symbols_reader.lua
	./mklayout.lua -q $(layouts)

ljclang_clean:
	$(MAKE) -C ljclang clean

ljclang_veryclean:
	$(MAKE) -C ljclang veryclean

ljclang_deps:
	$(MAKE) -C ljclang all app_dependencies

# Docker

LJREMARKABLE_BRANCH ?= master

docker-build:
	DOCKER_BUILDKIT=1 docker build . --tag ljremarkable-dev --build-arg ljrM_branch="$(LJREMARKABLE_BRANCH)"

docker-rebuild:
	DOCKER_BUILDKIT=1 docker build . --tag ljremarkable-dev --no-cache --build-arg ljrM_branch="$(LJREMARKABLE_BRANCH)"

docker-run:
	docker run -it --rm ljremarkable-dev

# Linux framebuffer interface exposed to us

CHECK_EXTRACTED_LINUX_CMD := $(luajit) -e "require'linux_decls'"

linux_fb_h ?= /usr/include/linux/fb.h
linux_input_h ?= /usr/include/linux/input.h
sed_replace_ints_cmds := s/__u16 /uint16_t /g; s/__u32 /uint32_t /g

$(linux_decls_lua): check_extractdecls $(linux_fb_h) Makefile
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

decls: $(linux_decls_lua) $(remarkable_decls_lua)

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

app_name := grabscreen.app.lua

# Application unity file
grabscreen.app.lua: grabscreen.lua $(linux_decls_lua) $(remarkable_decls_lua) ljclang_deps
	@$(MAKE_APP_ENV) $(luajit) -l ljclang.mkapp $< -Q >/dev/null 2>&1 && test -e $(app_name) && \
		chmod +x $(app_name) && printf "* \033[1mCreated $(app_name)\033[0m\n" || \
		(printf "* \033[1;31mError\033[0m creating $(app_name)\n" && false)

# TODO: make app_dependencies in ./ljclang
upload: decls
	./cp2rM.sh
