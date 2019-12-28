
########## CONFIG ##########

luajit ?= luajit

########## PROGRAMS ##########

extractrange := ./ljclang/extractrange.lua
# Expected to be installed:
extractdecls := extractdecls

########## RULES ##########

.PHONY: all clean decls upload

linux_decls_lua := linux_decls.lua
linux_decls_lua_tmp := $(linux_decls_lua).tmp
remarkable_decls_lua := remarkable_decls.lua
remarkable_decls_lua_tmp := $(remarkable_decls_lua).tmp

all: decls

clean:
	$(RM) $(remarkable_decls_lua) $(remarkable_decls_lua_tmp) \
		$(linux_decls_lua) $(linux_decls_lua_tmp)

veryclean: clean
	$(RM) $(remarkable_decls_lua).reject $(linux_decls_lua).reject

# Linux framebuffer interface exposed to us

CHECK_EXTRACTED_LINUX_CMD := $(EXTRACT_CMD_ENV) $(luajit) \
    -e "require'linux_decls'"

linux_fb_h ?= /usr/include/linux/fb.h
sed_replace_ints_cmds := s/__u16 /uint16_t /g; s/__u32 /uint32_t /g

$(linux_decls_lua): $(EXTRACTED_ENUMS_LUA) $(linux_fb_h) Makefile
	@echo 'local ffi=require"ffi"' > $(linux_decls_lua_tmp)
	@echo 'ffi.cdef[[' >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_fix_screeninfo {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_bitfield {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@$(extractrange) $(linux_fb_h) '^struct fb_var_screeninfo {' '^};' | sed "$(sed_replace_ints_cmds)" >> $(linux_decls_lua_tmp)
	@echo ']]' >> $(linux_decls_lua_tmp)
	@echo 'return { FBIO = ffi.new[[struct {' >> $(linux_decls_lua_tmp)
	@$(extractdecls) -w MacroDefinition -C -p '^FBIO.*SCREENINFO' -s '^FBIO' $(linux_fb_h) >> $(linux_decls_lua_tmp)
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
	@echo ''
	@mv $(remarkable_decls_lua_tmp) $@
	@($(luajit) -e "require 'remarkable_decls'" && \
	    printf "* \033[1mGenerated $@\033[0m\n") \
	|| (printf "* \033[1;31mError\033[0m generating $@\n" && \
	    mv $@ $@.reject && false)

decls: $(linux_decls_lua) $(remarkable_decls_lua)

upload: decls
	./cp2rM.sh
