
########## CONFIG ##########

luajit ?= luajit

########## RULES ##########

.PHONY: clean upload

remarkable_decls_lua := remarkable_decls.lua
remarkable_decls_lua_tmp := $(remarkable_decls_lua).tmp

clean:
	$(RM) \
		$(remarkable_decls_lua) $(remarkable_decls_lua_tmp)

veryclean: clean
	$(RM) $(remarkable_decls_lua).reject

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

upload:
	./cp2rM.sh
