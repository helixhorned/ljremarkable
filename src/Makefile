
opt_warn_flags := -O2
opt_warn_flags += -Werror -Wall -Wextra -pedantic

lib_flags := -shared -fPIC -ldl

cflags := -std=c99 $(opt_warn_flags) $(lib_flags)

.PHONY: all clean

all: ljrM_pdfium_interpose.so

clean:
	$(RM) ljrM_pdfium_interpose.so

ljrM_pdfium_interpose.so: pdfium_interpose.c pdfium/fpdfview.h interpose/include/interpose.h
	$(CC) $(cflags) -o $@ $<
