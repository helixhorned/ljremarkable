FROM arm32v6/alpine:3.11.5

LABEL maintainer="Philipp Kutin <philipp.kutin@gmail.com>"

# Common:
RUN apk add git make mg
# For building LuaJIT:
RUN apk add gcc libc-dev

RUN adduser -D user

USER user
WORKDIR /home/user

########## Check out, build and install LuaJIT 2.1 ##########

RUN git clone https://github.com/LuaJIT/LuaJIT.git \
    --single-branch --branch=v2.1 --shallow-since=2020-03-19 \
    ./luajit-2.1
# Check out a specific commit.
# "FFI/ARM64: Fix pass-by-value struct calling conventions."
RUN (cd luajit-2.1 && git checkout 9143e86498436892cb4316550be4d45b68a61224)
RUN git clone luajit-2.1 ./luajit-2.1-rM

## Build for the Pi.
WORKDIR /home/user/luajit-2.1
RUN make -j4
USER root
RUN make install
RUN ln -s /usr/local/bin/luajit-2.1.0-beta3 /usr/local/bin/luajit
USER user

## Build for the rM.
WORKDIR /home/user/luajit-2.1-rM
# NOTE: the reMarkable toolchain also has '-march=armv7-a' but that seems redundant.
RUN sed -i 's/^CCOPT_arm=$/CCOPT_arm= -mfpu=neon -mfloat-abi=hard -mcpu=cortex-a9/' ./src/Makefile
RUN make -j4

WORKDIR /home/user

########## Check out and build ljremarkable ##########

# NOTE: passing a branch as opposed to a commit is problematic wrt Dockerfile caching:
#  advances of the remote are not picked up here if a cached build is present.
#  And somewhat contrary to good Dockerfile style, the intent of this Dockerfile *is*
#  to build the latest commit at the specified branch.
ARG ljrM_branch=master
RUN git clone https://github.com/helixhorned/ljremarkable \
    --single-branch --branch="$ljrM_branch" --depth=1
WORKDIR ljremarkable

RUN git submodule init
RUN git submodule update ljclang

# Avoid checking out the whole libremarkable repository. We only need one header.
RUN mkdir -p libremarkable/legacy-c-impl/libremarkable
WORKDIR libremarkable/legacy-c-impl/libremarkable
RUN wget https://raw.githubusercontent.com/canselcik/libremarkable/master/legacy-c-impl/libremarkable/lib.h
RUN sha256sum lib.h | grep -q 2e718a10e9c47e4cde5387e834b1a0d76964378cf6247e8e045bf92f6d1a6db4

## Build

USER root
RUN apk add bash
RUN apk add clang-dev linux-headers
USER user

# Do not install llvm-dev, do not rely on llvm-config. (See 'sed -i' invocations below.)
# On Alpine Linux, clang-c/Index.h is in /usr/include and libclang.so is in /usr/lib.
#  Using 'llvm-config' of package 'llvm-dev' would wrongly point us to /usr/lib/llvm9.
#
# TODO in ljclang:
#  - remove altogether in favor of an alternative detection scheme that addresses all
#    now supported platforms (Ubuntu x86_64, Raspbian 32-bit, Alpine Linux here)?
ENV LLVM_CONFIG=true

# Install 'extractdecls'.
WORKDIR /home/user/ljremarkable/ljclang
RUN mkdir /home/user/bin
RUN sed -i 's|llvm_version :=.*|llvm_version := 9.0.0|' ./Makefile
RUN sed -i 's|bindir :=.*|bindir := /usr/bin|' ./Makefile
RUN sed -i 's|incdir :=.*|incdir := /usr/include|' ./Makefile
RUN sed -i 's|libdir :=.*|libdir := /does-not-exist-and-is-not-relevant-here|' ./Makefile
RUN make install-dev

# Build the application.
WORKDIR /home/user/ljremarkable
RUN PATH="$HOME/bin:$PATH" make app

# Run the LJClang tests.
USER root
RUN apk add luarocks5.1
# NOTE: CFLAGS=-I/usr/local/include/luajit-2.1 before 'luarocks-5.1' does not work:
RUN apk add lua5.1-dev
USER user
RUN luarocks-5.1 --local install busted
#
WORKDIR /home/user/ljremarkable/ljclang
RUN LJCLANG_TESTS_NO_CXX_STDLIB=1 make test

WORKDIR /home/user

########## END ljremarkable ##########

ENTRYPOINT ["/bin/sh"]
