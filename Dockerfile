FROM arm32v6/alpine:3.11.5

LABEL maintainer="Philipp Kutin <philipp.kutin@gmail.com>"

RUN apk add git make
RUN apk add clang-dev llvm-dev
RUN apk add gcc
RUN apk add libc-dev linux-headers

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

RUN git clone https://github.com/helixhorned/ljremarkable
WORKDIR ljremarkable

RUN git submodule init
RUN git submodule update ljclang

# Avoid checking out the whole libremarkable repository. We only need one header.
RUN mkdir -p libremarkable/legacy-c-impl/libremarkable
WORKDIR libremarkable/legacy-c-impl/libremarkable
RUN wget https://raw.githubusercontent.com/canselcik/libremarkable/master/legacy-c-impl/libremarkable/lib.h
RUN sha256sum lib.h | grep -q 2e718a10e9c47e4cde5387e834b1a0d76964378cf6247e8e045bf92f6d1a6db4

# TODO: build

WORKDIR /home/user

########## END ljremarkable ##########

ENTRYPOINT ["/bin/sh"]
