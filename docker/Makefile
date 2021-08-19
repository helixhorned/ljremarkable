
# From the 'GNU make' manual:
#  "backslash/newline handling is modified slightly to conform to POSIX.2"
.POSIX:

########## VARIABLES ##########

## User

# For target 'run' and 'get-artifacts', to be used interactively, e.g.
#  $ make run ljc<TAB...>                            --(completion)-->
#  $ make run ljremarkable-dev-arm32v7/debian        --(edit)-->
#  $ make run IMAGE=ljremarkable-dev-arm32v7/debian  <--(invoke this)
IMAGE ?=

## Private

THIS_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

include $(THIS_DIR)/../config.make

commit := $(shell git rev-parse --short=12 HEAD)
image_arch_os_pair :=

ifneq ($(IMAGE),)
  ifeq ($(findstring ~ljremarkable-dev-,~$(IMAGE)),)
    $(error IMAGE must start with 'ljremarkable-dev-')
  endif
  _image_arch_os_pair := $(subst ~ljremarkable-dev-,,~$(IMAGE))
  image_arch_os_pair := $(subst /,-,$(_image_arch_os_pair))
endif

native_machine := $(shell uname -m)
# The architecture part of Docker image names:
native_arch = null

ifeq ($(native_machine),armv7l)
  native_arch := arm32v7
endif
ifeq ($(native_machine),aarch64)
  native_arch := arm64v8
endif
ifeq ($(native_machine),x86_64)
  native_arch := amd64
endif

_os-release_contents := $(file < /etc/os-release)
host_distro := $(subst ID=,,$(filter ID=%,$(_os-release_contents)))
host_distro_supported := $(filter alpine debian ubuntu,$(host_distro))

########## RULES ##########

_upload_userhost := $(LJREMARKABLE_TABLET_USER)@$(LJREMARKABLE_TABLET_HOST)

.PHONY: all
all:
	@echo 'Usage:'
	@echo ' $$ make ljremarkable-dev-<arch>/<distro>'
	@echo '  Build a Docker image. It has the given name and is tagged'
	@echo '  with the short-commit-SHA of *this* Git repository (ljremarkable).'
	@echo "  Passing NOCACHE=1 will add '--no-cache' to the 'docker build' invocation(s)."
	@echo ''
	@echo ' $$ make run IMAGE=ljremarkable-dev-<arch>/<distro>'
	@echo '  Run the specified Docker image, which must have been built before.'
	@echo ''
	@echo ' The following two convenience targets build the appropriate Docker images'
	@echo ' automatically and take certain configuration from ../config.make:'
	@echo ''
	@echo ' $$ make install'
	@echo '  Install the application and control script to the configured BINDIR,'
	@echo "  currently: '$(BINDIR)'."
	@echo ''
	@echo ' $$ make upload'
	@echo '  Upload the LuaJIT binary and the application to the reMarkable tablet.'
	@echo "  Currently configured SSH connection destination: '$(_upload_userhost)'."
	@echo ''

.PHONY: run
run:
	@test -n '$(IMAGE)'
	docker run -it --rm '$(IMAGE):$(commit)'

## Rules for the actual Docker images
# NOTE: no convenience logic like in ljclang to prevent code duplication.

.PHONY: ljremarkable-dev-arm32v7/alpine ljremarkable-dev-arm32v7/debian ljremarkable-dev-arm32v7/ubuntu
.PHONY: ljremarkable-dev-arm64v8/alpine ljremarkable-dev-arm64v8/debian ljremarkable-dev-arm64v8/ubuntu
.PHONY: ljremarkable-dev-amd64/alpine ljremarkable-dev-amd64/debian ljremarkable-dev-amd64/ubuntu

_build_args_file_prefix := BUILD_ARGS_FILE_PREFIX='$(THIS_DIR)/vars'
_suffix_Dockerfile_in := SUFFIX_DOCKERFILE_IN='$(THIS_DIR)/Dockerfile.in'
_overrides := OVERRIDE_IMAGE_NAME=ljremarkable-dev OVERRIDE_IMAGE_TAG=$(commit)
_additional_context_repo := ADDITIONAL_CONTEXT_REPO='$(THIS_DIR)/..'
recursive_make_vars := $(_build_args_file_prefix) $(_suffix_Dockerfile_in) $(_overrides) $(_additional_context_repo)

# NOTE: the '--silent' is there to prevent recursive 'make --silent' invocations (which are
#  intended to merely print a value) to not output text like "make[2]: Entering directory".
ljremarkable-dev-arm32v7/alpine \
ljremarkable-dev-arm32v7/debian \
ljremarkable-dev-arm32v7/ubuntu \
ljremarkable-dev-arm64v8/alpine \
ljremarkable-dev-arm64v8/debian \
ljremarkable-dev-arm64v8/ubuntu \
ljremarkable-dev-amd64/alpine \
ljremarkable-dev-amd64/debian \
ljremarkable-dev-amd64/ubuntu:
	$(MAKE) --silent -C ../ljclang/docker $(subst ljremarkable,ljclang,$@) $(recursive_make_vars)

_get-artifacts-cat := docker run --rm --entrypoint cat '$(IMAGE):$(commit)'
_out_dir := out/$(image_arch_os_pair)

define get_file =
  $(_get-artifacts-cat) /home/user/$(1) > '$(_out_dir)/$(2)'
endef

define get_executable =
  $(call get_file,$(1),$(2)) && chmod +x '$(_out_dir)/$(2)'
endef

.PHONY: get-artifacts
get-artifacts: $(IMAGE)
	@test -n "$(image_arch_os_pair)" || (echo 'Must pass IMAGE=ljremarkable-dev-<arch>/<os>.' && false)
	@docker image inspect '$(IMAGE):$(commit)' > /dev/null
	mkdir -p '$(_out_dir)'
	$(call get_executable,luajit-2.1/src/luajit,luajit)
ifeq ($(image_arch_os_pair),arm32v7-debian)
	$(call get_executable,luajit-2.1-rM/src/luajit,luajit-rM)
endif
	$(call get_executable,ljremarkable/grabscreen.app.lua,grabscreen.app.lua)
	$(call get_file,ljremarkable/layouts/.charpics,.charpics)
	$(call get_executable,ljremarkable/_setup_rM-app.lua,_setup_rM-app.lua)

# NOTE: Mark a real output file as phony so as to avoid specifying dependencies.
#  Will be always re-obtained, but better have redundant updates than missed ones.
.PHONY: out/arm32v7-debian/grabscreen.app.lua
out/arm32v7-debian/grabscreen.app.lua:
	IMAGE=ljremarkable-dev-arm32v7/debian $(MAKE) --silent get-artifacts

_native_app_path := out/$(native_arch)-$(host_distro)/grabscreen.app.lua

.PHONY: install
install:
ifeq ($(native_arch),null)
	@echo "ERROR: unsupported CPU hardware type '$(native_machine)'." && false
else
ifeq ($(host_distro_supported),)
	@echo "ERROR: unsupported host Linux distribution '$(host_distro)'."
	@echo "       Consider contacting the author for support." && false
else
ifneq ($(native_arch)-$(host_distro),arm32v7-debian)
	IMAGE=ljremarkable-dev-$(native_arch)/$(host_distro) $(MAKE) --silent get-artifacts
	install $(_native_app_path) $(BINDIR)
	install $(THIS_DIR)/../pi-rM-control.sh $(BINDIR)
endif
endif
endif

.PHONY: upload
upload: out/arm32v7-debian/grabscreen.app.lua out/arm32v7-debian/.charpics $(THIS_DIR)/../rM_ul_eye_menu_hidden_46-28.dat
	test -f out/arm32v7-debian/luajit-rM
	scp $^ "$(_upload_userhost):"
	ssh "$(_upload_userhost)" mkdir -p bin
	scp out/arm32v7-debian/luajit-rM "$(_upload_userhost):bin/luajit"
