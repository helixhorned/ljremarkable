
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

########## RULES ##########

.PHONY: run
run:
	@test -n '$(IMAGE)'
	docker run -it --rm '$(IMAGE):$(commit)'

## Rules for the actual Docker images
# NOTE: no convenience logic like in ljclang to prevent code duplication.

.PHONY: ljremarkable-dev-arm32v7/alpine ljremarkable-dev-arm32v7/debian ljremarkable-dev-arm32v7/ubuntu
.PHONY: ljremarkable-dev-arm64v8/alpine ljremarkable-dev-arm64v8/debian ljremarkable-dev-arm64v8/ubuntu
.PHONY: ljremarkable-dev-amd64/alpine ljremarkable-dev-amd64/debian ljremarkable-dev-amd64/ubuntu

_suffix_Dockerfile_in := SUFFIX_DOCKERFILE_IN='$(THIS_DIR)/Dockerfile.in'
_overrides := OVERRIDE_IMAGE_NAME=ljremarkable-dev OVERRIDE_IMAGE_TAG=$(commit)
_additional_context_repo := ADDITIONAL_CONTEXT_REPO='$(THIS_DIR)/..'
recursive_make_vars := $(_suffix_Dockerfile_in) $(_overrides) $(_additional_context_repo)

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

define get_executable =
  $(_get-artifacts-cat) /home/user/$(1) > '$(_out_dir)/$(2)' && chmod +x '$(_out_dir)/$(2)'
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

# NOTE: Mark a real output file as phony so as to avoid specifying dependencies.
#  Will be always re-obtained, but better have redundant updates than missed ones.
.PHONY: out/arm32v7-debian/grabscreen.app.lua
out/arm32v7-debian/grabscreen.app.lua:
	IMAGE=ljremarkable-dev-arm32v7/debian $(MAKE) --silent get-artifacts

_upload_userhost := $(LJREMARKABLE_TABLET_USER)@$(LJREMARKABLE_TABLET_HOST)

.PHONY: upload
upload: out/arm32v7-debian/grabscreen.app.lua $(THIS_DIR)/../rM_ul_eye_menu_hidden_46-28.dat
	test -f out/arm32v7-debian/luajit-rM
	scp $^ "$(_upload_userhost):"
	ssh "$(_upload_userhost)" mkdir -p bin
	scp out/arm32v7-debian/luajit-rM "$(_upload_userhost):bin/luajit"
