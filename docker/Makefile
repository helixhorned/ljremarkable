
# From the 'GNU make' manual:
#  "backslash/newline handling is modified slightly to conform to POSIX.2"
.POSIX:

########## VARIABLES ##########

## User

# For target 'run', to be used interactively, e.g.
#  $ make run ljc<TAB...>                            --(completion)-->
#  $ make run ljremarkable-dev-arm32v7/debian        --(edit)-->
#  $ make run IMAGE=ljremarkable-dev-arm32v7/debian  <--(invoke this)
IMAGE ?=

## Private

THIS_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

commit := $(shell git rev-parse --short=12 HEAD)

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
