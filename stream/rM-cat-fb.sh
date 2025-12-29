#!/bin/sh
# SPDX-License-Identifier: MIT
# Copyright (C) 2025 Philipp Kutin

# This script is supposed to run on the reMarkable 2 tablet, interpreted by its '/bin/ash'
# which is a symlink to the BusyBox binary. We make use of the following features:

# [[ ]]
# shellcheck disable=SC3010

# =~
# shellcheck disable=SC3015

# ${str:offset:len}
# shellcheck disable=SC3057

set -e

assert() {
	test "$@" || (echo "assertion failed: $*" 1>&2 && false)
}

print_usage_and_exit() {
	exec 1>&2
	echo "Usage: $0 --max-fps {1-9}"
	exit 1
}

max_fps=

while [ $# -gt 0 ]; do
    case "$1" in
		--max-fps)
			max_fps="$2"
			shift 2
			;;
		-h | --help)
			print_usage_and_exit
			;;
		*)
			echo "ERROR: unrecognized option '$1'" 1>&2
			print_usage_and_exit
			;;
	esac
done

message() {
	echo "$*" 1>&2
}

error_and_exit() {
	echo "ERROR: $*" 1>&2
	exit 1
}

if [[ ! "$max_fps" =~ ^[1-9]$ ]]; then
	error_and_exit "invalid argument to '--max-fps'"
fi

min_usecs_per_frame=$((1000000 / max_fps))

if [ "$max_fps" -eq 1 ]; then
	wait_seconds=1.0
else
	assert ${#min_usecs_per_frame} -eq 6
	wait_seconds="0.${min_usecs_per_frame}"
fi

MIN_SOFTWARE_VERSION=324
MAX_SOFTWARE_VERSION=324

RM_SOFTWARE_VERSION_FILE=/usr/share/remarkable/update.conf
RM_HARDWARE_VERSION_FILE=/sys/devices/soc0/machine

regex='^REMARKABLE_RELEASE_VERSION=[1-9]\.[1-9][0-9]\.'
sedcmd='s/.*\([1-9]\)\.\([1-9][0-9]\).*/\1\2/'
if ! rM_sw_version=$(grep -o -m 1 "$regex" "$RM_SOFTWARE_VERSION_FILE" | sed "$sedcmd"); then
	error_and_exit "Failed obtaining rM software version."
elif [[ ! ($rM_sw_version -ge $MIN_SOFTWARE_VERSION && $rM_sw_version -le $MAX_SOFTWARE_VERSION) ]]; then
	error_and_exit "Unsupported reMarkable software version."
fi

if ! rM_hw_version=$(cat "$RM_HARDWARE_VERSION_FILE"); then
	error_and_exit "Failed obtaining reMarkable tablet model."
elif [ "$rM_hw_version" != "reMarkable 2.0" ]; then
	error_and_exit "Unsupported reMarkable tablet model."
fi

PAGE_SIZE=4096

width=1404
height=1872
pixel_size=4  # bgra, 8 bits per component

count_of() {
	byte_size=$1
	unit=$2
	echo $(((byte_size + unit - 1) / unit))
}

page_count_for() {
	count_of "$1" $PAGE_SIZE
}

GRAY8_FB_PAGE_COUNT=0x282
gray8_fb_page_count=$(page_count_for $((width * height)))
assert "$gray8_fb_page_count" -eq $((GRAY8_FB_PAGE_COUNT))

bgra_fb_byte_size=$((pixel_size * width * height))
bgra_fb_page_count=$(page_count_for "$bgra_fb_byte_size")
assert "$bgra_fb_page_count" -eq $((0xA07))
bgra_fb_tail_byte_size=$((bgra_fb_byte_size % PAGE_SIZE))
assert "$bgra_fb_tail_byte_size" -eq $((0xB00))
bgra_fb_full_page_count=$((bgra_fb_page_count - 1))

if ! xochitl_pid=$(/bin/pidof xochitl) || [[ ! "$xochitl_pid" =~ ^[1-9][0-9]*$ ]]; then
	error_and_exit "Failed obtaining PID of xochitl."
fi

maps_file="/proc/$xochitl_pid/maps"

if ! mm=$(grep -A1 /dev/fb0 "$maps_file" | sed 's/ .*//g; s/-/ /g'); then
	error_and_exit "Failed obtaining memory mappings from '$maps_file'."
elif [[ ! $mm =~ ^.....000[[:space:]].....000[[:space:]].....000[[:space:]].....000$ ]]; then
	error_and_exit "Memory mappings from '$maps_file' have unexpected format."
fi

get_page() {
	str="$1"
	which="$2"

	if [[ ! "$str" =~ ^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]000$ ]]; then
		error_and_exit "Unexpected $which element of mappings from '$maps_file'."
	fi

	echo "0x${str:0:5}"
}

get_page "${mm:0:8}" first > /dev/null
b=$(get_page "${mm:9:8}" second)
b_repeated=$(get_page "${mm:18:8}" third)
c=$(get_page "${mm:27:8}" fourth)

if [ "$b" != "$b_repeated" ]; then
	error_and_exit "Mapping after '/dev/fb0' does not immediately follow its predecessor."
fi

target_mapping_page_count=$((c - b))

if [[ "$target_mapping_page_count" -lt $((gray8_fb_page_count + bgra_fb_page_count)) ]]; then
	error_and_exit "Mapping after '/dev/fb0' has insufficient size."
fi

mem_file="/proc/$xochitl_pid/mem"
page_offset=$((b + gray8_fb_page_count))
tail_page_offset=$((page_offset + bgra_fb_full_page_count))

# Wrapper over 'dd' additionally checking that exactly as many full records were read and
# written as specified by the first argument.
#
# Thus, we do not allow partial reads -- which may occur for *each* input record to 'dd'
# (and which does not count as error). In practice, this seems to be more of a problem when
# receiving from a pipe. Basically, we *hope* that reading from '/proc/<pid>/mem' is
# equivalent to copying from mapped memory -- having previously validated the region to
# access.
checked_dd() {
	ct="$1"
	shift

	{
		# Invoke 'dd', saving its stderr for inspection.
		# https://unix.stackexchange.com/a/474195
		if ! output=$(dd "$@" count="$ct" 2>&1 >&3 3>&-); then
			exit $?
		fi

		expected_output=$(printf "%u+%u records in\n%u+%u records out" "$ct" 0 "$ct" 0)
		if [ "$output" != "$expected_output" ]; then
			error_and_exit "Unexpected stderr output from 'dd $* count=$ct': $output"
		fi
	} 3>&1
}

checked_dd_pages() {
	ct="$1"
	shift
	checked_dd "$ct" if="$mem_file" bs="$PAGE_SIZE" "$@"
}

tail_page_file=/dev/shm/rM-cat-fb.tail-page

if ! touch "$tail_page_file"; then
	error_and_exit "Failed touching '$tail_page_file'."
fi

message "Assuming BGRA framebuffer to start at page $b+$GRAY8_FB_PAGE_COUNT of '$mem_file'."
message "Starting capture, waiting $wait_seconds seconds between frames."

while true; do
	# NOTE: The framebuffer size is not evenly divisible by the page size. And on the
	#  reMarkable (as well as in POSIX), there is no '-c' to 'head', nor a 'truncate'
	#  utility. So, handle the last (expected-partial) page in two steps:
	#   1. copy a full page to a temporary file, then
	#   2. from it, copy only the necessary number of bytes.
	checked_dd_pages "$bgra_fb_full_page_count" skip="$page_offset"
	checked_dd_pages 1 skip="$tail_page_offset" of="$tail_page_file"
	checked_dd 1 if="$tail_page_file" bs="$bgra_fb_tail_byte_size"
	sleep "$wait_seconds"
done
