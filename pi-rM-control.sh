#!/bin/bash

PIGS_PROGRAM=/usr/bin/pigs
pigsProgram="$PIGS_PROGRAM"
if [ ! -x "$pigsProgram" ]; then
    pigsProgram=
fi

# LED GPIO pins.
R=13
G=12

# Expectation: the desided default IP address of the rM has been added to /etc/hosts.
DEFAULT_REMARKABLE_HOST=remarkable

cmd="$1"
rMHost="$2"

if [ -z "$cmd" ]; then
    echo "Usage: $0 {after-login|ping|connect|kill} [<rM-host>]"
    echo " * <rM-host> defaults to '$DEFAULT_REMARKABLE_HOST'"
    echo " * If '$PIGS_PROGRAM' is present, expects GPIO pins 12 and 13 to"
    echo "   be connected to an LED for success and failure, respectively"
    exit 1
fi

if [ -z "$rMHost" ]; then
    rMHost=$DEFAULT_REMARKABLE_HOST
fi

function pigs() {
    if [ -n "$pigsProgram" ]; then
        "$pigsProgram" "$@"
    fi
}

function get_color_arg_list() {
    ramp_color=$1

    if [ x"$ramp_color" = x'r' ]; then
        color_arg_list="v, 0, 0, 1.0"
    elif [ x"$ramp_color" = x'g' ]; then
        color_arg_list="0, v, 0"
    else
        color_arg_list=""
    fi

    echo "$color_arg_list"
}

function led_ramp_fixed_and_cycle() {
    ramp_colors=`get_color_arg_list "$1"`
    fixed_colors=`get_color_arg_list "$2"`
    cycle_colors=`get_color_arg_list "$3"`
    cycle_steps="$4"
    cycle_periods="$5"
    cycle_offset="$6"

    if [[ ! "$cycle_steps" =~ ^[1-9][0-9]*$ ]]; then
        echo "ERROR: argument #4 must be a decimal integer"
        exit 2
    fi
    if [[ ! "$cycle_periods" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
        echo "ERROR: argument #5 must be a number"
        exit 2
    fi
    if [ -z "$cycle_offset" ]; then
        cycle_offset=0
    elif [[ ! "$cycle_offset" =~ ^[1-9][0-9]*$ ]]; then
        echo "ERROR: argument #6 must be a decimal integer"
        exit 2
    fi

    echo "
import math
import time
import blinkt

if len('$ramp_colors') > 0:
    RAMP_STEP_COUNT = 3
    for pix in range(blinkt.NUM_PIXELS):
        for step in range(RAMP_STEP_COUNT):
            v = (255 * step) // (RAMP_STEP_COUNT - 1)
            blinkt.set_pixel(pix, $ramp_colors)
            blinkt.show()

if len('$fixed_colors') > 0:
    v = 255
    blinkt.set_all($fixed_colors)
    blinkt.show()
    time.sleep(0.6)

if len('$cycle_colors') > 0:
    CYCLE_STEP_COUNT = $cycle_steps # 26
    CYCLE_PERIOD_COUNT = $cycle_periods # 2.5

    for i in range(int(CYCLE_PERIOD_COUNT * CYCLE_STEP_COUNT)):
        ang = 2*math.pi * ($cycle_offset + i)/CYCLE_STEP_COUNT
        v = 255 * (1 + math.cos(ang))/2
        blinkt.set_all($cycle_colors)
        blinkt.show()
" | python3
}

function rampLED() {
    led="$1"
    start="$2"
    stop="$3"

    if [[ -z $led || -z $start || -z $stop ]]; then
        return
    fi

    if [[ $start -lt $stop ]]; then
        step=1
    elif [[ $start -gt $stop ]]; then
        step=-1
    else
        return
    fi

    current=$start

    while [[ $current -ne $stop ]]; do
        current=$((current + step))
        pigs pwm $led $current
    done
}

function killRemoteApp() {
    # TODO: more selective.
    # TODO: user override.
    ssh "$USER"@"$rMHost" killall -q luajit
}

case "$cmd" in
    after-login)
        # Indication that we logged in to a graphical session.
        # Ramp up and pulsate twice with green color.
        led_ramp_fixed_and_cycle g 0 g 26 2.5
        ;;

    ping)
        ping -w 1 -c 1 "$rMHost" > /dev/null
        code=$?

        if [ $code -eq 0 ]; then
            led=g
        else
            led=r
        fi

        # Cycle twice with indication color.
        led_ramp_fixed_and_cycle 0 0 $led 14 2.0 7
        ;;

    connect)
        # Handled outside the case/esac block.
        ;;

    kill)
        killRemoteApp
        # TODO: more selective / careful!
        killall --quiet luajit

        # green -> red, then fade out red
        led_ramp_fixed_and_cycle 0 g r 80 0.5
        ;;

    *)
        echo "ERROR: Unrecognized command."
        exit 3
esac

if [ x"$cmd" != x"connect" ]; then
    exit 0
fi

## connect

# Ensure single grabscreen process on the reMarkable.
killRemoteApp
# Prevent "bind: address already in use" after even what is intended to be a clean shutdown.
secsSinceEpoch=$(date +%s)
portOffset=$((secsSinceEpoch % 100))

# Start server (on the reMarkable).
ssh -o ConnectionAttempts=1 -o ConnectTimeout=1 "$USER"@"$rMHost" \
    "$HOME/bin/luajit" grabscreen.app.lua --fork "s+$portOffset" 5000
exitCode1=$?

if [ $exitCode1 == 0 ]; then
    # Start client which must previously have been installed.
    grabscreen.app.lua --fork "c+$portOffset" "$rMHost"
    exitCode2=$?
fi

# Signal via LEDs.

if [[ $exitCode1 -eq 0 && $exitCode2 -eq 0 ]]; then
    led_ramp_fixed_and_cycle g 0 g 16 3.5
    ret=0
else
    killRemoteApp
    led_ramp_fixed_and_cycle 0 0 r 16 3.0 8
    ret=1
fi

exit $ret
