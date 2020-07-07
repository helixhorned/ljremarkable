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

        # Clear red LED.
        pigs w $R 0

        # Blink green LED a couple of times.
        for i in {1..5}; do
            pigs w $G 1 mils 500 w $G 0 mils 500
        done
        ;;

    ping)
        ping -w 1 -c 1 "$rMHost" > /dev/null
        code=$?

        if [ $code -eq 0 ]; then
            off=$R
            on=$G
        else
            off=$G
            on=$R
        fi

        pigs w $off 0
        cmd="w $on 1 mils 300 w $on 0 mils 300"
        pigs $cmd $cmd
        ;;

    connect)
        # Handled outside the case/esac block.
        ;;

    kill)
        killRemoteApp
        # TODO: more selective / careful!
        killall --quiet luajit

        # green -> red
        pigs w $G 1 mils 500 w $G 0 w $R 1 mils 500 w $R 0 pwm $R 255
        # fade red
        rampLED $R 255 0
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
    led=$G
    rampLED $led 0 255
    ret=0
else
    led=$R
    killRemoteApp
    ret=1
fi

cmd="w $led 1 mils 300 w $led 0 mils 300"
pigs $cmd $cmd $cmd $cmd

exit $ret
