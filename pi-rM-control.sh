#!/bin/bash

pigs=/usr/bin/pigs

# LED GPIO pins.
R=13
G=12

# Expectation: the desided default IP address of the rM has been added to /etc/hosts.
DEFAULT_REMARKABLE_HOST=remarkable

cmd="$1"
rMHost="$2"

if [ -z "$cmd" ]; then
    echo "Usage: $0 [after-login|ping|kill] [<rM-host>]"
    echo " <rM-host> defaults to '$DEFAULT_REMARKABLE_HOST'"
    exit 1
fi

if [ -z "$rMHost" ]; then
    rMHost=$DEFAULT_REMARKABLE_HOST
fi

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
        $pigs pwm $led $current
    done
}

case "$cmd" in
    after-login)
        # Indication that we logged in to a graphical session.

        # Clear red LED.
        $pigs w $R 0

        # Blink green LED a couple of times.
        for i in {1..5}; do
            $pigs w $G 1 mils 500 w $G 0 mils 500
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

        $pigs w $off 0
        cmd="w $on 1 mils 300 w $on 0 mils 300"
        $pigs $cmd $cmd
        ;;

    connect)
        # Handled outside the case/esac block.
        ;;

    kill)
        # TODO: more selective / careful!
        killall --quiet ssh luajit
        # TODO: user override.
        ssh "$USER"@"$rMHost" killall -q luajit

        # green -> red
        $pigs w $G 1 mils 500 w $G 0 w $R 1 mils 500 w $R 0 pwm $R 255
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

# connect
echo "connect: not yet implemented here"
exit 4
