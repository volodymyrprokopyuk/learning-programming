#!/usr/bin/env bash

# set -eux

NAME=Vlad
DATE=$(date)

echo $NAME
echo $DATE

set +x

echo -n "Can you write device drivers? "
# read ANSWER
ANSWER=y

ANSWER=$(echo $ANSWER | tr [a-z] [A-Z])
if [ $ANSWER = Y ]; then
    echo "Wow, you must be very skilled"
else
    echo "Neither can I"
fi

printf "%10s\n" $NAME


function alert() {
    local last_return_code=$?
    if (( $last_return_code != 0 )); then
        echo "ERROR: $* with return code $last_return_code" >&2
        exit $last_return_code
    fi
}

cat debugging.shx
alert "cannot cat debugging.sh"
