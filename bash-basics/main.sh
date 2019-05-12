#!/usr/bin/env bash

# set -eu

source stdlib.sh

ostype

echo $OSTYPE

NUMBER=123
is_even $NUMBER
if [ $? -eq 0 ]; then
    echo "$NUMBER is even"
else
    echo "$NUMBER is odd"
fi

NUMBER=1234
is_even $NUMBER
if [ $? -eq 0 ]; then
    echo "$NUMBER is even"
else
    echo "$NUMBER is odd"
fi

is_connected
if [ $? -eq 0 ]; then
    echo "Is connected"
else
    echo "Is not connected"
fi
