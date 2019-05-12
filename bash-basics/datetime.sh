#!/usr/bin/env bash

# Array initializaiton
declare -a DATE_ARRAY=($(date +"%Y %m %d %H %M %S"))
# Array indexing
echo ${DATE_ARRAY[0]}
echo ${DATE_ARRAY[1]}
echo ${DATE_ARRAY[2]}
echo ${DATE_ARRAY[3]}
echo ${DATE_ARRAY[4]}
echo ${DATE_ARRAY[5]}

function is_within_range {
    if (( $# != 4 )); then
        echo "Usage: $0 <being_day 0-6> <end_day> <begin_hour 0-23> <end_hour>"
        exit 1
    fi

    BEGIN_DAY=$1
    END_DAY=$2
    BEGIN_HOUR=$3
    END_HOUR=$4

    DAY=$(date +%w)
    HOUR=$(date +%H)

    if (( ($DAY >= $BEGIN_DAY) && ($DAY <= $END_DAY) \
        && ($HOUR >= $BEGIN_HOUR) && ($HOUR <= $END_HOUR) ))
    then
        echo "Working hours"
    else
        echo "Out of working hours"
    fi
}

is_within_range 0 4 8 17

is_within_range 0 6 0 23
