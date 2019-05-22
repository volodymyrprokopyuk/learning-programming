#!/usr/bin/env bash

PLIMIT=6

# Parse process information and create JSON for each process
ps -ef | head -n $PLIMIT | grep -v '^UID' | sort -n -k 2 | while read process; do
    read user pid ppid c stime tty time cmd <<<$process
    echo "$user|$pid|$cmd"
    # jq -nR --arg user "$user" --arg pid "$pid" --arg cmd "$cmd" '{user: $user, pid: $pid, cmd: $cmd}'
done | jq -nR '(inputs | split("|")) as $process | {user: $process[0], pid: $process[1], cmd: $process[2]}'
