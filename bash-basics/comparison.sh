#!/usr/bin/env bash

NUM=1
STR=A

# Integer comparison
if (( $NUM == 1 )); then
    echo "One"
fi

# Integer comparison AND compoound command
# test $NUM -eq 1 && {
(( $NUM == 1 )) && {
    echo "Test: One"
    echo "Test: done"
}

# Integer comparision -a = &&
test $NUM -eq 1 -a $NUM -gt 0 && echo "Equals One AND greater than Zero"
(( $NUM == 1 && $NUM > 0 )) && echo "Equals One AND greater than Zero"

# String comparison
if [[ $STR = "A" ]]; then
    echo "A"
fi

# String comparison OR compound command
# test $STR = "B" || {
[[ $STR = "B" ]] || {
    echo "Test: A"
    echo "Test: done"
}

# String comparison -o = ||
test $STR = "B" -o $STR != "A" || echo "Not equal B OR not equal A OR this message"
[[ $STR = "B" || $STR != "A" ]] || echo "Not equal B OR not equal A OR this message"

# Stirng length test
[[ -n $STR ]] && echo "Non zero length"
[[ -z $STR ]] || echo "Non zero length"
[[ -n "" ]] || echo "Zero length"
[[ -z "" ]] && echo "Zero length"
VAR="FOUR"
(( ${#VAR} == 4 )) && echo "Length is four"

# Comparing whether STRING OUTPUT from a subshell command is not empty
[[ $(grep 'vld' /etc/passwd) ]] && echo "vld is in users"

# Comparing the RETURN CODE of a command
# ping -c 1 www.google.com >/dev/null 2>&1 && echo "Connected to Google"
# if ping -c 1 www.google.com >/dev/null 2>&1; then
#     echo "Connected to Google"
# fi
