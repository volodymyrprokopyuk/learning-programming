#!/usr/bin/env bash

# Math: integers > $((...))
ONE=1
TWO=$(( $ONE+1 ))
echo $TWO
RESULT=$(( ($ONE + 2) * ($TWO - 3) ))
echo $RESULT
(( r = $ONE - $TWO * 2 ))
echo $r

# Math: arbitrary precision > bc
RESULT=$(echo "3.14 * 2" | bc)
echo $RESULT
RESULT=$(bc <<< "2.5 + 3.7")
echo $RESULT
