#!/usr/bin/env bash

# Set default value for undefined variable
[[ ! $NAME ]] && NAME="Default name"
echo $NAME

[[ $NAME2 ]] || NAME2="Default 23 name 23"
echo $NAME2

# Parameter expansion: default value SUBSTITUTION/GET
# : means both: undefined variable or empty string
echo ${NAME3:-"Default name 3"}
echo $NAME3 # value remains unchanged

# Parameter expansion: default value ASSIGNMENT/SET
echo ${NAME3:="Default name 3"}
echo $NAME3 # value is assigned

# Parameter expansion: exit with ERROR
echo ${NAME:?"NAME is undefined or empty"}
# echo ${NAME4:?"NAME4 is undefined or empty"}

# Substring expansion
echo ${NAME:2}
echo ${NAME:2:5}

# Get value length
echo ${#NAME}

# TODO
echo ${NAME2#[a-zA-Z]}
echo ${NAME2%[0-9]}
echo ${NAME2/[0-9]/_}
echo ${NAME2//[0-9]/_}
