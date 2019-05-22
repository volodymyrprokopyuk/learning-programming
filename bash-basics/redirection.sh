#!/usr/bin/env bash

# Shell is not programming language
# Shell is job control orchestrator (processes connected via pipes)

# ** REDIRECTION OPERATORS **

# 0 STDIN
# 1 STDOUT
# 2 STDERR
# command >/>> file -- write/append to file
# command < file -- read from file
# command <<EOF -- read Here docuent as input
# Here document
# EOF
# command <<<$string -- read Here string as input

# Redirect script STDERR to a file from within the script
# exec 2>error.log

# Redirect funciton STDERR to a file when defining a function
# function f {} 2>error.log

# ** CONTROL OPERATORS **

# command1 ; command2 -- run command1 in foreground, then run command2
# command1 & command2 -- run command1 in background, simultaneously run command2
# command1 && command2 -- run command2 only on command1 success
# command1 || command2 -- run command2 only on command1 failure
# ! command -- nagate reutnr status
# command1 | command2 -- pipe command1 output into command2 input
# $( command1 ; command2 ) -- run command in a subshell. Command substitution
# { command1 ; command2 } -- compound command in the same shell
#     ${VAR} -- parameter expanison
# $(( integer1 + integer2 )) -- integer expression
# [[ stirng1 == stirng2 && string3 ]] == conditional expression

function stdout_stderr {
    echo "1 STDOUT"
    echo "2 STDERR" >&2
}

echo "* STDOUT and STDERR"
stdout_stderr

echo "* only STDERR"
stdout_stderr >/dev/null

echo "* only STDOUT"
stdout_stderr 2>/dev/null

echo "* STDOUT and STDERR to console only"
stdout_stderr 2>&1

echo "* STDOUT and STDERR to file only"
rm -f out.log
stdout_stderr >out.log 2>&1 && cat out.log

echo "* STDOUT and STDERR to console and file"
rm -f out.log
stdout_stderr 2>&1 | tee out.log && cat out.log
