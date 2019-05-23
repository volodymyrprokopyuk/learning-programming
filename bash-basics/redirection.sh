#!/usr/bin/env bash

# Shell is not programming language
# Shell is job control orchestrator (processes connected via pipes with redirection)

# ** REDIRECTION OPERATORS **

# 0 STDIN
# 1 STDOUT
# 2 STDERR
# command >/>> file -- write/append to file (default 1 STDOUT)
# command < file -- read from file (defautl 0 STDIN)
# command <<EOF -- read Here docuent as input
# Here document
# EOF
# command <<<$string -- read Here string as input

# ** CONTROL OPERATORS **

# command1 ; command2 -- run command1 in foreground, then run command2
# command1 & command2 -- run command1 in background, simultaneously run command2 in foreground
# command1 && command2 -- run command2 only on command1 success
# command1 || command2 -- run command2 only on command1 failure
# ! command -- nagate command return status
# command1 | command2 -- pipe command1 output into command2 input
# $( command1 ; command2 ) -- run command in a subshell and get command output. Command substitution
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
# redirection of 2 must be after the redirection of 1
stdout_stderr >out.log 2>&1 && cat out.log

echo "* STDOUT and STDERR to console and file"
stdout_stderr 2>&1 | tee out.log && cat out.log

# Redirection in control flow statements if, case, for, while
# while read LINE; do
#     echo $LINE
# done <redirection.sh >out.log && cat out.log

# Redirect output of a compound command
# { cat file1 ; cat file2 } > file

# Redirect funciton STDERR to a file when defining a function
# function f {} 2>error.log

# Redirect script STDERR to a file from within the script
# exec 2>error.log

# Explicit file descriptors
# Open file for reading
exec 3< redirection.sh
# Open file for writing
exec 4> out.log
while read LINE; do
    echo $LINE;
# Access files though file descriptors
done <&3 >&4 && cat out.log
# Close file descriptors
exec 3>&-
exec 4>&-
