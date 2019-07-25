#!/usr/bin/env bash

set -eux

function greet {
    local name=${1?ERROR: mandatory name is not provided}
    # Return text from function
    echo "Hello $1"
    # Function return code
    return 1 # Return is used only in functions
}

set +e
# Capture text returned from function
GREETING=$(greet Vlad)
# Capture function return code
readonly RETURN_CODE=$?
set -e

echo $GREETING
echo $RETURN_CODE

# Exit from script with function return code
exit $RETURN_CODE # exit is used only in scripts
