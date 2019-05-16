#!/usr/bin/env bash

[[ ! -f parameters.sh ]] && echo "No parameters.sh" || echo "Yes parameters.sh"

# HEREDOC to STDOUT
# cat <<EOF
# Usage:
#     ./parameters.sh -f --flag2 -p param --param2 param2 subject
# EOF

# HEREDOC to variable
read -d '' USAGE <<EOF
Usage:
    ./parameters.sh -f --flag2 -p param --param2 param2 -u --unsupported subject subject2
EOF

# Check number of provided parameters
if (( ! $# )); then
    echo "$USAGE"
fi

# Iterate over parameters
while (( $# )); do
    case $1 in
        # Parse simple flags and compound parameters
        -f|--flag)
            echo $1 is set
            shift
            ;;
        -f2|--flag2)
            echo $1 is set
            shift
            ;;
        # Parse compound parameters
        -p|--param)
            echo $1 is $2
            shift 2
            ;;
        -p2|--param2)
            echo $1 is $2
            shift 2
            ;;
        # Parse unsupported parameters
        -*)
            echo $1 is unsupported
            shift
            ;;
        # Parse subject
        *)
            echo $1 is subject
            shift
            ;;
    esac
done
