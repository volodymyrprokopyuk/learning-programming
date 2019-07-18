#!/usr/bin/env bash

set -eux

readonly LINE_LENGTH=88
readonly SOURCE=*.py

# Validate Python source code
black --line-length $LINE_LENGTH $SOURCE
flake8 --max-line-length=$LINE_LENGTH $SOURCE
pylint --max-line-length=$LINE_LENGTH --exit-zero $SOURCE
