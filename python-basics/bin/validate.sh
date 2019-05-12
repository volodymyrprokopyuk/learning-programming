#!/usr/bin/env bash

set -eux

SOURCE=*.py
LINE_LENGTH=88

black --line-length $LINE_LENGTH --check $SOURCE
flake8 --max-line-length=$LINE_LENGTH $SOURCE
pylint --max-line-length=$LINE_LENGTH --exit-zero $SOURCE
