#!/usr/bin/env bash

set -eux

readonly SOURCE=$(pwd)
readonly TEST=$SOURCE/test
export PYTHONPATH=$SOURCE

pytest -x -v -s --disable-pytest-warnings \
    --cov $SOURCE --cov-report term --cov-report html $TEST
