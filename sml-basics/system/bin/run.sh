#!/usr/bin/env bash

set -eu

ROOT_DIR=$(pwd)

# CM_FILE=hw.cm
# ENTRY_POINT=Main.main
# HEAP_IMAGE=hw.amd64-linux

CM_FILE=echo.cm
ENTRY_POINT=Echo.main
HEAP_IMAGE=echo.amd64-linux

# Build heap image
ml-build $CM_FILE $ENTRY_POINT $HEAP_IMAGE
# Run heap image
sml @SMLload $HEAP_IMAGE "${@}"
