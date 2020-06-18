#!/usr/bin/env bash

set -eu

ROOT_DIR=$(pwd)

# CM_FILE=hw.cm
# ENTRY_POINT=HelloWorld.main
# HEAP_IMAGE=hw.amd64-linux

# CM_FILE=echo.cm
# ENTRY_POINT=Echo.main
# HEAP_IMAGE=echo.amd64-linux

# CM_FILE=wc.cm
# ENTRY_POINT=Wc.main
# HEAP_IMAGE=wc.amd64-linux

CM_FILE=getopt.cm
ENTRY_POINT=GetOpt.main
HEAP_IMAGE=getopt.amd64-linux

# Build heap image
ml-build $CM_FILE $ENTRY_POINT $HEAP_IMAGE
# Run heap image
sml @SMLload $HEAP_IMAGE "${@}"
