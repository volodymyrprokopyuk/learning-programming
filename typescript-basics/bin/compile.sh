#!/usr/bin/env bash

set -eu
shopt -s globstar

source ./bin/util.sh

export PATH=./node_modules/.bin:$PATH
export NODE_PATH=.

readonly SOURCE_TARGET=$(ls types.ts)

format "${SOURCE_TARGET}"
# validate "${SOURCE_TARGET}"
compile

node types.js
