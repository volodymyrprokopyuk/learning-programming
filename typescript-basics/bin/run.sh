#!/usr/bin/env bash

set -eux

export PATH=./node_modules/.bin:$PATH
readonly LINE_LENGTH=88
readonly TAB_WIDTH=4
readonly TARGET=main

prettier --print-width=$LINE_LENGTH --tab-width=$TAB_WIDTH --no-bracket-spacing \
    --write $TARGET.ts
# tslint $TARGET.ts
tsc $TARGET.ts
node $TARGET.js
