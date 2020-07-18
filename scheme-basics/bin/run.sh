#!/usr/bin/env bash

set -eu

readonly ROOT_DIR=$(pwd)
readonly SRFI_DIR=$HOME/Projects/scheme-upgrade

export GUILE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

# Chapters 1-5
# guile3.0 a1-tspl-2009.scm
# Chapters 6-11
# guile3.0 a2-tspl-2009.scm

guile3.0 b1-grm-2020.scm "${@}"
