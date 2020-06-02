#!/usr/bin/env bash

set -eu

ROOT_DIR=$(pwd)

# Chapters 1-5
# guile3.0 -L "${ROOT_DIR}" a1-tspl-2009.scm
# Chapters 6-
guile3.0 -L "${ROOT_DIR}" a2-tspl-2009.scm
