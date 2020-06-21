#!/usr/bin/env bash

set -eu

ROOT_DIR=$(pwd)

# Chapters 1-5
# guile3.0 -L "${ROOT_DIR}" a1-tspl-2009.scm
# Chapters 6-11
# guile3.0 -L "${ROOT_DIR}" a2-tspl-2009.scm

# Chapters 1-
guile3.0 -L "${ROOT_DIR}" b1-tls-1996.scm
