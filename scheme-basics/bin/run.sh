#!/usr/bin/env bash

set -eu

ROOT_DIR=$(pwd)

guile3.0 -L "${ROOT_DIR}" a-tspl-2011.scm
