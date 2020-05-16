#!/usr/bin/env bash

set -eu

TARGET=main

mlton $TARGET.sml && ./$TARGET
