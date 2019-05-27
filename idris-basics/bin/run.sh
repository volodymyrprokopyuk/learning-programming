#!/usr/bin/env bash

SOURCE=Average.idr
TARGET_DIR=bin
TARGET=$TARGET_DIR/${SOURCE%.idr}

idris $SOURCE -o $TARGET && ./$TARGET
