#!/usr/bin/env bash

source ./config/config.sh

set -eu

psql -f $SQL_DIR/payment_routing_data.sql -v ON_ERROR_STOP=1 -v ECHO=queries
