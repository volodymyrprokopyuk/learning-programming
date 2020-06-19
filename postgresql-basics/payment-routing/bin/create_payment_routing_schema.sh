#!/usr/bin/env bash

source ./config/config.sh

set -eu

export PGDATABASE=postgres
psql -c "DROP DATABASE IF EXISTS paymentrouting" -v ON_ERROR_STOP=1 -v ECHO=queries
psql -c "CREATE DATABASE paymentrouting WITH OWNER vlad" -v ON_ERROR_STOP=1 -v ECHO=queries
export PGDATABASE=paymentrouting
psql -f $SQL_DIR/payment_routing_schema.sql -v ON_ERROR_STOP=1 -v ECHO=queries
