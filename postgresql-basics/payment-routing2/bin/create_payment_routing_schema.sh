#!/usr/bin/env bash

source ./config/config.sh

set -eu

export PGDATABASE=postgres
psql -c "DROP DATABASE IF EXISTS payment_routing" -v ON_ERROR_STOP=1 -v ECHO=queries
psql -c "CREATE DATABASE payment_routing WITH OWNER vlad" -v ON_ERROR_STOP=1 -v ECHO=queries
export PGDATABASE=payment_routing
psql -f $SQL_DIR/payment_routing_schema.sql -v ON_ERROR_STOP=1 -v ECHO=queries
