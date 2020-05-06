#!/usr/bin/env bash

source ./config/config.sh

set -eu

export PGDATABASE=postgres
psql -c "DROP DATABASE IF EXISTS pricing" -v ON_ERROR_STOP=1 -v ECHO=queries
psql -c "CREATE DATABASE pricing WITH OWNER vlad" -v ON_ERROR_STOP=1 -v ECHO=queries
export PGDATABASE=pricing
psql -f $SQL_DIR/pricing_schema.sql -v ON_ERROR_STOP=1 -v ECHO=queries
