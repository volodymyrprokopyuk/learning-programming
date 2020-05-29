#!/usr/bin/env bash

source ./config/config.sh

set -eu

export PGDATABASE=postgres
psql -c "DROP DATABASE IF EXISTS language" -v ON_ERROR_STOP=1 -v ECHO=queries
psql -c "CREATE DATABASE language WITH OWNER vlad" -v ON_ERROR_STOP=1 -v ECHO=queries
export PGDATABASE=language
psql -f $SQL_DIR/language_schema.sql -v ON_ERROR_STOP=1 -v ECHO=queries
