#!/usr/bin/env bash

# Deployment of booking database from https://postgrespro.ru/education/demodb

readonly DB_USER=vld
readonly DB_NAME=demo
readonly DB_ROOT=data
readonly DB_ARCHIVE_URL=https://edu.postgrespro.ru/demo-small.zip
readonly DB_ARCHIVE_FILE=${DB_ARCHIVE_URL##*/}

mkdir -p $DB_ROOT
cd $DB_ROOT
curl -O $DB_ARCHIVE_URL
unzip $DB_ARCHIVE_FILE
psql -f *.sql postgres $DB_USER >$DB_NAME.log 2>$DB_NAME.err
