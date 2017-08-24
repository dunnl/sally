FROM alpine:3.6
RUN apk update &&
    apt add sqlite
COPY scripts/ /sally/scripts/
COPY static/  /sally/static/
RUN /usr/bin/sqlite3 /sally/sally.sqlite3 < scripts/initdb.sql
WORKDIR /sally
