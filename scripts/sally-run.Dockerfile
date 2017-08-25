FROM alpine:3.6
RUN apk update && apk add sqlite
COPY scripts/ /sally/scripts/
COPY static/  /sally/static/
RUN /usr/bin/sqlite3 /sally/sally.sqlite3 < /sally/scripts/initdb.sql
WORKDIR /sally
