FROM alpine:edge
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
    && apk update \
    && apk add wget ghc ca-certificates musl-dev shadow linux-headers zlib-dev \
    && update-ca-certificates
RUN apk add sqlite
ADD scripts/ /sally/scripts
RUN /usr/bin/sqlite3 /sally/sally.sqlite3 < /sally/scripts/initdb.sql
WORKDIR /sally
