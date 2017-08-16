FROM alpine:edge
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
    && apk update \
    && apk add wget ghc ca-certificates musl-dev shadow linux-headers zlib-dev \
    && update-ca-certificates \
    && apk add sqlite3
ADD scripts/ scripts/
RUN sqlite3 sally.sqlite3 < scripts/initdb.sql
