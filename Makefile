build: js-build
	stack build

build-docker: js-build
	stack build --docker

build-nodocker: js-build
	stack build --no-docker

js-build:
	npm run build

js-init:
	npm install

database-init:
	sqlite3 sally.sqlite3 < scripts/initdb.sql

init-all: js-init database

alpine:
	docker build -t alpine-stack -f scripts/Dockerfile .

run:
	stack exec sally -- run -p 8080 -d sally.sqlite3

clean:
	-rm sally.sqlite3
	-npm run clean 2&> /dev/null

all: js-init js-build database-init alpine build
