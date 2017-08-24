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

alpine-build:
	docker build -t alpine-sally-build -f scripts/sally-build.Dockerfile .

alpine-run:
	docker build -t alpine-sally-run -f scripts/sally-run.Dockerfile .

run:
	stack exec sally -- run -p 8080 -d sally.sqlite3

clean:
	-rm sally.sqlite3
	-npm run clean 2&> /dev/null

all: js-init js-build database-init alpine build

image:
	stack image container
