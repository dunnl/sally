# sally

A toy web app implementing the Silly Sally game. Supported by the warp server
and a SQLite database.

## Building

Notes:

* This project includes raw javascript modules managed by NPM/package.json. The
server expects to find the bundled file under `static/`, which is handled
automatically by NPM.

* The `stack.yaml` is currently setup to build inside a docker container. The
  docker container is `alpine:edge` (which now has ghc available) with few extra
  utilities that stack needs to compile the project.

The full procedure to build/initialize:

```
npm install
npm run build
docker build -t alpine-sally -f scripts/Dockerfile .
stack build
sqlite3 sally.sqlite3 < scripts/initdb.sql
```

or simply

```
make all
```

To execute:

```
stack exec sally -- run -p 8080 -d sally.sqlite3
```

or 

```
stack image container
docker run dunnl/sally sally -p 8080 -d sally.sqlite
```


## Testing

There is no test suite for this project (!). It's an old project and not really
worth building a test suite for now.
