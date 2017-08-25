# sally

A toy web app implementing the Silly Sally game. Supported by the warp server
and a SQLite database.

## Initialization steps

Sally uses raw javascript managed with NPM. To build those files, run 

```
npm install
scripts/set_js_domain.sh "<URL for websocket connections">
npm run build
```

## Building with Docker

The build system uses two Docker images that must be built before building and running the main application. Both are based on [alpine linux](https://alpinelinux.org). The build image (also used for `stack exec`) adds GHC (which exists now in alpine edge) and some other things needed for stack. The runtime image merely adds Sqlite3 to create an empty initial sqlite3 database. The main executable is statically linked in the build image to ensure the runtime image can stay small.

```
# These processes should not take more than a minute or so, maximum
make alpine-build # Approximately 1GB
make alpine-run   # Approximately 10MB
```

You can use stack like normal now. Be aware that `stack exec` runs slowly because of the time required to spin-up the large build image into a container. The executable will located inside the images at `/usr/local/bin/sally` and runs in the directory `/sally`, where the directory `static/` and database `sally.sqlite3` have been copied during the image creation. To create a docker image containing the executable within the runtime image above, run

```
#Approximately 30MB
stack image container
```

And to run the final image as a Docker container:

```
docker run dunnl/sally sally -p 8080 -d sally.sqlite3
# or with entrypoint set
docker run dunnl/sally-sally -p 8080 -d sally.sqlite3
```


## Testing

There is no test suite for this project (!). It's an old project and not really
worth building a test suite for now.
