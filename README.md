# sally

A toy web app implementing the Silly Sally game. Supported by the warp server
and a SQLite database.

# Initializing

Eventually

```
sally initialize
```

will work. Until then, you must manually create the database:

```
sqlite3 db.sqlite < scripts.initdb
```


# Deploying

This should do the trick, assuming the database is in order

```
sally run --port 8080 -d db.sqlite
```

# Building
```
npm install
npm run build
stack build
sqlite3 sally.sqlite3 < scripts/initdb.sql
stack exec sally -- run -p 8080 -d sally.sqlite3
```
