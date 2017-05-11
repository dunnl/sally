#! /bin/sh

sed -e 's/WSTARGET/'"$1"'/g' static/app.js.template > static/app.js
#Sample target: ws:\/\/dunnl.io
