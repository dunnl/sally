awk -v R="$1" '{ sub("SALLY_URL", R, $0); print $0;}' src-js/app-template.js > src-js/app.js
