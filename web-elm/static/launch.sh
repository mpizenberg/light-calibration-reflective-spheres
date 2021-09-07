#!/bin/sh
cd ..

echo "###"
echo "Compile :"
echo "###"

echo
echo "Build elm module"
elm make src/Main.elm --output=static/Elm.js --optimize | sed 's/^/--  /'

cd ../select-ball-wasm/
echo
echo "Build wasm"
wasm-pack build --target web -- --features console_error_panic_hook | sed 's/^/--  /'

cd ../web-elm/static/
echo
echo "Build worker"
esbuild worker.mjs --bundle --preserve-symlinks --outfile=worker.js | sed 's/^/--  /'

echo "###"
echo "Compilation : OK"
echo "###"


echo "###"
echo "Clear cache :"
echo "###"
rm -f /home/floffy/.cache/chromium/Default/Code\ Cache/js/* 2> /dev/null
rm -f /home/floffy/.cache/chromium/Default/Code\ Cache/wasm/* 2> /dev/null
echo "###"
echo "Clear cache ok"
echo "###"
python -m http.server 8002

