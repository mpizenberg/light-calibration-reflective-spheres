#!/bin/sh
cd ..

echo "###"
echo "Compile :"
echo "###"

echo
echo "Build elm module"
elm make src/Main.elm --output=static/Elm.js --debug | sed 's/^/--  /'

echo "###"
echo "Compilation : OK"
echo "###"


echo "###"
echo "Clear cache :"
echo "###"
rm -f /home/floffy/.cache/chromium/Default/Code\ Cache/js/* 2> /dev/null
echo "###"
echo "Clear cache ok"
echo "###"
python -m http.server 8080

