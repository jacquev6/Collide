#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

function switch_flavor {
  mkdir -p _builds/$1
  rm -rf _build
  ln -sf _builds/$1 _build
}


rm -f _builds/coverage/default/src/*.sentinel

# https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#Jbuilder suggests
# modifying the jbuild file for release. Let's modify it for tests instead.
sed -i "s/^;\(.*; Uncomment for dev mode\)$/\1/; s/^;*\(.*; Comment for dev mode\)$/;\1/" $(find . -name jbuild)
switch_flavor coverage
jbuilder runtest --dev
switch_flavor none
sed -i "s/^;*\(.*; Uncomment for dev mode\)$/;\1/; s/^;\(.*; Comment for dev mode\)$/\1/" $(find . -name jbuild)
echo
bisect-summary _builds/coverage/default/src/bisect????.out
echo
bisect-ppx-report -I _builds/coverage/default -html _builds/coverage/default/bisect _builds/coverage/default/src/bisect????.out
echo "See coverage report in $(pwd)/_builds/coverage/default/bisect/index.html"

switch_flavor release
jbuilder build src/collide.exe
switch_flavor none
rm -f *.png
RATE=25
DURATION=30
time _builds/release/default/src/collide.exe $RATE $DURATION
# rm -f callgrind.out.*
# valgrind --tool=callgrind _builds/release/default/src/collide.exe $RATE $DURATION
# kcachegrind callgrind.out.*
ffmpeg -y -r $RATE -i %08d.png -vcodec libx264 video.mp4
rm -f ????????.png
