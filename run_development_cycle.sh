#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

jbuilder runtest --dev
jbuilder build src/collide.exe
rm -f *.png
RATE=25
DURATION=30
time _build/default/src/collide.exe $RATE $DURATION
# rm -f callgrind.out.*
# valgrind --tool=callgrind _build/default/src/collide.exe $RATE $DURATION
# kcachegrind callgrind.out.*
ffmpeg -y -r $RATE -i %08d.png -vcodec libx264 video.mp4
rm -f ????????.png
