#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

jbuilder runtest --dev
jbuilder build --dev src/collide.bc
rm -f *.png
RATE=25
DURATION=20
_build/default/src/collide.bc $RATE $DURATION
# rm -f callgrind.out.*
# valgrind --tool=callgrind _build/default/src/collide.bc $RATE $DURATION
# kcachegrind callgrind.out.*
ffmpeg -y -r $RATE -i %08d.png video.avi
rm -f *.png
