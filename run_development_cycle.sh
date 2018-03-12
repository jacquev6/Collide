#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

jbuilder build --dev src/collide.bc
rm -f *.png
_build/default/src/collide.bc
