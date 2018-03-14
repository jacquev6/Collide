#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

function jbuilder_flavor {
  flavor=$1
  shift

  rm -rf _build
  mkdir -p _builds/$flavor
  ln -sf _builds/$flavor _build
  for gen in $(find . -name jbuild.py)
  do
    $gen $flavor
  done

  jbuilder "$@"

  rm -rf _build
  for gen in $(find . -name jbuild.py)
  do
    $gen publish
  done
}


rm -f _builds/*/default/src/*.sentinel


jbuilder_flavor coverage runtest --dev
echo
bisect-summary _builds/coverage/default/src/bisect????.out
echo
bisect-ppx-report -I _builds/coverage/default -html _builds/coverage/default/bisect _builds/coverage/default/src/bisect????.out
echo "See coverage report in $(pwd)/_builds/coverage/default/bisect/index.html"


jbuilder_flavor release build src/collide.exe
rm -f *.png
RATE=25
DURATION=30
_builds/release/default/src/collide.exe $RATE $DURATION
ffmpeg -y -r $RATE -i %08d.png -vcodec libx264 video.mp4
rm -f ????????.png
