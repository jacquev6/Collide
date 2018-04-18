#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General jbuilder bisect_ppx bisect-summary lablgtk cairo2

clear

PROJECT_ROOT=$(pwd)

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

  rm -f _build/default/src/*.sentinel
  jbuilder "$@"

  rm -rf _build
  for gen in $(find . -name jbuild.py)
  do
    $gen publish
  done
}


jbuilder_flavor coverage runtest --dev
echo
bisect-summary _builds/coverage/default/src/bisect????.out
echo
bisect-ppx-report -I _builds/coverage/default -html _builds/coverage/default/bisect _builds/coverage/default/src/bisect????.out
echo "See coverage report in $PROJECT_ROOT/_builds/coverage/default/bisect/index.html"
echo


jbuilder_flavor release build src/collide_gtk.exe
echo
echo "Try the GTK+ application: $PROJECT_ROOT/_builds/release/default/src/collide_gtk.exe"
echo


if which browserify >/dev/null
then
  jbuilder_flavor release build @collide_browser

  rm -rf docs
  mkdir docs
  touch docs/.nojekyll
  grep -v "cordova\.js" _builds/release/default/src/collide_browser.html >docs/index.html
  cp _builds/release/default/src/*.css docs
  cp _builds/release/default/src/*.js docs
  cp icon.png docs
  echo
  echo "Try the in-browser application: $PROJECT_ROOT/docs/index.html"
  echo

  if (which cordova && which cordova-icon) >/dev/null
  then
    if ! [ -d _builds/cordova ]
    then
      cordova create _builds/cordova net.jacquev6.Collide Collide
      cd _builds/cordova
      cordova platform add android
      cordova platform add browser
      rm -r www/js www/css www/img
    fi
    cd $PROJECT_ROOT
    cp _builds/release/default/src/collide_browser.html _builds/cordova/www/index.html
    cp _builds/release/default/src/*.css _builds/cordova/www
    cp _builds/release/default/src/*.js _builds/cordova/www
    cp icon.png _builds/cordova
    cd _builds/cordova
    cordova-icon >cordova-icon.stdout 2>cordova-icon.stderr || (echo "Error during cordova-icon. Have a look at $PROJECT_ROOT/_builds/cordova/cordova-icon.stdout and $PROJECT_ROOT/_builds/cordova/cordova-icon.stderr"; false)
    cordova build >cordova-build.stdout 2>cordova-build.stderr || (echo "Error during cordova build. Have a look at $PROJECT_ROOT/_builds/cordova/cordova-build.stdout and $PROJECT_ROOT/_builds/cordova/cordova-build.stderr"; false)
    cd $PROJECT_ROOT
    echo
    echo "Try the Cordova browser application: $PROJECT_ROOT/_builds/cordova/platforms/browser/www/index.html"
    echo
    if [ $(adb devices -l | grep -c model:) == 1 ]
    then
      adb install -r -t _builds/cordova/platforms/android/app/build/outputs/apk/debug/app-debug.apk
      adb shell monkey -p net.jacquev6.Collide 1
      echo
      echo "Try the Cordova Android application: on your connected device"
      echo "Use 'adb logcat | grep chromium' to see OCaml's StdOut and JavaScript's console.log"
    else
      echo "Try the Cordova Android application: adb install $PROJECT_ROOT/_builds/cordova/platforms/android/app/build/outputs/apk/debug/app-debug.apk"
    fi
    echo
  fi
fi


jbuilder_flavor release build src/collide_cli.exe
rm -rf _builds/video/*
mkdir -p _builds/video
cd _builds/video
FPS=10
FORMAT=frame_%05d.png
../release/default/src/collide_cli.exe --help
../release/default/src/collide_cli.exe \
  --width 640 --height 480 \
  --balls 10 --max-speed 100 \
  --min-density 1 --max-density 1 \
  --min-radius 5 --max-radius 15 \
  --velocity-vectors --previous-positions 5 \
  --fps $FPS --duration 3 \
  $FORMAT
ffmpeg -loglevel panic -hide_banner -nostats -y -r $FPS -i $FORMAT -vcodec libx264 video.mp4
# cvlc video.mp4 vlc://quit
cd ../..
echo
echo "Watch the video generated by the CLI application: $PROJECT_ROOT/_builds/video/video.mp4"
echo


echo "Development cycle OK"
