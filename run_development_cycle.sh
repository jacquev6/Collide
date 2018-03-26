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
  sed "s|collide_browser.bc.js|collide.js|" _builds/release/default/src/collide_browser.html | grep -v "cordova\.js" >docs/index.html
  cp _builds/release/default/src/collide_browser.bc.js docs/collide.js
  cp _builds/release/default/src/FileSaver.js docs
  echo
  echo "Try the in-browser application: $PROJECT_ROOT/docs/index.html"
  echo

  if which cordova >/dev/null
  then
    if ! [ -d _builds/cordova ]
    then
      cordova create _builds/cordova net.jacquev6.Collide Collide
      cd _builds/cordova
      cordova platform add android
      cordova platform add browser
      rm www/js/index.js www/css/index.css
    fi
    cd $PROJECT_ROOT
    jbuilder_flavor release build @collide_browser
    sed "s|collide_browser.bc.js|collide.js|" _builds/release/default/src/collide_browser.html >_builds/cordova/www/index.html
    cp _builds/release/default/src/collide_browser.bc.js _builds/cordova/www/collide.js
    cp _builds/release/default/src/FileSaver.js _builds/cordova/www
    cd _builds/cordova
    cordova build >build.stdout 2>build.stderr || (echo "Error during cordova build. Have a look at $PROJECT_ROOT/_builds/cordova/build.stdout and $PROJECT_ROOT/_builds/cordova/build.stderr"; false)
    cd $PROJECT_ROOT
    echo
    echo "Try the Cordova browser application: $PROJECT_ROOT/_builds/cordova/platforms/browser/www/index.html"
    echo
    if [ $(adb devices -l | grep -c model:) == 1 ]
    then
      adb install -r -t $PROJECT_ROOT/_builds/cordova/platforms/android/app/build/outputs/apk/debug/app-debug.apk
      adb shell monkey -p net.jacquev6.Collide 1
      echo
      echo "Try the Cordova Android application: on your connected phone"
    else
      echo "Try the Cordova Android application: adb install $PROJECT_ROOT/_builds/cordova/platforms/android/app/build/outputs/apk/debug/app-debug.apk"
    fi
    echo
  fi
fi


jbuilder_flavor release build src/collide_cli.exe
rm -rf _builds/video
mkdir -p _builds/video
cd _builds/video
RATE=25
DURATION=30
../release/default/src/collide_cli.exe $RATE $DURATION
ffmpeg -loglevel panic -hide_banner -nostats -y -r $RATE -i %08d.png -vcodec libx264 video.mp4
rm -f ????????.png
echo
echo "Watch the video generated by the CLI application: $PROJECT_ROOT/_builds/video/video.mp4"
echo


echo "Development cycle OK"
