#!/bin/bash

# Copyright 2018 Vincent Jacques <vincent@vincent-jacques.net>

# GENI: prologue(which_or_skip=[("cordova", "cordova"), ("cordova-icon", "cordova"), ("ffmpeg", "video"), ("jarsigner", "publish-apk"), ("zipalign", "publish-apk")])
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
set -o errexit -o pipefail
IFS=$'\n\t'

PROJECT_ROOT=$(pwd)

SHOW_IN_BROWSER=false
function show_in_browser {
  echo
  echo "$1: $PROJECT_ROOT/$2"
  echo
  if $SHOW_IN_BROWSER
  then
    python -m webbrowser -t file://$PROJECT_ROOT/$2
  fi
}

DO_CORDOVA=true
DO_OPAM_INSTALL=true
DO_OPAM_REMOVE=true
DO_OPAM_UPGRADE=true
DO_PUBLISH_APK=true
DO_VIDEO=true

while [[ "$#" > 0 ]]
do
  case $1 in
    -wb|--web-browser)
      SHOW_IN_BROWSER=true
      ;;
    --skip-cordova)
      DO_CORDOVA=false
      ;;
    --skip-opam-install)
      DO_OPAM_INSTALL=false
      ;;
    --skip-opam-remove)
      DO_OPAM_REMOVE=false
      ;;
    --skip-opam-upgrade)
      DO_OPAM_UPGRADE=false
      ;;
    --skip-publish-apk)
      DO_PUBLISH_APK=false
      ;;
    --skip-video)
      DO_VIDEO=false
      ;;
    -q|--quick)
      DO_CORDOVA=false
      DO_OPAM_INSTALL=false
      DO_OPAM_REMOVE=false
      DO_OPAM_UPGRADE=false
      DO_PUBLISH_APK=false
      DO_VIDEO=false
      ;;
    *)
      echo "Unknown parameter passed: $1"
      exit 1;;
  esac
  shift
done

if $DO_CORDOVA && ! which cordova >/dev/null
then
  echo "WARNING: cordova not found in PATH, use --skip-cordova to acknowledge"
  DO_CORDOVA=false
fi

if $DO_CORDOVA && ! which cordova-icon >/dev/null
then
  echo "WARNING: cordova-icon not found in PATH, use --skip-cordova to acknowledge"
  DO_CORDOVA=false
fi

if $DO_VIDEO && ! which ffmpeg >/dev/null
then
  echo "WARNING: ffmpeg not found in PATH, use --skip-video to acknowledge"
  DO_VIDEO=false
fi

if $DO_PUBLISH_APK && ! which jarsigner >/dev/null
then
  echo "WARNING: jarsigner not found in PATH, use --skip-publish-apk to acknowledge"
  DO_PUBLISH_APK=false
fi

if $DO_PUBLISH_APK && ! which zipalign >/dev/null
then
  echo "WARNING: zipalign not found in PATH, use --skip-publish-apk to acknowledge"
  DO_PUBLISH_APK=false
fi

function opam_switch {
  eval `opam config env --switch=$1 --set-switch`
}

function dune_ {
  SWITCH=$1
  shift
  FLAVOR=$1
  shift

  opam_switch $SWITCH
  rm -rf _build
  DIRECTORY=_builds/$SWITCH/$FLAVOR
  mkdir -p $DIRECTORY
  ln -sf $DIRECTORY _build
  for gen in $(find . -name dune.py)
  do
    $gen $FLAVOR >${gen%.py}
  done

  find $DIRECTORY -name "*.sentinel" -delete
  dune "$@"

  rm -rf _build
  for gen in $(find . -name dune.py)
  do
    echo "; THIS FILE IS GENERATED by ./dune.py" >${gen%.py}
    echo "; MANUAL CHANGES WILL BE LOST" >>${gen%.py}
    echo "" >>${gen%.py}
    $gen publish >>${gen%.py}
  done
}
# END OF GENERATED SECTION


# GENI: install_dependencies
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
DEV_WITH_OLDEST_VERSIONS=4.06.1.Collide.dev_with_oldest_versions
if ! opam switch list --short | grep "^$DEV_WITH_OLDEST_VERSIONS$" >/dev/null
then
  opam switch create $DEV_WITH_OLDEST_VERSIONS 4.06.1 --no-switch
fi
opam_switch $DEV_WITH_OLDEST_VERSIONS
opam install --yes General.0.6.0 JsOfOCairo.1.1.1 bisect-summary bisect_ppx cairo2.0.5 dune.1.4.0 lablgtk.2.18.6

DEV_WITH_NEWEST_VERSIONS=4.07.1.Collide.dev_with_newest_versions
if ! opam switch list --short | grep "^$DEV_WITH_NEWEST_VERSIONS$" >/dev/null
then
  opam switch create $DEV_WITH_NEWEST_VERSIONS 4.07.1 --no-switch
fi
opam_switch $DEV_WITH_NEWEST_VERSIONS
opam install --yes General JsOfOCairo cairo2 dune lablgtk
if $DO_OPAM_UPGRADE; then opam upgrade --yes; fi
# END OF GENERATED SECTION


opam install --yes cairo2.0.5


# GENI: run_tests
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
test -d _builds && find _builds -name "*.sentinel" -delete
opam_switch $DEV_WITH_OLDEST_VERSIONS
dune_ $DEV_WITH_OLDEST_VERSIONS coverage runtest
echo
bisect-summary _builds/$DEV_WITH_OLDEST_VERSIONS/coverage/default/*/bisect????.out
bisect-ppx-report -I _builds/$DEV_WITH_OLDEST_VERSIONS/coverage/default -html _builds/$DEV_WITH_OLDEST_VERSIONS/coverage/default/bisect _builds/$DEV_WITH_OLDEST_VERSIONS/coverage/default/*/bisect????.out
show_in_browser "See coverage report" _builds/$DEV_WITH_OLDEST_VERSIONS/coverage/default/bisect/index.html

opam_switch $DEV_WITH_NEWEST_VERSIONS
dune_ $DEV_WITH_NEWEST_VERSIONS debug runtest
# END OF GENERATED SECTION


dune_ $DEV_WITH_NEWEST_VERSIONS release build src/collide_gtk.exe
echo
echo "Try the GTK+ application: $PROJECT_ROOT/_builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/collide_gtk.exe"
echo


mkdir -p _builds/media
for size in 16 114 512 1240
do
  # 'convert' comes from ImageMagick
  convert media/icon-1240.png -resize ${size}x${size} _builds/media/icon-${size}.png
done

dune_ $DEV_WITH_NEWEST_VERSIONS release build --profile release @collide_browser

rm -rf docs
mkdir docs
touch docs/.nojekyll
grep -v "cordova\.js" _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/collide_browser.html >docs/index.html
cp _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/*.css docs
cp _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/*.js docs
cp _builds/media/icon-16.png docs
show_in_browser "Try the in-browser application" docs/index.html

if $DO_CORDOVA
then
  mkdir -p _builds/cordova/www
  cp cordova/config.xml _builds/cordova
  cp cordova/package.json _builds/cordova
  cp _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/collide_browser.html _builds/cordova/www/index.html
  cp _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/*.css _builds/cordova/www
  cp _builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/*.js _builds/cordova/www
  cp _builds/media/icon-1240.png _builds/cordova/icon.png
  cd _builds/cordova
  cordova prepare
  cordova-icon >cordova-icon.stdout 2>cordova-icon.stderr || (echo "Error during cordova-icon. Have a look at $PROJECT_ROOT/_builds/cordova/cordova-icon.stdout and $PROJECT_ROOT/_builds/cordova/cordova-icon.stderr"; false)
  cordova build >cordova-build.stdout 2>cordova-build.stderr || (echo "Error during cordova build. Have a look at $PROJECT_ROOT/_builds/cordova/cordova-build.stdout and $PROJECT_ROOT/_builds/cordova/cordova-build.stderr"; false)
  cd $PROJECT_ROOT
  show_in_browser "Try the Cordova browser application" _builds/cordova/platforms/browser/www/index.html
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

  if $DO_PUBLISH_APK
  then
    # I tried signing the .apk as described here:
    #   https://cordova.apache.org/docs/en/latest/guide/platforms/android/index.html#signing-an-app
    # But I got errors like
    #   Execution failed for task ':app:packageRelease'.
    #   > Failed to generate v1 signature
    # So I went back to manual signing as described in:
    #   https://codeburst.io/publish-a-cordova-generated-android-app-to-the-google-play-store-c7ae51cccdd5
    cd _builds/cordova
    cordova build android --release >cordova-build-release.stdout 2>cordova-build-release.stderr || (echo "Error during cordova build. Have a look at $PROJECT_ROOT/_builds/cordova/cordova-build-release.stdout and $PROJECT_ROOT/_builds/cordova/cordova-build-release.stderr"; false)

    GOOGLE_PLAY_APK=Collide.$(git describe --tags --dirty).GooglePlay.apk
    cp platforms/android/app/build/outputs/apk/release/app-release-unsigned.apk $GOOGLE_PLAY_APK.tmp.apk
    jarsigner -verbose -keystore $ANDROID_KEYSTORE_FILE -storepass $ANDROID_KEYSTORE_PASSWORD -sigalg SHA1withRSA -digestalg SHA1 $GOOGLE_PLAY_APK.tmp.apk GooglePlayUpload >jarsigner.stdout 2>jarsigner.stderr || (echo "Error during .apk signing. Have a look at $PROJECT_ROOT/_builds/cordova/jarsigner.stdout and $PROJECT_ROOT/_builds/cordova/jarsigner.stderr"; false)
    zipalign -v -f 4 $GOOGLE_PLAY_APK.tmp.apk $GOOGLE_PLAY_APK >zipalign.stdout 2>zipalign.stderr || (echo "Error during .apk zipalign. Have a look at $PROJECT_ROOT/_builds/cordova/zipalign.stdout and $PROJECT_ROOT/_builds/cordova/zipalign.stderr"; false)
    rm $GOOGLE_PLAY_APK.tmp.apk

    AMAZON_APPSTORE_APK=Collide.$(git describe --tags --dirty).AmazonAppstore.apk
    cp platforms/android/app/build/outputs/apk/release/app-release-unsigned.apk $AMAZON_APPSTORE_APK

    cd $PROJECT_ROOT
    echo "Production application files:"
    echo "  - $PROJECT_ROOT/_builds/cordova/$GOOGLE_PLAY_APK"
    echo "  - $PROJECT_ROOT/_builds/cordova/$AMAZON_APPSTORE_APK"
    echo
  fi
fi


dune_ $DEV_WITH_NEWEST_VERSIONS release build src/collide_cli.exe
_builds/$DEV_WITH_NEWEST_VERSIONS/release/default/src/collide_cli.exe --help

if $DO_VIDEO
then
  rm -rf _builds/video/*
  mkdir -p _builds/video
  cd _builds/video
  FPS=10
  FORMAT=frame_%05d.png
  ../$DEV_WITH_NEWEST_VERSIONS/release/default/src/collide_cli.exe \
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
fi

# GENI: check_code
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
# END OF GENERATED SECTION

# GENI: documentation
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
# END OF GENERATED SECTION


# GENI: install
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
if $DO_OPAM_INSTALL
then
  if ! opam switch list --short | grep "^4.07.1.Collide.pin_on_newest_ocaml$" >/dev/null
  then
    opam switch create 4.07.1.Collide.pin_on_newest_ocaml 4.07.1 --no-switch
  fi
  opam_switch 4.07.1.Collide.pin_on_newest_ocaml
  if $DO_OPAM_REMOVE
  then
    opam remove --yes --auto-remove $(opam list --short --installed-roots | grep -v -e "^ocaml-base-compiler$")
    opam pin list --short | grep "." && opam pin remove $(opam pin list --short)
  fi
  opam pin --yes --no-action add --kind=path .
  opam remove --yes Collide
  opam install --yes Collide --deps-only
  opam install --yes Collide --build-test

  if ! opam switch list --short | grep "^4.06.1.Collide.pin_on_oldest_ocaml$" >/dev/null
  then
    opam switch create 4.06.1.Collide.pin_on_oldest_ocaml 4.06.1 --no-switch
  fi
  opam_switch 4.06.1.Collide.pin_on_oldest_ocaml
  if $DO_OPAM_REMOVE
  then
    opam remove --yes --auto-remove $(opam list --short --installed-roots | grep -v -e "^ocaml-base-compiler$")
    opam pin list --short | grep "." && opam pin remove $(opam pin list --short)
  fi
  opam pin --yes --no-action add --kind=path .
  opam remove --yes Collide
  opam install --yes Collide --deps-only
  opam install --yes Collide --build-test
fi
# END OF GENERATED SECTION


# GENI: epilogue
# GENERATED SECTION, MANUAL EDITS WILL BE LOST
echo
echo "Development cycle OK"
# END OF GENERATED SECTION
