#!/bin/bash

# GENERATED SECTION, MANUAL EDITS WILL BE LOST
# GENI: prologue(ocaml=dict(dependencies=["JsOfOCairo", "General", "lablgtk", "cairo2"]))
set -o errexit

eval `opam config env`
opam install --yes General JsOfOCairo bisect-summary bisect_ppx cairo2 jbuilder lablgtk

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

# END OF GENERATED SECTION


jbuilder_flavor release build src/collide_gtk.exe
echo
echo "Try the GTK+ application: $PROJECT_ROOT/_builds/release/default/src/collide_gtk.exe"
echo


if which convert
then
  cd media
  git clean -fXd resized
  for size in 16 114 512
  do
    convert icon-1240.png -resize ${size}x${size} resized/icon-${size}.png
  done
  cd $PROJECT_ROOT
fi


if which browserify >/dev/null
then
  jbuilder_flavor release build @collide_browser

  rm -rf docs
  mkdir docs
  touch docs/.nojekyll
  grep -v "cordova\.js" _builds/release/default/src/collide_browser.html >docs/index.html
  cp _builds/release/default/src/*.css docs
  cp _builds/release/default/src/*.js docs
  cp media/resized/icon-16.png docs
  echo
  echo "Try the in-browser application: $PROJECT_ROOT/docs/index.html"
  echo

  if (which cordova && which cordova-icon) >/dev/null
  then
    mkdir -p _builds/cordova/www
    cp cordova/config.xml _builds/cordova
    cp cordova/package.json _builds/cordova
    cp _builds/release/default/src/collide_browser.html _builds/cordova/www/index.html
    cp _builds/release/default/src/*.css _builds/cordova/www
    cp _builds/release/default/src/*.js _builds/cordova/www
    cp media/icon-1240.png _builds/cordova/icon.png
    cd _builds/cordova
    cordova prepare
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

    if [ -f "$ANDROID_KEYSTORE_FILE" -a ! -z "$ANDROID_KEYSTORE_PASSWORD" ]
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

# GENERATED SECTION, MANUAL EDITS WILL BE LOST
# GENI: epilogue(ocaml=dict(package="Collide"))
opam pin --yes --no-action add .
opam reinstall --yes Collide --build-test

echo "Development cycle OK"
# END OF GENERATED SECTION
