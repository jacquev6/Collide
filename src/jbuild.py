#!/usr/bin/env python3

import sys

template = """\
; THIS FILE IS GENERATED by ./jbuild.py
; MANUAL CHANGES WILL BE LOST

(jbuild_version 1)

(library (
  (name Collide)
  (modules (Drawer Simulation JsonSimulation GraphicalApplication))
  (libraries (yojson General JsOfOCairo)){coverage}
))


(executable (
  (name collide_cli)
  (modules (collide_cli))
  (libraries (Collide cairo2))
))
))


(rule (
  (targets (node_modules.sentinel))
  (action (progn
    {npm_install}
    (write-file node_modules.sentinel sentinel)
  ))
))

(rule (
  (targets (FileSaver.js))
  (deps (node_modules.sentinel))
  (action (progn
    (with-stdout-to ${{@}} (system "node_modules/.bin/browserify -s FileSaver node_modules/file-saver/FileSaver.min.js"))
  ))
))

(executable (
  (name collide_browser)
  (modules (collide_browser))
  (libraries (Collide))
  (preprocess (pps (js_of_ocaml-ppx)))
  (js_of_ocaml ((flags (+nat.js))))
))

(alias (
  (name collide_browser)
  (deps (collide_browser.html collide_browser.bc.js FileSaver.js))
))


(executable (
  (name unit_test)
  (modules (unit_test))
  (libraries (Collide)){coverage}
))

(rule (
  (targets (unit_test.sentinel))
  (deps (unit_test.bc))
  (action (progn
    (run ${{exe:unit_test.bc}})
    (write-file unit_test.sentinel sentinel)
  ))
))

(alias (
  (name runtest)
  (deps (unit_test.sentinel))
))
"""

npm_packages = [
    "file-saver@>=1.3.3 <2",
    "browserify",
]

coverage = dict(
    coverage="\n  (preprocess (pps (bisect_ppx)))",
    npm_install="(system \"if [ -d /tmp/node_modules_for_Collide ]; then rm -rf node_modules; cp -R /tmp/node_modules_for_Collide node_modules; else npm install {}; rm -rf /tmp/node_modules_for_Collide; cp -R node_modules /tmp/node_modules_for_Collide; fi\")".format(" ".join("'{}'".format(pack) for pack in npm_packages)),
)

release = dict(
    coverage="",
    npm_install=coverage["npm_install"],
)

publish = dict(
    coverage="",
    npm_install="(run ${{bin:npm}} install {})".format(" ".join('"{}"'.format(pack) for pack in npm_packages)),
)

flavors = dict(
  coverage=coverage,
  release=release,
  publish=publish,
)

with open(__file__[:-3], "w") as f:
    f.write(template.format(**flavors[sys.argv[1]]))
