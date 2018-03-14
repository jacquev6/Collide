#!/usr/bin/env python3

import sys

template = """\
(jbuild_version 1)

(library (
  (name Collide_)
  (modules (Drawer Simulation))
  (libraries (General JsOfOCairo)){coverage}
))


(executable (
  (name collide)
  (modules (collide))
  (libraries (cairo2 Collide_))
))


(executable (
  (name unit_test)
  (modules (unit_test))
  (libraries (Collide_)){coverage}
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

flavors = dict(
    coverage={
        "coverage": "\n  (preprocess (pps (bisect_ppx)))",
    },
    release={
        "coverage": "",
    },
    publish={
        "coverage": "",
    }
)

with open(__file__[:-3], "w") as f:
    f.write(template.format(**flavors[sys.argv[1]]))
