.. GENI: intro
.. GENERATED SECTION, MANUAL EDITS WILL BE LOST

*Collide* is an `event-based simulation <https://en.wikipedia.org/wiki/Discrete_event_simulation>`_ of `elastic collisions <https://en.wikipedia.org/wiki/Elastic_collision>`_ between balls.
It demonstrates how a single OCaml code base can run on multiple platforms (see below).
And it's mildly entertaining to watch.

.. END OF GENERATED SECTION

.. GENI: info
.. GENERATED SECTION, MANUAL EDITS WILL BE LOST

It's licensed under the `MIT license <http://choosealicense.com/licenses/mit/>`_.
Its `source code <https://github.com/jacquev6/Collide>`_ is on GitHub.

.. END OF GENERATED SECTION

.. GENI: badges
.. GENERATED SECTION, MANUAL EDITS WILL BE LOST

Questions? Remarks? Bugs? Want to contribute? `Open an issue <https://github.com/jacquev6/Collide/issues>`_!

.. image:: https://img.shields.io/travis/jacquev6/Collide/master.svg
    :target: https://travis-ci.org/jacquev6/Collide

.. image:: https://img.shields.io/github/issues/jacquev6/Collide.svg
    :target: https://github.com/jacquev6/Collide/issues

.. image:: https://img.shields.io/github/forks/jacquev6/Collide.svg
    :target: https://github.com/jacquev6/Collide/network

.. image:: https://img.shields.io/github/stars/jacquev6/Collide.svg
    :target: https://github.com/jacquev6/Collide/stargazers

.. END OF GENERATED SECTION

Platforms
=========

Web browser
-----------

Just go to http://jacquev6.github.io/Collide.

Native applications
-------------------

Native applications can be installed using `OPAM <https://opam.ocaml.org/>`_::

    $ opam pin add --yes Collide https://github.com/jacquev6/Collide.git

You can then run the command-line application (generating video frames)::

    $ collide_cli --help

And the graphical application::

    $ collide_gtk

Mobile applications
-------------------

Thanks to `Apache Cordova <http://cordova.apache.org/>`_, Collide is available on the following app stores:

- `Google Play <https://play.google.com/store/apps/details?id=net.jacquev6.Collide>`_
- `Amazon Appstore <https://www.amazon.com/dp/B07C8BWRZW>`_

It should be easy to publish as an iPhone, BlackBerry or Windows Phone app, but I have not tried.
