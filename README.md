flashcards
==========

[![Travis-CI Build
Status](https://travis-ci.org/zachary-foster/flashcards.svg?branch=master)](https://travis-ci.org/zachary-foster/flashcards)
[![Coverage
Status](https://img.shields.io/codecov/c/github/zachary-foster/flashcards/master.svg)](https://codecov.io/github/zachary-foster/flashcards?branch=master)

The `flashcards` package provides a way to make and practice digital
flashcards in R. The "decks" are composed of a simple tsv file, a
directory of images (optional), and a YAML configuration file. Each user
has a "home" folder containing a folder of decks and .tsv files that
track progress. The functions `init_deck` and `init_home_dir` are
provided to initialize empty decks and user "home" folders. There is
also functions to practice one or deck (`practice`), add cards to decks
(`add_card`), and show progress (`plot_progress`). Cards can also be
added by simply editing the deck.tsv files in a spreadsheet editor.

This package is inspired by the Duolingo tinycards app, which I found
useful. While the graphics are not nearly as sophisticated, this R
package replicates most of the functionality of tinycards with some
improvements:

-   Multiple decks can be practiced at once
-   Unlimited number of decks and number of cards per deck
-   Decks are plain text and image files, so they can be generated
    automatically and shared easily

My hope is that this system is adopted by the community and numerous
community-maintained decks become available on Github or elsewhere.
Instead of making flashcards individually, we can work together to make
extensive, refined decks that will be universally useful. Contributions
in the forms of new decks, additions to existing decks, and to the
package itself are welcome!

Creating a user folder
----------------------

\[UNDER CONSTRUCTION\]

Creating a deck
---------------

\[UNDER CONSTRUCTION\]

Adding cards
------------

\[UNDER CONSTRUCTION\]

Practicing
----------

\[UNDER CONSTRUCTION\]

Reviewing progress
------------------

\[UNDER CONSTRUCTION\]

Comments and contributions
--------------------------

We welcome comments, criticisms, and especially contributions! GitHub
issues are the preferred way to report bugs, ask questions, or request
new features. You can submit issues here:

<https://github.com/zachary-foster/flashcards/issues>
