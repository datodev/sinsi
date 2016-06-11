# Dev setup

## Installation

Make sure you've already installed [OPAM](https://opam.ocaml.org/):

    brew install opam

Now run the following to checkout the project, create an opam switch
(like a sandbox so the dependencies in multiple projects don't cause
havoc with one another), and install all the dependencies (this will
take awhile if it's your first time, a lot to compile):

    git clone git@github.com:datodev/sinsi.git
    make deps
    eval `opam config env`

To add opam dependencies, edit `Makefile.user` under the `deps` target
to add the opam invocation.

## Running the server

    make clean && make && ./main.byte

## Running tests

    make clean && make test && ./test.byte

## Regenerating the Makefile

The make file is generated via
*
  [Oasis](https://ocaml.org/learn/tutorials/setting_up_with_oasis.html)
  a project management tool that wraps
* [Ocamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), a
  general app build tool that uses
* [Ocamlfind](http://projects.camlcity.org/projects/findlib.html), a
  tool used to automatically figure out module names and dependencies
  in a project

If you make an edit to the `_oasis` file, then to get a new
`Makefile`, run:

    oasis setup -setup-update dynamic

# Requirements

* Multitenant (per-user read/unread status, etc.)
* Parse RSS/Atom feeds
* Disambiguate individual posts for storage
* Mark post read/unread
* Subscribe/unsubscribe from feeds
* Worker thread to fetch global list of feeds and store them in the db
* HTML UI to read posts
** Manage feeds view
** Read posts view
** Don't show read posts older than N minutes
** Sort by newest
* UUID-based feed lists
* User-based feed lists as a stretch goal

