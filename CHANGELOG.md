
# HaTeX Changelog

This is the logchange of HaTeX. It might not be exhaustive.
For a full list of changes, see the commit history of the
git repository:

https://github.com/Daniel-Diaz/HaTeX/commits/master

# Changelog by versions

## From 3.14.0.0 to 3.15.0.0

* New package implemented: AMSSymb.
* Package beamer further developed.
* Bug fix: [#35](https://github.com/Daniel-Diaz/HaTeX/issues/35).
* Added common numeric sets to AMSSymb.
* Breaking change: AMSMath functions 'pm' and 'mp' changed their
  type from `LaTeXC l => l -> l -> l` to `LaTeXC l => l`.
* Additions to the AMSMath module.

## From 3.13.1.0 to 3.14.0.0

* Fixed link in cabal file.
* Added support for arguments delimited by parenthesis (experimental).
* More tests on parsing.
* Parser now backtracks when failing in argument parsing.

## From 3.13.0.1 to 3.13.1.0

* New function ``matrixTabular`` to create tables from matrices.
* Modified LaTeX Monoid instance to make monoid laws hold.
* Some documentation improvements.
* Added this CHANGELOG!
