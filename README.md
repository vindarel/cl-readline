# cl-readline

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/cl-readline.svg?branch=master)](https://travis-ci.org/mrkkrp/cl-readline)
[![Quicklisp](http://quickdocs.org/badge/cl-readline.svg)](http://quickdocs.org/cl-readline/)

Common Lisp bindings
to [GNU Readline library](http://directory.fsf.org/wiki/Readline).

The Readline library provides a set of functions for use by applications
that allow users to edit command lines as they are typed in. Both Emacs and
vi editing modes are available. The Readline library includes additional
functions to maintain a list of previously-entered command lines, to recall
and perhaps reedit those lines, and perform csh-like history expansion on
previous commands.

These bindings provide a Lispy interface to GNU Readline somewhat reducing
the pain you are bound to experience when you use it. Some minor features
are omitted, they may be added on request. Open an issue if you have any
propositions.

## Installation

Via Quicklisp (recommended):

```common-lisp
(ql:quickload "cl-readline")
```

If you are using Homebrew on a Mac, please note that GNU Readline has
*keg-only* formula, which means you may need to link the library yourself:

```
$ brew link readline --force
```

## Documentation

See contents of the directory `doc`. Documentation is also available online:

https://vindarel.github.io/cl-readline/

See also our
[cl-readline-example](https://github.com/vindarel/cl-readline-example)
and a list of projects using the library
[on the wiki](https://github.com/vindarel/cl-readline/wiki).

## License

Copyright © 2015–2018 Mark Karpov

Distributed under GNU GPL, version 3.
