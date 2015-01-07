# cl-readline

Common Lisp bindings to [GNU Readline
library](http://directory.fsf.org/wiki/Readline).

The Readline library provides a set of functions for use by applications
that allow users to edit command lines as they are typed in. Both Emacs and
vi editing modes are available. The Readline library includes additional
functions to maintain a list of previously-entered command lines, to recall
and perhaps reedit those lines, and perform csh-like history expansion on
previous commands.

These bindings provide Lispy interface to GNU Readline somewhat reducing its
hair. Some minor features are omitted, they may be added by request. Open an
issue if you have any propositions.

## Installation

Download or clone the repository and put it into some place where ASDF can
find it. Note that `cl-readline` depends on CFFI and Alexandria, so you
should have installed them.

## Documentation

See contents of directory `doc`. Documentation is also available online:

https://mrkkrp.github.io/cl-readline/

## License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL.
