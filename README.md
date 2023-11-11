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

See contents of the directory `doc`. The documentation is also available online:

https://vindarel.github.io/cl-readline/

See also our
[cl-readline-example](https://github.com/vindarel/cl-readline-example),
our [replic](https://github.com/vindarel/replic) library and executable,
that helps building a readline application, and a list of projects
using the library
[on the wiki](https://github.com/vindarel/cl-readline/wiki).

See also [linedit](https://github.com/sharplispers/linedit).

### Get to know readline

A `man readline` will teach a lot.

You can configure how your readline prompt behaves, system-wide, with
a configuration file: `~/.inputrc`, that is read by your shell and by
other applications using readline. The file `/etc/inputrc` contains
examples.

[This post by masteringemacs](https://www.masteringemacs.org/article/keyboard-shortcuts-every-command-line-hacker-should-know-about-gnu-readline) contains a whole lot of tips.

If you came here to improve the default SBCL readline REPL, started
with `rlwrap` (readline wrapper), this library is of no help, you'll
need to tweak your readline or use another SBCL readline REPL
([cl-repl](https://github.com/koji-kojiro/cl-repl/),
[sbcli](https://github.com/hellerve/sbcli), the newer [CIEL
repl](https://ciel-lang.github.io/CIEL/#/repl)). To improve the completion with Lisp symbols, read this: [rlwrap settings for SBCL](https://gist.github.com/vindarel/2309154f4e751be389fa99239764c363).

This library is to write applications on readline and build your own
logic: your own prompt, your own completion, your own key bindings,
etc.

### Update the documentation

Edit the .texi file.

Update the index.html (displayed as-is by Github pages) by running:

    $ make html

which calls the `makeinfo` program, and commit `doc/index.html` to the gh-pages branch.


## License

Copyright © 2015–2018 Mark Karpov <br>
Copyright © 2018–present vindarel and contributors

Distributed under GNU GPL, version 3.
