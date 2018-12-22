ido-ghq
-------

Introduction
============

`ivy-ghq` provides interfaces of ghq with ivy.

Requirements
============

* Emacs 24.5 or higher: I use this program on Emacs 26.1.
* [ghq](https://github.com/motemen/ghq)

Setup and Customize
===================

``` common-lisp
(add-to-list 'load-path "somewhere")
(require 'ivy-ghq)
(setq ivy-ghq-short-list t)   ;;  Whether display full path or short path
```

Usage
=====

`ivy-ghq`: Execute `ghq list --full-path` and Open selected directory by dired.


License
=======

License is same as original, GPL-3+.
