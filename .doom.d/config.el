;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.doom.d/elisp"))
(require 'open-junk-file)

(load! "+ui")
(load! "+funcs")
(load! "+org")
(load! "+keys")
(load! "+misc")
