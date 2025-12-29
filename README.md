wks-mode 
======

An Emacs major-mode for syntax highlighting `wk`'s `.wks`
files. 

# Installation

## `package-vc`

If you are on Emacs 29 and newer, you can use
`package-vc-install`:

``` emacs-lisp
(package-vc-install
 '(wks-mode . (:url "https://github.com/3L0C/wks-mode")))
```

[vc-use-package](https://github.com/slotThe/vc-use-package)
provides
[use-package](https://github.com/jwiegley/use-package)
integration, if that's your thing. 

``` emacs-lisp
(use-package wks-mode
  :vc (:fetcher github :repo 3L0C/wks-mode))
```

Alternatively, if you're on Emacs 30,
a `:vc` keyword is built into use-package:

``` emacs-lisp
(use-package wks-mode
  :vc (:url "https://github.com/3L0C/wks-mode" :rev :newest))
```

## Manually

Copy `wks-mode.el` into a directory within your `load-path`
and require it.  For example, assuming that this file was
placed within the `~/.config/emacs/elisp` directory: 

``` emacs-lisp
(add-to-list 'load-path "~/.config/emacs/elisp/")
(require 'wks-mode)
```

If you use
[use-package](https://github.com/jwiegley/use-package), you
can express the above as

``` emacs-lisp
(use-package wks-mode
  :load-path "~/.config/emacs/elisp/")
```

## Spacemacs

If you use [Spacemacs](https://develop.spacemacs.org/), add
the following in the `dotspacemacs-additional-packages`
section: 

``` emacs-lisp
(wks-mode
 :location
 (recipe
  :fetcher github
  :repo "3L0C/wks-mode"))
```

## Doom Emacs

If you use [Doom
Emacs](https://github.com/hlissner/doom-emacs), 
add the followings in the `packages.el` and `config.el`,
respectively. 

``` emacs-lisp
(package! wks-mode
  :recipe (:host github
           :repo "3L0C/wks-mode"))
```

``` emacs-lisp
(use-package! wks-mode)
```

# Acknowledgments

My thanks to 
- Xah Lee who showed me how to create a major mode package
  with his 
  [tutorial](http://xahlee.info/emacs/emacs/elisp_write_major_mode_index.html). 
- The [zig-mode](https://github.com/ziglang/zig-mode) devs
  for their very readable major mode.
  
# Contributing

Contributions are welcome! If you find any issues or have
suggestions for improvements, please open an issue or submit
a pull request.
