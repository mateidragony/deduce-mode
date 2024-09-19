# deduce-mode

`deduce-mode` is an Emacs major mode for syntax highlighting in deduce.

## Usage

To turn it on:

```
M-x deduce-mode
```

To turn it on automatically for `.pf` files:

```lisp
(add-to-list 'auto-mode-alist '("\\.pf\\'" . deduce-mode))
```

## Installation

For now you can install the package from the buffer. Open up `deduce-mode.el` and issue

```
M-x package-install-from-buffer
```
