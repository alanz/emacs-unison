# Use Unison Codebase Manager in emacs, via its API

Status: experimental, does not work yet, liable to change/break at any time.

## Getting started

This process is currently brittle, works for me in my dev setup. It
assumes you have installed the [unison codebase
manager](https://www.unison-lang.org/) and it is in your path.

I am working against version `M4e`.

Check out this repo, and add it to your code path in emacs

```elisp
(add-to-list 'load-path "/home/alanz/tmp/unison-emacs-play")
(require 'ucm)
```

Open an emacs shell, and start ucm

```elisp
M-x shell
ucm
```

Start the `ucm-repl`

```elisp
M-x ucm-repl
```

## Currently implemented commands

### list

Lists the contents of your codebase.

