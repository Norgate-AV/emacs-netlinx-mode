# emacs-netlinx-mode

A [NetLinx][netlinx] major mode for [Emacs][emacs] using [Tree-Sitter][tree-sitter].

[netlinx]: https://www.amx.com/en/site_elements/amx-language-reference-guide-netlinx-programming-language
[emacs]: https://www.gnu.org/software/emacs/
[tree-sitter]: https://tree-sitter.github.io/tree-sitter/

## Features

-   Syntax highlighting via tree-sitter
-   Automatic grammar installation
-   Built on Emacs 29+ native tree-sitter support

## Requirements

-   Emacs 29.1 or later with tree-sitter support
-   C compiler (gcc/clang) for building the grammar
-   Git (for downloading the grammar)

## Installation

### From MELPA (Coming Soon)

```elisp
(use-package netlinx-mode
  :ensure t
  :mode ("\\.axs\\'" "\\.axi\\'"))
```

### Manual Installation

1. Clone this repository:

```bash
git clone https://github.com/Norgate-AV/emacs-netlinx-mode.git
```

2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/emacs-netlinx-mode")
(require 'netlinx-mode)
```

## Usage

The mode will automatically activate for `.axs` and `.axi` files. When you first open a NetLinx file, the tree-sitter grammar will be automatically downloaded and compiled.

If automatic installation fails, you can manually install the grammar:

```elisp
M-x netlinx-mode--install-grammar
```

## Configuration

You can customize the grammar repository location:

```elisp
(setq netlinx-mode-grammar-location "https://github.com/Norgate-AV/tree-sitter-netlinx")
```

## Development

This mode uses the built-in `treesit.el` library (Emacs 29+) and the [tree-sitter-netlinx](https://github.com/Norgate-AV/tree-sitter-netlinx) grammar.

### Testing Locally

1. Load the mode in Emacs:

```elisp
(load-file "/path/to/netlinx-mode.el")
```

2. Open a NetLinx file to test syntax highlighting

3. Use `M-x treesit-explore-mode` to inspect the parse tree

## LICENSE

[MIT](./LICENSE)
