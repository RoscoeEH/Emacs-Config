# Emacs-Config
The config file for my emacs so I can move it between machines easily.

This is stored a separte path that is isnside a git repo. The actual text in my .emacs file is:
```
(load "<path to init.el>")
```

## Packages
I use the following packages:
- company-mode - Autocompletion
- vterm - Terminal emulator
- magit - Git interface
- evil-mode - Vim keybindings
- evil-collection - Evil mode extensions
- ace-window - Window management
- evil-snipe - Quick navigation
- rainbow-delimiters - Colorful parentheses
- bm - Visual bookmarks
- flycheck - Syntax checking
- goto-last-change - Navigate to last edit
- aggressive-indent - Keep code properly indented
- key-chord - Key combinations
- doom-themes - Theme collection
- tuareg - OCaml mode
- merlin - OCaml IDE features
- ocamlformat - OCaml code formatting
- flycheck-rust - Rust syntax checking

## Custom Commands
- Improved compile command that automatically sets the right compile command based on file type
- wrap selections in parentheses, brackets, braces, double quotes, and single quotes
- evil EOL moves to last non-whitespace character
