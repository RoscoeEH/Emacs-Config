# Emacs-Config
The config file for my emacs so I can move it between machines easily.

This is stored a separte path that is isnside a git repo. The actual text in my .emacs file is:
```
(load "<path to init.el>")
```

The image on the start page is also here stored in the repo.

## Packages
- company-mode (dabbrev backend) - Autocompletion
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
- direnv - Environment variable management based on directory
- grip - Markdown viewing in browser
- epa - Managing GPG keys

## Custom Commands
- Improved compile command that automatically sets the right compile command based on file type
- wrap selections in parentheses, brackets, braces, double quotes, and single quotes
- evil EOL moves to last non-whitespace character
- evil-delete with the 'd' key does now copy to clipboard
- evil-paste-after/before delete selected text and paste over in visual mode
- Commands for changing capitalization in normal mode
- Delete a single char and enter insert mode
- M-g <g, l> for different grep commands
- Move path in minibuffer back one directory

## Other
- exec-path-from-shell arguments set to '-l' for performance
