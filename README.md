# Emacs-Config
The config file for my emacs so I can move it between machines easily.

This is stored a separate path that is inside a git repo. The actual text in my .emacs file is:
```
(load "<path/to/init.el>")
```

## Structure
The main init.el file defines paths to necessary files for loading emacs that should be changed between machines. These include:
- "EMACS_PATH" - path to the ".emacs.d" directory
- "ELPA_PATH" - path to the "elpa" directory, generally a sub-directory of ".emacs.d"
- "CONFIG_PATH" - path to the directory containing this repo
- "PACKAGES_PATH" - path to the sub-directory in this repo containing package configs
- "FILE_TYPE_PATH" - path to the sub-directory in this repo containing file type configs, i.e. programming languages, markdown, or pdf

It then loads five other files that contain different components of the whole config. These include:
- "setup.el" - loads in melpa, performance enhancements, visual components, or anything else not directly called by the user
- "package-list.el" - loads in each individual package that is used separately, see the package list below
- "custom-commands.el" - loads in any commands not related to a specific package, it utilizes evil and key-chord so it needs to be loaded after "package-list.el"
- "file-types.el" - loads any specifics needed for a particular programming language or type of text to edit or view
- "homescreen.el" - used the dashboard package and image in this repo the create the homescreen


## Packages
The packages listed below each have an individual load file which contains setup and any associated commands.
- "company-config.el" - text auto-complete
- "vterm-config.el" - a better shell than the base one
- "flycheck-config.el" - syntax checking
- "magit-config.el" - git interface
- "evil-config.el" - vim key-binds
- "ace-window-config.el" - window movement
- "bm-config.el" - in file bookmarks for better file navigation
- "goto-last-change-config.el" - goes to the last change
- "key-chord-config.el" - binds commands to a set of keys being pressed at the same time
- "rainbow-delimiters-config.el" - colors delimiters to easily find pairs
- "ispell-config.el" - spell check
- "vertico-config.el" - helpful mini-buffer presentation
- "epa-config.el" - gpg management
- "origami-config.el" - collapse blocks
- "consult-config.el" - helpful for viewing repos


## File Types
This handles the coding languages I expect to use as well as things such as markdown or pdf. It contains configs for:
- rust
- python
- c/cpp
- ocaml
- markdown
- latex
- pdf


---
## Pensec Branch
This branch is setup to run on a my work computer (a windows machine). It has fewer file type options as I do not use things like rust or ocaml for work. It also has additional files for work specific commands and a custom xref backend for FIPS requirements.
