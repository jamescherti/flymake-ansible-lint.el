# flymake-ansible-lint.el - A Emacs Flymake backend for ansible-lint
![Build Status](https://github.com/jamescherti/flymake-ansible-lint.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/flymake-ansible-lint-badge.svg)](https://melpa.org/#/flymake-ansible-lint)
[![MELPA Stable](https://stable.melpa.org/packages/flymake-ansible-lint-badge.svg)](https://stable.melpa.org/#/flymake-ansible-lint)
![License](https://img.shields.io/github/license/jamescherti/flymake-ansible-lint.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The [flymake-ansible-lint](https://github.com/jamescherti/flymake-ansible-lint.el) package provides a Flymake backend for ansible-lint, enabling real-time syntax and style checking for Ansible playbooks and roles within Emacs.

*(This package can also work with Flycheck: simply use the `flymake-flycheck` package, which allows any Emacs Flymake backend to function as a Flycheck checker.)*

## Requirements

- [ansible-lint](https://github.com/ansible/ansible-lint) >= 1.0.4

## Installation

To install `flymake-ansible-lint` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install `flymake-ansible-lint` from MELPA:
```emacs-lisp
(use-package flymake-ansible-lint
  :ensure t
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode)))
```

## Customizations

You can configure `ansible-lint` parameters using the `flymake-ansible-lint-args` variable:

```emacs-lisp
(setq flymake-ansible-lint-args '("--offline"
                                  "-x" "run-once[play],no-free-form"))
```

## Frequently asked questions

### Why are some ansible-lint error messages truncated?

This issue is a known bug in `ansible-lint`, not in `flymake-ansible-lint`.

It is `ansible-lint` that truncates some error messages:
```
$ ansible-lint -p test.yaml
test.yaml:5: yaml[truthy]: Truthy value should be one of
```

### Where does the package create temporary files?

By default, the *flymake-ansible-lint* package creates temporary files in the same directory as the Ansible YAML file currently being edited.

For example, when editing: `/home/user/test/myfile.yaml`, the package may generate temporary files such as: `/home/user/test/flymake_1_myfile.yaml` `/home/user/test/flymake_2_myfile.yaml`...

**These temporary files are automatically removed once flymake-ansible-lint completes its analysis.**

(This behavior is controlled by the variable `flymake-ansible-lint-tmp-files-enabled`. Keeping `flymake-ansible-lint-tmp-files-enabled` enabled is important because it allows Flymake to lint the most recent in-memory state of the buffer, including unsaved modifications. Without these temporary files, Flymake would have to rely on the last saved version of the file, which might not reflect the current edits. This ensures that linting diagnostics remain accurate, up to date, and synchronized with the actual buffer contents, providing immediate feedback while editing.)

## License

The `flymake-ansible-lint` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version. This package uses *flymake-quickdef*, by Karl Otness.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [flymake-ansible-lint.el @GitHub](https://github.com/jamescherti/flymake-ansible-lint.el)
- [flymake-ansible-lint.el @MELPA](https://melpa.org/#/flymake-ansible-lint)
- [ansible-lint @GitHub](https://github.com/ansible/ansible-lint)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
