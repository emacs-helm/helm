<p align="center"><a href="http://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3" /></a>
  <a href="https://melpa.org/#/helm"><img alt="MELPA" src="https://melpa.org/packages/helm-badge.svg"/></a>
  <a href="https://stable.melpa.org/#/helm"><img alt="MELPA Stable" src="https://stable.melpa.org/packages/helm-badge.svg"/></a>
</p>


<h1 align="center">Emacs-helm</h1>

<p align="center">
    <img src="https://avatars3.githubusercontent.com/u/1541688?v=3&amp;s=200" alt="Emacs-helm" title="" />
</p>

***
<p align="justify">
  <b>Helm</b> is an Emacs framework for incremental completions and narrowing
  selections. It helps to rapidly complete file names, buffer names, or
  any other Emacs interactions requiring selecting an item from a list of
  possible choices.
  Helm is a fork of <b>anything.el</b>, which was originally written by Tamas
  Patrovic and can be considered to be its successor. <b>Helm</b> cleans the
  legacy code that is leaner, modular, and unchained from constraints of
  backward compatibility.
</p>


<h1>Emacs-helm</h1>

<p>
Maintainance of Helm is a <a href="https://github.com/emacs-helm/helm/commits?author=thierryvolpiatto"><b>Lot of work</b></a>
I have done freely since 2011.<br>
As it is taking a lot of my time it starts to be more and more difficult<br>
maintaining it without financial help.<br>
If you feel Helm is making your daily work easier,<br><b>please consider making a donation.</b>
</p>

<p>
<i>Thank you! &mdash; Thierry Volpiatto</i>
</p>

***

[![](https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&color=%23fe8e86)](https://github.com/sponsors/emacs-helm)


[Support via Patreon](https://www.patreon.com/user?u=86324343)

***

***

<p align="center">
  <a href="https://github.com/emacs-helm/helm/wiki"><b>Helm wiki</b></a> |
  <a href="https://github.com/emacs-helm/helm/wiki/FAQ"><b>FAQ</b></a>
</p>

***
<!-- markdown-toc start -->
**Table of Contents**

- [Introduction](#introduction)
- [Screenshots](#screenshots)
- [Features](#features)
- [Requirements](#requirements)
- [Getting Started](#getting-started)
    - [Install from Straight](#install-from-straight)
    - [Quick install from git](#quick-install-from-git)
    - [Install from Emacs packaging system](#install-from-emacs-packaging-system)
    - [Installing just the helm-core package](#installing-just-the-helm-core-package)
    - [Warning about alternate installation methods](#warning-about-alternate-installation-methods)
    - [Configuration](#configuration)
        - [Minimal helm configuration: ](#minimal-helm-configuration)
        - [Extended configuration](#extended-configuration)
    - [General recommandations](#general-recommandations)
    - [Basic usage](#basic-usage)
        - [Windows and frames configuration](#windows-and-frames-configuration)
        - [Matching methods](#matching-methods)
        - [Display icons with all-the-icons package](#display-icons-with-all-the-icons-package)
        - [Have brief infos on the selected candidate](#have-brief-infos-on-the-selected-candidate)
        - [Creating custom helm sources](#creating-custom-helm-sources)
- [Helm Applications](#helm-applications)
- [Recommended Helm extensions](#recommended-helm-extensions)
- [Other emacs extensions recommended with helm](#other-emacs-extensions-recommended-with-helm)
- [External programs recommended with helm](#external-programs-recommended-with-helm)
- [Known issues](#known-issues)
- [Contributors](#contributors)
- [Bugs & Improvements](#bugs--improvements)
- [Getting help](#getting-help)

<!-- markdown-toc end -->

# Introduction

`Helm` is an Emacs framework for incremental completions and narrowing
selections. It helps to rapidly complete file names, buffer names, or
any other Emacs interactions requiring selecting an item from a list of
possible choices.

Helm is a fork of `anything.el`, which was originally written by Tamas
Patrovic and can be considered to be its successor. `Helm` cleans the
legacy code that is leaner, modular, and unchained from constraints of
backward compatibility.

# Screenshots

![helm-grep-ag](images/helm-grep-ag-persistent.png)

![Screenshot1](images/screenshot001.png)

![Screenshot2](images/screenshot002.png)

![Screenshot3](images/screenshot003.png)

![Screenshot4](images/screenshot004.png)

![Screenshot5](images/screenshot005.png)

![Screenshot6](images/screenshot006.png)

![Screenshot7](images/screenshot007.png)

# Features

In addition of its framework where you can build your own `Helm`
applications, `Helm` provides preconfigured commands to browse and
search incrementally in files, buffers, bookmarks etc... and much
more.  Helm allows displaying different sources in same session.  Helm
displays its candidates in a window or a frame, keeping the minibuffer for
user input.  Helm provides a full set of actions for each of its
sources, each action apply on a single candidate or a set of marked
candidates.

_Note:_ You will find many helm extensions providing diverse features sometimes already implemented in `Helm`,
prefer generally the ones that are natively in `Helm`, e.g. `Helm` support natively most grep implementations as backend (ack, ag, rg),
no need to install 3rd party packages for this, same for managing projects etc...

# Requirements

Helm requires Emacs-25.1 or later versions.

Helm installs [async](https://github.com/jwiegley/emacs-async) and [wfnames](https://github.com/thierryvolpiatto/wfnames) package as dependencies
when Helm is installed using [MELPA](https://melpa.org/).

It is recommended to install [all-the-icons](https://github.com/domtronn/all-the-icons.el) to have icons in several places (files, buffers etc..) even if it is not mandatory.

# Getting Started

## Install from Straight

See [Install from straight package manager](https://github.com/emacs-helm/helm/wiki#from-straight-package-manager)

## Quick install from git

See [Install Helm from Git](https://github.com/emacs-helm/helm/wiki#from-source)

## Install from Emacs packaging system

See [Install Helm from Melpa](https://github.com/emacs-helm/helm/wiki#from-melpa)

## Installing just the helm-core package

`helm-core` package is available on [MELPA](https://melpa.org/) for
third party packages that depend on helm libraries. These packages
should require helm as follows:

     (require 'helm)

Requiring helm builds and runs helm code necessary for multiple regexp
and fuzzy matching. See
[helm wiki](https://github.com/emacs-helm/helm/wiki#developpingusinghelmframework)
for details.

## Warning about alternate installation methods

Installation methods that circumvent `helm-config` are known to fail
if the careful safeguards are not implemented in the hacks.

## Configuration

Helm is fully configurable with dozen of user variables and for more advanced users,
even some helm sources are configurable throught `cl-defmethod` and `helm-setup-user-source`.

### Minimal helm configuration:

```elisp
(require 'helm)
(helm-mode 1) ; facultative.
```

### Extended configuration
The full configuration I (the helm maintainer) use is
[here](https://github.com/thierryvolpiatto/emacs-config/blob/master/init-helm.el).

Also see helm customizable variables with the customize interface.
You have access to the specific custom-variables for a source with `C-h c` while running Helm.

Enabling `helm-mode` will enable helm for many features of emacs
requiring completions, see below how to enable `helm-mode`.

## General recommandations

- When you have problems like Helm beeing slow or something not working, always fallback to default settings.

- Configure Helm yourself, don't use preconfigured Emacs that configure wrongly Helm for you (e.g. Spacemacs).

- Refrain installing all Helm packages you find in Melpa, check first if the feature you are looking for is available in Helm.

- Nowaday, there is dozen of completion UI available for Emacs, refrain using many of those in addition with Helm unless you know what you are doing.


## Basic usage

Helm use by default a prefix key (`C-x c`) which is not very convenient but allow users to discover
most important helm commands, once you find them you can rebind to a convenient key of your choice,
here some of them:

`C-x c M-x helm-` lists helm commands ready for narrowing and selecting with `helm-M-x`.

To bind to `M-x`:

`(global-set-key (kbd "M-x") 'helm-M-x)`

`C-x c C-x C-f` browses files on your system.

To bind it to `C-c C-f`

`(global-set-key (kbd "C-c C-f") 'helm-find-files)`

To discover such commands more easily, you can install [helm-descbinds](https://github.com/emacs-helm/helm-descbinds).
Once installed and `helm-descbinds-mode` enabled, you can do `C-x c C-h` to see all commands starting with helm prefix key.

- _IMPORTANT:_

In any helm session,

`C-h m` pops an org buffer with detailed documentation about current command and more generalized infos about helm.

Use it with no moderation!

Use these embedded Info screens first before reporting bugs.

`M-x helm-mode` to enable helm completion for common Emacs
commands. Note that the helm functionality enabled through helm-mode
comes from a generic implementation and does not include all helm
features available through equivalent helm-specific commands.
See [FAQ](https://github.com/emacs-helm/helm/wiki/FAQ#why-after-enabling-helm-mode-m-x-and-c-x-c-f-are-not-helmized)
about `M-x` and `C-x C-f`.

To make helm-mode starts with Emacs init file:

```elisp
(helm-mode 1)
```
To make `helm-mode` nicer you can set the variable `completions-detailed` to non nil (emacs-28+),
with emacs-27 use `helm-completions-detailed`.

`helm-mode` is fully configurable throught the variables
`helm-completing-read-handlers-alist`, `helm-completion-style`,
`helm-completion-styles-alist`.

_NOTE_: `helm-mode` support Emacs `completion-styles` for matching candidates
when `helm-completion-style` is set to `emacs`, it is not the default because
it is slower than the `helm` style used by default.

You can also discover basic helm commands with helm menu items in Emacs menu.

Another way to discover helm commands: run the shell script:
`./emacs-helm.sh` from helm directory and then look in the scratch
buffer. `emacs-helm.sh` accepts emacs command line
options. `emacs-helm.sh -h` opens an Info screen with more details.

_Note:_ When helm is installed with "make install" you will have a
shell command named helm that you can run from any places i.e. not
only from the helm directory, you can do this yourself if you have
installed from package by making a symlink of emacs-helm.sh in your
load-path e.g. "~/bin".

See https://github.com/emacs-helm/helm/wiki#quick-try-with-emacs-helmsh

### Windows and frames configuration

Helm allows displaying its candidates in several ways, side windows, frames, one window etc...
You will find the documentation with `C-h m` as usual or on wiki, use as well `C-h c` to see the relevant variables you can set.
Here helm started from emacs-helm.sh script and displaying its candidates in a separate frame:

![Helm displayed in frame](images/helm-displayed-in-a-separate-frame.gif)

To use frames to display helm candidates see the variable `helm-display-function`.

_NOTE_: A Helm package called `helm-posframe` exists which use itself the `posframe` package,
please DO NOT use this to display helm completions in frame, it uses child frames internally
which is the wrong approach for Helm.

### Matching methods

Helm support by default multi pattern matching, it is the standard way
of matching in helm.
E.g You can use a pattern like "foo bar" to match a line containing "foo" and "bar"
or "bar" and "foo".
Each pattern can be a regexp.

In addition helm support [fuzzy matching](https://github.com/emacs-helm/helm/wiki/Fuzzy-matching).

### Display icons with all-the-icons package

You can have icons in many places, see the corresponding variables or modes where you can have them.
For example you can have icons in `helm-find-files` with M-x `helm-ff-icon-mode`
once you have properly installed and configured `all-the-icons` package.

### Have brief infos on the selected candidate

For this turn on `helm-popup-tip-mode`, you will have a popup at end of line showing
detailed infos on candidate (only available in some sources).

### Creating custom helm sources

An example:

```elisp

(helm :sources (helm-build-sync-source "test"
                 :candidates '(foo foa fob bar baz)
                 :fuzzy-match t)
      :buffer "*helm test*")
```

The candidates list may be replaced by a function that produces a list.
See ([helm wiki](https://github.com/emacs-helm/helm/wiki#25-developing-using-helm-framework))
for details.

# Helm Applications

Here are some popular applications developed using helm completion and
narrowing framework and available in `Helm` package.
This list is not exhaustive.

- `helm-mode`: turns on helm completions for most standard emacs
  completions. Helm provides even more optimized helm completions for
  some commands in helm-mode. Prefer these natively optimized versions
  over the ones in helm-mode.

- `helm-find-files`: one command that handles all the files related
  commands (bind to `C-x C-f`).

- `helm-buffers-list`: provides enhanced buffers listing.

- `helm-browse-project`: handles project files and buffers; defaults
   to current directory; works with `helm-find-files`; recommended
   with [helm-ls-git](https://github.com/emacs-helm/helm-ls-git),
   [helm-ls-hg](https://github.com/emacs-helm/helm-ls-hg) and
   `helm-ls-svn` for a better handling of version control files.
   Each time a project under version control is visited it is added
   to `helm-browse-project-history` and can be visted with `helm-projects-history`.

- `helm-dabbrev`: enhanced dabbrev implementation with helm
  completion; does not use emacs code.

- `helm-occur`: enhanced occur for one or more buffers; launch from
  `helm-buffers-list` or `current-buffer`.

- `helm-M-x`: enhanced `execute-extended-command` (bind it to `M-x`).

- `helm-imenu` and `helm-imenu-in-all-buffers`: provide imenus for
  current or all buffers.

- `helm-etags-select`: enhanced etags with helm-completion; usable
  everywhere with `helm-find-files`.

- `helm-apropos`: enhanced apropos for functions and variables that
  `C-h` commands provide.

- Helm `Grep`, `Ack-grep`, `Ag`, `Rg`: provided as actions in all helm
  commands for file; Support back-ends `grep`, `ack-grep`, `git-grep`,
  `ag`,`rg` and probably others.  You will find several grep actions
  in the action menu of `helm-find-files`.  You can launch as well
  from your current buffer on `default-directory` `helm-do-grep-ag`,
  `helm-grep-do-git-grep`.
  NOTE: helm-*-ag functions and command use
  "ag" name for historical reason but support as well "rg".

- `helm-gid`: Helm interface for `gid` from
  [id-utils](https://www.gnu.org/software/idutils/).

- `helm-show-kill-ring`: A helm browser for kill ring.

- `helm-all-mark-rings`: A helm browser for mark ring; retrieves last positions in buffers.

- `helm-filtered-bookmarks`: enhanced browser for bookmarks.

- `helm-packages`: enhanced browser for elisp package management.

# Recommended Helm extensions

Normally all Helm modules coming from [Emacs-helm](https://github.com/emacs-helm) organisation
are safe to use otherwise it is mentioned in their README if they are broken or unmaintained.
Here some of them I am using:

- [helm-ls-git](https://github.com/emacs-helm/helm-ls-git)
- [helm-addressbook](https://github.com/emacs-helm/helm-addressbook)
- [helm-dictionary](https://github.com/emacs-helm/helm-dictionary)
- [helm-mu](https://github.com/emacs-helm/helm-mu)
- [helm-slime](https://github.com/emacs-helm/helm-slime)
- [helm-descbinds](https://github.com/emacs-helm/helm-descbinds)
- [helm-firefox](https://github.com/emacs-helm/helm-firefox)
- [helm-w3m](https://github.com/emacs-helm/helm-w3m)
- [helm-emms](https://github.com/emacs-helm/helm-emms)
- [helm-apt](https://github.com/emacs-helm/helm-apt)
- [helm-org](https://github.com/emacs-helm/helm-org)
- [helm-bm](https://github.com/emacs-helm/helm-bm)
- [helm-wikipedia](https://github.com/emacs-helm/helm-wikipedia)

**Warning** Helm development has sparked quite a few extensions, many
of which duplicate features already included in helm. Some of these
packages (about 20 at last count in the MELPA repository) are either
deprecated or unmaintained. Moreover, many remain out-of-sync with
`helm` core development cycles causing incompatibilities. To avoid
helm problems or unstable emacs, please look for comparable features
within [helm](https://github.com/emacs-helm/helm) and
[emacs-helm](https://github.com/emacs-helm) before installing such
extensions e.g. helm-swoop which is badly written and unmaintained vs helm-occur which is part of Helm.

# Other emacs extensions recommended with helm

- [Emacs-wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
- [all-the-icons](https://github.com/domtronn/all-the-icons.el)
- [svg-lib](https://github.com/rougier/svg-lib)

svg-lib allows using a svg progress-bar for Rsync in helm-find-files, see the variable `helm-rsync-progress-bar-function`.

# External programs recommended with helm

- [Ripgrep](https://github.com/BurntSushi/ripgrep)

# Known issues

The Helm project has a current unresolved
[issue list](https://github.com/emacs-helm/helm/issues?sort=created&direction=desc&state=open).
Please feel free to fix any of them; send a pull request.

Most issues come from a wrong configuration or a use of Helm modified externally by third party package e.g. Spacemacs.
Some other Helm packages that are no more maintained (or badly maintained) and use deprecated Helm code will clash also with recent Helm,
be aware!

# Contributors

The Helm project maintains a
[list](https://github.com/emacs-helm/helm/contributors) of
contributors.

# Bugs & Improvements

The Helm Team welcomes bug reports and suggestions. Note that not all
bugs when using Helm are due to Helm. Because of the way Helm
interacts with many Emacs features, bugs may be related to Emacs
itself.

One way to ascertain that the bugs are helm-related, recreate the
error either by using `Emacs -Q` or by running the included package
script `./emacs-helm.sh` located in the helm directory.  You can now
also run M-x helm-packages and from the installed packages source run
the "Isolate packages" action after having selected helm, this of
course if you have installed Helm from package.

If you want a specific feature not already implemented in Helm, fill a feature request
report, I will be happy to implement this for you if possible.

Helm comes now with a template for filling bugs, when reporting issues,
be sure to fill all sections and to run helm from a minimal install as
described above to reproduce your bug.

_NOTE_: Please don't cheat when checking "I use a minimal configuration",
you will make me loosing my time and your time to try to find what is wrong.

# Getting help

[Helm Wiki](https://github.com/emacs-helm/helm/wiki)

Or ask directly on [Helm discussions](https://github.com/emacs-helm/helm/discussions),
I will be happy to answer your questions or help you to configure Helm or more generally Emacs.

Do not expect the right answer when you ask on Reddit or similar Forums.

Cheers,<br>
The Helm Team

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
