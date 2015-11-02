<p><a href="http://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3" /></a>
<a href="http://melpa.org/#/helm"><img src="http://melpa.org/packages/helm-badge.svg" alt="MELPA" title="" /></a>
<a href="http://stable.melpa.org/#/helm"><img src="http://stable.melpa.org/packages/helm-badge.svg" alt="MELPA Stable" title="" /></a></p>

<h1>Emacs-helm</h1>

<p><img src="https://avatars3.githubusercontent.com/u/1541688?v=3&amp;s=200" alt="Emacs-helm" title="" /></p>

<p>You can <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&amp;business=thierry.volpiatto@gmail.com&amp;lc=US&amp;currency_code=EUR&amp;bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted"><img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate" title="" /></a> to help this project.</p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
- [Requirements](#requirements)
- [Getting Started](#getting-started)
    - [Quick install from git](#quick-install-from-git)
    - [Install from Emacs packaging system](#install-from-emacs-packaging-system)
    - [Install and use only helm-core package](#install-and-use-only-helm-core-package)
    - [Alternate install warning](#alternate-install-warning)
    - [Configuration](#configuration)
    - [Basic usage](#basic-usage)
    - [Advanced usage](#advanced-usage)
        - [Create your own helm source (overview)](#create-your-own-helm-source-overview)
        - [Fuzzy matching](#fuzzy-matching)
        - [Autoresize](#autoresize)
- [Features](#features)
    - [Applications (not exhaustive)](#applications-not-exhaustive)
- [Helm extensions you should install](#helm-extensions-you-should-install)
- [Known issues](#known-issues)
- [Contributors](#contributors)
- [Bugs & Improvements](#bugs--improvements)
- [Getting help](#getting-help)

<!-- markdown-toc end -->

# Introduction

`Helm` is incremental completion and selection narrowing framework for
Emacs. It will help steer you in the right direction when you're looking
for stuff in Emacs (like buffers, files, etc).

Helm is a fork of `anything.el` originally written by Tamas Patrovic
and can be considered to be its successor. 
`Helm` sets out to clean up the legacy code in `anything.el`
and provide a cleaner, leaner and more modular tool, that's not tied in
the trap of backward compatibility. 

# Requirements

You need a recent Emacs to use latest helm, at least Emacs-24.3.

[async](https://github.com/jwiegley/emacs-async) will be installed as dependency
when installing from melpa but is facultative when installing from git (recommended though
as it may fix installation of all packages from (m)elpa and will allow you to
copy/rename asynchronously your files from helm and/or dired if needed).

# Getting Started

## Quick install from git

  1. Clone the `helm` repository to some directory:
  
    ```elisp
    $ git clone https://github.com/emacs-helm/helm.git /path/to/helm/directory
    ```

  2. Clone the `async` repository to some directory (facultative)

    ```elisp
    $ git clone https://github.com/jwiegley/emacs-async.git /path/to/async/directory
    ```
  3. Run `make` from the `helm` directory.
  
  3. Add to `.emacs.el` (or equivalent):

    ```elisp
    ;; [Facultative] Only if you have installed async.
    (add-to-list 'load-path "/path/to/async/directory")
    
    (add-to-list 'load-path "/path/to/helm/directory")
    (require 'helm-config)
    ```
    
_NOTE:_ Installing helm like this (i.e from git+make) is the safest way.

You can have a quick try to `helm` by launching from the helm directory:

`./emacs-helm.sh`

It is also recommended to use this when reporting bug.

_NOTE:_ That this will not work on Windows systems.

## Install from Emacs packaging system

Helm is now available on Melpa at http://melpa.org/
You will find there instructions to install.
See also https://github.com/milkypostman/melpa#usage to startup correctly
with the emacs packaging system.
Then you should need only in your init file:

```elisp
(require 'helm-config)
```

_WARNING:_ Due to a bad concept of package.el which is in charge of fetching helm files
and compiling them, users had errors most of the time when upgrading from melpa and `list-package`.
To avoid this [Async](https://github.com/jwiegley/emacs-async) have been added as dependency to
helm to force package.el compiling its files in a clean environment.
People installing from git and using the make file will not suffer from this problem and don't need
[Async](https://github.com/jwiegley/emacs-async) though it is recommended as it fix installation
of all other packages you may install with package.el from (m)elpa.
See [FAQ](https://github.com/emacs-helm/helm/wiki#faq) for more infos.

_Note:_ After upgrading from the emacs packaging system you should restart emacs for the changes take effect.

**Note to Linux Distributions Maintainers**

`Only the extensions present in the github emacs-helm organisation are supported.`

## Install and use only helm-core package

Third party helm packages can use only helm-core package if they don't need more helm libraries
for their packages. It is available at http://melpa.org/.

All you need to add in your packages is

     (require 'helm)

This will provide the necessary code to build and run helm sources with multiple regexp matching
or fuzzy matching.

See [wiki](https://github.com/emacs-helm/helm/wiki#developpingusinghelmframework) for more infos.

## Alternate install warning

Some people are installing `helm` with their own config using diverses `require`, `autoload`
and other hacks, not using `helm-config`.
Expect failures and slowdown at startup unless you really know what you are doing when you do so.

## Configuration

For a minimal helm configuration, run the startup script `./emacs-helm.sh`
and look at the file `/tmp/helm-cfg.el`.

The full configuration I use (helm maintainer) can be found [here](https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el).

Don't hesitate also to visit all helm customizable variables with the customize interface.
Enabling `helm-mode` will give you completion in the diverse customize commands.

Also you will find some packages like [Emacs Prelude](https://github.com/bbatsov/prelude) that
have Helm built-in and properly set-up.

## Basic usage

Just type `M-x helm-M-x RET helm-`, you will have completion on all helm commands.

You can bind this to `M-x` like this:

`(global-set-key (kbd "M-x") 'helm-M-x)`

- _IMPORTANT:_

Once you are in the helm session (of `helm-M-x` or any one else) you can hit either `C-h m` or
`C-c ?`, the former is will popup a general info buffer about helm while the second will
popup a specialized info of the current source you are into.
Sometime `C-c ?` is not available, in this case you will see in mode-line `C-h m` instead of `C-c ?`.
PLEASE USE and ABUSE of these `helm` embeded infos before reporting a bug about how to do things
in `helm`, you will find also useful infos in mode-line.

You can also start with `M-x helm-mode` and enjoy helm completion in your favourites
Emacs commands (e.g `M-x`, `C-x C-f`, etc...).
You can enable this by adding in your init file:

```elisp
(helm-mode 1)
```

- _NOTE_ that the helmized emacs commands are different and much more basic than the helm ones.

As a startup point you can also look at the helm section in Emacs menu to
discover some of the commands provided by helm.

For those who have a system able to run shell scripts, a convenient way to discover helm
is to run `./emacs-helm.sh` from the helm directory, you will find interesting infos in
your scratch buffer.
`emacs-helm.sh` accept all emacs command line options, see `emacs-helm.sh -h` for more
infos.

## Advanced usage

Helm is capable of a lot. Here is a demo of `helm-buffers-list` used with `helm-moccur`:

![helm-buffers-list](doc/helm-buffers-list.gif)

The demo starts when you see `Eval: START` in the minibuffer.

- All the C buffers are selected using the regexp `*C`. In the demo, I also select Tcl buffers with `*Tcl` and then switched back to C buffers with `*C`.
- I only want to have buffers that contains only the string "crash". To do that, I add a space, then add the pattern `@crash`.
- After the initial search pattern, I hand over the current matching buffers to `helm-moccur` - `moccur` with Helm interface. In the above demo, I only switch to one file, that is `kexec.c`. However, you can select multiple buffers with `C-SPC` or select all buffers with `M-a`.
- Candidates can be filtered gradually by adding more pattern, i.e. I added `memory` to filtered down to buffers that contain the string "memory" among the buffers that are containing "crash".

As you can see, as you filtered out, the number of candidates decreases, as displayed in the modeline. At the end, there were 12 buffers remained as the result of filtering, down from the total 253 buffers.

You can read [this guide](http://tuhdo.github.io/helm-intro.html) to quickly get started with Helm.

You can find all the gory details on the [Helm Wiki](https://github.com/emacs-helm/helm/wiki).

### Create your own helm source (overview)

Here a quick example using sync method with candidates generated by a simple list, a function can be used instead of course ([see helm wiki for more infos](https://github.com/emacs-helm/helm/wiki#25-developping-using-helm-framework)).


```elisp

(helm :sources (helm-build-sync-source "test"
                 :candidates '(foo foa fob bar baz)
                 :fuzzy-match t)
      :buffer "*helm test*")

```

### Fuzzy matching

Helm has a built-in fuzzy matcher that is activated for some commands. Fuzzy matching is disabled by default. Currently these commands supports fuzzy matching:

- `helm-recentf`: Enable by setting `helm-recentf-fuzzy-match` to `t`.
- `helm-mini`: Enable by setting `helm-buffers-fuzzy-matching` and `helm-recentf-fuzzy-match` to `t`.
- `helm-buffers-list`: Enable by setting `helm-buffers-fuzzy-matching` to `t`.
- `helm-find-files`: Enable by default.
- `helm-locate`: Enable by setting `helm-locate-fuzzy-match` to `t`.
- `helm-M-x`: Enable by setting `helm-M-x-fuzzy-match` to `t`.
- `helm-semantic`: Enable by setting `helm-semantic-fuzzy-match` to `t`.
- `helm-imenu`: Enable by setting `helm-imenu-fuzzy-match` to `t`.
- `helm-apropos`: Enable by setting `helm-apropos-fuzzy-match` to `t`.
- `helm-lisp-completion-at-point`: Enable by setting `helm-lisp-fuzzy-completion` to `t`.

You can also enable fuzzy matching globally in all functions helmized by `helm-mode` with `helm-mode-fuzzy-match`
and `helm-completion-in-region-fuzzy-match`.

**IMPORTANT**: To make fuzzy-matching fast, you must not set `helm-candidate-number-limit` too high. It is recommended that you leave the variable with its default value 100. The higher you set `helm-candidate-number-limit`, the slower fuzzy-matching will be.

### Autoresize

Helm can now resize according to the number of candidates with `helm-autoresize-mode`:

    (helm-autoresize-mode 1)

You can customize the minimum and maximum height that Helm can resize with these two variables:

- `helm-autoresize-max-height`
- `helm-autoresize-min-height`

By default, `helm-autoresize-max-height` is set to 40, which makes Helm candidate buffer has the maximum height of 40% of current frame height. Similarly, `helm-autoresize-min-height` specifies the minimum height that Helm candidate buffer cannot be smaller.

If you don't want the Helm window to be resized, but a smaller Helm window, you can set `helm-autoresize-min-height` equal to `helm-autoresize-max-height`.

# Features

## Applications (not exhaustive)

- `helm-mode`: Allow turning on helm in all completions provided by emacs, when available you should use instead the same feature provided natively by helm.
- `helm-find-files`: Replace in one command all the files related commands (Bind it to `C-x C-f`!).
- `helm-buffers-list`: Enhanced buffers listing.
- `helm-browse-project`: Show all buffers and files related to project or current directory (Usable everywhere with `helm-find-files`) you will want to install
[helm-ls-git](https://github.com/emacs-helm/helm-ls-git), [helm-ls-hg](https://github.com/emacs-helm/helm-ls-hg) and `helm-ls-svn` for a better experience.
- `helm-dabbrev`: Enhanced dabbrev with helm completion (Own implementation of dabbrev for helm, don't reuse emacs code).
- `helm-moccur`: Enhanced occur for one or more buffers, launch it from `helm-buffers-list` or `current-buffer`(Own implementation).
- `helm-M-x`: Enhanced version of `execute-extended-command` (Bind it to `M-x`!).
- `helm-imenu` and `helm-imenu-in-all-buffers`: Imenu in `current-buffer` or in all your buffers.
- `helm-etags-select`: Enhanced version of etags with helm-completion (Usable everywhere with `helm-find-files`).
- `helm-apropos`: Description of functions, variables, etc... Use it instead of Most `C-h` commands.
- `Grep`: You can launch it (recursively or not) from any files related helm commands, support as backends `grep`, `ack-grep`, `git-grep`,
`ag` and `pt` (Own implementation).
- `helm-gid`: Helm interface to `gid` from [id-utils](https://www.gnu.org/software/idutils/).
- `helm-show-kill-ring`: A kill ring browser for helm.
- `helm-all-mark-rings`: A mark ring for helm, allow retrieving your last position(s) in a buffer.
- `helm-filtered-bookmarks`: An enhanced bookmark listing.
- `helm-list-elisp-packages`: Manage emacs packages with helm.

# Helm extensions you should install

- [helm-ls-git](https://github.com/emacs-helm/helm-ls-git)
- [helm-ls-hg](https://github.com/emacs-helm/helm-ls-hg)
- [helm-descbinds](https://github.com/emacs-helm/helm-descbinds)
- [helm-firefox](https://github.com/emacs-helm/helm-firefox)

**Warning** You will find many extensions outside of [emacs-helm](https://github.com/emacs-helm)
that provide nothing more than what is provided natively by [helm](https://github.com/emacs-helm/helm) and may not be in sync will `helm` core.
So generally prefer what is provided natively in `helm` instead of its counterpart provided externally.
Actually more than 20 unuseful or deprecated packages you can find on MELPA, be aware.

# Known issues

Check out the project's
[issue list](https://github.com/emacs-helm/helm/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send us a pull request. :-)

# Contributors

Here's a [list](https://github.com/emacs-helm/helm/contributors) of all the people who have contributed to the
development of Helm.

# Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome, be sure though they are related to helm, many bugs are coming from emacs itself
or other packages. GitHub pull requests are even better! :-)

NOTE: When trying if something is working or not, be sure to start helm from `Emacs -Q` or even better
Start it from your helm directory with `./emacs-helm.sh`.

# Getting help

If [Helm Wiki](https://github.com/emacs-helm/helm/wiki) is not enough, you can ask for help
on [emacs-helm google group](https://groups.google.com/group/emacs-helm?hl=en).


Cheers,<br>
The Helm Team

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
