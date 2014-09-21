```
 _____                            _   _      _
| ____|_ __ ___   __ _  ___ ___  | | | | ___| |_ __ ___
|  _| | '_ ` _ \ / _` |/ __/ __| | |_| |/ _ \ | '_ ` _ \
| |___| | | | | | (_| | (__\__ \ |  _  |  __/ | | | | | |
|_____|_| |_| |_|\__,_|\___|___/ |_| |_|\___|_|_| |_| |_|
```

## Abstract

`Helm` is incremental completion and selection narrowing framework for
Emacs. It will help steer you in the right direction when you're looking
for stuff in Emacs (like buffers, files, etc).

Helm is a fork of `anything.el` originaly written by Tamas Patrovic
and can be considered to be its successor. 
`Helm` sets out to clean up the legacy code in `anything.el`
and provide a cleaner, leaner and more modular tool, that's not tied in
the trap of backward compatibility. 

## Requirements

You need a recent Emacs to use latest helm, at least Emacs-24.3.

[async](https://github.com/jwiegley/emacs-async) will be installed as dependency
when installing from melpa but is facultative when installing from git (recommended though
as it may fix installation of all packages from (m)elpa and will allow you to
copy/rename asynchronously your files from helm and/or dired if needed).

## Getting Started

### Quick install from git

  1. Clone the `helm` repository to some directory:
  
    ```elisp
    $ git clone https://github.com/emacs-helm/helm.git /path/to/helm/directory
    ```

  2. Clone the `async` repository to some directory (facultative)

    ```elisp
    $ git clone git@github.com:jwiegley/emacs-async.git /path/to/async/directory
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

### Install from Emacs packaging system

Helm is now available on Melpa at `http://melpa.milkbox.net/`
You will find there instructions to install.
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

### Alternate install warning

Some people are installing `helm` with their own config using diverses `require`, `autoload`
and other hacks, not using `helm-config`.
Expect failures and slowdown at startup unless you really know what you are doing when you do so.

### Emacs Prelude

If you're afraid to play with Emacs's configuration, but want to try
out Helm - have NO FEAR. Have a look at
[Emacs Prelude](https://github.com/bbatsov/prelude) - it has
Helm built-in and properly set-up.

### Basic usage

Just type `M-x helm-mini` and enjoy. You might want to bind that command to
a keyboard shortcut. Here's a suggestion:

```elisp
(global-set-key (kbd "C-c h") 'helm-mini)
```
You can also start with `M-x helm-mode` and enjoy helm completion in your favourites
Emacs commands (e.g `M-x`, `C-x C-f`, etc...).
You can enable this by adding in your init file:

```elisp
(helm-mode 1)
```

As a startup point you can also look at the helm section in Emacs menu to
discover some of the commands provided by helm.

### Advanced usage

Helm is capable of a lot. Here is a demo of `helm-buffers-list`:

![helm-buffers-list](doc/helm-buffers-list.gif)

The demo starts when you see `Eval: START` in the minibuffer.

- All the C buffers are selected using the regexp `*C`. In the demo, I also select Tcl buffers with `*Tcl` and then switched back to C buffers with `*C`.
- I only want to have buffers that contains only the string "crash". To do that, I add a space, then add the pattern `@crash`.
- After the initial search pattern, I hand over the current matching buffers to `helm-moccur` - `moccur` with Helm interface. In the above demo, I only switch to one file, that is `kexec.c`. However, you can select multiple buffers with `C-SPC` or select all buffers with `M-a`.
- Candidates can be filtered gradually by adding more pattern, i.e. I added `memory` to filtered down to buffers that contain the string "memory" among the buffers that are containing "crash".

As you can see, as you filtered out, the number of candidates decreases, as displayed in the modeline. At the end, there were 12 buffers remained as the result of filtering, down from the total 253 buffers.

You can read [this guide](http://tuhdo.github.io/helm-intro.html) to quickly get started with Helm.

You can find all the gory details on the [Helm Wiki](https://github.com/emacs-helm/helm/wiki).

## Known issues

Check out the project's
[issue list](https://github.com/emacs-helm/helm/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and sent us a pull request. :-)

## Contributors

Here's a [list](https://github.com/emacs-helm/helm/contributors) of all the people who have contributed to the
development of Helm.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome, be sure though they are related to helm, many bugs are coming from emacs itself
or other packages. GitHub pull requests are even better! :-)

NOTE: When trying if something is working or not, be sure to start helm from `Emacs -Q` or even better
Start it from your helm directory with `./emacs-helm.sh`.

## Getting help

If [Helm Wiki](https://github.com/emacs-helm/helm/wiki) is not enough, you can ask for help
on [emacs-helm google group](https://groups.google.com/group/emacs-helm?hl=en).


Cheers,<br>
The Helm Team

