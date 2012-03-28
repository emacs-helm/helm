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

## Getting Started

### Quick install

  1. Clone the `helm` repository to some directory:
  
    ```elisp
    $ git clone https://github.com/emacs-helm/helm.git /path/to/helm/directory
    ```
  
  2. Run make from this directory.
  3. Add to `.emacs.el` (or equivalent):

    ```elisp
    (add-to-list 'load-path "/path/to/helm/directory")
    (require 'helm-config)
    ```

**Note to Linux Distributions Maintainers**

`Only the extensions present in the github emacs-helm organisation are supported.`

### Emacs Prelude

If you're afraid to play with Emacs's configuration, but want to try
out Helm - have to fear. Have a look at
[Emacs Prelude](https://github.com/bbatsov/emacs-prelude) - it has
Helm built-in and properly set-up.

### Basic usage

Just type `M-x helm-mini` and enjoy. You might want to bind that command to
a keyboard shortcut. Here's a suggestion:

```elisp
(global-set-key (kbd "C-c h") 'helm-mini)
```
You can also start with `M-x helm-mode` and enjoy helm completion in your favourites
Emacs commands (e.g `M-x', `C-x C-f', etc...).
You can enable this by adding in your init file:

```elisp
(helm-mode 1)
```

As a startup point you can also look at the helm section in Emacs menu to
discover some of the commands provided by helm.

### Advanced usage

Helm is capable of a lot. For now you can find all the gory details
only in the
[pdf manual](https://github.com/emacs-helm/helm/raw/master/doc/helm.pdf). Soon
we'll provide supplementary documentation.

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
welcome. GitHub pull requests are even better! :-)

### Send Bug Report from Emacs

If you have problems, send a bug report via `C-c C-x C-b` in helm session (best)
or `M-x helm-send-bug-report` outside helm session.
We implemented this bug report feature because we want to know your current state.
It helps usa solve problems easily.
The steps are:

  1. Setup mail in Emacs, the easiest way is:
  
    ```elisp
    (setq user-mail-address "your@mail.address")
    (setq user-full-name "Your Full Name")
    (setq smtpmail-smtp-server "your.smtp.server.jp")
    (setq mail-user-agent 'message-user-agent)
    (setq message-send-mail-function 'message-smtpmail-send-it)
    ```
  
  2. Be sure to use the **LATEST** version of `helm.el`.
  3. Enable debugger. `M-x toggle-debug-on-error` or `(setq debug-on-error t)`
  4. Use Lisp version instead of compiled one: `(load "helm.el")`
  5. Do it!
  6. If you got an error, please do not close _*Backtrace*_ buffer.
  7. Type `C-c C-x C-b` (helm session, best!)
     or `M-x helm-send-bug-report` (outside) and
     then `M-x insert-buffer` *Backtrace* (if you got error)
  8. Describe the bug using a precise recipe.
  9. Type `C-c C-c` to send.

Cheers,<br>
The Helm Team

