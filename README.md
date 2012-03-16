```
 _____                            _   _      _
| ____|_ __ ___   __ _  ___ ___  | | | | ___| |_ __ ___
|  _| | '_ ` _ \ / _` |/ __/ __| | |_| |/ _ \ | '_ ` _ \
| |___| | | | | | (_| | (__\__ \ |  _  |  __/ | | | | | |
|_____|_| |_| |_|\__,_|\___|___/ |_| |_|\___|_|_| |_| |_|
```

## Abstract

`Helm` is incremental search and narrowing selection framework for
Emacs. It will help steer in the right direction when you're looking
for stuff in Emacs (like buffers, files, etc).

Helm is a fork of `anything.el` and can be considered to be its
successor. `Helm` sets out to clean up the legacy code in `anything.el`
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

### Manual install

  1. Clone the `helm` repository to some directory:
  
    ```elisp
    $ git clone https://github.com/emacs-helm/helm.git /path/to/helm/directory
    ```

  2. Put following files somewhere in your `load-path`:
     - `helm.el`
     - `helm-config.el`
     - `helm-match-plugin.el`

  3. Byte-compile these files.
  4. Add this code to `.emacs.el`:

  ```elisp
  (require 'helm-config)
  ```

**Note to Linux Distribution (e.g. Debian) Maintainers**

`Use only the files mentioned above in the **Manual install** section
for your packages, other files found on emacswiki or elsewhere are not
supported.`

### Emacs Prelude

If you're afraid to play with Emacs's configuration, but want to try
out Helm - have to fear. Have a look at
[Emacs Prelude](https://github.com/bbatsov/emacs-prelude) - it has
Helm built-in and properly set-up.

### Basic usage

Just type `M-x helm` and enjoy. You might want to bind that command to
a keyboard shortcut. Here's a suggestion:

```elisp
(global-set-key (kbd "C-c h") 'helm)
```

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

Cheers,<br>
The Helm Team

