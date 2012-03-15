```
 _____                            _   _      _
| ____|_ __ ___   __ _  ___ ___  | | | | ___| |_ __ ___
|  _| | '_ ` _ \ / _` |/ __/ __| | |_| |/ _ \ | '_ ` _ \
| |___| | | | | | (_| | (__\__ \ |  _  |  __/ | | | | | |
|_____|_| |_| |_|\__,_|\___|___/ |_| |_|\___|_|_| |_| |_|
```

## Getting Started

### Quick install

  1. Put the helm directory in `load-path`.
  2. Run make from this directory.
  3. Add to `.emacs.el` (or equivalent):

    ```elisp
    (add-to-list 'load-path "/path/to/helm/directory")
    (require 'helm-config)
    ```

### Manual install

  1. Put following files somewhere in your `load-path`:
     - `helm.el`
     - `helm-config.el`
     - `helm-match-plugin.el`

  2. Byte-compile these files
  3. Add this code to `.emacs.el`:

  ```elisp
  (require 'helm-config)
  ```

**NOTE to Distribution maintainers:(Ubuntu etc...)**

Use only the files mentioned above in the **Manual install** section
for your packages, other files found on emacswiki or elsewhere are not
supported.

Thanks.
