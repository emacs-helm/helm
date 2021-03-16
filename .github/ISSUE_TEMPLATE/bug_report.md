---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

# Important! Before filing an issue, please consider the following:

    * Try to find if your bug is not already reported.

    * If you are using a preconfigured Emacs (Spacemacs, Doom etc...) \
      reproduce your bug out of this environment (see note below).

    * Please follow the below template.

    Thanks!

NOTE:
Helm provides a script named `emacs-helm.sh` which runs Helm in a neutral environment.

When possible, use it to reproduce your Helm issue to ensure no other package is
interfering.

To run it, simply switch to the directory where Helm is installed and call `./emacs-helm.sh`.
If necessary you can specify emacs executable path on command line with "-P" option.

For people using straight to manage their packages you have to specify
the path to you emacs-async installation:

cd ~/.emacs.d/straight/repos/helm
EMACSLOADPATH="../emacs-async:" ./emacs-helm.sh 



## Expected behavior

## Actual behavior (from `emacs-helm.sh` if possible, see note at the bottom)

## Steps to reproduce (recipe)

## Backtraces if any (`M-x toggle-debug-on-error`)

## Describe versions of Helm, Emacs, operating system, etc.

## Are you using `emacs-helm.sh` to reproduce this bug? (yes/no):

