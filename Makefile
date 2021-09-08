# makefile for helm.

# Author: Michael Markert.
# Copyright (C) 2011~2012, Michael Markert, all rights reserved.

## This file is NOT part of GNU Emacs
##
## License
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.

# Emacs invocation
EMACS_COMMAND   := emacs

# Use -q to have /usr/local/share/emacs/site-lisp and subdirs in load-path
EMACS		:= $(EMACS_COMMAND) -q -batch

EVAL := $(EMACS) --eval

PKGDIR := .

# Additional emacs loadpath
LOADPATH	:= -L $(PKGDIR)

# Prefer emacs config folder in XDG_CONFIG_HOME to ~/.emacs.d
# Assume emacs-user-directory is ~/.emacs.d
# Try to find ELPA directory or STRAIGHT directory.
XDG_ELPA_DIR	:= $(if $(XDG_CONFIG_HOME), $(XDG_CONFIG_HOME)/emacs/elpa, $(HOME)/.config/emacs/elpa)
ELPA_DIR := $(if $(shell test -d $(XDG_ELPA_DIR)), $(XDG_ELPA_DIR), $(HOME)/.emacs.d/elpa)

XDG_STRAIGHT_DIR := $(if $(XDG_CONFIG_HOME), $(XDG_CONFIG_HOME)/emacs/straight/build, $(HOME)/.config/emacs/straight/build)
STRAIGHT_DIR := $(if $(shell test -d $(XDG_STRAIGHT_DIR)), $(XDG_STRAIGHT_DIR), $(HOME)/.emacs.d/straight/build)

ASYNC_ELPA_DIR  =  $(shell \
	test -d $(ELPA_DIR) && \
	find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/async-[.0-9]*' 2> /dev/null | \
	sort | tail -n 1)
ifneq "$(ASYNC_ELPA_DIR)" ""
	LOADPATH += -L $(ASYNC_ELPA_DIR)
endif

ASYNC_STRAIGHT_DIR  =  $(shell \
	test -d $(STRAIGHT_DIR) && \
	find -L $(STRAIGHT_DIR) -maxdepth 1 -regex '.*/async' 2> /dev/null | \
	sort | tail -n 1)
ifneq "$(ASYNC_STRAIGHT_DIR)" ""
	LOADPATH += -L $(ASYNC_STRAIGHT_DIR)
endif

# Files to compile
EL			:= $(sort $(wildcard helm*.el))

# Compiled files
ELC			:= $(EL:.el=.elc)

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf --batch --clean --expand

# How to make a pdf file from a tex file
PDFTEX = pdftex

# How to create directories with leading path components
MKDIR	= mkdir -m 755 -p # try this if you have no install
# MKDIR	= install -m 755 -d

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections --css-ref "https://www.gnu.org/software/emacs/manual.css"

# Name of the program to install info files
# INSTALL_INFO = ginstall-info # Debian: avoid harmless warning message
INSTALL_INFO = install-info

ORGFILES			:=  doc/helm-bugs.org  doc/helm-devel.org  doc/helm-manual-1.org  doc/helm-manual.org doc/helm.org doc/helm-classes.org
TEXIFILES			:= $(ORGFILES:.org=.texi)
INFOFILES			:= $(ORGFILES:.org=.info)
HTMLFILES			:= $(ORGFILES:.org=.html)
PDFFILES			:= $(ORGFILES:.org=.pdf)

.PHONY: clean autoloads batch-compile clean-doc

all: clean autoloads batch-compile

doc:	doc/ox-texinfo.el
doc: info
doc: html
# doc: pdf

pdf: $(PDFFILES)
html: $(HTMLFILES)
info: $(INFOFILES)
texi: $(TEXIFILES)

$(TEXIFILES): | doc/ox-texinfo.el

# doc/ox-texinfo.el:
# wget  https://code.orgmode.org/bzg/org-mode/raw/maint/lisp/ox-texinfo.el -O $@
# test -f doc/ox-texinfo.el || wget  https://code.orgmode.org/bzg/org-mode/raw/maint/lisp/ox-texinfo.el -O $@

%.texi:		doc/ox-texinfo.el

%.texi:		%.org
	$(EMACS) $(LOADPATH) -l doc/ox-texinfo.el -l doc/ox-texinfo+.el  --file=$< --eval '(org-texinfo-export-to-texinfo+)'

%.info:		%.texi
	$(MAKEINFO) --no-split $< -o $@

# the following two lines work around a bug in some versions of texi2dvi
%.pdf:		LC_ALL=C
%.pdf:		LANG=C
%.pdf:		%.texi
	$(TEXI2PDF) $<
%.pdf:		%.tex
	PDFLATEX=$(PDFTEX) $(TEXI2PDF) $<

%.html:		%.texi
	$(TEXI2HTML) --no-split -o $@ $<

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(EL)

# Remove all generated files
clean-doc:
	rm -f doc/ox-texinfo.el $(TEXIFILES) $(INFOFILES)  $(HTMLFILES) $(PDFFILES)

clean : clean-doc
clean: clean-doc
	rm -f $(ELC)

# Make autoloads file
autoloads:
	$(EVAL) "(progn (setq generated-autoload-file (expand-file-name \"helm-autoloads.el\" \"$(PKGDIR)\")) \
(setq backup-inhibited t) (update-directory-autoloads \"$(PKGDIR)\"))"

PREFIX=/usr/local/
BIN=${PREFIX}bin/
DESTDIR=${PREFIX}share/emacs/site-lisp/helm/

# On Debian, paths in `Info-default-directory-list' are in the order
# as below:

# INFODIR	= /usr/local/share/info
# INFODIR	= /usr/local/info
INFODIR		= /usr/share/info
# INFODIR	= /usr/local/share/info

# /usr/share/info is where Debian puts the info file for EMMS. So,
# just mimic it.

# For non-standard value of INFODIR, see `Info-directory-list'.

install:
	test -d ${DESTDIR} || mkdir ${DESTDIR}
	rm -f ${DESTDIR}*.el
	rm -f ${DESTDIR}*.elc
	cp -f *.el $(DESTDIR)
	cp -f *.elc $(DESTDIR)
	cp -f helm-autoloads.el $(DESTDIR)
	cp -f emacs-helm.sh $(DESTDIR)
	ln -fs ${DESTDIR}emacs-helm.sh ${BIN}helm
uninstall:
	rm -vf ${DESTDIR}*.elc
	rm -vf ${DESTDIR}*.el
	rm -vf ${DESTDIR}emacs-helm.sh
	rm -vf ${BIN}helm

$(DESTDIR)$(infodir)/%.info: doc/%.info

install-info:	$(INFOFILES)
	if [ ! -d  $(INFODIR) ]; then $(MKDIR) $(INFODIR); else true; fi ;
	if [ ! -d  $(INFODIR)/helm-figures ]; then $(MKDIR) $(INFODIR)/helm-figures; else true; fi ;
	cp -r doc/helm-figures $(INFODIR);
	for f in $(INFOFILES:doc/%=%) ; do				\
		cp doc/$$f $(INFODIR);					\
		$(INSTALL_INFO) --info-dir=$(INFODIR) $(INFODIR)/$$f;	\
	done

clean-install-info:
	$(RM) -r $(INFODIR)/helm-figures;
	for f in $(INFOFILES:doc/%=%) ; do						\
		$(INSTALL_INFO) --remove --info-dir=$(INFODIR) --remove $(INFODIR)/$$f;	\
		$(RM) $(INFODIR)/$$f;							\
	done
