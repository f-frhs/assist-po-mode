#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
EMACS="emacs"
fi

$EMACS -batch -l dev/ert.el -l dev/examples-to-tests.el -l dev/po-mode.el -l assist-po-mode.el -l dev/examples.el -f ert-run-tests-batch-and-exit
