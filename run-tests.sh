#!/bin/sh

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# This script tests functions from ar-emacs-scala-mode.

# Code:

if [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e30 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    EMACS=emacs
fi

#  EMACS=emacs
echo "before shift \$EMACS: $EMACS"
shift

echo "\$*: $*"
PDIR=$PWD
echo "\$PWD: $PWD"

TESTDIR=${PWD}/test
export TESTDIR
echo "\$TESTDIR: $TESTDIR"

SCALAMODE=$HOME/arbeit/emacs-lisp/emacs-scala-mode
echo "\$SCALAMODE: $SCALAMODE"
export SCALAMODE

ARSCALAM=$HOME/werkstatt/ar-emacs-scala-mode
echo "\$ARSCALAM: $ARSCALAM"
export ARSCALAM

IFLOCAL=${IFLOCAL:=1}

SETUP=${TESTDIR}/ar-emacs-scala-mode-setup-tests.el

FILE1=${PWD}/ar-subr.el
FILE2=${PWD}/beg-end.el
FILE3=${PWD}/ar-thingatpt-basic-definitions
FILE4=${PWD}/thingatpt-utils-core
FILE5=${PWD}/thing-at-point-utils
FILE6=${PWD}/ar-sexp
FILE7=${PWD}/ar-navigate.el
FILE8=${PWD}/ar-navigate-backward-forms.el
FILE9=${PWD}/ar-navigate-forward-forms.el
FILE10=${SCALAMODE}/scala-mode.el
FILE11=${PWD}/ar-emacs-scala-mode.el
FILE12=${PWD}/ar-emacs-scala-navigate.el

TEST1=${TESTDIR}/ar-emacs-scala-backward-nav-tests.el
TEST2=${TESTDIR}/ar-emacs-scala-forward-nav-tests.el


h1 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"PWD\"))" \
--eval "(add-to-list 'load-path (getenv \"SCALAMODE\"))" \
--eval "(require 'scala-mode)" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
\
-load $SETUP \
-load $TEST1 \
-f ert-run-tests-batch-and-exit
}

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

# -load $TEST19 \

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
\
-load $SETUP \
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}


if [ $IFLOCAL -eq 0 ]; then
    aesm_aktu

    # [ $HOME/werkstatt/thingatpt-utils-core/ar-subr.el -nt ${PWD}/ar-subr.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD
    # [ $HOME/werkstatt/thingatpt-utils-core/beg-end.el -nt ${PWD}/beg-end.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD
    # [ $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el -nt ${PWD}/ar-thingatpt-basic-definitions.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el $PWD
    # [ $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el -nt ${PWD}/thingatpt-utils-core.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD
    # [ $HOME/werkstatt/thing-at-point-utils/thing-at-point-utils.el -nt ${PWD}/thing-at-point-utils.el ] && echo "cp -u $HOME/werkstatt/thing-at-point-utils/thing-at-point-utils.el $PWD"; cp -u $HOME/werkstatt/thing-at-point-utils/thing-at-point-utils.el $PWD
    # [ $HOME/werkstatt/thing-at-point-utils/ar-sexp.el -nt ${PWD}/ar-sexp.el ] && echo "cp -u $HOME/werkstatt/thing-at-point-utils/ar-sexp.el $PWD"; cp -u $HOME/werkstatt/thing-at-point-utils/ar-sexp.el $PWD    
    # [ $HOME/werkstatt/emacs-generics/ar-navigate.el -nt ${PWD}/ar-navigate.el ] && echo "cp -u $HOME/werkstatt/emacs-generics/ar-navigate.el $PWD"; cp -u $HOME/werkstatt/emacs-generics/ar-navigate.el $PWD
    # [ $HOME/werkstatt/emacs-generics/ar-navigate-backward-forms.el -nt ${PWD}/ar-navigate-backward-forms.el ] && echo "cp -u $HOME/werkstatt/emacs-generics/ar-navigate-backward-forms.el $PWD"; cp -u $HOME/werkstatt/emacs-generics/ar-navigate-backward-forms.el $PWD
    # [ $HOME/werkstatt/emacs-generics/ar-navigate-forward-forms.el -nt ${PWD}/ar-navigate-forward-forms.el ] && echo "cp -u $HOME/werkstatt/emacs-generics/ar-navigate-forward-forms.el $PWD"; cp -u $HOME/werkstatt/emacs-generics/ar-navigate-forward-forms.el $PWD

    while getopts 123456789abcdefghijklmnopqrstuvwxyz option

    do
	case $option in
	    1) echo "Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "Lade \$TEST4: \"$TEST4\"";h4;;
	    5) echo "Lade \$TEST5: \"$TEST5\"";h5;;
	    6) echo "Lade \$TEST6: \"$TEST6\"";h6;;
	    7) echo "Lade \$TEST7: \"$TEST7\"";h7;;
	    8) echo "Lade \$TEST8: \"$TEST8\"";h8;;
	    9) echo "Lade \$TEST9: \"$TEST9\"";h9;;
	    a) echo "Lade \$TEST10: \"$TEST10\"";h10;;
	    b) echo "Lade \$TEST11: \"$TEST11\"";h11;;
	    c) echo "Lade \$TEST12: \"$TEST12\"";h12;;
	    d) echo "Lade \$TEST13: \"$TEST13\"";h13;;
	    e) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    f) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    g) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    h) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    i) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    j) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    k) echo "Lade \$TEST20: \"$TEST20\"";h20;;
	    l) echo "Lade \$TEST21: \"$TEST21\"";h21;;
	    m) echo "Lade \$TEST22: \"$TEST22\"";h22;;
            # n unten für alle
	    o) echo "Lade \$TEST14: \"$TEST14\"";h23;;
	    # p) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    # q) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    # r) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    # s) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    # p) echo "Lade \$TEST12: \"$TEST12\"";h12;;
	    # q) echo "Lade \$TEST13: \"$TEST13\"";h13;;
	    # #  r) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    # s) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    # t) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    # u) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    # v) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    # w) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    x) echo "Lade testumgebung \"ENTFERNT\""; entfernt;;
	    n) echo "Lade testumgebung \"HIER\"";hier;;

	esac
    done

else

    echo "Lade testumgebung \"ENTFERNT\""
    entfernt

fi
