# Scheme SuperCollider, a Tutorial

## Prerequisites

Scheme SuperCollider
([rsc3](http://rohandrape.net/?t=rsc3))
requires that both the
[SuperCollider](http://www.audiosynth.com/),
synthesiser and a scheme interpreter
(either
[Ikarus](https://launchpad.net/ikarus/)
or
[Chez](https://www.scheme.com/)
or
[Guile](https://www.gnu.org/software/guile/))
are all installed and working properly.

The interaction environment is ordinarily
[Emacs](http://www.gnu.org/software/emacs/).

## Setting up Scheme SuperCollider

Scheme SuperCollider is currently only available as a set of
[darcs](http://darcs.net/)
repositories, the basic system is called `rsc3` and
requires the rhs and sosc libraries.  A simple build utility is also
required.

    $ darcs get http://rohandrape.net/sw/mk-r6rs
    $ darcs get http://rohandrape.net/sw/rhs
    $ darcs get http://rohandrape.net/sw/sosc
    $ darcs get http://rohandrape.net/sw/rsc3

To build ensure that the `mk-r6rs` library is on the scheme library
path and run the below in each project.

    $ (cd mk; make prefix=~/opt)

This installs the r6rs libraries to `~/opt/lib/r6rs`.

<!--
If using `PLT` scheme the following steps are also required:

    $ plt-r6rs --install ~/opt/lib/r6rs/rhs.sls
    $ plt-r6rs --install ~/opt/lib/r6rs/sosc.mzscheme.sls
    $ plt-r6rs --install ~/opt/lib/r6rs/rsc3.mzscheme.sls
-->

## Setting up the Scheme SuperCollider Emacs mode

Add an appropriately modified variant of the following to `~/.emacs`

    (push "/home/rohan/sw/rsc3/emacs" load-path)
    (setq rsc3-help-directory "/home/rohan/sw/rsc3/help/")
    (setq rsc3-interpreter (list "ikarus" "/home/rohan/.rsc3"))
    (require 'rsc3)

The rsc3 emacs mode associates itself with files having the extensions
`.scm` and `.lisp`.  When the `rsc3` emacs mode is active there
is a `Scheme SuperCollider` menu available.

Ordinarily the `~/.rsc3` file will load a standard set of modules.

~~~~
(import (rhs core)) ; rhs
(import (sosc core)) ; sosc
(import (rsc3 core) (rsc3 server) (rsc3 ugen)) ; rsc3
(import (rsc3 arf) (rsc3 dot) (rsc3 lang)) ; rsc3-arf rsc3-dot rsc3-lang
~~~~

## Documentation

The documentation for Scheme SuperCollider, including this tutorial,
is written in plain text as ordinary scheme source files.

Unlike ordinary programs the Scheme SuperCollider help files cannot be
compiled to executables.  Each help file contains multiple independant
examples that can be evaluated using editor commands, either by
selecting from the `Scheme SuperCollider` menu or using the associated
keybinding.

## Interpreter Interaction

To start the scheme interpreter use `C-cC->` (Scheme SuperCollider →
Scheme → See scheme).

Starting the interpreter splits the current window into two windows.
If the scheme output window becomes obscured during a session you can
see it again by typing `C-cC-s` again.

To stop scheme type `C-cC-q` (Scheme SuperCollider → Scheme
→ Quit scheme).

## Starting the SuperCollider server

The SuperCollider server can be started from the command
line.  The help files assume that scsynth is listening for
UDP connections at the standard port on the local machine.

    $ scsynth -u 57110

## Basic SuperCollider Interaction

The SuperCollider server manages a graph of nodes with
integer identifiers.  The root node has ID zero.  By
convention ordinary graph nodes are placed in a group with
identifier 1, however this node is not created when scsynth
starts.

To create this node we need to send an OSC message to the
server, the expression to do this is written below.  To
evaluate an expression move the cursor to the closing
parenthesis and type `C-cC-e` (Scheme SuperCollider →
Expression → Evaluate).

    (withSc3 (lambda (fd) (sendMessage fd (g_new1 1 addToTail 0))))

We can then audition a quiet sine oscillator at A440 by typing
`C-cC-a` (Scheme SuperCollider → Expression → Play).

    (Mul (SinOsc 440 0) 0.1)

To stop the sound we can delete the group it is a part of,
the audition function places the synthesis node into the
group node with ID 1, the expression below deletes that
group.

    (withSc3 (lambda (fd) (sendMessage fd (n_free1 1))))

In order to audition another graph we need to re-create a group with
ID 1.  rsc3 includes a function `reset` that sequences these two
actions, first deleting the group node, then re-creating a new empty
group.

    (withSc3 reset)

Using this command is so common there is a keybinding for it, `C-cC-k`
(Scheme SuperCollider → SCSynth → Reset scsynth).  After a reset we
can audition a new graph.

    (Mul (SinOsc 220 0) 0.1)

To see the server status type `C-cC-p` (Scheme SuperCollider → SCSynth
→ Status), which sends:

    (withSc3 displayServerStatus)

This prints a table indicating server activity to the scheme output
window.

    ***** SuperCollider Server Status *****
    # Ugens                     0
    # Synths                    0
    # Groups                    2
    # Instruments               0
    % CPU (Average)             1.2261821031570435
    % CPU (Peak)                1.2635631561279297
    Sample Rate (Nominal)       44100.0
    Sample Rate (Actual)        44098.31557093504

## Multiple line expressions

Multiple line expressions are evaluated using the same mechanism as
single line expressions, so `C-cC-a` will play the expression below:

    (let* ((f1 (XLine 1 1000 9 removeSynth))
           (f2 (MulAdd (SinOsc f1 0) 200 800)))
      (Mul (SinOsc f2 0) 0.1))

## Help Files

To find help on a Ugen or on a SuperCollider server command place the
cursor over the identifier and type `C-cC-h` (Scheme SuperCollider →
Help → Scheme SuperCollider help).  This opens the help file, which
ought to have working examples in it, the above graph is in the
`SinOsc` help file.

There is also a collection of Ugen graphs,
for instance `help/graph/jmcc-analog-bubbles.lisp`.

## Monitoring incoming server messages

To monitor what OSC messages scsynth is receiving use the `dumpOSC`
server command to request that scsynth print text traces of incoming
messages to its standard output.

    (withSc3 (lambda (fd) (sendMessage fd (dumpOSC 1))))

To end printing send:

    (withSc3 (lambda (fd) (sendMessage fd (dumpOSC 0))))
