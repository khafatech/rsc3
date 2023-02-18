#lang scribble/manual
@(require (for-label racket))

@title{03_Comments}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Comments

Comments are descriptive remarks that are meant to be read by humans but ignored by computers. Programmers use comments to annotate how code works or what it does. It's also the case that some find it helpful to write programs by first notating comments and then filling in matching code.

////////////////////////////////////////////////////////////////////////////////////////////////////

To write a comment in SuperCollider, either precede text with


@racketblock[
//
::

as in

]

@racketblock[
// Everything up to the end of the line is a comment
::

or place text on one or more lines between

]

@racketblock[
/* and */
::

as in

]

@racketblock[
/*

This
is
a
comment

 */
::

If (when) evaluated, a comment will return nil, which is the value SuperCollider uses for uninitialized data.

////////////////////////////////////////////////////////////////////////////////////////////////////

Use Format->Syntax Colorize (or cmd-') to syntax-colorize comments.

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/04_Help::
]


