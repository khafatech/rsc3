#lang scribble/manual
@(require (for-label racket))

@title{12_UnaryOp_synthesis}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Unary messages

Some synthesis processes can be initiated with a unary message (a message with no arguments).

////////////////////////////////////////////////////////////////////////////////////////////////////

For example, compare


@racketblock[
{ SinOsc.ar(500, 0, 0.5) }.scope;
::

to

]

@racketblock[
{ SinOsc.ar(500, 0, 0.5).distort }.scope;
::

The .distort message modulates the SinOsc to create more partials.

////////////////////////////////////////////////////////////////////////////////////////////////////

Q: Where does the .distort message come from?

A: It's defined in the AbstractFunction class. The UGen class is a subclass of the AbstractFunction class. The idea is that all classes inherit methods defined in their superclasses; all ugens thus inherit from AbstractFunction).

Compare

]

@racketblock[
{ SinOsc.ar(500, 0, 0.5) }.scope;
::

to

]

@racketblock[
// .cubed is a unary operation
{ SinOsc.ar(500, 0, 0.5).cubed }.scope;
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/13_BinaryOp_synthesis::
]


