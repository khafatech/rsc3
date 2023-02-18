#lang scribble/manual
@(require (for-label racket))

@title{11_Test_functions}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Functions and .scope messages

An easy way to audition synthesis processes is to test them within a function. To do this, append a .scope or a .play message to a function. The .scope message, which works only with the internal server, displays a visual representation of a sound wave.

////////////////////////////////////////////////////////////////////////////////////////////////////

Boot (turn on) the server


@racketblock[
s.boot;
::

Run this example, and look at the scope window.

]

@racketblock[
// test a synthesis process in a function
(
	{
		SinOsc.ar([440.067, 441.013], 0, 1)
		*
		SinOsc.ar([111, 109], 0, 0.2)
	}.scope;
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

Code can be transfered from a test function into a synthdef. In the following example, the code from the function (above) is the second argument to the Out ugen.

]

@racketblock[
(
SynthDef("ringMod", {
	Out.ar(
		0,
		SinOsc.ar([440.067, 441.013], 0, 1)
		*
		SinOsc.ar([111, 109], 0, 0.2)
	)
}).add;
)

Synth("ringMod")
::

]
@section{section}
 Multi-channel expansion

Expand a ugen to two channels with an array in any of the argument (control) slots.


@racketblock[
{ Saw.ar([500, 933], 0.1) }.scope;
::

Another (longer) way to write the same thing is

]

@racketblock[
{ [ Saw.ar(500, 0.1), Saw.ar(933, 0.1)] }.scope;
::

Expand a ugen to three channels by adding values to the array.

]

@racketblock[
{ Saw.ar([500, 933, 2033], 0.1) }.scope;

// 4 channels

{ Saw.ar([500, 933, 2033, 895], 0.1) }.scope;
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/12_UnaryOp_synthesis::
]


