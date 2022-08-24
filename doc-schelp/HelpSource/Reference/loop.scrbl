#lang scribble/manual
@(require (for-label racket))

@title{loop / repeat}
 Repeat stuff@section{categories}
 Core,Common methods

@section{method}
  loop, repeat

Create an object that behaves like a stream that returns values for a limited (or infinite) number of times.

For a full list of implementing classes, see link::Overviews/Methods#loop:: and link::Overviews/Methods#repeat::

@section{definitionlist}
 
## link::Classes/Function#-loop:: ||
	repeats the function forever.

@racketblock[
	f = { 3.yield };
	x = Routine({ f.loop });
	10.do({ x.next.postln })
::

## link::Classes/Object#-repeat:: (n) ||
	repeat to yield the object
]

@racketblock[
	x = 5;
	y = x.repeat(6);
	y.nextN(8);
::

## link::Classes/Pattern#-repeat:: (n) ||

]

@racketblock[
	x = Prand([1, 2]).repeat(6).asStream;
	x.nextN(8);
::

## link::Classes/Pattern#-loop:: ||

]

@racketblock[
	x = Prand([1, 2]).loop.asStream;
	x.nextN(8);
::

## link::Classes/Stream#-repeat:: (n) ||

	embeds the stream repeatedly

]

@racketblock[
	x = Routine({ 3.do({ arg i; i.yield }) }).repeat(6);
	x.nextN(8);
::

## link::Classes/Stream#-loop:: ||

	embeds the stream repeatedly

]

@racketblock[
	x = Routine({ 3.do({ arg i; i.yield }) }).loop;
	x.nextN(8);
::
::
]


