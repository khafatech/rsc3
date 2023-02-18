#lang scribble/manual
@(require (for-label racket))

@title{Mix}
 Sum an array of channels.@section{categories}
   UGens>Multichannel


@section{description}


Mix will mix an array of channels down to a single channel or an array
of arrays of channels down to a single array of channels.
More information can be found under link::Guides/Multichannel-Expansion::.


@section{note}
 

Note that  
@racketblock[Mix.ar::  and  ]

@racketblock[Mix.kr::  in
SC2 are equivalent to  ]

@racketblock[Mix.new::  in SC3,
and that  ]

@racketblock[Mix.arFill::  and  ]

@racketblock[Mix.krFill::  are
equivalent to  ]

@racketblock[Mix.fill::.

::

]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 array
The array of channels or arrays.

@section{method}
 fill

@section{argument}
 n
The size of array to create.

@section{argument}
 function
The array filling function.

@section{returns}
  Returns:
A newly created
link::Classes/UGen:: .

@section{Examples}
 


@racketblock[

s.boot;

{ Mix.new([ PinkNoise.ar(0.1), FSinOsc.ar(801, 0.1), LFSaw.ar(40, 0.1)]) }.play

(
play({
	Mix.new( Array.fill(8, { SinOsc.ar(500 + 500.0.rand, 0, 0.05) }) );
}))

(
play({
	Mix.fill(8, { SinOsc.ar(500 + 500.0.rand, 0, 0.05) });
}))

::

]


