#lang scribble/manual
@(require (for-label racket))

@title{MantissaMask}
 Reduce precision.@section{categories}
   UGens>Filters>Nonlinear


@section{description}


Masks off bits in the mantissa of the floating point sample value.
This introduces a quantization noise, but is less severe than linearly
quantizing the signal.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 bits

The number of mantissa bits to preserve. A number from 0 to 23.


@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

// preserve only 3 bits of mantissa.
{ MantissaMask.ar(SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4), 3) }.play

// the original
{ SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4) }.play

// the difference.
(
{
	var in;
	in = SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4);
	Out.ar(0, in - MantissaMask.ar(in, 3));
}.play
)


// preserve 7 bits of mantissa.
// This makes the lower 16 bits of the floating point number become zero.
{ MantissaMask.ar(SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4), 7) }.play

// the original
{ SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4) }.play

// the difference.
(
{
	var in;
	in = SinOsc.ar(SinOsc.kr(0.2,0,400,500), 0, 0.4);
	Out.ar(0, in - MantissaMask.ar(in, 7));
}.play
)

::
]


