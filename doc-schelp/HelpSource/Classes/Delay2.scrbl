#lang scribble/manual
@(require (for-label racket))

@title{Delay2}
 Two sample delay.@section{related}
  Classes/Delay1
@section{categories}
   UGens>Delays


@section{description}


Delays the input by 2 samples.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
Input signal.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[

(
plot({
	var z;
	z = Dust.ar(1000);
	[z, z - Delay2.ar(z)] 	// [ original, subtract delayed from original ]
}))

::

]


