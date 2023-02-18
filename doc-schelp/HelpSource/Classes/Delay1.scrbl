#lang scribble/manual
@(require (for-label racket))

@title{Delay1}
 Single sample delay.@section{related}
  Classes/Delay2
@section{categories}
   UGens>Delays


@section{description}


Delays the input by 1 audio frame or control period.


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

@section{discussion}
 
For audio-rate signals the delay is 1 audio frame, and for control-rate signals the delay is 1 control period.


@section{Examples}
 


@racketblock[

(
plot({
	var z;
	z = Dust.ar(1000);
	[z, z - Delay1.ar(z)] 	// [ original, subtract delayed from original ]
}))

::

]


