#lang scribble/manual
@(require (for-label racket))

@title{Pan4}
 Four channel equal power pan.@section{related}
  Classes/Balance2, Classes/LinPan2, Classes/Pan2, Classes/PanAz
@section{categories}
   UGens>Multichannel>Panners


@section{description}


Four channel equal power panner. Outputs are in order LeftFront,
RightFront, LeftBack, RightBack.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 xpos

X  pan position from -1 to +1 (left to right).


@section{argument}
 ypos

Y pan position from -1 to +1 (back to front).


@section{argument}
 level

A control rate level input.


@section{Examples}
 


@racketblock[

// You'll only hear the front two channels on a stereo setup.
(
SynthDef("help-Pan4", {
	Out.ar(0, Pan4.ar(PinkNoise.ar, FSinOsc.kr(2), FSinOsc.kr(1.2), 0.3))
}).play;
)

play({ Pan4.ar(PinkNoise.ar, -1,  0, 0.3) }); // left pair
play({ Pan4.ar(PinkNoise.ar,  1,  0, 0.3) }); // right pair
play({ Pan4.ar(PinkNoise.ar,  0, -1, 0.3) }); // back pair
play({ Pan4.ar(PinkNoise.ar,  0,  1, 0.3) }); // front pair

play({ Pan4.ar(PinkNoise.ar,  0,  0, 0.3) }); // center

::

]


