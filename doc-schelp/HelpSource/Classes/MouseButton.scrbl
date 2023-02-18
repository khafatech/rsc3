#lang scribble/manual
@(require (for-label racket))

@title{MouseButton}
 Mouse button UGen.@section{related}
  Classes/KeyState, Classes/MouseX, Classes/MouseY
@section{categories}
   UGens>User interaction


@section{description}


Mouse button UGen.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 minval

Value when the button is not pressed.


@section{argument}
 maxval

Value when the button is pressed.


@section{argument}
 lag

Lag factor.


@section{Examples}
 


@racketblock[

{ SinOsc.ar(MouseButton.kr(400, 440, 0.1), 0, 0.1) }.play;
{ SinOsc.ar(MouseButton.kr(400, 740, 2), 0, 0.1) }.play;

(
SynthDef( \mousexyb, { |out=0|
	var mousex, mousey, mousebutton;
	mousex = MouseX.kr( 500, 1000 ); // this will determine the frequency of the sound (minimum value, maximum value, warp, lag)
	mousey = MouseY.kr( 0, 0.3 ); // this will determine the amplitude of the sound
	mousebutton = MouseButton.kr( 0, 1, 2 ); // this will turn the sound on or off (minimum value, maximum value, lag)
	Out.ar( out, SinOsc.ar( mousex, 0, mousey ) * mousebutton );
}).add
)

x = Synth.new( \mousexyb );
x.free;
::

]


