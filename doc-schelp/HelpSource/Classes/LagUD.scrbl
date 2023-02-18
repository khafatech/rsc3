#lang scribble/manual
@(require (for-label racket))

@title{LagUD}
 Exponential lag@section{categories}
  UGens>Filters
@section{related}
  Classes/Lag, Classes/Lag2, Classes/Lag3, Classes/Lag2UD, Classes/Lag3UD

@section{description}

This is essentially the same as link::Classes/Lag:: except that you can supply a different 60 dB time for when the signal goes up, from when the signal goes down. This is useful for smoothing out control signals, where "fade in" should be different from "fade out".

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  in
input signal.
@section{argument}
  lagTimeU
60 dB lag time in seconds for the upgoing signal.
@section{argument}
  lagTimeD
60 dB lag time in seconds for the downgoing signal.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
( // used to lag pitch
SynthDef( \lagud_help,
{	arg freq=300,lagup=1, lagdown=5;
	Out.ar( 0,
		SinOsc.ar( // sine wave
			LagUD.kr( // lag the frequency
				freq,
				lagup,
				lagdown
			),
			0, // phase
			0.2 // sine amplitude
		)
	);
}).add;
)
x = Synth.new( \lagud_help ); // create the synth
x.set( \freq, 500 ); // set the frequency to a higher value (takes 1 second)
x.set( \freq, 100 ); // set the frequency to a lower value (takes 5 seconds)
x.free;
::
]


