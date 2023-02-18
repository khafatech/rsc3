#lang scribble/manual
@(require (for-label racket))

@title{Lag3UD}
 Exponential lag@section{categories}
  UGens>Filters
@section{related}
  Classes/Lag, Classes/Lag2, Classes/Lag3, Classes/LagUD, Classes/Lag2UD

@section{description}

Lag3UD is equivalent to LagUD.kr(LagUD.kr(LagUD.kr(in, timeU, timeD), timeU, timeD), timeU, timeD), thus resulting in a smoother transition. This saves on CPU as you only have to calculate the decay factor once instead of three times. See link::Classes/LagUD:: for more details.

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

@section{examples}
 

@racketblock[
(
// used to lag pitch
       SynthDef( \lag3ud_help,

	{ arg freq=300,lagup=1, lagdown=5;
		Out.ar( 0,
			SinOsc.ar( // sine wave
				Lag3UD.kr( // lag the frequency
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

x = Synth.new( \lag3ud_help ); // create the synth

x.set( \freq, 500 ); // set the frequency to a higher value (takes 1 second)
x.set( \freq, 100 ); // set the frequency to a lower value (takes 5 seconds)
x.free;
::
]


