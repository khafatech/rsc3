#lang scribble/manual
@(require (for-label racket))

@title{Dconst}
 Constrain a demand-rate stream to a given sum@section{categories}
  UGens>Demand

@section{description}

A demand-rate analog to link::Classes/Pconst::. It outputs values from the child demand stream until the sum of those values reaches or exceeds a given total. The last value will be truncated so that the sum of Dconst's output values will match the total exactly.

@section{CLASSMETHODS}
 

@section{METHOD}
  new

@section{ARGUMENT}
  sum
The sum to reach. This may be a number, demand UGen or any other UGen. When a Dconst instance resets, one value will be taken for the sum, and it can't be modulated until the next reset.

@section{ARGUMENT}
  in
A demand-rate stream, providing the output values.

@section{ARGUMENT}
  tolerance
Because of floating point rounding error, it isn't safe to stop only when the output's running sum is equal to the desired total. teletype::tolerance:: is how close the running sum can get to stop the output: 
@racketblock[abs(runningsum - sum) <= tolerance::.

]
@section{returns}
  A demand-rate stream.


@section{EXAMPLES}
 


@racketblock[
// fast notes of random duration for 0.5 seconds
// then a single note for 0.5 seconds
(
a = {
	var freq = Duty.kr(
		Dseq([
			Dconst(0.5, Dwhite(0.05, 0.08, inf)),
			0.5
		], inf),
		0,
		// workaround for the lack of Dexprand
		Dwhite(0, 1, inf).linexp(0, 1, 200, 600)
	);
	VarSaw.ar(Lag.kr(freq, 0.02), 0, 0.3, 0.1).dup
}.play;
)

a.free;
]

