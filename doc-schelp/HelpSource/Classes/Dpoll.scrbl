#lang scribble/manual
@(require (for-label racket))

@title{Dpoll}
 Print the current output value of a demand rate UGen@section{categories}
  UGens>Demand, UGens>Info

@section{description}

Print the current output value of a demand rate UGen.
@section{WARNING}
  Printing values from the Server in intensive for the CPU. Poll should be used for debugging purposes. ::

@section{classmethods}
 
@section{private}
  new1

@section{method}
  new
@section{argument}
  in
ugen to poll values from
@section{argument}
  label
a label string
@section{argument}
  run
active if 1, inactive if 0. can be a demand ugen (default: 1)
@section{argument}
  trigid
@section{discussion}
 
the print-out is in the form: 
@racketblock[ label: value block offset: offset. ::

structurally related: link::Classes/Poll::

]
@section{examples}
 

@racketblock[

{ Duty.kr(0.5, 0, Dpoll(Dseries(0, 1, inf) * 2)); 0.0 }.play;

// posts only when cursor is in right half of the screen
{ Duty.kr(0.5, 0, Dpoll(Dseries(0, 1, inf) * 2, run: MouseX.kr > 0.5)); 0.0 }.play;

// label
{ LFPulse.ar(Duty.kr(0.5, 0, Dpoll(Dseries(0, 1, inf) % 10 + 1 * 30, "value"))) * 0.1 }.play;

// block offset:
{ Duty.ar(0.511, 0, Dpoll(Dseries(0, 1, inf) * 2)); 0.0 }.play;

// multichannel expansion:

(
	{
	var x = Duty.kr(0.5, 0,
		Dpoll([Dseries(0, 1, inf), Dgeom(1, 1.1, inf)], ["first", "second"])
	);
	LFPulse.ar(100 + (x * 100)) * 0.1
	}.play
)

{ Duty.kr(0.5, 0, Dpoll(( Dseries(0, 1, inf) + _ ).dup, ("value" + _).dup)); 0.0 }.play;


// the message dpoll(label, run, trigid) is a shorthand:

{ Duty.kr(0.5, 0, Dseries(0, 1, inf).dpoll); 0.0 }.play;
{ Duty.ar(0.5, 0, SinOsc.ar(0.1).dpoll); 0.0 }.play;
::

]


