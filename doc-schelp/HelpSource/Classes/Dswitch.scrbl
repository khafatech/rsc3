#lang scribble/manual
@(require (for-label racket))

@title{Dswitch}
 Demand rate generator for embedding different inputs@section{related}
  Classes/Demand, Classes/Dswitch1
@section{categories}
  UGens>Demand

@section{description}

Demand rate generator for embedding different inputs.
In difference to Dswitch1, Dswitch embeds all items of
an input demand ugen first before looking up the next index.

@section{classmethods}
 
@section{method}
  new
@section{argument}
  list
array of values or other ugens
@section{argument}
  index
which of the inputs to return
@section{discussion}
 
structurally related: link::Classes/Pswitch::

@section{examples}
 

@racketblock[
(
	{
		var d, trig;
		d = Dswitch([
			Dwhite(3, 4, 2),
			Dwhite(0, 1, 2),
			Dseq([1, 1, 1, 0], 2)
		], Dseq([0, 1, 2, 1, 0], 2));

		trig = Impulse.kr(4);

		SinOsc.ar(
			Demand.kr(trig, 0, d).poll(trig)
			* 300 + 400
		)
		* 0.1 ! 2
	}.play;
);

// compare with Dswitch1:

(
	{
		var d, trig;
		d = Dswitch1([
			Dwhite(3, 4, 2),
			Dwhite(0, 1, 2),
			Dseq([1, 1, 1, 0], 2)
		], Dseq([0, 1, 2, 1, 0], 2));

		trig = Impulse.kr(4);

		SinOsc.ar(
			Demand.kr(trig, 0, d).poll(trig)
			* 300 + 400
		)
		* 0.1 ! 2
	}.play;
);
::

]


