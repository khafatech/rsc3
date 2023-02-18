#lang scribble/manual
@(require (for-label racket))

@title{WrapIndex}
 Index into a table with a signal.@section{related}
  Classes/Index, Classes/Shaper
@section{categories}
   UGens>Buffer


@section{description}


The input signal value is truncated to an integer value and used as an
index into the table. Out-of-range index values are wrapped cyclically to
the valid range.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 bufnum

Index of the buffer.


@section{argument}
 in

The input signal.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

(
// indexing into a table
s = Server.local;
t = [ 200, 300, 400, 500, 600, 800 ];
b = Buffer(s,t.size,1);

// alloc and set the values
s.listSendMsg( b.allocMsg( b.setnMsg(0, t) ).postln );

SynthDef("help-Index",{ arg out=0,i_bufnum=0;
	Out.ar(0,
		SinOsc.ar(
			WrapIndex.kr(
				i_bufnum,
				MouseX.kr(0, t.size * 3)
			),
			0,
			0.5
		)
	)
}).play(s,[\i_bufnum,b.bufnum]);

)

::

]


