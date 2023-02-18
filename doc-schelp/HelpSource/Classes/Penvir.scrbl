#lang scribble/manual
@(require (for-label racket))

@title{Penvir}
 use an environment when embedding the pattern in a stream@section{related}
  Classes/Pkey
@section{categories}
  Streams-Patterns-Events>Patterns>Data Sharing

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 envir
an environment with objects to embed.

@section{argument}
 pattern
pattern or stream, usually a link::Classes/Pfunc::, link::Classes/Prout::.

@section{argument}
 independent
if true streams can write to the environment without influencing other streams created from this pattern. if false, the streams write to a common namespace.

@section{Examples}
 


@racketblock[
(
x = (a:8);
y = Penvir(
	x,
	Pfunc { ~a * 2 }
);

t = y.asStream;
)

t.next;



(
x = (a:8);
y = Penvir(
	x,
	Pfunc { ~a = ~a * 2 }
);

t = y.asStream;
z = y.asStream;
)

t.next;
t.next;
x.postln;	// x stays unchanged
::
]


