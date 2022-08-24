#lang scribble/manual
@(require (for-label racket))

@title{PstepNfunc}
 combinatoric pattern@section{related}
  Classes/PstepNadd
@section{categories}
  Streams-Patterns-Events>Patterns>Time

@section{description}


Combines an arbitrary number of patterns by evaluating a function (depth first traversal). When a stream ends it is recreated from its pattern until the top stream ends.

@section{Examples}
 


@racketblock[
(
f = { arg vals;
	vals.postln;
};
x = PstepNfunc(f, [
		Pseq([1, 2, 3]), Pseq([4, 5, 6]), Pseq([7, 8, 9])
	]).asStream;
50.do({ x.next });
)

(
f = { arg vals;
	var r;
	r = vals.copy.removeAt(0);
	vals.do({ arg item;  r = item / r.squared * 10 });
	r
};
x = PstepNfunc(f,
	[
		Pseq([1, 2, 3], inf),
		Pseq([2, pi, 1]),
		Pseq([0.1, 3, 0.2, 3])
	]
	).asStream;

50.do({ x.next.postln });
)

// note that if the last pattern loops it will stick to that one:
(
f = { arg vals;
	vals.postln;
};
x = PstepNfunc(f, [Pseq([1, 2, 3]), Pseq([10, 20, 30, 40]), Pseq([100, 200, 300], inf)]).asStream;
50.do({ x.next });
)


(
f = { arg vals;
	vals.inject(1, { arg x, y; x * y })
};
x = PstepNfunc(f,
	[
		Pseq([1, 2, 3], inf),
		Pseq([2, pi, 1]),
		Pseq([0.1, 3, 0.2, 3])
	]
	).asStream;

50.do({ x.next.postln });
)
::
]


