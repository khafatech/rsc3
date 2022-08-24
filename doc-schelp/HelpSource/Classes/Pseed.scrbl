#lang scribble/manual
@(require (for-label racket))

@title{Pseed}
 set the random seed in subpattern@section{related}
  Reference/randomSeed
@section{categories}
  Streams-Patterns-Events>Patterns>Language Control

@section{description}


Set the random generator seed of the resulting stream.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 randSeed
integer number, pattern or stream that return an integer number.

@section{note}
 
randSeed is always treated as a pattern/stream. If you provide a single, constant seed value, it will behave as an infinite-length stream. This will cause the subpattern to be embedded an infinite number of times. Compare:


@racketblock[
// Pwhite repeats its three values forever
Pseed(1000, Pwhite(1, 10, 3)).asStream.nextN(10);

// Pwhite runs once:
// the output stream consists of three values, then 'nil' ad infinitum
Pseed(Pn(1000, 1), Pwhite(1, 10, 3)).asStream.nextN(10);
::
::

]
@section{argument}
 pattern

@section{Examples}
 


@racketblock[
a = Pseed(1972, Prand([1,2,3], inf));

b = a.asStream;
10.do({ b.next.post });

c = a.asStream;
10.do({ c.next.post });


// using a seed pattern as input:

a = Pseed(Pseq([1812, 1912], inf), Prand([1,2,3], 5));

b = a.asStream;
2.do({ 5.do({ b.next.post });"".postln;  });

c = a.asStream;
2.do({ 5.do({ c.next.post });"".postln;  });


// outer thread is independant:

a = Pseed(Prand([1534, 1600, 1798, 1986, 2005], inf), Pshuf([1, Prand([7, 9], 2), 1, 2, 3], 1));

// returns random streams
b = a.asStream;
2.do({ 5.do({ b.next.post });"".postln;  });

c = a.asStream;
2.do({ 5.do({ c.next.post });"".postln;  });
::
]


