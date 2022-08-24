#lang scribble/manual
@(require (for-label racket))

@title{Pkey}
 access a key in an event stream@section{related}
  Classes/Penvir
@section{categories}
  Streams-Patterns-Events>Patterns>Data Sharing

@section{description}


Pkey simplifies backward access to values in an event being processed by link::Classes/Pbind:: or another event pattern.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 key
The name of the event variable to read from.

@section{Examples}
 


@racketblock[
// \b should thus take twice the value of \a in each event:
p = Pbind(\a, Pwhite(1, 10, inf), \b, Pkey(\a) * 2).asStream;


p.next(())	// for Pbind, must pass in a default event even if empty

( 'a': 10, 'b': 20 )
( 'a': 2, 'b': 4 )
( 'a': 5, 'b': 10 )
( 'a': 4, 'b': 8 )
( 'a': 2, 'b': 4 )
::
]


