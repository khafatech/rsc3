#lang scribble/manual
@(require (for-label racket))

@title{Pcollect}
 Apply a function to a pattern@section{categories}
  Streams-Patterns-Events>Patterns>Filter
@section{related}
  Classes/Pselect, Classes/Preject

@section{description}

Modifies each value by passing it to the function.


@section{classmethods}
 

@section{method}
  new
@section{argument}
  func
A link::Classes/Function::. Receives values from 
@racketblock[pattern::.
]
@section{argument}
  pattern
A link::Classes/Pattern::.


@section{examples}
 

@racketblock[
(
a = Pcollect( { |item| item * 3 }, Pseq( #[ 1, 2, 3 ], inf ) );
x = a.asStream;
9.do( { x.next.postln; } );
)
::

The message ]

@racketblock[collect:: returns a Pcollect when passed to a pattern. Note that because the pattern is converted to a link::Classes/Stream:: (more precisely a link::Classes/FuncStream::) the collect function is evaluated for one item each time the message ]

@racketblock[next:: is passed.
]

@racketblock[
(
a = Pseq( #[ 1, 2, 3 ], inf ).collect( { arg item; item * 3 } );
a.postln;

x = a.asStream;
9.do( { x.next.postln; } );
)
::
]


