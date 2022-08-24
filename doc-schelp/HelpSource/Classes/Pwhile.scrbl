#lang scribble/manual
@(require (for-label racket))

@title{Pwhile}
 While a condition holds, repeatedly embed stream@section{categories}
  Streams-Patterns-Events>Patterns>Language Control

@section{description}

Repeatedly strong::embed:: a link::Classes/Stream:: while the result of 
@racketblock[func:: is ]

@racketblock[true::.


]
@section{classmethods}
 

@section{method}
  new
@section{argument}
  func
Stream function. In an event stream receives the current link::Classes/Event:: as argument.
@section{argument}
  pattern
A link::Classes/Pattern::.


@section{examples}
 

@racketblock[
(
z = true;
a = Pwhile({ z }, Pseq(#[1, 2, 3]));
x = a.asStream;
);

7.do({ x.next.postln; }); // while z == true, the values are embedded
z = false; // set z to false
x.next; // the rest of the stream is still embedded
x.next;
x.next; // but then it is not continued.
x.next;
x.next;
x.next;
::
]


