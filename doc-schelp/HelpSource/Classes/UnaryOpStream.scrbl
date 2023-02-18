#lang scribble/manual
@(require (for-label racket))

@title{UnaryOpStream}
 stream modified by a unary operator@section{related}
  Classes/BinaryOpStream, Classes/NAryOpStream
@section{categories}
  Streams-Patterns-Events

@section{description}


A UnaryOpStream is created as a result of a unary math operation on a Stream. It is defined to respond to strong::next:: by returning the result of the math operation on the strong::next:: value from the stream. It responds to strong::reset:: by resetting the Stream.

@section{Examples}
 


@racketblock[
x = Routine { 6.do { arg i; i.yield; } }.squared;
x.dump;
::

]

@racketblock[
(
x = Routine { 6.do { arg i; i.yield; } }.squared;
x.next.postln;
x.next.postln;
x.next.postln;
x.next.postln;
x.next.postln;
x.next.postln;
x.next.postln;
)
::
]


