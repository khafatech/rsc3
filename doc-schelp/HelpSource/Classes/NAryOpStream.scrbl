#lang scribble/manual
@(require (for-label racket))

@title{NAryOpStream}
 several streams combined by an n-ary operator@section{related}
  Classes/UnaryOpStream, Classes/BinaryOpStream
@section{categories}
  Streams-Patterns-Events

@section{description}


A NAryOpStream is created as a result of a n-ary math operation on a Stream. It is defined to respond to strong::next:: by returning the result of the math operation on the strong::next:: value from the stream. It responds to strong::reset:: by resetting the Stream.

@section{Examples}
 


@racketblock[
x = Routine { 6.do { arg i; i.yield; } }.wrap(0, 3);
x.dump;
::

]

@racketblock[
(
x = Routine { 6.do { arg i; i.yield; } }.wrap(0, 3);
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


