#lang scribble/manual
@(require (for-label racket))

@title{BinaryOpStream}
 two streams combined by a binary operator@section{related}
  Classes/UnaryOpStream, Classes/NAryOpStream
@section{categories}
  Streams-Patterns-Events

@section{description}


A BinaryOpStream is created as a result of a binary math operation on a pair of Streams. It is defined to respond to strong::next:: by returning the result of the math operation on the strong::next:: value from both streams. It responds to strong::reset:: by resetting both Streams.

@section{Examples}
 


@racketblock[
x = Routine { 6.do { arg i; i.yield; } } + 64;
x.dump
::

]

@racketblock[
(
x = Routine { 6.do { arg i; i.yield; } } + 64;
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

@racketblock[
(
x = Routine { 6.do { arg i; i.yield; } } + Routine { (1..7).do { arg i; (1 / i).yield; } };
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


