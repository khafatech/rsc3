#lang scribble/manual
@(require (for-label racket))

@title{PparGroup}
 Starts a new ParGroup and plays the pattern in this group@section{related}
  Classes/ParGroup, Classes/Pgroup
@section{categories}
  Streams-Patterns-Events>Patterns>Server Control

@section{description}


The class has a semantics similar to link::Classes/Pgroup::, but instead of a Group, it creates a ParGroup on the
server.

@section{Examples}
 


@racketblock[
(
var p, q, r, o;
p = Pbind(\degree, Prand((0..7),12), \dur, 0.3, \legato, 0.2);

PparGroup(p).play;

// post the node structure:
fork {
	s.queryAllNodes;
	3.wait;
	s.queryAllNodes;
	2.wait;
	s.queryAllNodes;
}
)
::
]


