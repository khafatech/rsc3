#lang scribble/manual
@(require (for-label racket))

@title{DebugNodeWatcher}
 watches a server address for debug-related messages@section{related}
  Reference/Server-Command-Reference, Classes/Node, Classes/NodeWatcher
@section{categories}
  Control, Server>Nodes

@section{description}

Posts when these messages are received from the server:
n_go n_end n_off n_on

For debugging, it can be useful to see every node start and end. It doesn't require registration, reacts to each message.

@section{Examples}
 


@racketblock[
s.boot;

d = DebugNodeWatcher(s);
d.start;

y = Group.new;
y.run(false);
y.free;

d.stop;
::
]


