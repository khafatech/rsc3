#lang scribble/manual
@(require (for-label racket))

@title{RootNode}
 The persistent root group on the server@section{categories}
  Server>Nodes
@section{related}
  Classes/Group, Reference/default_group

@section{description}

A RootNode is the Group with the nodeID of 0 which is always present on each Server and represents the root of that server's node tree.

It is always playing, and always running, cannot be freed, or moved anywhere.

Caching is used so that there is always one RootNode per link::Classes/Server::.

@racketblock[
s = Server.local;

a = RootNode(s);
b = RootNode(s);

a === b; // identical object
::
sending ]

@racketblock["/s_new":: messages to the server, the target 0 is what is represented by this object.
]

@racketblock[
s.sendMsg("/s_new", "default", -1, 0, 0);//the last argument is the target id
::
IMPORTANT: In general one should strong::not:: add nodes to the RootNode unless one has a specific reason to do so. Instead one should add nodes to the default_group.
This provides a known basic node order and protects functionality like Server.record, Server.scope, etc. The default group is the default target for all new nodes, so when using object style nodes will normally not be added to the RootNode unless that is explicitly specified. See link::Reference/default_group:: for more information.

]
@section{classmethods}
 
@section{private}
  initClass
@section{instancemethods}
 
@section{private}
  rninit, free, moveBefore, moveAfter, moveToHead, moveToTail, run



