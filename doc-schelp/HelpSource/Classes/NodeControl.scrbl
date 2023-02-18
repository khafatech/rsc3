#lang scribble/manual
@(require (for-label racket))

@title{NodeControl}
 Encapsulates in an object a node and an index.@section{categories}
  Server>Nodes

@section{description}

This object can be held by a client and have its value set without otherwise having to store the details about where the node's input is.

@section{classmethods}
 
@section{method}
  new
@section{argument}
  node
The node to encapsulate
@section{argument}
  index
The index to encapsulate

@section{instancemethods}
 
@section{method}
  value
set the value


@section{examples}
 

@racketblock[
d = SynthDef("help-NodeControl",{ arg out=0,freq=400;
	Out.ar(out,
		 SinOsc.ar(freq, 0, 0.5)
	)
});
y = d.play; // the synth

c = NodeControl(y,1);

c.value = 500;

c.value = 300;
::

]


