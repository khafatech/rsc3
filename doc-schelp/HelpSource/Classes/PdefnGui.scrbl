#lang scribble/manual
@(require (for-label racket))

@title{PdefnGui}
 a simple gui for Pdefns@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/Pdefn, Classes/PdefnAllGui, Classes/TdefAllGui

@section{description}

PdefnGui displays a PdefnGui, and allows editing and evaluating its code.


@section{CLASSMETHODS}
 

@section{METHOD}
  observedClass
Pdefn

@section{INSTANCEMETHODS}
 

strong::JITGui methods: ::
@section{METHOD}
  accepts
test whether object can be displayed

@section{METHOD}
  getState, checkUpdate

@section{EXAMPLES}
 


@racketblock[
g = PdefnGui();
Pdefn(\abc, [1, 2, 3]);
g.object_(Pdefn(\abc));
Pdefn(\abc, 345);

// Note: When editing code in the csView and evaluating,
// there is a short delay before displaying. this is intended.

]


