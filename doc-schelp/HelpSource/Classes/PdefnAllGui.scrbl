#lang scribble/manual
@(require (for-label racket))

@title{PdefnAllGui}
 show all current Pdefns as code@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/Pdefn, Classes/PdefnGui

@section{description}

PdefnAllGui displays all current Pdefns as code.
See also TdefAllGui for general info, search functions, etc.

@section{CLASSMETHODS}
 

@section{METHOD}
  observedClass
Pdefn

@section{METHOD}
  tpGuiClass
PdefnGui

@section{EXAMPLES}
 


@racketblock[
PdefnAllGui(();
Pdefn(\a, 1);
Pdefn(\b, [1, 2, 3]);
Pdefn(\x, Pwhite());
]


