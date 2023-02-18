#lang scribble/manual
@(require (for-label racket))

@title{SynthDescLib}
 SynthDesc library@section{categories}
  Server>Nodes
@section{related}
  Classes/SynthDesc

@section{description}

See link::Classes/SynthDesc::

@section{INSTANCEMETHODS}
 

@section{method}
  add
Add a SynthDesc to this SynthDescLib. Doing this triggers an update message with the key 
@racketblock[\synthDescAdded:: for any dependants this lib may have. See link::Classes/Object#Dependancy::.
]
@section{ARGUMENT}
  synthdesc 
The link::Classes/SynthDesc:: to be added.

