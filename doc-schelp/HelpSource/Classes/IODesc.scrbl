#lang scribble/manual
@(require (for-label racket))

@title{IODesc}
 Description of SynthDesc input or output@section{categories}
  Server>Nodes
@section{related}
  Classes/SynthDesc

@section{description}

IODesc describes an input or output of a SynthDesc, as returned by link::Classes/SynthDesc#-outputs:: and link::Classes/SynthDesc#-inputs::

@section{classmethods}
 
@section{private}
  new

@section{instancemethods}
 
@section{private}
  printOn

@section{method}
  rate
A link::Classes/Symbol:: for the rate.

@section{method}
  numberOfChannels
The number of channels.

@section{method}
  startingChannel
This can either be a link::Classes/String::, a link::Classes/Float:: or an link::Classes/UGen::.
@section{table}
 
## String || The name of the control that provides the bus index
## Float || A hard-coded bus index
## UGen || The UGen providing the bus index
::

@section{method}
  type
The class of the input/output ugen, like link::Classes/In::, link::Classes/Out::, link::Classes/ReplaceOut::, etc.



