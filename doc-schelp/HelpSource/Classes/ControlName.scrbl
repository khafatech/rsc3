#lang scribble/manual
@(require (for-label racket))

@title{ControlName}
 Object to store control information of SynthDef (used internally)@section{related}
  Classes/SynthDesc, Classes/SynthDef
@section{categories}
  UGens>Synth control

@section{description}

For an explicit creation of control names see: link::Classes/NamedControl::, link::Classes/Control::

@section{examples}
 

@racketblock[
a = SynthDescLib.global; // the global library of SynthDescs
x = a.synthDescs.at(\default); // get the default SynthDesc
x.controls.do { |ctl| [\name, ctl.name, \defaultValue, ctl.defaultValue].postln };"";
::

]
@section{instancemethods}
 
@section{private}
  printOn

@section{method}
 name
The name of the control.
@section{returns}
  a link::Classes/Symbol::

@section{method}
 index
The index of the control.
@section{returns}
  an link::Classes/Integer::

@section{method}
 rate
The rate of the control.
@section{returns}
  a link::Classes/Symbol:: like 
@racketblock['audio':: or ]

@racketblock['control'::

]
@section{method}
 defaultValue
Default value of this control. Will be an link::Classes/Array:: for multichannel controls.

@section{method}
 numChannels
The number of channels.
@section{returns}
  an link::Classes/Integer::



