#lang scribble/manual
@(require (for-label racket))

@title{PureUGen}
 Pure UGen@section{categories}
  UGens
@section{related}
  Classes/UGen

@section{description}


A Pure UGen is a UGen, which does not access any shared resources like busses, buffers or random number generators. UGen
classes which are derived from PureUGen are candidates for common subexpression elimination and dead code elimination
passes during the SynthDef compilation.

@section{INSTANCEMETHODS}
 
@section{PRIVATE}
  optimizeGraph


