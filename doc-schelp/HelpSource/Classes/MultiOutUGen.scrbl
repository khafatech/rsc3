#lang scribble/manual
@(require (for-label racket))

@title{MultiOutUGen}
 Superclass for all UGens with multiple outputs@section{categories}
  UGens>Base
@section{related}
  Classes/OutputProxy

@section{description}

This is a superclass for all UGens with multiple outputs.
MultiOutUGen creates the link::Classes/OutputProxy:: ugens needed for the multiple outputs.

@section{classmethods}
 
@section{private}
  categories

@section{instancemethods}
 

@section{method}
  initOutputs
Create an array of OutputProxies for the outputs.



