#lang scribble/manual
@(require (for-label racket))

@title{InterplEnv}
 envelope specification@section{related}
  Classes/IEnvGen, Classes/Env
@section{categories}
  Control, Envelopes

@section{note}
 Env fully supports all functionality of InterplEnv, InterplXYC, InterplPairs and InterplChord. These are now deprecated and  will be removed in the future.::

@section{description}

An InterplEnv is a specification for a segmented envelope. InterplEnvs can be used both server-side, by an link::Classes/IEnvGen:: within a SynthDef, and clientside, with methods such as at. An InterplEnv can have any number of segments. An InterplEnv can have several shapes for its segments.


@section{subsection}
 Differences between InterplEnv and Env
InterplEnvs do not have release or loop nodes.  They are of a fixed duration. Mostly, it is meant to be used with IEnvGen, where 'times' are actually an strong::index into the envelope:: shape.

@section{ClassMethods}
 

@section{private}
 initClass, new

@section{InstanceMethods}
 

@section{private}
 prAsArray


