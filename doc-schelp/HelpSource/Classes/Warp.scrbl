#lang scribble/manual
@(require (for-label racket))

@title{Warp}
 specification of a shape for a mapping of numerical input@section{related}
  Classes/ControlSpec, Classes/Spec
@section{categories}
  Control, Spec

@section{description}

The subclasses of Warp specify translations from input (a number) to an output (another number). This is an abstract class - already available shapes are emphasis::linear, exponential, sine, cosine, decibel, curve:: (this is similar to the curves in envelopes, see also link::Classes/Env::).

Warps are internally created by link::Classes/ControlSpec::. Usually they are created by the message strong::asWarp::, understood by symbols and numbers. A warp has a link::Classes/Spec:: to specify a certain range of input and output values.


@racketblock[
// create a new warp:
a = \exp.asWarp;
a = -4.asWarp; // a curve warp;
::

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{InstanceMethods}
 

@section{method}
 map
Maps and constrains a strong::value:: between 0 and 1 to the output domain.

@section{method}
 unmap
in the output domain to a value in the range between 0 and 1.


@racketblock[
g = -3.asWarp;
g.map(0.5);
g.unmap(0.9);

// fore and back translation should be identical:
g.unmap(g.map(0.5));
::
]


