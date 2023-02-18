#lang scribble/manual
@(require (for-label racket))

@title{OutputProxy}
 Place holder for multiple outputs@section{categories}
  UGens>Base

@section{description}


OutputProxy is used by some UGens as a place holder for multiple outputs.
There is no reason for a user to create an OutputProxy directly.


@racketblock[
var out;
// Pan2 uses an OutputProxy for each of its two outputs.
out = Pan2.ar(WhiteNoise.ar, 0.0);
out.postln;
::

]
@section{classmethods}
 
@section{private}
  categories

@section{InstanceMethods}
 

@section{method}
  source
The UGen that is the source for this OutputProxy.
@section{discussion}
 

@racketblock[
var left, right;
// Pan2 uses an OutputProxy for each of its two outputs.
# left, right = Pan2.ar(WhiteNoise.ar, 0.0);
left.source.postln;
::

The ]

@racketblock[source:: method is also defined in Array, so that the source can be obtained this way as well:

]

@racketblock[
var out;
// Pan2 uses an OutputProxy for each of its two outputs.
out = Pan2.ar(WhiteNoise.ar, 0.0);
out.postln;
out.source.postln;
::

]


