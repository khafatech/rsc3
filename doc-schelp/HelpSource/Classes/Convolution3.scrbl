#lang scribble/manual
@(require (for-label racket))

@title{Convolution3}
 Time based convolver.@section{related}
  Classes/Convolution, Classes/Convolution2, Classes/Convolution2L
@section{categories}
   UGens>Convolution

@section{description}

Strict convolution with fixed kernel which can be updated using a trigger signal. The convolution is performed in the time domain.


@section{note}
 
Doing convolution in time domain is highly inefficient, and probably only useful for either very short kernel sizes, or for control rate signals.
See link::Classes/Convolution2:: and link::Classes/Convolution2L:: for more efficient convolution UGens.
::

@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 in
processing target

@section{argument}
 kernel
buffer index for the fixed kernel, may be modulated in combination with the trigger

@section{argument}
 trigger
update the kernel on a change from <=0 to >0

@section{argument}
 framesize
maximum size of the buffer containing the kernel

@section{argument}
  mul

@section{argument}
  add


