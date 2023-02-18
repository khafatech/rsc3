#lang scribble/manual
@(require (for-label racket))

@title{Filter}
 Base class for filter UGens@section{categories}
  UGens>Filters

@section{description}


"Filter" is an abstract class - in other words, a class that you do not use directly. Instead, use one of its subclasses. Many common filters inherit from this abstract class, including LPF, HPF, MoogFF, Ringz, Integrator, Median, LeakDC... and many more.

The Filter class also provides a simple way to visualise the frequency-wise effect of applying a filter, see 
@racketblock[scopeResponse:: below.

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
  scopeResponse
Provides a simple way to visualise the frequency-wise effect of applying a filter
@section{discussion}
 

@racketblock[
s.boot // boot the server 
MoogFF.scopeResponse
HPF.scopeResponse
BRF.scopeResponse
Median.scopeResponse
::

]
@section{instancemethods}
 
@section{private}
  checkInputs



