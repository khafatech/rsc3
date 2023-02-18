#lang scribble/manual
@(require (for-label racket))

@title{Sanitize}
 Remove infinity, NaN, and denormals@section{categories}
  UGens>Info

@section{description}

Replaces infinities, NaNs, and subnormal numbers (denormals) with a given signal, zero by default. The method link::Classes/UGen#-sanitize:: provides a shorthand for this.

See also link::Classes/CheckBadValues::, which allows you to discriminate specific kinds of bad values and print information about them to the post window.

@section{classmethods}
 

@section{method}
  ar, kr
@section{argument}
  in
Input signal to sanitize.

@section{argument}
  replace
The signal that replaces bad values.



