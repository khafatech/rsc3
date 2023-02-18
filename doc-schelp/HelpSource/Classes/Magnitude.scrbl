#lang scribble/manual
@(require (for-label racket))

@title{Magnitude}
 Comparable value in a linear continuum@section{categories}
  Math

@section{description}

Magnitudes represent values along a linear continuum which can be compared against each other.

@section{instancemethods}
 
@section{method}
  <
@section{returns}
  a link::Classes/Boolean:: whether the receiver is less than strong::aMagnitude::.

@section{method}
  <=
@section{returns}
  a link::Classes/Boolean:: whether the receiver is less than or equal to strong::aMagnitude::.

@section{method}
  >
@section{returns}
  a link::Classes/Boolean:: whether the receiver is greater than strong::aMagnitude::.

@section{method}
  >=
@section{returns}
  a link::Classes/Boolean:: whether the receiver is greater than or equal to strong::aMagnitude::.

@section{method}
  min
@section{returns}
  the minimum of the receiver and aMagnitude.

@section{method}
  max
@section{returns}
  the maximum of the receiver and aMagnitude.

@section{method}
  clip
If the receiver is less than minVal then answer minVal, else if the receiver is greater than maxVal then answer maxVal, else answer the receiver.

@section{method}
  inclusivelyBetween
@section{returns}
  whether the receiver is greater than or equal to minVal and less than or equal to maxVal.

@section{method}
  exclusivelyBetween
@section{returns}
  whether the receiver is greater than minVal and less than maxVal.



