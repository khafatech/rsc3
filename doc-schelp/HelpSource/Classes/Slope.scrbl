#lang scribble/manual
@(require (for-label racket))

@title{Slope}
 Slope of signal@section{categories}
  UGens>Analysis, UGens>Filters>Linear, UGens>Maths

@section{description}

Measures the rate of change per second of a signal.
Formula implemented is:


@racketblock[
out[i] = (in[i] - in[i-1]) * sampling_rate
::

]
@section{classmethods}
 
@section{method}
  ar, kr
@section{argument}
 in
Input signal to measure.
@section{argument}
 mul
@section{argument}
 add

@section{examples}
 

@racketblock[
(
{
    var a, b, c, scale;
    a = LFNoise2.ar(2000);  // quadratic noise
    b = Slope.ar(a);        // first derivative produces line segments
    c = Slope.ar(b);        // second derivative produces constant segments
    scale = 0.0002; // needed to scale back to +/- 1.0
    [a, b * scale, c * scale.squared]
}.plot
)
::

For another example of Slope see link::Classes/AbstractFunction#hypot#AbstractFunction:hypot::.

]


