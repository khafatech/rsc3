#lang scribble/manual
@(require (for-label racket))

@title{LatoocarfianN}
 Latoocarfian chaotic generator@section{categories}
  UGens>Generators>Chaotic
@section{related}
  Classes/LatoocarfianL, Classes/LatoocarfianC

@section{description}

A non-interpolating sound generator based on a function given in Clifford Pickover's book Chaos In Wonderland, pg 26. The function is:

teletype::
        x(n+1) = sin(b * y(n)) + c * sin(b * x(n))
        y(n+1) = sin(a * x(n)) + d * sin(a * y(n))
::

According to Pickover, parameters 
@racketblock[a:: and ]

@racketblock[b:: should be in the range from -3 to +3, and parameters ]

@racketblock[c:: and ]

@racketblock[d:: should be in the range from 0.5 to 1.5. The function can, depending on the parameters given, give continuous chaotic output, converge to a single value (silence) or oscillate in a cycle (tone).

sclang code translation:

]

@racketblock[
(
var a = 1, b = 3, c = 0.5, d = 0.5, xi = 0.5, yi = 0.5, size = 64;
plot(size.collect { var x = xi;
xi = sin(b * yi) + (c * sin(b * xi));
yi = sin(a * x) + (d * sin(a * yi));
xi
});
)
::

]
@section{note}
 This UGen is experimental and not optimized currently, so is rather hoggish of CPU.::

@section{classmethods}
 
@section{method}
  ar
@section{argument}
  freq
Iteration frequency in Hertz
@section{argument}
  a
Equation variable
@section{argument}
  b
Equation variable
@section{argument}
  c
Equation variable
@section{argument}
  d
Equation variable
@section{argument}
  xi
Initial value of x
@section{argument}
  yi
Initial value of y
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
// default initial params
{ LatoocarfianN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }.play(s);
::

]

@racketblock[
// randomly modulate all params
(
{ LatoocarfianN.ar(
	SampleRate.ir/4,
	LFNoise2.kr(2,1.5,1.5),
	LFNoise2.kr(2,1.5,1.5),
	LFNoise2.kr(2,0.5,1.5),
	LFNoise2.kr(2,0.5,1.5)
) * 0.2 }.play(s);
)
::
]


