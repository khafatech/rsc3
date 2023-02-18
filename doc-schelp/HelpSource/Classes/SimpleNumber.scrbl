#lang scribble/manual
@(require (for-label racket))

@title{SimpleNumber}
 one-dimensional value@section{categories}
  Math
@section{related}
 Classes/Polar, Classes/Complex, Classes/Float, Classes/Integer, Classes/UnaryOpUGen, Classes/BinaryOpUGen

@section{description}

Base class for numbers which can be represented by a single one dimensional value.

Most of the Unary and Binary operations are also implemented by link::Classes/UnaryOpUGen:: and link::Classes/BinaryOpUGen::, so you can get more examples by looking at the help for those.

@section{CLASSMETHODS}
 

@section{method}
  new
allocates a new SimpleNumber.

@section{INSTANCEMETHODS}
 

@section{private}
  prSimpleNumberSeries

@section{subsection}
  math support

@section{method}
  +
Addition


@section{method}
  -
Subtraction

@section{method}
  *
Multiplication

@section{method}
  /
Division

@section{method}
  %
Modulo

@section{method}
  mod
Modulo

@section{method}
  div
Integer Division

@section{method}
  **
Exponentiation

@section{method}
  !=
Is not

@section{method}
  >
greater than

@section{method}
  <
greater than

@section{method}
  >=
greater or equal than

@section{method}
  <=
smaller or equal than

@section{method}
  lcm
Least common multiple

@section{method}
  gcd
Greatest common divisor

@section{method}
  round
Round to multiple of aNumber
@section{method}
  roundUp
round up to a multiply of aNumber

@section{method}
  thresh

@section{method}
  min
Minimum

@section{method}
  max
Maximum

@section{method}
  wrap2

@section{method}
  trunc
Truncate to multiple of aNumber

@section{method}
  atan2
Arctangent of (this/aNumber)

@section{method}
  hypot
Square root of the sum of the squares.


@section{method}
  log
@section{returns}
  Base e logarithm.

@section{method}
  log2
@section{returns}
  Base 2 logarithm.

@section{method}
  log10
@section{returns}
  Base 10 logarithm.

@section{method}
  neg
@section{returns}
  negation

@section{method}
  abs
@section{returns}
  absolute value.

@section{method}
  sign
@section{returns}
  Answer -1 if negative, +1 if positive or 0 if zero.

@section{method}
  ceil
@section{returns}
  next larger integer.

@section{method}
  floor
@section{returns}
  next smaller integer

@section{method}
  sin
Sine

@section{method}
  cos
Cosine

@section{method}
  tan
Tangent

@section{method}
  asin
Arcsine

@section{method}
  acos
Arccosine

@section{method}
  atan
Arctangent

@section{method}
  sinh
Hyperbolic sine

@section{method}
  cosh
Hyperbolic cosine

@section{method}
  tanh
Hyperbolic tangent

@section{method}
  frac
fractional part

@section{method}
  squared
the square of the number

@section{method}
  cubed
the cube of the number

@section{method}
  sqrt
the square root of the number.

@section{method}
  exp
e to the power of the receiver.

@section{method}
  reciprocal
1 / this

@section{method}
  pow
this to the power of aNumber

@section{method}
  fold2
the folded value, a bitwise or with aNumber

@section{method}
  previousPowerOf
the number relative to this that is the previous power of aNumber

@section{method}
  nextPowerOf
the next power of aNumber

@section{method}
  nextPowerOfTwo
@section{returns}
  the number relative to this that is the next power of 2

@section{method}
  nextPowerOfThree
the next power of three

@section{method}
  hash
@section{returns}
  a hash value

@section{method}
  <!
@section{returns}
  the receiver. aNumber is ignored.

@section{method}
  &
Bitwise And

@section{method}
 |
Bitwise Or

@section{method}
  bitXor
Bitwise Exclusive Or

@section{method}
  bitHammingDistance
Binary Hamming distance: the count of bits that are not the same in the two numbers

@section{method}
  bitTest
@section{returns}
  true if bit at index aNumber is set.

@section{method}
  bitNot
@section{returns}
  ones complement

@section{method}
  <<
Binary shift left.

@section{method}
  >>
Binary shift right.

@section{method}
  +>>
Unsigned binary shift right.

@section{method}
  rightShift
@section{returns}
  performs a binary right shift

@section{method}
  unsignedRightShift
@section{returns}
  performs an unsigned right shift

@section{method}
  leftShift
@section{returns}
  performs a binary left shift

@section{method}
  bitOr
@section{returns}
  performs a bitwise or with aNumber

@section{method}
  bitAnd
@section{returns}
  performs a bitwise and with aNumber

@section{method}
  ring1
(a * b) + a

@section{method}
  ring2
((a*b) + a + b)

@section{method}
  ring3
(a * a *b)

@section{method}
  ring4
((a*a *b) - (a*b*b))

@section{method}
  difsqr
(a*a) - (b*b)

@section{method}
  sumsqr
(a*a) + (b*b)

@section{method}
  sqrdif
(a - b) ** 2

@section{method}
  sqrsum
(a + b) ** 2

@section{method}
  absdif
(a - b).abs

@section{method}
  moddif
On a circle, there are two distances between two points. This operator returns the smaller value of the two.

@racketblock[
moddif(0.75, 0, 1)
::

]
@section{method}
  amclip
0  when  b <= 0,  a*b  when  b > 0

@section{method}
  scaleneg
a * b when a < 0, otherwise a.

@section{method}
  clip2
clips receiver to +/- aNumber

@section{method}
  excess
Returns the difference of the receiver and its clipped form.
@section{discussion}
 

@racketblock[
(a - clip2(a,b))
::

]
@section{method}
  madd

@racketblock[
this * a + b
::

]
@section{subsection}
  testing
@section{method}
  isPositive
Answer if the number is >= 0.

@section{method}
  isNegative
Answer if the number is < 0.

@section{method}
  isStrictlyPositive
Answer if the number is > 0.

@section{method}
  booleanValue
@section{returns}
  true, if strictly positive ( > 0), otherwise false (see link::Classes/Boolean::)

@section{method}
  isNaN
@section{method}
  ==

@section{subsection}
  conversion

@section{method}
  asFraction
@section{argument}
 denominator
@section{argument}
 fasterBetter
if true, asFraction may find a much closer approximation and do it faster.
@section{returns}
  an array of denominator and divisor of the nearest and smallest fraction

@section{method}
  asAudioRateInput
Converts this into an audiorate input.

@section{method}
  asTimeString
Compile a time string.
@section{argument}
  precision
how accurate
@section{argument}
 maxDays
the maximum number of days
@section{argument}
 dropDaysIfPossible
a link::Classes/Boolean::
@section{returns}
  a string corresponding to the hours:minutes:seconds based on the receiver as number of seconds
@section{discussion}
 

@racketblock[
(
var start;
start = Main.elapsedTime;
{ loop({(Main.elapsedTime - start).asTimeString.postln; 0.05.wait}) }.fork;
)
::

]
@section{method}
  asPoint
@section{returns}
  this as link::Classes/Point::. x = y = this.

@section{method}
  asComplex
@section{returns}
  this as link::Classes/Point::. x = y = this.

@section{method}
  asWarp
@section{argument}
 spec
a link::Classes/ControlSpec::
@section{returns}
  this as link::Classes/CurveWarp:: according to spec.

@section{method}
  asFloat
@section{returns}
  this as link::Classes/Float::

@section{method}
  asRect
@section{returns}
  a link::Classes/Rect:: with x = y = w = h = this.

@section{method}
  asBoolean
@section{returns}
  this as a link::Classes/Boolean::.  this > 0

@section{method}
  asQuant
@section{returns}
  the values as link::Classes/Quant::

@section{method}
  asInteger
@section{returns}
  this as link::Classes/Integer::

@section{subsection}
  timing

@section{method}
 wait
within a routine, yield the number so that the clock can wait for this many beats. Outside a Routine, this trows an error (see also Routine for details).

@section{discussion}
 
Create a routine by a function fork

@racketblock[
(
fork {
	1.wait;
	"I did wait".postln;
	1.0.rand.wait;
	"No you didn't".postln;
	2.wait;
	(1..).do { |i|
		"yes I did".postln;
		i.asFloat.rand.wait;
		"no you didn't".postln;
		i.wait
	}
}
)
::

]
@section{method}
  waitUntil
like wait, only specify a time (measured in beats of the current thread's clock). Outside a Routine, this trows an error (see also Routine for details).

@section{method}
  sleep
make the current thread sleep, until woken up by re-scheduling. Outside a Routine, this trows an error (see also Routine for details).

@section{method}
  nextTimeOnGrid
@section{argument}
 clock
@section{returns}
  the next possible multiple of the clock's beats.

@section{method}
  schedBundleArrayOnClock



@section{subsection}
  series and arrays

@section{method}
  nearestInList
@section{returns}
  the value in the list closest to this

@section{discussion}
 

@racketblock[
(
l = [0, 0.5, 0.9, 1];
(0, 0.05..1).collect { |i| i.nearestInList(l) }
)
::

]
@section{method}
  nearestInScale
@section{argument}
  scale
an array of SimpleNumbers each treated as a step in the octave.
@section{argument}
  stepsPerOctave
12 by default
@section{returns}
  the value in the collection closest to this, assuming an octave repeating table of note values.

@section{discussion}
 

@racketblock[
(
l = [0, 1, 5, 9, 11]; // pentatonic scale
(60, 61..76).collect { |i| i.nearestInScale(l, 12) }
)
::

]
@section{method}
  series
return an arithmetic series from this over second to last.
@section{discussion}
 
This is used in the shortcuts:

@racketblock[
(0..100);
(1, 3 .. 17)
::
If second is nil, it is one magnitude step towards last (1 or -1).
Examples:
]

@racketblock[
series(5, 7, 10);
series(5, nil, 10);
(5, 7 .. 10)
::

]
@section{method}
  seriesIter
@section{returns}
  a Routine that iterates over the numbers from this to last.

@section{discussion}
 
Since this is a lazy operation, last may be inf, generating an endless series
(see also link::Guides/ListComprehensions::)

@racketblock[
r = seriesIter(0, 5);
r.nextN(8);
r.nextN(8);
::


]
@section{subsection}
  windowing

@section{method}
  rectWindow
@section{returns}
  a value for a rectangular window function between 0 and 1.

@section{method}
  hanWindow
@section{returns}
  a value for a hanning window function between 0 and 1.

@section{method}
  welWindow
@section{returns}
  a value for a welsh window function between 0 and 1.

@section{method}
  triWindow
@section{returns}
  a value for a triangle window function between 0 and 1.

@section{subsection}
  mapping

@section{method}
  distort
a nonlinear distortion function.

@section{method}
  softclip
Distortion with a perfectly linear region from -0.5 to +0.5

@section{method}
  scurve
Map receiver in the onto an S-curve.
@section{discussion}
 

@racketblock[
((0..100) / 100 ).collect(_.scurve).plot
::

]
@section{method}
  ramp
Map receiver onto a ramp starting at 0.
@section{discussion}
 

@racketblock[
((-100..100) / 100 ).collect(_.ramp).plot
::

]
@section{method}
 magnitude
@section{returns}
  absolute value (see link::Classes/Polar::, link::Classes/Complex::)

@section{method}
 angle
@section{returns}
  angle of receiver conceived as link::Classes/Polar:: or link::Classes/Complex:: number.


@section{method}
  degreeToKey
@section{argument}
  scale
an array of SimpleNumbers each treated as a step in the octave.
@section{argument}
  stepsPerOctave
12 is the standard chromatic scale.
@section{discussion}
 
the value is truncated to an integer and used as an index into an octave repeating table of note values. Indices wrap around the table and shift octaves as they do.


@racketblock[
(
l = [0, 1, 5, 9, 11]; // pentatonic scale
(1, 2..15).collect{|i|
	i.degreeToKey(l, 12)
};
)
::

]
@section{method}
  keyToDegree
inverse of degreeToKey.
@section{argument}
  scale
an array of SimpleNumbers each treated as a step in the octave.
@section{argument}
  stepsPerOctave
12 is the standard chromatic scale.
@section{discussion}
 

@racketblock[
(
l = [0, 1, 5, 9, 11]; // pentatonic scale
(60, 61..75).collect { |i| i.keyToDegree(l, 12) }
)
::
]

@racketblock[
(
l = [0, 1, 5, 9, 11]; // pentatonic scale
(60, 61..75).postln.collect { |i| i.keyToDegree(l, 12).degreeToKey(l) }
)
::



]
@section{method}
 gaussCurve
map the receiver onto a gauss function.

@section{discussion}
 
Uses the formula:

@racketblock[
a * (exp(squared(this - b) / (-2.0 * squared(c)))) Default values: a = 1; b = 0; c = 1
::
Example code
]

@racketblock[
(0..1000).normalize(-10, 10).collect { |num| num.gaussCurve }.plot;
::


]
@section{method}
  equalWithPrecision

@section{argument}
 that
the number to compare with within precision

@section{argument}
 precision
The absolute precision, independent of the value compared

@section{argument}
 relativePrecision
The precision relative to the larger absolute of the values compared.


@section{returns}
  true if receiver is closer to that than precision.

@section{discussion}
 

@racketblock[
3.1.equalWithPrecision(3.0, 0.05); // false
3.1.equalWithPrecision(3.0, 0.1); // false
3.1.equalWithPrecision(3.0, 0.11); // true
3000.1.equalWithPrecision(3000.0, 0, 0.01); // true
3.1.equalWithPrecision(3.0, 0, 0.01); // false

::



]
@section{method}
  quantize
round the receiver to the quantum.
@section{argument}
 quantum
amount.
@section{argument}
 tolerance
allowed tolerance.
@section{argument}
 strength
Determines how much the value is allowed to differ in the tolerance range.
@section{discussion}
 

@racketblock[
((0..10) / 10).collect { |num| num.quantize(1, 0.3, 0.5) }.postcs.plot;
((0..10) / 10).collect { |num| num.quantize(1, 0.6, 0.5) }.postcs.plot;
((0..10) / 10).collect { |num| num.quantize(1, 1.0, 0.5) }.postcs.plot;
::

]
@section{method}
  linlin
map the receiver from an assumed linear input range to a linear output range. If the input exceeds the assumed input range, the behaviour is specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).

@section{discussion}
 

@racketblock[
(0..10).collect { |num| num.linlin(0, 10, -4.3, 100) };
(0..10).linlin(0, 10, -4.3, 100); // equivalent.
::

]
@section{method}
 linexp
map the receiver from an assumed linear input range (inMin..inMax) to an exponential output range (outMin..outMax). The output range must not include zero. If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
(0..10).collect { |num| num.linexp(0, 10, 4.3, 100) };
(0..10).linexp(0, 10, 4.3, 100); // equivalent.
::

]
@section{method}
 explin
map the receiver from an assumed exponential input range (inMin..inMax) to a linear output range (outMin..outMax). If the input exceeds the assumed input range. The input range must not include zero.
If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
(1..10).collect { |num| num.explin(0.1, 10, -4.3, 100) };
(1..10).explin(0.1, 10, -4.3, 100); // equivalent.
::

]
@section{method}
 expexp
map the receiver from an assumed exponential input range (inMin..inMax) to an exponential output range (outMin..outMax). If the input exceeds the assumed input range. Both input range and output range must not include zero.
If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
(1..10).collect { |num| num.expexp(0.1, 10, 4.3, 100) };
(1..10).expexp(0.1, 10, 4.3, 100); // equivalent.
::

]
@section{method}
 lincurve
map the receiver from an assumed linear input range (inMin..inMax) to an exponential curve output range (outMin..outMax). A curve is like the curve parameter in Env. Unlike with linexp, the output range may include zero.
If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 curve
0 (linear) <0 (concave, negatively curved) >0 (convex, positively curved)
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
(0..10).collect { |num| num.lincurve(0, 10, -4.3, 100, -3) };
(0..10).lincurve(0, 10, -4.3, 100, -3); // equivalent.
::
]

@racketblock[
// different curves:
(-4..4).do { |val|
	(0..100).collect(_.lincurve(0, 100, 0, 1, val)).plot
}
::

]
@section{method}
 curvelin
map the receiver from an assumed curve-exponential input range (inMin..inMax) to a linear output range (outMin..outMax). If the input exceeds the assumed input range. A curve is like the curve parameter in Env. Unlike with explin, the input range may include zero. If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 curve
0 (linear) <0 (concave, negatively curved) >0 (convex, positively curved)
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).

@section{discussion}
 

@racketblock[
(1..10).collect { |num| num.curvelin(0, 10, -4.3, 100, -3) };
(1..10).curvelin(0, 10, -4.3, 100, -3); // equivalent.
::
]

@racketblock[
// different curves:
(-4..4).do { |val|
	(0..100).collect(_.curvelin(0, 100, 0, 1, val)).plot
}
::

]
@section{method}
 bilin
map the receiver from two assumed linear input ranges (inMin..inCenter) and (inCenter..inMax) to two linear output ranges (outMin..outCenter) and (outCenter..outMax). If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inCenter
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outCenter
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
var center = 0.5, ctlCenter;
w = Window("bilin", Rect(100, 100, 200, 100)).front;
a = Slider(w, Rect(20, 20, 150, 20)).value_(0.5);
b = Slider(w, Rect(20, 45, 150, 20)).value_(0.5);
b.action = { center = b.value };
a.mouseDownAction = { ctlCenter = a.value };
a.action = {
	b.value = a.value.bilin(ctlCenter, 0, 1, center, 0, 1);
};
::


]
@section{method}
 biexp
map the receiver from two assumed exponential input ranges (inMin..inCenter) and (inCenter..inMax) to two linear output ranges (outMin..outCenter) and (outCenter..outMax). The input range must not include zero. If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inCenter
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outCenter
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).

@section{discussion}
 

@racketblock[
// doesn't properly work yet.
(
var center = 0.5, ctlCenter;
w = Window("biexp", Rect(100, 100, 200, 100)).front;
a = Slider(w, Rect(20, 20, 150, 20)).value_(0.5);
b = Slider(w, Rect(20, 45, 150, 20)).value_(0.5);
b.action = { center = b.value };
a.mouseDownAction = { ctlCenter = a.value + 0.05 };
a.action = {
	b.value = (a.value + 0.1).biexp(ctlCenter, 0.1, 1.1, center, 0, 1);
};
)
::

]
@section{method}
 lcurve
map the receiver onto an L-curve.

@section{discussion}
 
Uses the formula

@racketblock[
a * (m * exp(x) * rTau + 1) / (n * exp(x) * rTau + 1)
::
This is used for smoothing values and limiting them to a range.
]

@racketblock[
(0..1000).normalize(-10, 10).collect { |num| num.lcurve }.plot;
::


]
@section{method}
  degrad
@section{returns}
  converts degree to radian

@section{method}
  raddeg
@section{returns}
  converts radian to degree

@section{method}
  midicps
Convert MIDI note to cycles per second
@section{returns}
  cycles per second

@section{method}
  cpsmidi
Convert cycles per second to MIDI note.
@section{returns}
  midi note


@section{method}
  midiratio
Convert an interval in semitones to a ratio.
@section{returns}
  a ratio

@section{method}
  ratiomidi
Convert a ratio to an interval in semitones.
@section{returns}
  an interval in semitones

@section{method}
  ampdb
Convert a linear amplitude to decibels.


@section{method}
  dbamp
Convert a decibels to a linear amplitude.

@section{method}
  octcps
Convert decimal octaves to cycles per second.

@section{method}
  cpsoct
Convert cycles per second to decimal octaves.


@section{subsection}
  streams

@section{method}
  storeOn
stores this on the given stream
@section{method}
  printOn
prints this on the given stream

@section{subsection}
  random

@section{method}
  coin
Answers a Boolean which is the result of a random test whose probability of success in a range from zero to one is this.

@section{method}
  rand
@section{returns}
  Random number from zero up to the receiver, exclusive.

@section{method}
  rand2
@section{returns}
  a random number from -this to +this.

@section{method}
  rrand
@section{argument}
 aNumber
the upper limit
@section{argument}
 adverb
@section{returns}
  a random number in the interval ]a, b[.
@section{discussion}
 
If both a and b are link::Classes/Integer:: then the result will be an link::Classes/Integer::.

@section{method}
  linrand
@section{returns}
  a linearly distributed random number from zero to this.

@section{method}
  bilinrand
@section{returns}
  Bilateral linearly distributed random number from -this to +this.

@section{method}
  sum3rand
This was suggested by Larry Polansky as a poor man's gaussian.
@section{returns}
  A random number from -this to +this that is the result of summing three uniform random generators to yield a bell-like distribution.

@section{method}
  exprand
an exponentially distributed random number in the interval ]a, b[. This is always a link::Classes/Float::.
(Note that the distribution of numbers is not exactly an EMPHASIS::exponential distribution::, since that would be unbounded: we might call it a EMPHASIS::logarithmic uniform distribution::.)
@section{argument}
 aNumber
the upper limit
@section{argument}
 adverb

@section{method}
  gauss
a gaussian distributed random number.
@section{argument}
 standardDeviation
the upper limit
@section{discussion}
 
Always returns a link::Classes/Float::.

@racketblock[
(0..1000).collect { |num| gauss(0.0, num) }.plot;
::

]
@section{method}
  partition
randomly partition a number into parts of at least min size.
@section{argument}
  parts
number of parts
@section{argument}
  min
the minimum size

@section{discussion}
 

@racketblock[
75.partition(8, 3);
75.partition(75, 1);
::


]
@section{subsection}
  UGen Compatibility Methods

Some methods to ease the development of generic ugen code.

@section{method}
  lag, lag2, lag3, lagud, lag2ud, lag3ud, slew, varlag

@section{returns}
  
@racketblock[this::


]
@section{subsection}
  misc

@section{method}
  isValidUGenInput
@section{returns}
  false if receiver cannot be used in UGen.


