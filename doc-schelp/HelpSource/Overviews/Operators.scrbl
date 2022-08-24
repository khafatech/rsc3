#lang scribble/manual
@(require (for-label racket))

@title{Operators}
@section{categories}
  Language, Common methods
 common unary and binary operators@section{related}
  Reference/Adverbs

SuperCollider supports operator overloading. Operators can thus be applied to a variety of different objects; Numbers, Ugens, Collections, and so on. When operators are applied to ugens they result in link::Classes/BinaryOpUGen::s or link::Classes/UnaryOpUGen::s, through the methods of link::Classes/AbstractFunction::.

This is a list of some of the common unary and binary operators that are implemented by several classes.
See the specific classes for details and other operators.

You can see which classes implements a specific operator by clicking on the method name.

@section{section}
  Unary Operators
Unary operators may be written in two ways:

@racketblock[
a.operator
operator(a)
::


]
@section{subsection}
  Arithmetics

@section{method}
  neg
Inversion.
@section{discussion}
 

@racketblock[
{
	var a;
	a = FSinOsc.ar(300);
	[ a, a.neg ]
}.plot
::

]
@section{method}
  reciprocal
Reciprocal (1/x).

@section{method}
  abs
Absolute value.

@section{method}
  floor
Next lower integer.
@section{discussion}
 

@racketblock[
{
	var a;
	a = Line.ar(-1, 1, 0.01);
	[ a, a.floor ]
}.plot
::

]
@section{method}
  ceil
Next higher integer.
@section{discussion}
 

@racketblock[
{
	var a;
	a = Line.ar(-1, 1, 0.01);
	[ a, a.ceil ]
}.plot
::

]
@section{method}
  frac
Fractional part.

@section{method}
  sign
Sign function.
@section{returns}
 
-1 when a < 0, +1 when a > 0, 0 when a is 0

@section{method}
  squared
Squared value.
@section{returns}
 

@racketblock[ a*a ::

]
@section{method}
  cubed
Cubed value.
@section{returns}
 

@racketblock[ a*a*a ::

]
@section{method}
  sqrt
Square root.
@section{discussion}
 
The definition of square root is extended for signals so that sqrt(a) when a<0 returns
-sqrt(-a).

@section{method}
  exp
Exponential.


@section{subsection}
  Musical acoustics


@section{method}
  midicps
Convert MIDI note number to cycles per second.
@section{discussion}
 

@racketblock[
{
	Saw.ar(Line.kr(24,108,10).midicps, 0.2)
}.play
::

]
@section{method}
  cpsmidi
Convert cycles per second to MIDI note.

@section{method}
  midiratio
Convert an interval in MIDI notes into a frequency ratio.

@section{method}
  ratiomidi
Convert a frequency ratio to an interval in MIDI notes.

@section{method}
  dbamp
Convert decibels to linear amplitude.

@section{method}
  ampdb
Convert linear amplitude to decibels.

@section{method}
  octcps
Convert decimal octaves to cycles per second.

@section{method}
  cpsoct
Convert cycles per second to decimal octaves.

@section{subsection}
  Random operators

See also link::Guides/Randomness::

@section{method}
 rand
Returns an evenly distributed random value between this and zero.

@racketblock[
10.rand;

{ SinOsc.ar(110).rand }.plot;
::

]
@section{method}
 rand2
Returns an evenly distributed random value between [+this ... - this].

@racketblock[
10.rand2;

{ SinOsc.ar(110).rand2 }.plot;
::

]
@section{method}
 linrand
Returns a linearly distributed random value between this and zero.

@racketblock[
10.linrand;

{ SinOsc.ar(110).linrand }.plot;
::


]
@section{method}
 bilinrand
Returns a linearly distributed random value between [+this ... - this].

@racketblock[
10.bilinrand;

{ SinOsc.ar(110).bilinrand }.plot;
::

]
@section{method}
 sum3rand
Returns a value from a gaussian-like random distribution between this and zero.
Larry Polansky's poor man's gaussian generator, follows the formula:


@racketblock[
{ 1.0.rand }.dup(3).sum - 1.5 * (2/3)
::

]

@racketblock[
10.sum3rand;

{ SinOsc.ar(110).sum3rand }.plot;
::

]
@section{method}
 coin
Returns one or zero with the probability given by the argument.

@racketblock[
0.4.coin;

{ SinOsc.ar(110).coin }.plot;
::


]
@section{subsection}
  Other


@section{method}
  log
Natural logarithm.
@section{discussion}
 

@racketblock[
{
	var a, e;
	e = exp(1);
	a = Line.ar(e, 1/e, 0.01);
	a.log
}.plot
::

]
@section{method}
  log2
Base 2 logarithm.

@section{method}
  log10
Base 10 logarithm.

@section{method}
  sin
Sine.

@section{method}
  cos
Cosine.

@section{method}
  tan
Tangent.

@section{method}
  asin
Arcsine.

@section{method}
  acos
Arccosine.

@section{method}
  atan
Arctangent.

@section{method}
  sinh
Hyperbolic sine.

@section{method}
  cosh
Hyperbolic cosine.

@section{method}
  tanh
Hyperbolic tangent.

@section{method}
  distort
Nonlinear distortion.
@section{discussion}
 

@racketblock[
(
{
	var a;
	a = Line.ar(-4, 4, 0.01);
	a.distort
}.plot
)

{ FSinOsc.ar(500, 0.0, XLine.kr(0.1, 10, 10)).distort * 0.25 }.scope;
::

]
@section{method}
  softclip
Nonlinear distortion.
@section{discussion}
 
Distortion with a perfectly linear region from -0.5 to +0.5

@racketblock[
(
{
	var a;
	a = Line.ar(-2, 2, 0.01);
	a.softclip
}.plot
)


{ FSinOsc.ar(500,0.0, XLine.kr(0.1, 10, 10)).softclip * 0.25 }.scope(2);
::

]
@section{method}
  isPositive
Test if signal is >= 0.

@section{method}
  isNegative
Test if signal is < 0.

@section{method}
  isStrictlyPositive
Test if signal is > 0.

@section{section}
  Binary Operators
Three different syntaxes can be used for binary operators consisting of letters:

@racketblock[
operator(a, b)

a operator: b

a.operator(b)
::
Operators consisting of symbols are written like this:
]

@racketblock[
a + b
::

]
@section{subsection}
  Arithmetics

@section{method}
  +
Addition.

@section{method}
  -
Subtraction.

@section{method}
  *
Multiplication.

@section{method}
  /
Division.

@section{method}
  %
Floating point modulo.

@section{method}
  **
Exponentiation. When used with UGens which produce a negative signal this function extends the usual definition of
exponentiation and returns 
@racketblock[neg(neg(a) ** b)::. This allows exponentiation of negative signal values by noninteger exponents. For the normal behaviour use pow (see below).

]
@section{method}
  pow
Exponentiation.

@section{method}
  lcm
Least common multiple. This definition extends the usual definition and returns a negative number if strong::any of the operands:: is negative. This makes it consistent with the lattice-theoretical interpretation and its idempotency, commutative, associative, absorption laws.

Following the example of the programming language J (see: link::Guides/J-concepts-in-SC::), lcm is analogous to logical strong::and:: (see also: link::http://www.jsoftware.com/papers/eem/gcd.htm::).



@racketblock[
lcm(4, 6);
lcm(1, 1); // and
lcm(1624, 26);
lcm(1624, -26);
lcm(-1624, -26);
lcm(513, gcd(513, 44)) // absorption law -> 513.
::
]

@racketblock[
(
{
	var mx = MouseX.kr(-20, 20);
	var my = MouseY.kr(-20, 20);
	SinOsc.ar(SinOsc.kr(0.3) * 20 lcm: [mx, my] * 30 + 500) * 0.1
}.play;
)
::


]
@section{method}
  gcd
Greatest common divisor. This definition extends the usual definition and returns a negative number if strong::both operands:: are negative. This makes it consistent with the lattice-theoretical interpretation and its idempotency, commutative, associative, absorption laws.

"greater" means "divisible by" in this interpretation, so 
@racketblock[gcd(-1, -1):: returns a negative number. This is necessary to make the whole system consistent (fundamental law of arithmetics, idempotency and absorption laws would fail). See examples below.

Following the example of the programming language J (see: link::Guides/J-concepts-in-SC::), gcd is analogous to logical strong::or::  (see also: link::http://www.jsoftware.com/papers/eem/gcd.htm::).

]

@racketblock[
gcd(4, 6);
gcd(0, 1); // or
gcd(1024, 256);
gcd(1024, -256);
gcd(-1024, -256); // "greater" means "divisible by" in this case, so this returns a negative number.
gcd(-1024, lcm(-1024, 256)) // absorption law -> -1024.
gcd(66, 54) * lcm(66, 54) == (66 * 54); // true
::

]

@racketblock[
(
{
	var mx = MouseX.kr(-200, 200);
	var my = MouseY.kr(-200, 200);
	SinOsc.ar(SinOsc.kr(0.3) * 20 gcd: [mx, my] * 30 + 500) * 0.1
}.play;
)
::

Here is an overview of how negative numbers are treated:

]

@racketblock[

lcm(4, 6) // -> 12. "least multiple" interpreted as smallest in Z
lcm(4, -6) // -> -12 "least multiple" interpreted as smallest in Z
lcm(-4, -6) // -> -12 "least multiple" interpreted as smallest in Z

gcd(4, 6) // -> 2 "greatest divisor" interpreted as highest in Z
gcd(4, -6) // -> 2 "greatest divisor" is interpreted as highest in Z
gcd(-4, -6) // -> -2 "greatest divisor" is interpreted as *inverse* in Z. This is the only necessary asymmetry.
::


]
@section{subsection}
  Comparisons

@section{method}
  <
Less than.

@section{method}
  <=
Less than or equal.

@section{method}
  >
Greater than.
@section{discussion}
 
With UGens, this can be useful for triggering purposes, among other things:

@racketblock[
(
{ // trigger an envelope
	var trig;
	trig = SinOsc.ar(1) > 0;
	Out.ar(0,
		EnvGen.kr(Env.perc, trig, doneAction: Done.none)
			* SinOsc.ar(440,0,0.1)
	)
}.play
)

// trigger a synth
(
SynthDef("help-EnvGen",{ arg out=0;
	Out.ar(out,
		EnvGen.kr(Env.perc,1.0,doneAction: Done.freeSelf)
			* SinOsc.ar(440,0,0.1)
	)
}).add;

// This synth has no output. It only checks amplitude of input and looks for a transition from < 0.2
// to > 0.2

{ SendTrig.kr(Amplitude.kr(SoundIn.ar(0)) > 0.2) }.play;

// OSCFunc to trigger synth
OSCFunc({ "triggered".postln; Synth.new("help-EnvGen") },'/tr', s.addr);
)
::

]
@section{method}
  >=
Greater than or equal.

@section{method}
  ==
Equal.

@section{method}
  !=
Not equal.

@section{subsection}
  Other

@section{method}
  <!
Return first argument.


@racketblock[
// this is useful when two ugens need to be called, but only one of their outputs is needed
(
{
	var a, b, c;
	a = Dseq([1, 2, 3, 4], inf).dpoll("a");
	b = Dseq([1955, 1952, 1823, 1452], inf).dpoll("b");
	c = (a <! b).dpoll("------> a <! b = "); // c only
	Duty.kr(0.4, 0, c);
	0.0
}.play
)
::

]
@section{method}
  min
Minimum.
@section{discussion}
 

@racketblock[
{ // distorts and envelopes z
var z;
z = FSinOsc.ar(500);
z min: FSinOsc.ar(0.1);
}.play;
::

]
@section{method}
  max
Maximum.
@section{discussion}
 

@racketblock[
{ // modulates and envelopes z
var z;
z = FSinOsc.ar(500);
z max: FSinOsc.ar(0.1);
}.play;
::

]
@section{method}
  round
Quantization by rounding. Rounds a to the nearest multiple of b.

@section{method}
  trunc
Quantization by truncation. Truncate a to a multiple of b.

@section{method}
  hypot
Hypotenuse. Returns the square root of the sum of the squares of a and b. Or equivalently, the distance from the origin
to the point (x, y).
@section{discussion}
 
In this example, hypot is used to calculate a doppler shift pitch and amplitude based on distance.

@racketblock[
(
{
	var x, y, distance, velocity, pitchRatio, amplitude;
	// object travels 200 meters in 6 secs (=120kph) passing 10 meters
	// from the listener
	x = 10;
	y = LFSaw.kr(1/6, 0, 100);
	distance = hypot(x, y);
	velocity = Slope.kr(distance);
	pitchRatio = (344 - velocity) / 344;  // speed of sound is 344 meters/sec
	amplitude = 10 / distance.squared;
	FSinOsc.ar(1000 * pitchRatio, 0, amplitude)
}.play)
::
The next example uses the distance to modulate a delay line.
]

@racketblock[
(
{
	var x, y, distance, velocity, pitchRatio, amplitude, motorSound;
	// object travels 200 meters in 6 secs (=120kph) passing 10 meters
	// from the listener
	x = 10;
	y = LFSaw.kr(1/6, 0, 100);
	distance = hypot(x, y);
	amplitude = 40 / distance.squared;
	motorSound = RLPF.ar(FSinOsc.ar(200, 0, LFPulse.ar(31.3, 0, 0.4)), 400, 0.3);
	DelayL.ar(motorSound, 110/344, distance/344, amplitude)
}.play)
::

]
@section{method}
  hypotApx
Hypotenuse approximation. Returns an approximation of the square root of the sum of the squares of x and y.
@section{discussion}
 
The formula used is :

@racketblock[
abs(x) + abs(y) - ((sqrt(2) - 1) * min(abs(x), abs(y)))
::
hypotApx is used to implement Complex method magnitudeApx.
This should not be used for simulating a doppler shift because it is discontinuous. Use hypot.

See also link::#.hypot::, link::#.atan2::.

]
@section{method}
  atan2
Returns the arctangent of y/x.
@section{discussion}
 
OK, now we can add a pan to the link::#.hypot:: doppler examples by using atan2 to find the azimuth,
or direction angle, of the sound source.
Assume speakers at +/- 45 degrees and clip the direction to between those.

@racketblock[
(
{
	var x, y, distance, velocity, pitchRatio, amplitude, azimuth, panValue;
	// object travels 200 meters in 6 secs (=120kph) passing 10 meters
	// from the listener
	x = 10;
	y = LFSaw.kr(1/6, 0, 100);
	distance = hypot(x, y);
	velocity = Slope.kr(distance);
	pitchRatio = (344 - velocity) / 344;  // speed of sound is 344 meters/sec
	amplitude = 10 / distance.squared;
	azimuth = atan2(y, x); // azimuth in radians
	panValue = (azimuth / 0.5pi).clip2(1);
	Pan2.ar(FSinOsc.ar(1000 * pitchRatio), panValue, amplitude)
}.play)

(
{
	var x, y, distance, velocity, pitchRatio, amplitude, motorSound,
			azimuth, panValue;
	// object travels 200 meters in 6 secs (=120kph) passing 10 meters
	// from the listener
	x = 10;
	y = LFSaw.kr(1/6, 0, 100);
	distance = hypot(x, y);
	amplitude = 40 / distance.squared;
	motorSound = RLPF.ar(FSinOsc.ar(200, 0, LFPulse.ar(31.3, 0, 0.4)), 400, 0.3);
	azimuth = atan2(y, x); // azimuth in radians
	panValue = (azimuth / 0.5pi).clip2(1); // make a value for Pan2 from azimuth
	Pan2.ar(DelayL.ar(motorSound, 110/344, distance/344), panValue, amplitude)
}.play)
::

]
@section{method}
  ring1
Ring modulation plus first source.
@section{discussion}
 
Return the value of  ((a*b) + a). This is more efficient than using
separate unit generators for the multiply and add.

See also link::#.*::, link::#.ring1::, link::#.ring2::, link::#.ring3::, link::#.ring4::.

@racketblock[
{ (FSinOsc.ar(800) ring1: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	((a * b) + a) * 0.125
}.play)
::
normal ring modulation:
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	(a * b) * 0.125
}.play)
::

]
@section{method}
  ring2
Ring modulation plus both sources.
@section{discussion}
 
Return the value of  ((a*b) + a + b). This is more efficient than using
separate unit generators for the multiply and adds.

@racketblock[
{ (FSinOsc.ar(800) ring2: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	((a * b) + a + b) * 0.125
}.play)
::

]
@section{method}
  ring3
Ring modulation variant.
@section{discussion}
 
Return the value of  (a*a *b). This is more efficient than using
separate unit generators for each multiply.

@racketblock[
{ (FSinOsc.ar(800) ring3: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	(a * a * b) * 0.125;
}.play)
::

]
@section{method}
  ring4
Ring modulation variant.
@section{discussion}
 
Return the value of  ((a*a *b) - (a*b*b)). This is more efficient than using
separate unit generators for each operation.

@racketblock[
{ (FSinOsc.ar(800) ring4: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	((a * a * b) - (a * b * b)) * 0.125
}.play)
::

]
@section{method}
  sumsqr
Sum of squares.
@section{discussion}
 
Return the value of  (a*a) + (b*b). This is more efficient than using
separate unit generators for each operation.

@racketblock[
{ (FSinOsc.ar(800) sumsqr: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	((a * a) + (b * b)) * 0.125
}.play)
::

]
@section{method}
  difsqr
Difference of squares.
@section{discussion}
 
Return the value of  (a*a) - (b*b). This is more efficient than using
separate unit generators for each operation.

@racketblock[
{ (FSinOsc.ar(800) difsqr: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	((a * a) - (b * b)) * 0.125
}.play)
::

]
@section{method}
  sqrsum
Square of the sum.
@section{discussion}
 
Return the value of  (a + b)**2. This is more efficient than using
separate unit generators for each operation.

@racketblock[
{ (FSinOsc.ar(800) sqrsum: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b, c;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	c = a + b;
	(c * c) * 0.125
}.play)
::

]
@section{method}
  sqrdif
Square of the difference.
@section{discussion}
 
Return the value of  (a - b)**2. This is more efficient than using
separate unit generators for each operation.

@racketblock[
{ (FSinOsc.ar(800) sqrdif: FSinOsc.ar(XLine.kr(200,500,5))) * 0.125 }.play;
::
same as :
]

@racketblock[
(
{
	var a, b, c;
	a = FSinOsc.ar(800);
	b = FSinOsc.ar(XLine.kr(200,500,5));
	c = a - b;
	(c * c) * 0.125
}.play)
::

]
@section{method}
  absdif
Absolute value of the difference. 
@racketblock[ abs(a - b) ::
]
@section{discussion}
 

@racketblock[
(
{ // creates a rhythm
var mul = 0.2 absdif: FSinOsc.ar(2, 0, 0.5);
FSinOsc.ar(440, 0, mul);
}.play;
)
::

]
@section{method}
  moddif
On a circle, there are two distances between two points. This operator returns the smaller value of the two.
@section{discussion}
 

@racketblock[
{ Line.ar(0, 4, 0.01).moddif(0) }.plot;
(
{
var mul = 0.2 moddif: FSinOsc.ar(2, 0, 0.5);
FSinOsc.ar(440, 0, mul);
}.play;
)
::

]
@section{method}
  thresh
Thresholding.
@section{discussion}
 
0 when a < b, otherwise a.

@racketblock[
{ LFNoise0.ar(50, 0.5) thresh: 0.45 }.play // a low-rent gate
::

]
@section{method}
  amclip
Two quadrant multiply.
@section{discussion}
 
0  when  b <= 0,  a*b  when  b > 0

@racketblock[
{ WhiteNoise.ar.amclip(FSinOsc.kr(1,0.2)) }.play; // makes a sine envelope
::

]
@section{method}
  scaleneg
Scale negative part of input.
@section{discussion}
 
a*b when a < 0, otherwise a.

@racketblock[
{ FSinOsc.ar(500).scaleneg(Line.ar(1,-1,4)) }.play;
::

]
@section{method}
  clip2
Bilateral clipping.
@section{discussion}
 
clips input wave a to +/- b

@racketblock[
(
{
	var a;
	a = Line.ar(-2, 2, 0.01);
	a.clip2
}.plot2
)

{ FSinOsc.ar(400).clip2(0.2) }.scope; // clipping distortion

{ FSinOsc.ar(1000).clip2(Line.kr(0,1,8)) }.scope;
::

]
@section{method}
  wrap2
Bilateral wrapping.
@section{discussion}
 
wraps input wave to +/- b

@racketblock[
(
{
	var a;
	a = Line.ar(-2, 2, 0.01);
	a.wrap2
}.plot
)

{ FSinOsc.ar(1000).wrap2(Line.kr(0,1.01,8)) }.scope;
::

]
@section{method}
  fold2
Bilateral folding.
@section{discussion}
 
folds input wave a to +/- b

@racketblock[
(
{
	var a;
	a = Line.ar(-2, 2, 0.01);
	a.fold2
}.plot
)


{ FSinOsc.ar(1000).fold2(Line.kr(0,1,8)) }.scope;
::

]
@section{method}
  excess
Residual of clipping.
@section{discussion}
 
Returns the difference of the original signal and its clipped form: (a - clip2(a,b)).

@racketblock[
(
{
	var a;
	a = Line.ar(-2, 2, 0.01);
	a.excess
}.plot
)

{ FSinOsc.ar(1000).excess(Line.kr(0,1,8)) }.play;
::

]


