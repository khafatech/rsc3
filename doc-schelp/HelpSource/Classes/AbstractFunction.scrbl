#lang scribble/manual
@(require (for-label racket))

@title{AbstractFunction}
An object which responds to a set of messages that represent mathematical functions@section{categories}
 Core>Kernel
@section{related}
 Classes/UGen,Classes/Pattern,Classes/Function,Overviews/Operators

@section{description}


An AbstractFunction is an object which responds to a set of messages that represent
mathematical functions. Subclasses override a smaller set of messages to respond
to the mathematical functions.

The intent is to provide a mechanism for functions that do not calculate values directly but instead compose structures for calculating (lazy evaluation).

Function, Pattern, Stream and UGen are subclasses of AbstractFunction.
For example, if you multiply two UGens together the receiver responds by returning a new
instance of class BinaryOpUGen which has the two operands as inputs.


@racketblock[
{ var a, b; a = LFSaw.ar(220); b = LFPulse.ar(1442); [a, b, a * b] }.plot;
::

For an overview of common operators, see link::Overviews/Operators::, for specific examples, see also e.g. link::Classes/Function::, link::Classes/UGen::, link::Classes/Pattern::.
To see which classes implement a specific method, see that method in the generated link::Overviews/Methods:: overview.

]
@section{instanceMethods}
 

@section{subsection}
 Unary Messages

The following messages return an object which represents a delayed unary operation, i.e. an operation on one object. For example, the reciprocal of a function will result in a new function that, when called, returns the reciprocal of the evaluation of the operand.

All of the following messages send the message composeUnaryOp to the receiver with the
unary message selector as an argument.
See link::Classes/UnaryOpFunction::.

@section{method}
 neg

@racketblock[
a = { 10.rand.postln }; b = a.neg; b.value;
// Patterns, Streams, UGens, and Proxies are AbstractFunctions, too:
a = Pgeom(1, 2, 5).neg; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.neg] }.plot;
::
]
@section{method}
 reciprocal

@racketblock[
a = { 10.rand.postln }; b = a.reciprocal; b.value;
a = Pgeom(1, 2, 5).reciprocal; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500) + 2; [a, a.reciprocal] }.plot;
::
]
@section{method}
 bitNot
Bitwise integer negation.
@section{method}
 abs
Absolute value

@racketblock[
a = { 10.rand - 10.rand }; b = a.abs; b.value;
a = Pseries(3, -1.8, inf).abs; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.abs] }.plot;
::
]
@section{method}
 asFloat

@racketblock[
a = { "123.471".scramble }; b = a.asFloat; b.value;
::
]
@section{method}
 asInt

@racketblock[
a = { "123471".scramble }; b = a.asInt; b.value;
::
]
@section{method}
 ceil, floor, frac

@racketblock[
a = { 10.0.rand2.postln }; b = a.ceil; b.value;
a = { 10.0.rand2.postln }; b = a.floor; b.value;
a = Pgeom(1, 1.2, inf).ceil; a.asStream.nextN(8);
a = Pgeom(1, 1.2, inf).floor; a.asStream.nextN(8);
{ a = SinOsc.ar(150) * 1.5; [a, a.ceil, a.floor, a.frac] }.plot.superpose_(true);
::
]
@section{method}
 sign
Returns a function that returns -1 if receiver returns a negative number, 1 if positive, and 0 if zero.

@racketblock[
a = { 10.0.rand2.postln }; b = a.sign; b.value;
{ a = LFNoise1.ar(1500) * 1.5; [a, a.sign] }.plot;
::
]
@section{method}
 squared

@racketblock[
a = { |x| x + 1 }; b = a.squared; [a.value(1), b.value(1)];
a = Pseries(0, 1, inf).squared; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.squared] }.plot;
::
]
@section{method}
 cubed

@racketblock[
a = { |x| x + 1 }; b = a.cubed; [a.value(1), b.value(1)];
a = Pseries(0, 1, inf).cubed; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.cubed] }.plot;
::
]
@section{method}
 sqrt

@racketblock[
a = { |x| x + 1 }; b = a.sqrt; [a.value(1), b.value(1)];
a = Pseries(0, 1, inf).sqrt; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.sqrt] }.plot;
::
]
@section{method}
 exp
Returns e to the power of this.

@racketblock[
a = { |x| x + 1 }; b = a.exp; [a.value(1), b.value(1)];
a = Pseries(0, 0.25, inf).exp; a.asStream.nextN(8);
{ a = LFNoise1.ar(1500); [a, a.exp] }.plot;
::
]
@section{method}
 midicps
Converts midinote into cycles per seconds (Hz).

@racketblock[
a = { |x, root = 60| x + root }; b = a.midicps; [a.value(9), b.value(9)];
a = Pseries(60, 1, inf).midicps; a.asStream.nextN(12);
{ a = LFNoise1.ar(1) * 5 + 60; Pulse.ar(a.round.midicps) * 0.1 }.play;
::
]
@section{method}
 cpsmidi
Converts cycles per seconds (Hz) into midinote.

@racketblock[
a = { |x| #[440, 720, 801, 1020.2].at(x) }; b = a.cpsmidi; [a.value(3), b.value(3)];
a = Pseries(220, 220, inf).cpsmidi; a.asStream.nextN(12); // overtone series as midinotes
// follow but round to next midinote
{ a = Pitch.kr(SoundIn.ar).at(1); Pulse.ar(a.cpsmidi.round.midicps) * 0.1 }.play;
::
]
@section{method}
 midiratio
@section{method}
 ratiomidi
@section{method}
 ampdb
@section{method}
 dbamp
@section{method}
 octcps
@section{method}
 cpsoct
@section{method}
 log
@section{method}
 log2
@section{method}
 log10
@section{method}
 sin
@section{method}
 cos
@section{method}
 tan
@section{method}
 asin
@section{method}
 acos
@section{method}
 atan
@section{method}
 sinh
@section{method}
 cosh
@section{method}
 tanh
@section{method}
 rand
@section{method}
 rand2
@section{method}
 linrand
@section{method}
 bilinrand
@section{method}
 sum3rand
@section{method}
 distort
@section{method}
 softclip
@section{method}
 coin
@section{method}
 even
@section{method}
 odd
@section{method}
 isPositive
@section{method}
 isNegative
@section{method}
 isStrictlyPositive
@section{method}
 rho
@section{method}
 theta

@section{subsection}
 Binary Messages

The following messages return an object which represents a delayed binary operation, i.e. an operation between two objects. For example, adding two functions will result in a new function that, when called, adds the results of the evaluation of the two operands.

All of the following messages send the message composeBinaryOp to the receiver with the
binary message selector and the second operand as arguments.
See: link::Classes/BinaryOpFunction::.


Examples:

@racketblock[
(
// Add two functions:
var x = { |x| x + 1000 } + { |x| x * 100 };
// Evaluate the result, passing in one argument:
x.value(2); // posts 1202
)
// either operand can be another object:
(
// Add two functions:
var x = 1871 + { |x| x * 12 };
x.value(12);
)
::
]

@racketblock[
(
// Add two UGens
{
 SinOsc.ar(440, 0, 0.2) + PinkNoise.ar(0.1);
}.play
)
::
// Add two Patterns
]

@racketblock[
(Pseq([1, 2, 3, 4]) + Prand([0, 0.1, -0.1], inf)).asStream.nextN(5);
::
// Add two NodeProxies
]

@racketblock[
Ndef(\x, { SinOsc.ar(440, 0, 0.2) });
Ndef(\y, { PinkNoise.ar(0.1) });
Ndef(\z, Ndef(\x) + Ndef(\y)).play;
::

]
@section{method}
 +

@racketblock[
({ |x| x.squared } + 3).value(2);
::
]
@section{method}
 -

@racketblock[
({ |x| x.squared } - 3).value(2);
::
]
@section{method}
 *

@racketblock[
({ |x| x.squared } * { |x| x.squared }).value(2);
::
]
@section{method}
 /

@racketblock[
({ |x| x.squared } / 4).value(2);
::
]
@section{method}
 div

@racketblock[
({ |x| x.squared } div: 3).value(2);
::
]
@section{method}
 %

@racketblock[
({ |x| x.squared } % 3).value(2);
::
]
@section{method}
 **

@racketblock[
({ |x| x.squared } ** 3).value(2);
::
]
@section{method}
 min

@racketblock[
({ |x| x.squared } min: 0).value(2);
::
]
@section{method}
 max

@racketblock[
({ |x| x.squared } max: 0).value(2);
::
]
@section{method}
 <

@racketblock[
({ |x| x.squared } < 3).value(2);
::
]
@section{method}
 <=

@racketblock[
({ |x| x.squared } <= 3).value(2);
::
]
@section{method}
 >

@racketblock[
({ |x| x.squared } > 3).value(2);
::
]
@section{method}
 >=

@racketblock[
({ |x| x.squared } >= 3).value(2);
::
]
@section{method}
 &

@racketblock[
a = { |min, max| ({ rrand(min, max) } ! 4).postln };
(a & a).value(0, 8);
::
]
@section{method}
 |

@racketblock[
a = { |min, max| ({ rrand(min, max) } ! 4).postln };
(a | a).value(0, 8);
::
]
@section{method}
 lcm

@racketblock[
a = { |min, max| rrand(min, max).postln };
(a lcm: a).value(0, 8);
::
]
@section{method}
 gcd

@racketblock[
a = { |min, max| rrand(min, max).postln };
(a gcd: a).value(0, 8);
::
]
@section{method}
 round

@racketblock[
a = { |max| max.rand.postln };
(a round: 0.5).value(1.0);
::
]
@section{method}
 trunc

@racketblock[
a = { |max| max.rand.postln };
(a trunc: 2).value(10);
::
]
@section{method}
 atan2

@racketblock[
a = { 1.0.rand2 };
a.atan2.dup(10);
::
]
@section{method}
 hypot

@racketblock[
a = { 1.0.rand2 };
a.hypot.dup(10);
::
]
@section{method}
 hypotApx

@racketblock[
a = { 1.0.rand2 };
a.hypotApx.dup(10);
::
]
@section{method}
 >>

@racketblock[
a = { [2r10010, 2r101011, 2r11100].choose.postln };
b = a >> 2;
b.value.asBinaryDigits.join;
::
]
@section{method}
 +>>

@racketblock[
a = { [2r10010, 2r101011, 2r11100].choose.postln };
b = a +>> 2;
b.value.asBinaryDigits.join;
::
]
@section{method}
 ring1

(a * b) + a


@racketblock[
({ [5, 6, 2].choose.postln } ring1: { [2, -1, 3].choose.postln }).value

// UGens are also abstract functions
(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
ring1(a, b) * 0.1 }.play;
)
::
]
@section{method}
 ring2

((a*b) + a + b)


@racketblock[
({ [5, 6, 2].choose.postln } ring2: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
ring2(a, b) * 0.1 }.play;
)
::
]
@section{method}
 ring3

(a * a * b)


@racketblock[
({ [5, 6, 2].choose.postln } ring3: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
ring3(a, b) * 0.1 }.play;
)
::
]
@section{method}
 ring4

((a*a *b) - (a*b*b))


@racketblock[
({ [5, 6, 2].choose.postln } ring4: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
ring4(a, b) * 0.1 }.play;
)
::
]
@section{method}
 difsqr

(a*a) - (b*b)


@racketblock[
({ [5, 6, 2].choose.postln } difsqr: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
difsqr(a, b) * 0.1 }.play;
)
::
]
@section{method}
 sumsqr

(a*a) + (b*b)


@racketblock[
({ [5, 6, 2].choose.postln } sumsqr: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
sumsqr(a, b) * 0.1 }.play;
)
::
]
@section{method}
 sqrdif

(a - b) ** 2


@racketblock[
({ [5, 6, 2].choose.postln } sqrdif: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
ring4(a, b) * 0.1 }.play;
)
::
]
@section{method}
 sqrsum

(a + b) ** 2


@racketblock[
({ [5, 6, 2].choose.postln } sqrsum: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
sqrsum(a, b) * 0.1 }.play;
)
::
]
@section{method}
 absdif

(a - b).abs


@racketblock[
({ [5, 6, 2].choose.postln } absdif: { [2, -1, 3].choose.postln }).value

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
absdif(a, b) * 0.1 }.play;
)
::

]
@section{method}
 moddif
absolute difference in modulo arithmetics.

@section{method}
 amclip
0 when b <= 0, a*b when b > 0

@section{method}
 scaleneg
a * b when a < 0, otherwise a.

@section{method}
 clip2
clips receiver to +/- aNumber

@section{method}
 excess
Returns the difference of the receiver and its clipped form.
@section{method}
 <!
@section{method}
 rrand

@racketblock[
a = { |x| sin(x) } rrand: { |x| sin(x) *  -1 };
(0..1000).normalize(0, 5pi).collect(a).plot;

(
{ a = SinOsc.ar(335); b = SinOsc.ar(MouseX.kr(1, 1000, 1));
rrand(a, b) * 0.1 }.play;
)
::
]
@section{method}
 exprand
@section{method}
 rotate
@section{method}
 dist
@section{method}
 bitAnd
@section{method}
 bitOr
@section{method}
 bitXor
@section{method}
 bitHammingDistance
@section{method}
 @

@section{subsection}
  Messages with more arguments (n-ary Operators)

The following messages return an object which represents a delayed n-ary operation, i.e. an operation between several objects (often three). For example, rescaling a function with linlin will result in a new function that, when called, scales the results of the evaluation of all operands.

All of the following messages send the message 
@racketblock[composeNAryOp:: to the receiver with the
binary message selector and the other operands as arguments.
See link::Classes/NAryOpFunction::.

]
@section{method}
 clip
@section{method}
 wrap
@section{method}
 fold
@section{method}
 blend
@section{method}
 linlin
@section{method}
 linexp
@section{method}
 explin
@section{method}
 expexp

@section{subsection}
  other

@section{method}
 applyTo

Interface that allows us to combine selectors (Symbols) and Functions. Sends valueArray(args) to this.
@section{discussion}
 

@racketblock[
// example:

f = [{ |a, b| a * b * 100.rand }, { |a, b| sin(a) * sin(b) }, '*', '/'];
f.choose.postcs.applyTo(3, 4);

// this is used in SequenceableCollection reduce:
(1..10).reduce('+');
(1..10).reduce({ |a, b| a * b * 1.0.rand });
::

]
@section{method}
 asUGenInput

@section{returns}
  the result of sending the value(for) message to this.
@section{discussion}
 

@racketblock[
// example:
(
var f, g, product;
f = { SinOsc.ar(400) };
g = { LFPulse.kr(8)  };
product = f * g * 0.1;
{ Pan2.ar(product, SinOsc.kr(0.3)) }.play;
)
::

]
@section{method}
 sampled
Sample a function.
@section{discussion}
 

@racketblock[
//sample a function
f = { |x| sin(3*x)*cos(8*x) }
f.plotGraph(from:0,to:2);
f.sampled(10,0,2).plotGraph(from:0,to:2);
f.sampled(80,0,2).plotGraph(from:0,to:2);

//on complicated functions a sampled function is less cpy heavy.
f = { |x| 60.collect{ 2**((x-rrand(0.0,1.0))) }.sum/60 };
f.plotGraph(from:0,to:1);
g = f.sampled(200);
g.plotGraph(from:0,to:1);
{ 200.collect{ f.(rand(0.0,1.0)) } }.bench;
{ 200.collect{ g.(rand(0.0,1.0)) } }.bench;
::

]
@section{subsection}
 Function Composition

When unary, binary or n-ary operators are applied to an abstract function, it returns an object that represents
this operation, without evaluating the function: link::Classes/UnaryOpFunction::, link::Classes/BinaryOpFunction::, link::Classes/NAryOpFunction::.
Note that different subclasses like link::Classes/Pattern:: or link::Classes/UGen:: have their own composition scheme analogous to the one of AbstractFunction itself. For more about functions, see link::Classes/Function::.


@racketblock[
// compose a function that will return an array of random length
a = { |n| { 16.rand } ! n } <> { |x, y| rrand(4, 8) };
a.value;
// compose a function from a that selects only odd values
b = { |x| x.select(_.odd) } <> a;
b.value;
::


]
@section{examples}
 


@racketblock[
// examples

a = { 1.0.rand } + 8;
a.value;


y = { 8 } + { 1.0.rand };
y.value;
::

]

@racketblock[
// arguments are passed into both functions

y = { |x=0| x } + { 1.0.rand };
y.value(10);


y = { |x=0| x * 3 } + { |x=0| x + 1.0.rand };
y.value(10);

y.postcs;

y = { |x=0| x * 3 } + { |x=0| x + 1.0.rand } * { |x=0| [50, 100].choose + x } + 1.0;
y.value(10);
::

]

@racketblock[
// environments can be used as a lookup with valueEnvir:

(
Environment.use {
	~y = 10;
	~x = 2;
	~z = { |x=8| x } + { |y=0| y + 1.0.rand };
	~z.valueEnvir;
}
)
::

]

@racketblock[
// n-ary operators:

a = blend({ 3.0.rand }, { 1000.rand }, { |frac| frac });
a.value(0.5);

a.value((0, 0.06..1)); // creates a range of values..
::

]


