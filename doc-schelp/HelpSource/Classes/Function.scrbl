#lang scribble/manual
@(require (for-label racket))

@title{Function}
Implements a function@section{categories}
 Core>Kernel
@section{related}
 Classes/FunctionDef


@section{description}

A Function is a reference to a FunctionDef and its defining context Frame. When a FunctionDef is encountered in your code it is pushed on the stack as a Function. A Function can be evaluated by using the 'value' method. See the Functions help file for a basic introduction.

Because it inherits from AbstractFunction, Functions respond to math operations by creating a new Function.


@racketblock[
// example
(
var a, b, c;
a = { [100, 200, 300].choose };	// a Function
b = { 10.rand + 1 };	// another Function
c = a + b; 	// c is a Function.
c.value.postln;	// evaluate c and print the result
)
::

See AbstractFunction for function composition examples.

Because Functions are such an important concept, here some examples from related programming languages with functions as first class objects:

]

@racketblock[
// returning the first argument itself:
{ |x| x }.value(1) // SuperCollider
[:x | x ] value: 1 // Smalltalk
((lambda (x) x) 1) // Lisp
::

]
@section{subsection}
  Related Keywords

@section{method}
  thisFunction
The global pseudo-variable 
@racketblock[thisFunction:: always evaluates to the current
enclosing Function.

]
@section{note}
 
Be aware of link::Reference/Control-Structures#Inline
optimization#inline optimizations:: which will be reflected in the value of

@racketblock[thisFunction::.
::

See also: link::Classes/FunctionDef#.thisFunctionDef#thisFunctionDef::


]
@section{classMethods}
 

@section{private}
 new

@section{instancemethods}
 

@section{subsection}
 Access

@section{method}
 def

Get the definition ( FunctionDef ) of the Function.

@section{method}
 isClosed

returns true if the function is closed, i.e. has no external references and can thus be converted to a compile string safely.

@section{subsection}
 Evaluation

@section{method}
 value

Evaluates the FunctionDef referred to by the Function. The Function is passed the args given.


@racketblock[
{ |a, b| (a * b).postln }.value(3, 10);
{ arg a, b; (a * b).postln }.value(3, 10); // different way of expressing the same
::

]
@section{method}
 valueArray

Evaluates the FunctionDef referred to by the Function. If the last argument is an Array or List, then it is unpacked and appended to the other arguments (if any) to the Function. If the last argument is not an Array or List then this is the same as the 'value' method.


@racketblock[
{ |a, b, c| ((a * b) + c).postln }.valueArray([3, 10, 7]);

{ |a, b, c, d| [a, b, c, d].postln }.valueArray([1, 2, 3]);

{ |a, b, c, d| [a, b, c, d].postln }.valueArray(9, [1, 2, 3]);

{ |a, b, c, d| [a, b, c, d].postln }.valueArray(9, 10, [1, 2, 3]);
::

A common syntactic shortcut:

]

@racketblock[
{ |a, b, c| ((a * b) + c).postln }.value(*[3, 10, 7]);
::

]
@section{method}
 valueEnvir

As value above. Unsupplied argument names are looked up in the current Environment.


@racketblock[
(
Environment.use({
~a = 3;
~b = 10;
{ |a, b| (a * b).postln }.valueEnvir;
});
)
::

]
@section{method}
 valueArrayEnvir

Evaluates the FunctionDef referred to by the Function. If the last argument is an Array or List, then it is unpacked and appended to the other arguments (if any) to the Function. If the last argument is not an Array or List then this is the same as the 'value' method. Unsupplied argument names are looked up in the current Environment.


@section{method}
 valueWithEnvir

Evaluate the function, using arguments from the supplied environment
This is slightly faster than valueEnvir and does not require replacing the currentEnvironment


@racketblock[
(
e = Environment.make({ ~a = 3; ~b = 10 });
{ |a, b| (a * b) }.valueWithEnvir(e);
)
::

]
@section{method}
 functionPerformList

For Function, this behaves the same as valueArray(arglist). It is used  where Functions and other objects should behave differently to value, such as in the object prototyping implementation of Environment.


@section{method}
 performWithEnvir


@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performWithEnvir(\value, (a: 1, c: 3, d: 4, b: 2));
::

]
@section{argument}
 selector
A Symbol representing a method selector.
@section{argument}
 envir
The remaining arguments derived from the environment and passed as arguments to the method named by the selector.

@section{method}
 performKeyValuePairs


@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performKeyValuePairs(\value, [\a, 1, \b, 2, \c, 3, \d, 4]);
::

]
@section{argument}
 selector
A Symbol representing a method selector.
@section{argument}
 pairs
Array or List with key-value pairs.


@section{method}
 loop

Repeat this function. Useful with Task and Clocks.


@racketblock[
t = Task({ { "I'm loopy".postln; 1.wait;}.loop });
t.start;
t.stop;
::

]
@section{method}
 defer

Delay the evaluation of this Function by 
@racketblock[delta:: in seconds on AppClock.

This is equivalent to ]

@racketblock[AppClock.sched(0, function):: unless ]

@racketblock[delta:: is ]

@racketblock[nil::. In that case the function is only scheduled if current code is not running on AppClock, otherwise the function is evaluated immediately.

]

@racketblock[
{ "2 seconds have passed.".postln; }.defer(2);

(
{ "chicken".postln }.defer(0); // schedules on the AppClock
{ "egg".postln }.defer // evaluates immediately
)

(
fork { // schedules on a TempoClock
    { "chicken".postln }.defer // schedules on the AppClock
};
{ "egg".postln }.defer // evaluates immediately
)
::

]
@section{method}
 dup

Return an Array consisting of the results of n evaluations of this Function.


@racketblock[
x = { 4.rand; }.dup(4);
x.postln;
::

]
@section{method}
 !

equivalent to dup(n)


@racketblock[
x = { 4.rand } ! 4;
x.postln;
::

]
@section{method}
 sum

return the sum of n values produced.


@racketblock[
{ 4.rand }.sum(8);
::

]
@section{method}
 choose

evaluates the function. This makes it polymorphic to SequenceableCollection, Bag and Set.


@racketblock[
[{ 100.rand }, [20, 30, 40]].collect(_.choose);
::

]
@section{method}
 bench

Returns the amount of time this function takes to evaluate. print is a boolean indicating whether the result is posted. The default is true.


@racketblock[
{ 1000000.do({ 1.0.rand }); }.bench;
::

]
@section{method}
 fork

Returns a Routine using the receiver as it's function, and plays it in a TempoClock.


@racketblock[
{ 4.do({ "Threading...".postln; 1.wait;}) }.fork;
::

]
@section{method}
 forkIfNeeded

If needed, creates a new Routine to evaluate the function in, if the message is called within a routine already, it simply evaluates it.


@racketblock[
f = { 4.do({ "Threading...".postln; 1.wait;}) };
f.forkIfNeeded;
{ "we are now in a routine".postln; 1.wait; f.forkIfNeeded }.fork;
::

]
@section{method}
 block

Break from a loop. Calls the receiver with an argument which is a function that returns from the method block. To exit the loop, call .value on the function passed in. You can pass a value to this function and that value will be returned from the block method.


@racketblock[
block {|break|
	100.do {|i|
		i.postln;
		if (i == 7) { break.value(999) }
	};
}
::

]
@section{method}
 thunk

Return a Thunk, which is an unevaluated value that can be used in calculations


@racketblock[
x = thunk { 4.rand };
x.value;
x.value;
::

]
@section{method}
 flop

Return a function that, when evaluated with nested arguments, does multichannel expansion by evaluating the receiver function for each channel. A flopped function responds like the "map" function in languages like Lisp.


@racketblock[
f = { |a, b| if(a > 0) { a + b } { -inf } }.flop;
f.value([-1, 2, 1, -3.0], [10, 1000]);
f.value(2, 3);
::


]
@section{method}
 envirFlop

like flop, but implements an environment argument passing (valueEnvir).
Less efficient in generation than flop, but not a big difference in evaluation.


@racketblock[
f = { |a| if(a > 0) { a + 1 } { -inf } }.envirFlop;
e = (a: [20, 40]);
e.use { f.value }
::


]
@section{method}
 inEnvir

returns an "environment-safe" function. See Environment for more details.


@racketblock[
// prints nil because ~a is read from topEnvironment, not e
e = (a: "got it", f: { { ~a.postln }.defer(0.5) });
e.use { e.f };

// prints "got it" because { ~a.postln } is now bound to the e environment
e = (a: "got it", f: { { ~a.postln }.inEnvir.defer(0.5) });
e.use { e.f };
::


]
@section{method}
 case

Function implements a case method which allows for conditional evaluation with multiple cases. Since the receiver represents the first case this can be simply written as pairs of test functions and corresponding functions to be evaluated if true. Unlike Object-switch, this is inlined and is therefore just as efficient as nested if statements.


@racketblock[
(
var i, x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
i = z.choose;
x = case
	{ i == 1 }   { \no }
	{ i == 1.1 } { \wrong }
	{ i == 1.3 } { \wrong }
	{ i == 1.5 } { \wrong }
	{ i == 2 }   { \wrong }
	{ i == 0 }   { \true };
x.postln;
)
::

]
@section{method}
 matchItem

Interface shared with other classes that implements pattern matching. See also: matchItem.
Function.matchItem evaluates the function with the item as argument, expecting a Boolean as reply.

See also link::Reference/matchItem::.


@racketblock[
{ |x| x > 5 }.matchItem(6); // true
::


]
@section{method}
 performDegreeToKey

use a function as a conversion from scale degree to note number. See also SequenceableCollection and Scale


@racketblock[
// a strange mapping
(
var f = {|degree, stepsPerOctave, acc|
	(1.8 ** (degree % stepsPerOctave) + acc).postln
};
Pbind(
	\scale, f,
	\degree, Pseq([0, 1, 2b, 3s, 4s, 6, 14, [0, 2, 4], [1, 3, 6]], inf)
).play
)
::

]
@section{subsection}
 Exception Handling


For the following two methods a return ^ inside of the receiver itself cannot be caught. Returns in methods called by the receiver are OK.


@section{method}
 try

Executes the receiver. If an exception is thrown the catch function handler is executed with the error as an argument. handler itself can rethrow the error if desired.

@section{method}
 protect

Executes the receiver. The cleanup function handler is executed with an error as an argument, or nil if there was no error. The error continues to be in effect.

@section{subsection}
 Audio

@section{method}
 play

This is probably the simplest way to get audio in SC3. It wraps the Function in a SynthDef (adding an Out ugen if needed), creates and starts a new Synth with it, and returns the Synth object. A Linen is also added to avoid clicks, which is configured to allow the resulting Synth to have its \gate argument set, or to respond to a release message. Args in the function become args in the resulting def.


@racketblock[
x = { |freq = 440| SinOsc.ar(freq, 0, 0.3) }.play; // this returns a Synth object;
x.set(\freq, 880); // note you can set the freq argument
x.defName; // the name of the resulting SynthDef (generated automatically in a cycle of 512)
x.release(4); // fadeout over 4 seconds
::

Many of the examples make use of the Function.play syntax.
Note that reusing such code in a SynthDef requires the addition of an Out ugen.

]

@racketblock[
// the following two lines produce equivalent results
{ SinOsc.ar(440, 0, 0.3) }.play(fadeTime: 0.0);
SynthDef(\help_FuncPlay, { Out.ar(0, SinOsc.ar(440, 0, 0.3))}).play;
::

Function.play is often more convenient than SynthDef.play, particularly for short examples and quick testing. The latter does have some additional options, such as lagtimes for controls, etc. Where reuse and maximum flexibility are of greater importance, SynthDef and its various methods are usually the better choice.

]
@section{argument}
 target
a Node, Server, or Nil. A Server will be converted to the default group of that server. Nil will be converted to the default group of the default Server.
@section{argument}
 outbus
the output bus to play the audio out on. This is equivalent to Out.ar(outbus, theoutput). The default is 0.
@section{argument}
 fadeTime
a fadein time. The default is 0.02 seconds, which is just enough to avoid a click. This will also be the fadeout time for a release if you do not specify.
@section{argument}
 addAction
see Synth for a list of valid addActions. The default is \addToHead.
@section{argument}
 args
arguments

@section{method}
 scope

As play above, and calls Server-scope to open a scope window in which to view the output.


@racketblock[
{ FSinOsc.ar(440, 0, 0.3) }.scope(1)
::

]
@section{argument}
 numChannels
The number of channels to display in the scope window, starting from outbus. The default is 2.
@section{argument}
 outbus
The output bus to play the audio out on. This is equivalent to Out.ar(outbus, theoutput). The default is 0.
@section{argument}
 fadeTime
A fadein time. The default is 0.02 seconds, which is just enough to avoid a click.
@section{argument}
 bufsize
The size of the buffer for the ScopeView. The default is 4096.
@section{argument}
 zoom
A zoom value for the scope's X axis. Larger values show more. The default is 1.

@section{method}
 plot

Calculates duration in seconds worth of the output of this function, and plots it in a GUI window. Unlike play and scope it will not work with explicit Out Ugens, so your function should return a UGen or an Array of them. The plot will be calculated in realtime.


@racketblock[
{ SinOsc.ar(440) }.plot(0.01, bounds: Window.screenBounds);

{ {|i| SinOsc.ar(1 + i)}.dup(7) }.plot(1);
::


]
@section{argument}
 duration
The duration of the function to plot in seconds. The default is 0.01.
@section{argument}
 server
The Server on which to calculate the plot. This must be running on your local machine, but does not need to be the internal server. If nil the default server will be used.
@section{argument}
 bounds
An instance of Rect or Point indicating the bounds of the plot window.
@section{argument}
 minval
the minimum value in the plot. Defaults to -1.0.
@section{argument}
 maxval
the maximum value in the plot. Defaults to 1.0.
@section{argument}
 parent
a window to place the plot in. If nil, one will be created for you.

@section{subsection}
 Conversion

@section{method}
 asSynthDef

Returns a SynthDef based on this Function, adding a Linen and an Out ugen if needed.

@section{argument}
 rates
An Array of rates and lagtimes for the function's arguments (see SynthDef for more details).
@section{argument}
 prependArgs
arguments
@section{argument}
 outClass
The class of the output ugen as a symbol. The default is \Out.
@section{argument}
 fadeTime
a fadein time. The default is 0.
@section{argument}
 name
the name of the SynthDef

@section{method}
 asDefName

Performs asSynthDef (see above), sends the resulting def to the local server and returns the SynthDefs name. This is asynchronous.


@racketblock[
x = { SinOsc.ar(440, 0, 0.3) }.asDefName; // this must complete first
y = Synth(x);
::

]
@section{method}
 asRoutine

Returns a Routine using this as its func argument.

@section{method}
 r

Returns a Routine using this as its func argument.


@racketblock[
a = r { 5.do { |i| i.rand.yield } };
a.nextN(8);
::

]
@section{method}
 p

Returns a Prout using this as its func argument.


@racketblock[
a = p { 5.do { |i| i.rand.yield } };
x = a.asStream;
x.nextN(8);
::

This is useful for using ListComprehensions in Patterns:

]

@racketblock[
Pbind(\degree, p {:[x, y].postln, x<-(0..10), y<-(0..10), (x + y).isPrime }, \dur, 0.3).play;
::


]
@section{examples}
 

@section{subsection}
 Exception Handling


@racketblock[
// no exception handler
value { 8.zorg; \didnt_continue.postln; }

try { 8.zorg } {|error| error.postln; \cleanup.postln; }; \continued.postln;

protect { 8.zorg } {|error| error.postln; }; \didnt_continue.postln;
::

]

@racketblock[
try { 123.postln; 456.throw; 789.postln } {|error| [\catch, error].postln };

try { 123.postln; 789.postln } {|error| [\catch, error].postln };

try { 123.postln; nil.throw; 789.postln } {|error| [\catch, error].postln };

protect { 123.postln; 456.throw; 789.postln } {|error| [\onExit, error].postln };

protect { 123.postln; 789.postln } {|error| [\onExit, error].postln };

(
try {
	protect { 123.postln; 456.throw; 789.postln } {|error| [\onExit, error].postln };
} {|error| [\catch, error].postln };
)

value { 123.postln; 456.throw; 789.postln }

value { 123.postln; Error("what happened?").throw; 789.postln }
::

]

@racketblock[
(
a = [\aaa, \bbb, \ccc, \ddd];
a[1].postln;
a[\x].postln;
a[2].postln;
)

(
try {
	a = [\aaa, \bbb, \ccc, \ddd];
	a[1].postln;
	a[\x].postln;
	a[2].postln;
} {|error| \caught.postln; error.dump }
)

(
try {
	a = [\aaa, \bbb, \ccc, \ddd];
	a[1].postln;
	a[\x].postln;
	a[2].postln;
} {|error| \caught.postln; error.dump; error.throw }
)

(
protect {
	a = [\aaa, \bbb, \ccc, \ddd];
	a[1].postln;
	a[\x].postln;
	a[2].postln;
} {|error| \caught.postln; error.dump }
)
::



]


