#lang scribble/manual
@(require (for-label racket))

@title{Stream}
 Stream is the base class for classes that define streams@section{related}
  Classes/Routine, Classes/FuncStream, Classes/EventStreamPlayer
@section{categories}
  Streams-Patterns-Events

@section{description}


Stream is an abstract class that is not used directly. The following attempts to document some aspects of the use of Streams for music generation.

@section{subsection}
 Overview

A Stream represents a sequence of values that are obtained incrementally by repeated strong::next:: messages. A Stream can be restarted with a strong::reset:: message. (Not all streams actually implement reset semantics.)

The class link::Classes/Object:: defines strong::next:: to return the object itself. Thus every object can be viewed as a stream and most simply stream themselves.

In SuperCollider, Streams are primarily used for handling text and for generating music.

@section{subsection}
 FuncStream(nextFunction, resetFunction)

A link::Classes/Function:: defines a stream consisting of the Function itself, a link::Classes/FuncStream:: defines a stream that consists of emphasis::evaluations:: of its nextFunction.


@racketblock[
// Example 1: a Function vs. a FuncStream
(
	f = { 33.rand };
	x = FuncStream(f);
	10.do({ [f.next, x.next].postln });
)
::

]

@racketblock[
// Example 2: the reset function
(
	f = { 33.rand };
	x = FuncStream(f, {thisThread.randSeed_(345)});
	x.reset;
	10.do({ [f.next, x.next].postln });
	x.reset;
	10.do({ [f.next, x.next].postln });
)
::

]
@section{subsection}
 Routine(nextFunction, stacksize)

In a link::Classes/FuncStream::, the nextFunction runs through to completion for each element of the stream. In a link::Classes/Routine::, the nextFunction returns values with strong::yield:: and resumes execution (when it receives a strong::next:: message) at the expression following the yield. This allows a sequence of expressions in the function definition to represent a sequence of distinct events, like a musical score.


@racketblock[
// example
(
	x = Routine({
		1.yield;
		2.yield;
		3.yield;
	});
	4.do({ x.next.postln });
)
::

Once the nextFunction completes execution, the Routine simply yields nil repeatedly. Control structures (such as strong::do:: or strong::while::) can be used within the nextFunction in a manner analogous to repeat marks in a score.

]

@racketblock[
// example
(
	x = Routine({
		4.do({
			[1,2,3,4].do({ arg i; i.yield; });
		});
	});
	17.do({ x.next.postln });
)
::

]
@section{subsection}
 Playing streams

Because streams respond like functions to the value message, they can be used as a scheduling task.


@racketblock[
// compare:
// a function, returning 0.5
(
SystemClock.sched(0.0,
	{ "***".postln; 0.5 }
);
)

// a stream, returning 0.5 and 0.1
(
SystemClock.sched(0.0,
	Routine({ loop {
		"***".postln; 0.5.yield;
		"_*_".postln; 0.1.yield;
	} });
);
)

// this is the reason why 'wait' works the same (for numbers) like 'yield'
(
SystemClock.sched(0.0,
	Routine({ loop {
		"***".postln; 0.5.wait;
		"_*_".postln; 0.1.wait;
	} });
);
)
::

Streams that return strong::numbers:: can be played directly with the strong::play:: message.

]

@racketblock[
// play at the next beat, with offset 0.4
(
Routine({ loop {
	"***".postln; 0.5.wait;
	"_*_".postln; 0.1.wait;
} }).play(quant:[1, 0.4]);
)
::

Streams that return strong::Events:: need to be wrapped in an link::Classes/EventStreamPlayer::. The Event's strong::delta:: (can also be set by strong::dur::) is used as a scheduling beats value:

]

@racketblock[
// play at the next beat, with offset 0.4
(
Routine({ loop {
	"///".postln; (delta:0.5).yield;
	"_/_".postln; (delta: 0.1).wait;
} }).asEventStreamPlayer.play;
)
::

]
@section{subsection}
 Iteration

The method link::#-do:: effectively 'plays' a stream by iterating all of its contents.

And the following messages create a stream by filtering another stream in some way: link::#-collect::, link::#-reject::, link::#-select::, link::#-dot::, link::#-interlace::, link::#-appendStream::, link::#-embedInStream::, link::#-trace::.

@section{subsection}
 Composite Streams

Routines can be strong::embedded:: in each other, using link::#-embedInStream:: :


@racketblock[
// example
(
x = Routine({
	2.do({
		[1,2,3,4].do({ arg i; i.yield; });
	});
});
y = Routine({
	100.yield;
	30.yield;
	x.embedInStream;
	440.yield;
	1910.yield;
});
17.do({ y.next.postln });
)
::

Routines can be strong::concatenated:: just like Streams:

]

@racketblock[
(
x = Routine({
	2.do({
		[1,2,3,4].do({ arg i; i.yield; });
	});
});
y = Routine({
	100.yield;
	30.yield;
});
z = x ++ y;
17.do({ z.next.postln });
)
::

Routines can be strong::combined:: with the composition operator <>

]

@racketblock[
(
x = Routine({ arg inval;
	2.do({
		[1,2,3,4].do({ arg i;
			if(inval.isNil) { nil.alwaysYield };
			inval = (i * inval).yield;
		});
	});
});
y = Routine({
	100.yield;
	30.yield;
	4.do { 1.0.rand.yield };
});
z = x <> y;
17.do({ z.value.postln }); // call .value here, as this is a function.
)
::

Composite Streams can be defined as combinations of Streams using the unary and binary messages.

]
@section{subsection}
 Unary messages

Streams support most of the unary messages defined in link::Classes/AbstractFunction:: :


@racketblock[
(
a = Routine({ 20.do({ 33.rand.yield }) });
b = Routine({ [-100,00,300,400].do({ arg v; v.yield}) });

c = b.neg; // define a composite stream

// enumerate and perform all of the unary messages:
[
	\neg, \reciprocal, \bitNot, \abs, \asFloat, \asInteger, \ceil,
	\floor, \frac, \sign, \squared, \cubed, \sqrt, \exp, \midicps,
	\cpsmidi, \midiratio, \ratiomidi, \ampdb, \dbamp, \octcps,
	\cpsoct, \log, \log2, \log10, \sin, \cos, \tan, \asin, \acos, \atan,
	\sinh, \cosh, \tanh, \rand, \rand2, \linrand, \bilinrand, \sum3rand,
	\distort, \softclip, \coin, \even, \odd, \isPositive, \isNegative,
	\isStrictlyPositive
]
.do({ arg msg;
	postf("\n msg: % \n", msg);
	b.reset.perform(msg).do({arg v; v.post; " ".post;});
});
nil;
)
::

]
@section{subsection}
 Binary messages

Streams support the following binary messages defined in link::Classes/AbstractFunction:: :


@racketblock[
(
a = Routine({ 20.do({ 33.rand.yield }) });
b = Routine({ [-100,00,300,400].do({ arg v; v.yield}) });
[
	'+' , '-' , '*', '/', \div, '%', '**', \min, \max, '<', '<=', '>', '>=', '&', '|',
	\bitXor, \lcm, \gcd, \round, \trunc, \atan2,
	\hypot, '>>', '+>>', \ring1, \ring2, \ring3, \ring4,
	\difsqr, \sumsqr, \sqrdif, \absdif, \amclip,
	\scaleneg, \clip2, \excess, '<!', \rrand, \exprand
]
.do({ arg msg;
	postf("\n msg: % \n", msg);
	b.reset.perform(msg).do({ arg v; v.post; " ".post; });
});
nil;
)
::

]
@section{InstanceMethods}
 

@section{method}
 play
Streams that return strong::numbers:: can be played directly with the strong::play:: message. Streams that return strong::events:: need to be wrapped in an link::Classes/EventStreamPlayer::. See link::#-asEventStreamPlayer::.

@section{argument}
 clock
a clock. link::Classes/TempoClock:: by default.

@section{argument}
 quant
either a number strong::n:: (quantize to strong::n:: beats), or an array strong::[n, m]:: (quantize to n beats, with offset m).

@section{method}
 do
iterate until a nil is encountered.
@section{warning}
 
Applying do to an endless stream will lock up the interpreter!
::

@section{method}
 collect
iterate indefinitely.

@section{method}
 reject
return only those elements for which function.value(element) is false.

@section{method}
 select
return only those elements for which function.value(element) is true.

@section{method}
 dot
return function.value(this.next, stream.next) for each element.

@section{method}
 interlace
iterate all of stream for each element of this. Combine the values using function.

@section{method}
 appendStream
append stream after this returns nil. The same like ++

@section{method}
 embedInStream
iterate all of this from within whatever Stream definition it is called.

@section{method}
 trace
print out the results of a stream while returning the original values.

@section{argument}
 key
when streaming events, post only this key.

@section{argument}
 printStream
printOn this stream (default: link::Classes/Post::).

@section{argument}
 prefix
string added to the printout to separate different streams.


