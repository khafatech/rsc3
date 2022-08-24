#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 02: Basic Vocabulary}
 Common patterns to generate streams of single values@section{related}
  Tutorials/A-Practical-Guide/PG_01_Introduction, Tutorials/A-Practical-Guide/PG_03_What_Is_Pbind
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Basic Vocabulary: Generating values

Before getting to the really cool things patterns can do, we need to build up a basic vocabulary. We'll start with some words, then move into phrases in the next tutorial.

Some of the patterns will be demonstrated with a Pbind construct. This is a taste of things to come -- sequencing sonic events using patterns. Don't worry about how Pbind works just yet... all in good time.

Let's start with a very quick reference of some basic patterns. More complete descriptions follow this list. The list might seem long at first, but concentrate your attention on patterns marked with a star. Those are the most basic, and commonly used. Again, the purpose is to start learning the vocabulary of patterns -- like learning new words when studying a human language.

This document describes a lot of patterns, but what I call "primary patterns" are the most important. If you are new to patterns, concentrate on these first. You can always come back and look at the rest later.

For more information on any of these patterns, select the class name and use the help key for your editor to open its help file.

@section{section}
 Quick reference

@section{subsection}
 Primary Patterns

@section{definitionList}
 
## 
@racketblock[Pseq(list, repeats, offset):: || Play through the entire list ]

@racketblock[repeats:: times. Like ]

@racketblock[list.do::.
## ]

@racketblock[Prand(list, repeats):: || Choose items from the list randomly (same as ]

@racketblock[list.choose::).
## ]

@racketblock[Pxrand(list, repeats):: || Choose randomly, but never repeat the same item twice in immediate succession.
## ]

@racketblock[Pshuf(list, repeats):: || Shuffle the list in random order, and use the same random order ]

@racketblock[repeats:: times. Like ]

@racketblock[list.scramble::.
## ]

@racketblock[Pwrand(list, weights, repeats):: || Choose randomly, according to weighted probabilities (same as ]

@racketblock[list.wchoose(weights)::).

## ]

@racketblock[Pseries(start, step, length):: || Arithmetic series (addition).
## ]

@racketblock[Pgeom(start, grow, length):: || Geometric series (multiplication).

## ]

@racketblock[Pwhite(lo, hi, length):: || Random numbers, equal distribution ("white noise"). Like ]

@racketblock[rrand(lo, hi):: .
## ]

@racketblock[Pexprand(lo, hi, length):: || Random numbers, exponential distribution. Like ]

@racketblock[exprand(lo, hi):: .
## ]

@racketblock[Pbrown(lo, hi, step, length):: || Brownian motion, arithmetic scale (addition).

## ]

@racketblock[Pfunc(nextFunc, resetFunc):: || Get the stream values from a user-supplied function.
## ]

@racketblock[Pfuncn(func, repeats):: || Get values from the function, but stop after ]

@racketblock[repeats:: items.
## ]

@racketblock[Prout(routineFunc):: || Use the function like a routine. The function should return values using ]

@racketblock[.yield:: or ]

@racketblock[.embedInStream::.
::

]
@section{subsection}
 Additional List Patterns

@section{definitionList}
 
## 
@racketblock[Pser(list, repeats, offset):: || Play through the list as many times as needed, but output only ]

@racketblock[repeats:: items.
## ]

@racketblock[Pslide(list, repeats, len, step, start, wrapAtEnd):: || Play overlapping segments from the list.

## ]

@racketblock[Pwalk(list, stepPattern, directionPattern, startPos):: || Random walk over the list.

## ]

@racketblock[Place(list, repeats, offset):: || Interlace any arrays found in the main list.
## ]

@racketblock[Ppatlace(list, repeats, offset):: || Interlace any patterns found in the main list.
## ]

@racketblock[Ptuple(list, repeats):: || Collect the list items into an array as the return value.
::

]
@section{subsection}
 Additional Random Number Generators

@section{definitionList}
 
## 
@racketblock[Pgbrown(lo, hi, step, length):: || Brownian motion, geometric scale (multiplication).

## ]

@racketblock[Pbeta(lo, hi, prob1, prob2, length):: || Beta distribution, where ]

@racketblock[prob1 = α :: (alpha) and ]

@racketblock[prob2 = β :: (beta).
## ]

@racketblock[Pcauchy(mean, spread, length):: || Cauchy distribution.
## ]

@racketblock[Pgauss(mean, dev, length):: || Guassian (normal) distribution.
## ]

@racketblock[Phprand(lo, hi, length):: || Returns the greater of two equal-distribution random numbers.
## ]

@racketblock[Plprand(lo, hi, length):: || Returns the lesser of two equal-distribution random numbers.
## ]

@racketblock[Pmeanrand(lo, hi, length):: || Returns the average of two equal-distribution random numbers, i.e., ]

@racketblock[(x + y) / 2 ::.
## ]

@racketblock[Ppoisson(mean, length):: || Poisson distribution.

## ]

@racketblock[Pprob(distribution, lo, hi, length, tableSize):: || Arbitrary distribution, based on a probability table.
::

]
@section{section}
 Functional descriptions of patterns

@section{subsection}
 List Patterns

The most obvious thing one would want to do with a pattern is to give it a list of values and have it read them out in order. You have a couple of choices, which differ in their handling of the 
@racketblock[repeats:: parameter.

]
@section{definitionList}
 
## 
@racketblock[Pseq(list, repeats, offset):: || Play through the entire list ]

@racketblock[repeats:: times.
## ]

@racketblock[Pser(list, repeats, offset):: || Play through the list as many times as needed, but output only ]

@racketblock[repeats:: items.

]

@racketblock[
Pseq(#[1, 2, 3], 4).asStream.all;	// 12 items = 4 repeats * 3 items
Pser(#[1, 2, 3], 4).asStream.all;	// 4 items only
::

link::Classes/Pseq:: is an obvious choice for streaming out known pitch and rhythm values.

Before playing a Pbind pattern such as this, make sure the server is booted.

]

@racketblock[
s.boot;

(
p = Pbind(
	\degree, Pseq(#[0, 0, 4, 4, 5, 5, 4], 1),
	\dur, Pseq(#[0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1], 1)
).play;
)
::

To stop the examples in this file, use the "stop" keyboard shortcut (cmd-. on macOS, alt-. on Windows, check documentation for other editors). Or:

]

@racketblock[
p.stop;
::

A variation, link::Classes/Pslide::, plays overlapping segments of the input list.

## ]

@racketblock[Pslide(list, repeats, len, step, start, wrapAtEnd):: || Play overlapping segments from the list.
]
@section{definitionList}
 
## 
@racketblock[repeats:: || number of segments
## ]

@racketblock[len:: || length of each segment
## ]

@racketblock[step:: || is how far to step the start of each segment from previous.
## ]

@racketblock[start:: || what index to start at.
## ]

@racketblock[wrapAtEnd:: || if true (default), indexing wraps around if goes past beginning or end. If false, the pattern stops if it hits a nil element or goes outside the list bounds.
::

If ]

@racketblock[step == 1 ::, then the first segment is at ]

@racketblock[start::, the second at ]

@racketblock[start + 1 ::, and so on.

]

@racketblock[
Pslide(#[1, 2, 3, 4, 5, 6, 7, 8], 10, 3, 1, 0, false).asStream.all;

// or, to show the segments as separate arrays
Pslide(#[1, 2, 3, 4, 5, 6, 7, 8], 10, 3, 1, 0, false).clump(3).asStream.all;

// Flock of Seagulls!
(
p = Pbind(
	\degree, Pslide((-6, -4 .. 12), 8, 3, 1, 0),
	\dur, Pseq(#[0.1, 0.1, 0.2], inf),
	\sustain, 0.15
).play;
)
::
::

]
@section{subsection}
 Random-order list patterns

@section{definitionList}
 
## 
@racketblock[Prand(list, repeats):: || Choose items from the list randomly (same as ]

@racketblock[list.choose::).

]

@racketblock[
// Prand: given scale degrees (pentatonic) with equal probability of each
(
p = Pbind(
	\degree, Prand([0, 1, 2, 4, 5], inf),
	\dur, 0.25
).play;
)
::

## ]

@racketblock[Pxrand(list, repeats):: || Choose randomly, but never repeat the same item twice in immediate succession.

]

@racketblock[
// Pxrand: same as above but never repeats a pitch twice in a row
(
p = Pbind(
	\degree, Pxrand([0, 1, 2, 4, 5], inf),
	\dur, 0.25
).play;
)
::

## ]

@racketblock[Pshuf(list, repeats):: || Shuffle the list in random order, and use the same random order ]

@racketblock[repeats:: times. Like ]

@racketblock[list.scramble::.

]

@racketblock[
// Pshuf: randomly ordered once and repeated
(
p = Pbind(
	\degree, Pshuf([0, 1, 2, 4, 5], inf),
	\dur, 0.25
).play;
)
::

## ]

@racketblock[Pwrand(list, weights, repeats):: || Choose randomly, according to weighted probabilities (same as ]

@racketblock[list.wchoose(weights)::).

]

@racketblock[
// Pwrand: these probabilities favor triadic notes from scale degrees
(
p = Pbind(
	\degree, Pwrand((0..7), [4, 1, 3, 1, 3, 2, 1].normalizeSum, inf),
	\dur, 0.25
).play;
)
::

## ]

@racketblock[Pwalk(list, stepPattern, directionPattern, startPos):: || Random walk over the list. This pattern is a bit more complicated; see its link::Classes/Pwalk##help:: file for details.
::

]
@section{subsection}
 Interlacing values and making arrays

These are opposing operations: interlacing means splitting arrays and merging them into a stream of single values, and arrays can be made out of single-value streams as well.

@section{definitionList}
 
## 
@racketblock[Place(list, repeats, offset):: || Take one from each item in the main array item in succession. Hard to explain, easier to see:

]

@racketblock[
Place([0, [1, 2], [3, 4, 5]], 3).asStream.all;
--> [ 0, 1, 3, 0, 2, 4, 0, 1, 5 ]
::

If we turn this into a matrix and read vertically, the original arrays are clearly visible:

]

@racketblock[
Place([0, [1, 2], [3, 4, 5]], 3).clump(3).do(_.postln);

[ 0, 1, 3 ]	// leftmost column: 0 from first Place item
[ 0, 2, 4 ]	// second column: alternates between 1 and 2, from second Place item
[ 0, 1, 5 ]	// third column: 3, 4, 5 from third Place item
::

## ]

@racketblock[Ppatlace(list, repeats, offset):: || Take one value from each sub-pattern in order.

]

@racketblock[
// Hanon exercise
(
p = Pbind(
	\degree, Ppatlace([
		Pseries(0, 1, 8),	// first, third etc. notes
		Pseries(2, 1, 7)	// second, fourth etc. notes
	], inf),
	\dur, 0.25
).play;
)
::

That's also a taste of things to come: Patterns can be nested.

## ]

@racketblock[Ptuple(list, repeats):: || Get one value from each item in the array, and return all of them as an array of values.

]

@racketblock[
// Chords
// \degree receives [7, 9, 4], then [6, 7, 4] successively, expanded to chords on the server
(
p = Pbind(
	\degree, Ptuple([
		Pseries(7, -1, 8),
		Pseq([9, 7, 7, 7, 4, 4, 2, 2], 1),
		Pseq([4, 4, 4, 2, 2, 0, 0, -3], 1)
	], 1),
	\dur, 1
).play;
)
::
::

]
@section{subsection}
 Arithmetic and geometric series

Now, let's move to patterns that produce values mathematically, without using a predefined list.

@section{definitionList}
 
## 
@racketblock[Pseries(start, step, length):: || Arithmetic series, successively adding ]

@racketblock[step:: to the starting value, returning a total of ]

@racketblock[length:: items.
## ]

@racketblock[Pgeom(start, grow, length):: || Geometric series, successively multiplying the current value by ]

@racketblock[grow::.

]

@racketblock[
// Use Pseries for a scale and Pgeom for an accelerando
(
p = Pbind(
	\degree, Pseries(-7, 1, 15),
	\dur, Pgeom(0.5, 0.89140193218427, 15)
).play;
)
::

strong::Third-party extension alert:: : If you want an arithmetic or geometric series to start at one number and end at another specific number, the step size/multiplier must be calculated from the endpoints and the number of items desired. The strong::ddwPatterns:: quark includes a convenience method, ]

@racketblock[fromEndpoints::, for both Pseries and Pgeom that performs this calculation. It's necessary to give an exact number of repeats, at least two and less than infinity.

]

@racketblock[
p = Pgeom.fromEndpoints(0.5, 0.1, 15);	// error if ddwPatterns not installed
p.postcs;
::

Prints:

]

@racketblock[
Pgeom(0.5, 0.89140193218427, 15)
::
::

]
@section{subsection}
 Random numbers and probability distributions

@section{definitionList}
 
## 
@racketblock[Pwhite(lo, hi, length):: || Produces ]

@racketblock[length:: random numbers with equal distribution ('white' refers to white noise).
## ]

@racketblock[Pexprand(lo, hi, length):: || Same, but the random numbers have an exponential distribution, favoring lower numbers. This is good for frequencies, and also durations (because you need more notes with a shorter duration to balance the weight of longer notes).
## ]

@racketblock[Pbrown(lo, hi, step, length):: || Brownian motion. Each value adds a random ]

@racketblock[step:: to the previous value, where the ]

@racketblock[step:: has an equal distribution between ]

@racketblock[-step:: and ]

@racketblock[+step::.
## ]

@racketblock[Pgbrown(lo, hi, step, length):: || Brownian motion on a geometric scale. Each value multiplies a random ]

@racketblock[step:: factor to the previous value.

## ]

@racketblock[Pbeta(lo, hi, prob1, prob2, length):: || Beta distribution, where ]

@racketblock[prob1 = α :: (alpha) and ]

@racketblock[prob2 = β :: (beta).
## ]

@racketblock[Pcauchy(mean, spread, length):: || Cauchy distribution.
## ]

@racketblock[Pgauss(mean, dev, length):: || Gaussian (normal) distribution.
## ]

@racketblock[Phprand(lo, hi, length):: || Returns the greater of two equal-distribution random numbers.
## ]

@racketblock[Plprand(lo, hi, length):: || Returns the lesser of two equal-distribution random numbers.
## ]

@racketblock[Pmeanrand(lo, hi, length):: || Returns the average of two equal-distribution random numbers, i.e., ]

@racketblock[(x + y) / 2 ::.
## ]

@racketblock[Ppoisson(mean, length):: || Poisson distribution.

## ]

@racketblock[Pprob(distribution, lo, hi, length, tableSize):: || Given an array of relative probabilities across the desired range (a histogram) representing an arbitrary distribution, generates random numbers corresponding to that distribution.
::

To see a distribution, make a histogram out of it.

]

@racketblock[
Pmeanrand(0.0, 1.0, inf).asStream.nextN(10000).histo(200, 0.0, 1.0).plot;
::

]
@section{subsection}
 Catchall Patterns

Not everything is pre-written as a pattern class. These patterns let you embed custom logic.

@section{definitionList}
 
## 
@racketblock[Pfunc(nextFunc, resetFunc):: || The next value is the return value from evaluating ]

@racketblock[nextFunc::. If ]

@racketblock[.reset:: is called on a stream made from this pattern, ]

@racketblock[resetFunc:: is evaluated. The stream will run indefinitely until ]

@racketblock[nextFunc:: returns ]

@racketblock[nil::.

## ]

@racketblock[Pfuncn(func, repeats):: || Like Pfunc, output values come from evaluating the function. Pfuncn, however, returns exactly ]

@racketblock[repeats:: values and then stops. The default number of repeats is 1.

## ]

@racketblock[Prout(routineFunc):: || Use the ]

@racketblock[routineFunc:: in a routine. The stream's output values are whatever this function ]

@racketblock[.yield::s. Prout ends when it yields ]

@racketblock[nil::.
::

Next, we'll look at the central pattern for audio sequencing: link::Classes/Pbind::.

Previous:	link::Tutorials/A-Practical-Guide/PG_01_Introduction::

Next:		link::Tutorials/A-Practical-Guide/PG_03_What_Is_Pbind::
]


