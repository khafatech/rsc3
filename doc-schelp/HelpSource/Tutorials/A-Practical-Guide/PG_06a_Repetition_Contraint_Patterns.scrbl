#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 06a: Repetition Constraint Patterns}
 Patterns that repeat values, or cut other patterns off early@section{related}
  Tutorials/A-Practical-Guide/PG_060_Filter_Patterns, Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Repetition and Constraint patterns

These are essentially flow of control patterns. Each one takes a source pattern and repeats values from it, or stops the stream early based on a preset constraint.

@section{subsection}
 Repetition patterns

These patterns allow you to repeat single values, or (in the case of Pn) entire patterns.

@section{definitionList}
 
## 
@racketblock[Pclutch(pattern, connected):: || If the ]

@racketblock[connected:: pattern is true, link::Classes/Pclutch:: returns the next value from ]

@racketblock[pattern::. If ]

@racketblock[connected:: is false, the previous pattern value is repeated. It's like a clutch in a car: when engaged, the pattern moves forward; when disconnected, it stays where it is.
## ]

@racketblock[Pn(pattern, repeats):: || Embeds the source pattern ]

@racketblock[repeats:: times: simple repetition. This also works on single values: ]

@racketblock[Pn(1, 5):: outputs the number 1 5 times.
## ]

@racketblock[Pstutter(n, pattern):: || ]

@racketblock[n:: and ]

@racketblock[pattern:: are both patterns. Each value from ]

@racketblock[pattern:: is repeated ]

@racketblock[n:: times. If ]

@racketblock[n:: is a pattern, each value can be repeated a different number of times.
## ]

@racketblock[PdurStutter(n, pattern):: || Like Pstutter, except the pattern value is divided by the number of repeats (so that the total time for the repeat cycle is the duration value from the source pattern).

See also link::Classes/Pstep::, described in link::Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns::. Pstep can be used like link::Classes/Pstutter::, but repetition is controlled by time rather than number of repeats per item.

]

@racketblock[
// play repeated notes with a different rhythmic value per new pitch
// using Pstutter
p = Pbind(
		// making 'n' a separate stream so that degree and dur can share it
	\n, Pwhite(3, 10, inf),
	\degree, Pstutter(Pkey(\n), Pwhite(-4, 11, inf)),
	\dur, Pstutter(Pkey(\n), Pwhite(0.1, 0.4, inf)),
	\legato, 0.3
).play;

p.stop;


// using Pfin / Pn
// Pn loops the Pbind infinitely
// Plazy builds a new Pbind for each iteration
// Pfin cuts off the Pbind when it's time for a new value

p = Pn(
	Plazy {
		Pbind(
			\degree, Pfin(rrand(3, 10), rrand(-4, 11)),
			\dur, rrand(0.1, 0.4)
		)
	},
	inf
).play;

p.stop;


// using Pclutch
// the rule is, when degree changes, dur should change also
// if Pdiff returns 0, degree has not changed
// so here, nonzero Pdiff values "connect" the clutch and allow a new dur to be generated
// otherwise the old one is held
p = Pbind(
	\degree, Pstutter(Pwhite(3, 10, inf), Pwhite(-4, 11, inf)),
	\dur, Pclutch(Pwhite(0.1, 0.4, inf), Pdiff(Pkey(\degree)).abs > 0),
	\legato, 0.3
).play;

p.stop;
::
::

]
@section{subsection}
 Constraint (or interruption) patterns

Instead of prolonging a stream by repetition, these patterns use different methods to stop a stream dynamically. They are especially useful for modularizing pattern construction. One section of your code might be responsible for generating numeric or event patterns. By using constraint patterns, that part of the code doesn't have to know how many events or how long to play. It can just return an infinite pattern, and another part of the code can wrap it in one of these to stop it based on the appropriate condition.

@section{definitionList}
 
## 
@racketblock[Pfin(count, pattern):: || Returns exactly ]

@racketblock[count:: values from the source pattern, then stops. (link::Classes/Pfin:: has a cousin, Pfinval, that is deprecated.)
## ]

@racketblock[Pconst(sum, pattern, tolerance):: || Output numbers until the sum goes over a predefined limit. The last output value is adjusted so that the sum matches the limit exactly.
## ]

@racketblock[Pfindur(dur, pattern, tolerance):: || Like Pconst, but applying the "constrain" behavior to the event's rhythmic values. The source pattern runs up to the specified duration, then stops. This is very useful if you know how long a musical behavior should go on, but the number of events to fill up that time is not known.

]

@racketblock[
// Two variants on the same thing
// Use Pconst or Pfindur to create 4-beat segments with randomized rhythm
// Pconst and Pfindur both can ensure the total rhythm doesn't go above 4.0

p = Pn(Pbind(
		// always a low C on the downbeat
	\degree, Pseq([-7, Pwhite(0, 11, inf)], 1),
	\dur, Pconst(4, Pwhite(1, 4, inf) * 0.25)
), inf).play;

p.stop;

p = Pn(Pfindur(4, Pbind(
	\degree, Pseq([-7, Pwhite(0, 11, inf)], 1),
	\dur, Pwhite(1, 4, inf) * 0.25
)), inf).play;

p.stop;
::

## ]

@racketblock[Psync(pattern, quant, maxdur, tolerance):: || Like Pfindur, but does not have a fixed duration limit. Instead, it plays until either it reaches ]

@racketblock[maxdur:: (in which case it behaves like Pfindur, adjusting the last event so the total duration matches ]

@racketblock[maxdur::), or the pattern stops early and the last event is rounded up to the next integer multiple of ]

@racketblock[quant::. This is hard to explain; a couple of examples might make it clearer.

]

@racketblock[
(
// in this case, the pattern stops by reaching maxdur
// elapsed time = 4
var	startTime;
p = (Psync(Pbind(
	\dur, 0.25,	// total duration = infinite
	\time, Pfunc { startTime = startTime ? (thisThread.clock.beats.debug("time")) }
), 1, 4) ++ Pfuncn({
	thisThread.clock.beats.debug("finish time");
	(thisThread.clock.beats - startTime).debug("elapsed");
	nil
}, 1)).play;
)

(
// in this case, the pattern stops itself before maxdur (4)
// the Pbind's duration (1.25) gets rounded up to 2 (next multiple of 1)
var	startTime;
p = (Psync(Pbind(
	\dur, Pn(0.25, 5),	// total duration = 0.25 * 5 = 1.25
	\time, Pfunc { startTime = startTime ? (thisThread.clock.beats.debug("time")) }
), 1, 4) ++ Pfuncn({
	thisThread.clock.beats.debug("finish time");
	(thisThread.clock.beats - startTime).debug("elapsed");
	nil
}, 1)).play;
)
::
::

Previous:	link::Tutorials/A-Practical-Guide/PG_060_Filter_Patterns::

Next:		link::Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns::
]


