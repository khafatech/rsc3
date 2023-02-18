#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 060: Filter Patterns}
 Overview of patterns that modify the behavior of other patterns@section{related}
  Tutorials/A-Practical-Guide/PG_05_Math_on_Patterns, Tutorials/A-Practical-Guide/PG_06a_Repetition_Contraint_Patterns
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Filter patterns

Just like filter UGens modify an input signal, filter patterns modify the stream of values coming from a pattern.

We have already seen some operations that modify a stream of values: math operators (which render as link::Classes/Punop::, link::Classes/Pbinop:: or link::Classes/Pnaryop:: patterns) and certain collection methods (mainly 
@racketblock[collect::, ]

@racketblock[select:: and ]

@racketblock[reject::). Filter pattern classes can do some other surprising and useful things.

All filter patterns take at least one source pattern, providing the values/events to be filtered. Some filter patterns are designed for value patterns, others for event patterns. A handful work equally well with both single values and events.

Following is a categorized overview. See the separate category documents for more detail.

]
@section{subsection}
 Repetition and Constraint patterns
link::Tutorials/A-Practical-Guide/PG_06a_Repetition_Contraint_Patterns::

@section{definitionList}
 
## 
@racketblock[Pclutch(pattern, connected):: || If the ]

@racketblock[connected:: pattern is true, Pclutch returns the next value from ]

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

## ]

@racketblock[Pfin(count, pattern):: || Returns exactly ]

@racketblock[count:: values from the source pattern, then stops.
## ]

@racketblock[Pconst(sum, pattern, tolerance):: || Output numbers until the sum reaches a predefined limit. The last output value is adjusted so that the sum matches the limit exactly.
## ]

@racketblock[Pfindur(dur, pattern, tolerance):: || Like Pconst, but applying the "constrain" behavior to the event's rhythmic values. The source pattern runs up to the specified duration, then stops. This is very useful if you know how long a musical behavior should go on, but the number of events to fill up that time is not known.
## ]

@racketblock[Psync(pattern, quant, maxdur, tolerance):: || Like Pfindur, but does not have a fixed duration limit. Instead, it plays until either it reaches ]

@racketblock[maxdur:: (in which case it behaves like Pfindur, adjusting the last event so the total duration matches ]

@racketblock[maxdur::), or the pattern stops early and the last event is rounded up to the next integer multiple of quant.
::

]
@section{subsection}
 Time-based patterns
link::Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns::

@section{definitionList}
 
## 
@racketblock[Ptime(repeats):: || Returns the amount of time elapsed since embedding.
## ]

@racketblock[Pstep(levels, durs, repeats):: || Repeat a ]

@racketblock[level:: value for its corresponding duration, then move to the next.
## ]

@racketblock[Pseg(levels, durs, curves, repeats):: || Similar to Pstep, but interpolates to the next value instead of stepping abruptly at the end of the duration. Interpolation is linear by default, but any envelope segment curve can be used. ]

@racketblock[levels::, ]

@racketblock[durs:: and ]

@racketblock[curves:: should be patterns. Related: Use of link::Classes/Env:: as a pattern.
::

]
@section{subsection}
 Adding values into event patterns (Or, "Pattern Composition")
link::Tutorials/A-Practical-Guide/PG_06c_Composition_of_Patterns::

@section{definitionList}
 
## 
@racketblock[Pbindf(pattern, pairs):: || Adds new key-value pairs onto a pre-existing Pbind-style pattern.
## ]

@racketblock[Pchain(patterns):: || Chains separate Pbind-style patterns together, so that all their key-value pairs go into the same event.
::

]
@section{subsection}
 Parallelizing event patterns
link::Tutorials/A-Practical-Guide/PG_06d_Parallel_Patterns::

@section{definitionList}
 
## 
@racketblock[Ppar(list, repeats):: || Start each of the event patterns in the ]

@racketblock[]
@section{list}
  at the same time. When the last one finishes, the Ppar also stops. If 
@racketblock[repeats:: > 1, all the subpatterns start over again from the beginning.
## ]

@racketblock[Ptpar(list, repeats):: || Here, the list consists of ]

@racketblock[[timeOffset0, pattern0, timeOffset1, pattern1...]:: . Each pattern starts after the number of beats given as its time offset. The patterns can start at different times relative to each other.
## ]

@racketblock[Pgpar(list, repeats):: || Like Ppar, but it creates a separate group for each subpattern.
## ]

@racketblock[Pgtpar(list, repeats):: || This is supposed to be like Ptpar with separate groups for the sub patterns, but the class is currently broken.

## ]

@racketblock[Pspawner(routineFunc):: || The function is used to make a routine. A Spawner object gets passed into this routine, and this object is used to add or remove streams to/from the parallel stream. New patterns can be added in sequence or in parallel.
## ]

@racketblock[Pspawn(pattern, spawnProtoEvent):: || Supports most of the features of Pspawner, but uses a pattern to control the link::Classes/Spawner:: object instead of a routine function.
::

]
@section{subsection}
 Language control methods
link::Tutorials/A-Practical-Guide/PG_06e_Language_Control::

Some patterns mimic language-style control methods: conditionals ( link::Classes/Pif:: ), loops ( link::Classes/Pwhile:: ) and error cleanup ( link::Classes/Pprotect:: ).

@section{definitionList}
 
## 
@racketblock[Pif(condition, iftrue, iffalse, default):: || Evaluates a pattern ]

@racketblock[condition:: that returns true or false. Then, one value is taken from the true or false branch before going back to evaluate the condition again. The ]

@racketblock[default:: value or pattern comes into play when the true or false branch stops producing values (returns nil). If the ]

@racketblock[default:: is not given, Pif returns control to the parent upon nil from either branch.
## ]

@racketblock[Pseed(randSeed, pattern):: || Random number generators depend on seed values; setting a specific seed produces a repeatable stream of pseudorandom numbers. Pseed sets the random seed before embedding ]

@racketblock[pattern::, effectively restarting the random number generator at the start of the pattern.
## ]

@racketblock[Pprotect(pattern, func):: || Like the ]

@racketblock[protect:: error handling method, if an error occurs while getting the next value from the pattern, the function will be evaluated before the error interrupts execution.
## ]

@racketblock[Ptrace(pattern, key, printStream, prefix):: || For debugging, Ptrace prints every return value. Is your pattern really doing what you think? This will tell you. A Ptrace is created automatically by the ]

@racketblock[trace:: message: ]

@racketblock[aPattern.trace(key, printStream, prefix):: --> ]

@racketblock[Ptrace(aPattern, key, printStream, prefix)::.
## ]

@racketblock[Pwhile(func, pattern):: || Like link::Reference/Control-Structures##while:: as long as the function evaluates to true, the pattern is embedded. The function is checked once at the beginning and thereafter when the pattern comes to an end. If it's applied to an infinite pattern, there's no looping because the pattern never gives control back.
::

]
@section{subsection}
 Server control methods
link::Tutorials/A-Practical-Guide/PG_06f_Server_Control::

@section{definitionList}
 
## 
@racketblock[Pbus(pattern, dur, fadeTime, numChannels, rate):: || Creates a private group and bus for the synths played by the pattern. The group and bus are released when the pattern stops. Useful for isolating signals from different patterns.
## ]

@racketblock[Pgroup(pattern):: || Creates a private group (without private bus) for the pattern's synths.

## ]

@racketblock[Pfx(pattern, fxname, pairs):: ||
## ]

@racketblock[Pfxb(pattern, fxname, pairs):: || Both of these patterns play an effect synth at the tail of the target group. This synth should read from the bus identified by the ]

@racketblock[out:: argument, and write the processed signal onto the same bus using either link::Classes/ReplaceOut:: or link::Classes/XOut::. Pfx uses whatever bus and group are specified in the incoming event. Pfxb allocates a separate bus and group for the effect and the pattern.

## ]

@racketblock[Pproto(makeFunction, pattern, cleanupFunc):: || Allocate resources on the server and add references to them into the event prototype used to play ]

@racketblock[pattern::. When the pattern stops (or is stopped), the resources can be removed automatically.
::

]
@section{subsection}
 Data sharing
link::Tutorials/A-Practical-Guide/PG_06g_Data_Sharing::

@section{definitionList}
 
## 
@racketblock[Pkey(key):: || Read the ]

@racketblock[key:: in the input event, making previously-calculated values available for other streams.
## ]

@racketblock[Penvir(envir, pattern, independent):: || Run the pattern inside a given environment.
## ]

@racketblock[Pfset(func, pattern):: || Assign default values into the input event before getting each result event out of the given pattern.
## ]

@racketblock[Plambda(pattern, scope):: || Creates a "function scope" into which values are assigned using link::Classes/Plet::, and from which values are retrieved with link::Classes/Pget::. Pget is somewhat like link::Classes/Pkey::, except that its scope is strictly internal, hidden from the caller. With Pkey, the source values remain present in the event returned to the caller.
::

Previous:	link::Tutorials/A-Practical-Guide/PG_05_Math_on_Patterns::

Next:		link::Tutorials/A-Practical-Guide/PG_06a_Repetition_Contraint_Patterns::
]


