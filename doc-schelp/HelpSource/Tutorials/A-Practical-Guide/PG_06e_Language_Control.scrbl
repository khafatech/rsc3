#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 06e: Language Control}
 Patterns that mimic some language-side control structures@section{related}
  Tutorials/A-Practical-Guide/PG_06d_Parallel_Patterns, Tutorials/A-Practical-Guide/PG_06f_Server_Control
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Language control methods

Some patterns mimic language-style control methods: conditionals ( link::Classes/Pif:: ), loops ( link::Classes/Pwhile:: ) and error cleanup ( link::Classes/Pprotect:: ).

@section{definitionList}
 
## 
@racketblock[Pif(condition, iftrue, iffalse, default):: || Evaluates a pattern ]

@racketblock[condition:: that returns true or false. Then, one value is taken from the true or false branch before going back to evaluate the condition again. The ]

@racketblock[default:: value or pattern comes into play when the true or false branch stops producing values (returns nil). If the ]

@racketblock[default:: is not given, Pif returns control to the parent upon nil from either branch.

]

@racketblock[
p = Pbind(
	\degree, Pwhite(0, 11, inf),
		// odd numbered scale degrees get a shorter rhythmic value
	\dur, Pif(Pkey(\degree).odd, 0.25, 0.5)
).play;

p.stop;
::

## ]

@racketblock[Pseed(randSeed, pattern):: || Random number generators depend on seed values; setting a specific seed produces a repeatable stream of pseudorandom numbers. link::Classes/Pseed:: sets the random seed before embedding ]

@racketblock[pattern::, effectively restarting the random number generator at the start of the pattern.

]

@racketblock[
p = Pbind(
		// the random seed is generated once, when creating the Pattern object
		// so the same random seed is used every time whenever this pattern object plays
	\degree, Pseed(0x7FFFFFFF.rand, Pseries({ rrand(-7, 0) }, Pwhite(1, 3, inf), { rrand(4, 10) })),
	\dur, 0.25
);

q = p.play;	// uses one seed
q.stop;

r = p.play;	// uses the same seed
r.stop;

// reexecute the p = Pbind... and the seed will be different
::

## ]

@racketblock[Pprotect(pattern, func):: || Like the ]

@racketblock[protect:: error handling method, if an error occurs while getting the next value from the pattern, the function will be evaluated before the error interrupts execution.
## ]

@racketblock[Ptrace(pattern, key, printStream, prefix):: || For debugging, Ptrace prints every return value. Is your pattern really doing what you think? This will tell you. A Ptrace is created automatically by the ]

@racketblock[trace:: message: ]

@racketblock[aPattern.trace(key, printStream, prefix) --> Ptrace(aPattern, key, printStream, prefix):: .
## ]

@racketblock[Pwhile(func, pattern):: || Like while: as long as the function evaluates to true, the pattern is embedded. The function is checked once at the beginning and thereafter when the pattern comes to an end. If it's applied to an infinite pattern, there's no looping because the pattern never gives control back.

]

@racketblock[
// Pwhile and Ptrace
(
~go = true;
p = Pwhile({ ~go }, Pbind(
	\degree, Pseries({ rrand(-7, 0) }, Pwhite(1, 3, inf), { rrand(4, 10) })
		.trace(prefix: "degree: "),
	\dur, 0.25
)).play;
)

~go = false;	// will stop the whole pattern when the Pbind comes to an end
::
::

Previous:	link::Tutorials/A-Practical-Guide/PG_06d_Parallel_Patterns::

Next:		link::Tutorials/A-Practical-Guide/PG_06f_Server_Control::
]


