#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 06c: Composition of Patterns}
 Making multiple event patterns act as one@section{related}
  Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns, Tutorials/A-Practical-Guide/PG_06d_Parallel_Patterns
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Adding values to a base event pattern (Or, "Pattern Composition")

One way to use patterns is to write everything into the pattern up front. This has the advantage of clarity and ease of understanding. Another way is to modularize the behavior by creating smaller, simpler patterns and combining their results into single events that have keys and values from all the component patterns.

This is related to the computer science concept of "function composition," in which a complex calculation can be written not as a single large function, but as several smaller functions that are then chained together into a single function. Since Functions are normal objects in SuperCollider, it's easy to do an operation on a function that returns a composite function (which may then be used like any other function). http://en.wikipedia.org/wiki/Function_composition_(computer_science)

In mathematics, the 
@racketblock[·:: operator represents function composition.

]

@racketblock[
f(x) = x + 1
g(x) = x * 2

g · f = g(f(x)) = (x + 1) * 2
::

]

@racketblock[g · f:: means to evaluate ]

@racketblock[f:: first, then pass its result to ]

@racketblock[g::. The ]

@racketblock[·:: operator is written as ]

@racketblock[<>:: in SuperCollider.

]

@racketblock[
f = { |x| x + 1 };
g = { |x| x * 2 };

h = (g <> f);
--> a Function

h.value(1);
--> 4	// == (1+1) * 2

(f <> g).value(1)
--> 3	// == (1*2) + 1

// g · f == g(f(x)) -- f is evaluated first, and its result is passed to g
g.value(f.value(1));
--> 4
::

Event patterns can be similarly composed.

]
@section{definitionList}
 
## 
@racketblock[Pbindf(pattern, pairs):: || Adds new key-value pairs onto a pre-existing Pbind-style pattern. ]

@racketblock[Pbindf(Pbind(\a, patternA), \b, patternB, \c, patternC):: gets the same result as ]

@racketblock[Pbind(\a, patternA, \b, patternB, \c, patternC):: .
## ]

@racketblock[Pchain(patterns):: || Chains separate Pbind-style patterns together, so that all their key-value pairs go into the same event. For example, if one part of your code creates a Pbind instance ]

@racketblock[a = Pbind(\a, patternA):: and another part creates ]

@racketblock[b = Pbind(\b, patternB, \c, patternC)::, you could append ]

@racketblock[\b:: and ]

@racketblock[\c:: into the ]

@racketblock[\a:: result using ]

@racketblock[Pchain(b, a):: . The subpatterns evaluate in reverse order, in keeping with function composition notation.

For musical purposes, you could have one part of your code create a pattern defining rhythm and another part defining pitch material, then combine them with link::Classes/Pchain::.

]

@racketblock[
~rhythm = Pbind(
	\dur, Pwrand(#[0.125, 0.25, 0.5], #[0.3, 0.5, 0.2], inf),
	\legato, Pwrand(#[0.1, 0.6, 1.01], #[0.1, 0.3, 0.6], inf)
);
~melody = Pbind(
	\degree, Pwhite(-4, 11, inf)
);

p = Pchain(~melody, ~rhythm).play;
p.stop;
::

That in itself has some good potential for algorithmic composition. Introducing link::Classes/EventPatternProxy:: into the mix makes it possible to swap different melody and rhythm components in and out on the fly, with no interruption. We can even change the type of pattern ( link::Classes/Pbind::, link::Classes/Pmono::, link::Classes/PmonoArtic:: ) with no ill effect.

]

@racketblock[
~rhythm = EventPatternProxy(Pbind(
	\dur, Pwrand(#[0.125, 0.25, 0.5], #[0.3, 0.5, 0.2], inf),
	\legato, Pwrand(#[0.1, 0.6, 1.01], #[0.1, 0.3, 0.6], inf)
));

~melody = EventPatternProxy(Pbind(
	\degree, Pwhite(-4, 11, inf)
));

p = Pchain(~melody, ~rhythm).play;

~melody.source = PmonoArtic(\default, \degree, Pseries(4, Prand(#[-1, 1], inf), inf).fold(-4, 11));

~melody.source = Pbind(\degree, Pseries(4, Pwrand(#[-2, -1, 1, 2], #[0.3, 0.2, 0.2, 0.3], inf), inf).fold(-4, 11));

p.stop;
::
::

]
@section{subsection}
 Pset and cousins

A group of pattern classes allow single event keys to be overwritten, or one addition or multiplication to be performed. link::Classes/Pkey::, in combination with the link::Classes/Pchain:: or link::Classes/Pbindf:: "pattern composition" classes, can do everything the following classes can do (though this alternate notation may be more convenient in certain cases).

@section{definitionList}
 
## 
@racketblock[Pset(name, value, pattern):: || Get one event from ]

@racketblock[pattern::, and then put the next value from the ]

@racketblock[value:: pattern into the ]

@racketblock[name:: key. If the source pattern specifies a value for the same name, the value from the source will be replaced with the new one.
## ]

@racketblock[Padd(name, value, pattern):: || After getting the next event, replace the ]

@racketblock[name:: value with its existing value ]

@racketblock[+:: the next number from ]

@racketblock[value::.
## ]

@racketblock[Pmul(name, value, pattern):: || After getting the next event, replace the ]

@racketblock[name:: value with its existing value ]

@racketblock[*:: the next number from ]

@racketblock[value::.
::

These patterns remain in the library mainly for reasons of backward compatibility, since their behavior can be replicated easily using link::Classes/Pbindf::.

]
@section{table}
 
## 
@racketblock[Pset(name, value, pattern):: || ]

@racketblock[Pbindf(pattern, name, value)::
## ]

@racketblock[Padd(name, value, pattern):: || ]

@racketblock[Pbindf(pattern, name, Pkey(name) + value)::
## ]

@racketblock[Pmul(name, value, pattern):: || ]

@racketblock[Pbindf(pattern, name, Pkey(name) * value)::
::

The patterns link::Classes/Psetpre::, link::Classes/Paddpre::, and link::Classes/Pmulpre:: reverse the order of evaluation. link::Classes/Pchain:: is able to duplicate this functionality.

]
@section{definitionList}
 
## 
@racketblock[Psetpre(name, value, pattern):: || Get the next ]

@racketblock[value:: and put it into the event prototype before evaluating ]

@racketblock[pattern::.
::

]
@section{table}
 
## 
@racketblock[Psetpre(name, value, pattern):: || ]

@racketblock[Pchain(pattern, Pbind(name, value))::
## ]

@racketblock[Paddpre(name, value, pattern):: || ]

@racketblock[Pchain(pattern, Pbind(name, Pkey(name) + value))::
::
Similar for ]

@racketblock[Pmulpre::.

A third group -- link::Classes/Psetp::, link::Classes/Paddp::, link::Classes/Pmulp:: -- behave slightly differently, nesting pattern evaluation.

Previous:	link::Tutorials/A-Practical-Guide/PG_06b_Time_Based_Patterns::

Next:		link::Tutorials/A-Practical-Guide/PG_06d_Parallel_Patterns::
]


