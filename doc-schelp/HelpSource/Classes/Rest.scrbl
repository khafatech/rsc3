#lang scribble/manual
@(require (for-label racket))

@title{Rest}
 Represents a rest in event patterns@section{categories}
  Streams-Patterns-Events
@section{related}
  Classes/Pbind, Classes/Event

@section{description}

Rest may be used in event patterns to indicate that the resulting event should be a rest (i.e., silent). It should be used in one of the child patterns belonging to a Pbind, for instance.

@section{subsection}
  Expressing rests in event patterns

In addition to the Rest class, rests can be specified in two other ways (legacy usages).

@section{list}
 
## A link::Classes/Symbol:: may be specified in any frequency stream (under the keys degree, note, midinote or freq). The exception to this rule is control bus mapping symbols, beginning with 'c' followed by a number. Typical symbols that have been used include strong::\rest::, strong::\r:: and the empty symbol strong:: \ ::.


@racketblock[
p = Pbind(
	\degree, Pseq([
		0, 1, 2, 0, 0, 1, 2, 0,
		2, 3, 4, \rest, 2, 3, 4, \rest
	]),
	\dur, 0.25
).play;
::

## The event's strong::\type:: may be set to strong::\rest::.

]

@racketblock[
p = Pbind(
	\degree, Pseries(0, 1, inf).fold(-7, 7),
	\dur, 0.125,
	\type, Pwrand([\note, \rest], [0.9, 0.1], inf)
).play;

p.stop;
::
::

The Rest class allows rests to be indicated in any stream, not only frequency or event type. Also, using the duration argument (see the *new method below), rests may be embedded into a duration stream. That is, rests may be treated as part of the rhythmic specification, rather than the pitch specification.

]
@section{subsection}
  Usage

@section{list}
 
## The class Rest may be embedded directly in a child pattern. This sets the isRest flag and puts the number 1 into the event.
## Or, a Rest instance may be embedded. Rest.new's argument specifies the value that will be placed into the event. This allows rests to be given in a duration stream -- the argument is the rest's rhythmic value.
::

@section{section}
  How it works

When a Pbind child pattern returns a Rest, the Rest object sets a flag 'isRest' in the resulting event to be true. The child pattern's value in the event is a number. This is to prevent math errors if Rest is used in a pitch or duration stream (degree, note, midiNote, freq, dur, delta, stretch).

When a Rest is returned from a child stream, the rest object itself will not appear in the event. You can tell that the event is a rest by the presence of 
@racketblock['isRest': true::.

]

@racketblock[
p = Pbind(\degree, Pseq([4, Rest], 1)).asStream;

p.next(());
// prints: ( 'degree': 4 )

p.next(());
// prints: ( 'isRest': true, 'degree': 1 )
::

That is, the importance of the Rest object is not that it appears in the event. The importance is the side effect that this object has on the resulting event.

]
@section{CLASSMETHODS}
 
All methods of Rest except *new are private, and should not be used directly.

@section{private}
  processRest
@section{private}
  embedInStream
@section{private}
  asStream

@section{METHOD}
  new
Create an instance of Rest, with a value to be used in the resulting rest event.

@section{argument}
  dur
Typically, Rest instances will be used in duration streams, so the argument should be the rest's rhythmic value, or duration. Rest instances may also be used in any other stream, but the value will be ignored (since the event will do nothing except take time).


@section{INSTANCEMETHODS}
 
@section{private}
  dur
@section{private}
  processRest
@section{private}
  embedInStream
@section{private}
  asStream

@section{EXAMPLES}
 

Using the Rest class in a pitch stream


@racketblock[
p = Pbind(
	\degree, Pif(
		0.1.loop.coin,
		Rest,
		Pseries(0, 1, inf).fold(-7, 7)
	),
	\dur, 0.125
).play;

p.stop;
::

Using a Rest instance in a duration stream
]

@racketblock[
p = Pbind(
	\degree, Pseries(0, 1, inf).fold(-7, 7),
	\dur, Pseq([Pn(0.125, { rrand(3, 6) }), Rest(0.25)], inf)
).play;

p.stop;
::
]


