#lang scribble/manual
@(require (for-label racket))

@title{Event}
an environment that represents an action@section{related}
 Classes/Pattern
@section{categories}
 Collections>Unordered, Streams-Patterns-Events>Events

@section{description}

An Event is an Environment that specifies an action to be taken in response to a link::#-play:: message. The key/value pairs within the Event specify the parameters of that action. Most methods, Event inherits from its superclasses, especially from link::Classes/Dictionary::.

@section{CLASSMETHODS}
 

@section{private}
 initClass

@section{subsection}
 Class variables

@section{method}
 parentEvents
An IdentityDictionary of useful parent events.
@section{method}
 partialEvents
An IdentityDictionary of Events that define the default values and functions for different aspects of note generation (timing, volume, pitch, server to use, etc).

@section{subsection}
 Creation methods

@section{method}
 new
create an event with initial size strong::n::.
@section{argument}
 n
Initial size.
@section{argument}
 proto
May be provided as another event which is used to override keys in the event.
@section{argument}
 parent
May be provided as another event which is used to provide default keys for the event without modifying it.
@section{argument}
 know
If strong::know:: is set to link::Classes/True::, the event will respond to appropriate message calls. See link::Classes/Environment:: for more details.

@section{method}
 default
Returns an empty event with link::#.defaultParentEvent:: as parent.

@section{method}
 silent
Returns an event that describes a pause of strong::dur:: duration.

@section{method}
 addEventType
Event types define alternate play functions that are selected by the value of strong::~type::.
@section{argument}
 type
A name (usually a symbol) for the event type, which can be used to select it

@section{argument}
 func
A function which optionally takes the server as a first argument


@racketblock[
Event.addEventType(\happyEvent, { ("I am so happy to be silent sometimes, says" + ~who).postln; });
Pbind(\type, \happyEvent, \who, Prand(["Alice", "Bob", "Eve"], inf), \dur, Pwhite(0.1, 1.0, inf)).play;

// To a certain degree, it is possible to reuse some of another event type's functionality:
(
Event.addEventType(\happyEvent, { |server|
	~octave = [5, 6, 7]; // always play three octaves
	~detune = 10.0.rand2; // always play a bit out of tune
	~type = \note; // now set type to a different one
	currentEnvironment.play;
});
Pbind(\type, \happyEvent, \degree, Pseq([0, 1, 2, 3, 4, 4, 5, 5, 5, 5, 4, 2, 3, 2, 3, 1], inf), \dur, Pwhite(0.1, 1.0, inf)).play;
);

::


]
@section{method}
 makeDefaultSynthDef
This method is called in order to build the default SynthDef, which is stored under the key strong::\default::

@racketblock[
SynthDef(\default, { Out.ar(0, Line.kr(0.3, 0, 0.5) * SinOsc.ar(Rand(300, 500.0)) ) }).add; // overwrite default
(freq: 600).play;
Event.makeDefaultSynthDef; // reset default
(freq: 600).play;
::

]
@section{INSTANCEMETHODS}
 

@section{method}
 play
Play the event. This evaluates the function at strong::\play::.

@racketblock[
(freq: 800).play;
(play: {  "I rather do something else: ".post; ~x.postln; }, x: 800.rand).play;
::

]
@section{method}
 delta
Returns the inter onset time - the time delay until the next event in a sequence. This usually depends on strong::\dur:: and strong::\stretch::, but may be overridden by specifying strong::\delta:: directly.

@racketblock[
Pn((dur: 2, freq:8000)).play;
::

]
@section{method}
 next
Combines an event given in the argument with the current event. This is used to enable events to be composed.

@racketblock[
(a: 6, b: 7).next((c: 100));
::

]
@section{copymethod}
  Dictionary -embedInStream

@section{method}
 playAndDelta
Used by link::Classes/EventStreamPlayer:: to play Events and obtain a time increment.

@section{method}
 isRest
Returns strong::true:: if the event will be played as a rest, and strong::false:: otherwise. See link::Classes/Rest:: for a more complete discussion of rests in event patterns.

@section{method}
 asUGenInput
Calls link::#-asControlInput::.

@section{method}
 asControlInput
Enables events to represent the server resources they created in an Event.

@section{subsection}
 Methods that allow Events to provide user control for Synths on Groups

@section{method}
 synth
Makes the event a control interface to the resultant link::Classes/Synth:: when played.

@section{method}
 group
Makes the event a control interface to the resultant link::Classes/Group:: when played. This is experimental, does not work consistently yet.

@section{method}
 stop
Free the link::Classes/Synth:: or link::Classes/Group::.

@section{method}
 pause
Pause the link::Classes/Synth:: or link::Classes/Group::.

@section{method}
 resume
Resume the link::Classes/Synth:: or link::Classes/Group::.

@section{method}
 release
Release the link::Classes/Synth:: or link::Classes/Group::.

@section{method}
 set
Set a control value in the link::Classes/Synth:: or link::Classes/Group::.
(key1, val1, ....)

@racketblock[
a = (note: 2).play;
a.set(\freq, 700);
a.release;
::


]
@section{SECTION}
 Basic Usage

Events can be written as a series of key value pairs enclosed in parentheses. Empty parentheses will create an empty event. They may be also used for object prototyping - see link::Classes/Environment:: for more details.

@section{subsection}
 Event as a name space for keeping objects

Because of this simple syntax, Events are often used as name space for keeping objects:

@racketblock[
// using an event to store values
q = (n: 10, x: [1, 2, 3]);
q[\y] = q[\x] * q[\n];	// similar to ~y = ~x * ~n, but in a separate name space
q.y = q.x * q.n;	// shorter way to do the same (pseudo-methods)
q.y;			// [ 100, 200, 300 ]
::

]
@section{subsection}
 Event for specifying different things to happen

Event provides a link::#.defaultParentEvent:: that defines a variety of different event types and provides a complete set of default key/value pairs for each type. The type is determined by the value of the key strong::\type:: which defaults to strong::\@section{note}
 . Note events create synths on the server.

@racketblock[
( ).play;			// the default note
(freq: 500, pan: -1) .play;	// 500 Hz, panned left
(play: { ~what.postln }, what: "hello happening").play;	// something else
::
Per default, the play message derives its behaviour from the link::#.defaultParentEvent::, which provides many default values, such as default instrument (\default), note (0), legato (0.8) and so on. Depending on the event type, these may differ completely and need not even represent a sound.

]
@section{subsection}
 Events and SynthDefs

The key used to select what synthdef is to be played is strong::\instrument::. In order to use a link::Classes/SynthDef:: with an Event, send it an strong::add:: message. This creates a description of the SynthDef that the event can consult to determine its control names. The values of these names in the event are used when the event is played. (See link::Classes/SynthDesc:: for details.)

@racketblock[
(
SynthDef(\pm, { |out=0, freq=440, amp=0.1, pan=0, gate=1, ratio = 1, index = 1, ar = 0.1, dr = 0.1|
	var z;
	z = LPF.ar(
		PMOsc.ar(freq, freq * ratio, Linen.kr(gate, ar,index, dr), 0, 0.3),
		XLine.kr(Rand(4000, 5000), Rand(2500, 3200), 1)
	) * Linen.kr(gate, 0.01, 0.7, dr, 2);
	OffsetOut.ar(out, Pan2.ar(z, pan, amp));
}).add;
);

(instrument: \pm).play;

(instrument: \pm, ratio: 3.42, index: 12, freq: 150, ar: 8, dr: 3, sustain: 10).play;
::

]
@section{note}
 
The use of link::Classes/OffsetOut:: in the SynthDef prevents irregularities that can result from the interaction of the timing of a sequence of notes and the control rate of the Server.
::

@section{subsection}
 Multi-channel Expansion

If a key relevant to the action is assigned an link::Classes/Array::, the action is repeated on each element of the array:

@racketblock[
(degree: (0..12)).play;		// a diatonic cluster
::
If several keys are assigned arrays, the action is repeated for each element of the largest array.
Smaller arrays are rotated through repeatedly. Here are some examples:
]

@racketblock[
// every other note of the diatonic cluster: stacked thirds
(degree: (0..12), amp: [0, 0.1]).play;

// every other note of the semitone cluster: a wholetone cluster again
(note: (0..12), amp: [0, 0.1]).play;

// every third note of the semitone cluster: a diminished chord
(note: (0..12), amp: [0, 0, 0.1]).play;

// the same with different sustain times
(note: (0..12), amp: [0, 0, 0.1], sustain:[0.1, 0.3, 1.3, 2.5]).play;

// timingOffset gives a tempo-relative offset time to each synth
(instrument: \pm, ratio: [2.3, 4.5, 1.7], timingOffset: [0, 1.2, 3], sustain: [0.2, 2, 1]).play;
::

In the \note event, all keys multichannel expand apart from: \instrument, \dur, \delta, \strum.

]
@section{subsection}
 Arrayed Arguments

It is possible to assign an array to one of a link::Classes/SynthDef::'s control names. For example:

@racketblock[
(
SynthDef(\test, {
	| out = 0, amp = 0.01, freq = #[300,400,400], pan, gate = 1 |
	var audio, env;
	audio = Mix.ar(Pulse.ar(freq, 0.5));	// this is a mixture of three oscillators
	env = Linen.kr(gate, susLevel: amp , doneAction: Done.freeSelf);
	audio = audio * env;
	OffsetOut.ar(0, audio);
}).add;
)
::
Within an event, arrayed arguments of this sort must be enclosed within an additional array to distinguish them from arguments intended for multi-channel expansion.
]

@racketblock[
// one synth, use enclosing array to prevent multi-channel expansion
(instrument: \test, note: [[0, 2, 4]]).play;

// two synths
(instrument: \test, note: [[0, 2, 4], [6, 8, 10]]).play;
::

]
@section{subsection}
 Events and Patterns

Events are closely integrated with the Patterns library. Different patterns can be bound to different keys (or collections of keys) to specify the resultant music. See the help files link::Classes/Pattern:: and link::Classes/Pbind:: and the tutorials link::Tutorials/Streams-Patterns-Events4:: and link::Tutorials/Streams-Patterns-Events5:: for more details on Patterns.

Patterns that return events may be played on a clock: dur specifies the time between two subsequent events.

@racketblock[
// Pseq returns one item in the list after the other
(
Pseq([
	(note: 2, sustain: 1, dur: 1.5),
	(note: [5, 7], sustain: 0.5, dur: 0.8),
	(note: [2, 6], sustain: 1, dur: 0.8)
]).play;
)

// Pbind binds parameters to events:
(
Pbind(
	\note, Pseq([2, [5, 7], [2, 6]]),
	\sustain, Pseq([1, 0.5, 1]),
	\dur, Pseq([1.5, 0.8, 0.8])
).play;
)

// per-event timing may be specified:
(
Pbind(
	\note, Pseq([[0, 9], [5, 7], [2, 6]], inf),
	\sustain, Pseq([1, 0.5, 1], inf),
	\dur, Pseq([1.5, 0.8, 0.8], inf),
	\timingOffset, Pseq([[0, 0.3], [0, 0.01]], inf)
).play;
)
::

Here is an example that illustrates some more of the keys defined by the link::#.defaultParentEvent::. Note that it is equivalent to write ]

@racketblock[Pbind(\key, val, ...):: and ]

@racketblock[Pbind(*[key: val, ...])::.
]

@racketblock[
(
Pbind(*[
	stepsPerOctave:	Pstep(Pseq((2..12).mirror, inf),12),	// 3 - 12 tone e.t. scales
	note:		Pseq((0..12).mirror, inf),		// play full notes up and down
	ctranspose:	Pwhite(-0.2, 0.2),			// detune up to +-20 cents
	detune:		Pwhite(-1.0, 1.0),			// detune up to 1 Hz
	sustain:	Prand([0.2, 0.2, 0.2, 4], inf), 	// notes last 0.2 or 4 seconds
				// 1 in 6 chance waits 0.8 seconds:
	dur:		Prand([0.2, 0.2, 0.2, 0.2, 0.2, 0.8], inf),
	db:		Pstep(Pseq([-15, -25, -20, -25], inf), 0.8)// 4 beat accent structure
]).play;
)
::

]
@section{subsection}
 Event's play method

When an Event (or any other link::Classes/Environment::) receives a 
@racketblock[use(function):: message, it sets itself to be currentEnvironment, evaluates the function, and restores the original value of currentEnvironment. This allows the function to access and alter the contents of the event using the following shortcuts:
]

@racketblock[~keyName:: which is equivalent to	]

@racketblock[currentEnvironment.at(keyName):: and
]

@racketblock[~keyName = value:: which is equivalent to ]

@racketblock[currentEnvironment.put(keyName, value)::.

We will write ]

@racketblock[~keyName:: whenever referring to the value stored at the key keyName in the event.

Here is the definition of Event's play method:
]

@racketblock[
play {
	if (parent.isNil) { parent = defaultParentEvent };
	this.use { ~play.value };
}
::
Thus we can see that the link::#.defaultParentEvent:: is used unless otherwise specified and the function stored in ]

@racketblock[~play:: is executed in the context of the Event. It can be replaced in a given event for different behavior:
]

@racketblock[
(a: 6, b: 7, play: { (~a * ~b).postln }).play;
::

]
@section{subsection}
 Timing control with Event's delta method

Events also specify timing within a link::Classes/Pattern::. Event's 
@racketblock[delta:: method returns the value of ]

@racketblock[~delta:: or, if that is nil, ]

@racketblock[~dur * ~stretch::.

Patterns are played by link::Classes/TempoClock::s, which have their own tempo controls. This tempo which can be controlled through ]

@racketblock[~tempo:: in the event. Changes to the tempo affect everything else scheduled by the TempoClock, so ]

@racketblock[tempo:: provides a global tempo control while ]

@racketblock[stretch:: provides a control limited to the one pattern.

]
@section{subsection}
 The structure of defaultParentEvent

@section{method}
 defaultParentEvent
The default event used in most cases. This is a private class variable. See link::#*default::.

The default parent event provides the collection of default values and functions needed for the different uses of an Event. These defaults are defined in partialEvents that specify distinct aspects of default parent Event:

@racketblock[
playerEvent	// defines ~play, ~type and ~eventTypes
serverEvent	// server, group, addAction
durEvent	// duration, tempo and articulation
ampEvent	// volume, pan, MIDI velocity
pitchEvent	// pitch specified in many different ways
bufferEvent	// buffers on the server
midiEvent	// defines the sending of midi messages
::

]
@section{subsection}
 Useful keys for notes

Using Events is largely a matter of overwriting keys. Here is a list of keys useful for defining notes with their default values, grouped by the partialEvent within which they are defined.


@section{list}
 

## strong::serverEvent keys:::

The keys in serverEvent provide the values needed to identify the server to be used and where in the tree
of nodes to place the group.

@racketblock[
server:		nil,		// if nil, Server.default is used
instrument:	\default,	// this is the name of a SynthDef
group:		1,		// nodeID of group on the server
				// when adding before or after a node
				// this could be the nodeID of a synth instead of a group
addAction:	0,		// 0, 1, 2, 3 or \addToHead, \addToTail, \addBefore, \addAfter
out:		0,		// usually an output bus number, but depends on the SynthDef
::

## strong::ampEvent keys:::

The ampEvent determines volume. Notice that ]

@racketblock[~amp:: is a function that determines its value from ]

@racketblock[~db::. The user can choose to specify the amplitude directly by overwriting ]

@racketblock[~amp:: or to use a decibel specification by overwriting ]

@racketblock[~db::.
]

@racketblock[
amp:		#{ ~db.dbamp },	// the amplitude
db:		-20.0,		// the above described in decibel
pan:		0.0,		// pan position: -1 left 1 right
velocity:	64		// midi velocity
trig:		0.5		// trigger value
::



## strong::durEvent keys:::

The durEvent has keys that determine the timing of a note. Notice that ]

@racketblock[~sustain:: is a function that uses ]

@racketblock[~legato:: to determine the sustain. Like ]

@racketblock[~amp:: this can be overwritten to set the sustain directly.
]

@racketblock[
tempo:			nil,	// changes tempo of a TempoClock
dur:			1.0,	// time until next note (inter-onset time)
stretch:		1.0,	// inverse of tempo control, specific to the Event's stream
legato:			0.8,	// ratio of sustain to duration
sustain:		#{ ~dur * ~legato * ~stretch },
lag:			0.0,	// delay (in seconds) relative to current time position of Stream
timingOffset:		0.0,	// delay (in beats) relative to current time position of Stream
strum:			0.0	// "breaks" a chord. May be negative, playing the chord backward
strumEndsTogether:	false	// if true, the strummed notes end together (with gated synths)
sendGate:		nil  // override: true == always send a gate-release message; false == never send
::



## strong::pitchEvent keys:::

The pitchEvent has the most complex system of functions that provide a variety of useful ways to determine pitch:
]

@racketblock[
freq (->440)		// determines the pitch directly as a frequency in Hertz
midinote (-> 60)	// determines pitch as a fractional MIDI note (69 -> 440)
note (-> 0)		// determines pitch as a scale degree in an ~stepsPerOctave equal tempered scale
degree: 0		// determines pitch as a scale degree within the scale ~scale
::
The event also provides a set of transposition keys:
]

@racketblock[
mtranspose:	0	// modal transposition of degree within a scale
root:		0.0	// transposes root of the scale
gtranspose:	0.0	// gamut transposition within the ~stepsPerOctave equal tempered scale
ctranspose:	0.0	// chromatic transposition within the 12 tone equal tempered scale
harmonic:	1.0	// multiplies the frequency determined by ~midinote, typically to an overtone
detune:		0.0	// directly offsets frequency by adding this value
midiToCps		// a function taking a MIDI note number and turning it into frequency
			// Normally this is _.midicps, but you can override it for non-ET tunings

mtranspose:	0,	// modal transposition of degree
gtranspose:	0.0	// gamut transposition of note within a ~stepsPerOctave e.t. scale
ctranspose:	0.0	// chromatic transposition of midinote within 12 tone e.t. scale

octave:		5.0	// octave offest of note
root:		0.0	// root of the scale
degree:		0	// degree in scale
scale:		#[0, 2, 4, 5, 7, 9, 11]	// diatonic major scale
stepsPerOctave:	12.0	//
detune:		0.0,	// detune in Hertz
harmonic:	1.0	// harmonic ratio
octaveRatio:	2.0	// size of the octave (can be used with the Scale class)
::
The event calculates with these keys to derive parameters needed for the synth:
]

@racketblock[
note: #{	// note is the note in halftone steps from the root
	(~degree + ~mtranspose).degreeToKey(~scale, ~stepsPerOctave);
}
midinote: #{	// midinote is the midinote (continuous intermediate values)
	((~note.value + ~gtranspose + ~root) / ~stepsPerOctave + ~octave) * 12.0;
}
freq: #{
	(~midinote.value + ~ctranspose).midicps * ~harmonic;
}
detunedFreq: #{	// finally sent as "freq" to the synth as a parameter, if given
	~freq.value + ~detune
}
::

]
@section{IMAGE}
 Event-default-note.png::


::

@section{subsection}
 Event types

An Event responds to a play message by evaluating 
@racketblock[~play:: in the event, which by default uses the event's type to define the action to be performed. See link::Overviews/Event_types::.
]


