#lang scribble/manual
@(require (for-label racket))

@title{10_Controls}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

Evaluate


@racketblock[
(
SynthDescLib.global.read;
SynthDescLib.global.browse;
)
::

and examine the box that lists the controls for each synth.

]
@section{section}
 Controls (usually) are arguments

Use controls, which most often are defined as arguments in a ugenGraphFunc, to give information to a synth, either when it is created and/or after it is running. Supply default values to the arguments to make code more readable and to protect against user error (such as forgetting to supply a value to an argument).


@racketblock[
(
// 3 arguments (controls) with default values
SynthDef(
	"withControls",
	{ arg freq = 440, beatFreq = 1, mul = 0.22;
	Out.ar(
		0,
		SinOsc.ar([freq, freq+beatFreq], 0, mul)
	)
}).add;
)

// items in the array are passed to the controls in Synth when it's created
z = Synth("withControls", [\freq, 440, \beatFreq, 1, \mul, 0.1]);

// evaluate this line after the synth is running to reset its controls
z.set(\freq, 700, \beatFreq, 2, \mul, 0.2);
::

////////////////////////////////////////////////////////////////////////////////////////////////////

Write controls names and appropriate values in the array given as an argument to a synth. Control names can be given as symbols (a unique name within the SuperCollider system).

]

@racketblock[
Synth("withControls", [\freq, 440, \beatFreq, 0.5, \mul, 0.1]);
::

or as as strings (an array of characters)

]

@racketblock[
Synth("withControls", ["freq", 440, "beatFreq", 0.5, "mul", 0.1]);
::

Either way, the pattern is

[ controlName, value, controlName, value, controlName, value].

See the link::Classes/Symbol:: and link::Classes/String:: files in the SuperCollider help system.

////////////////////////////////////////////////////////////////////////////////////////////////////

A third way to pass controls to a synth is as

]

@racketblock[
Synth("withControls", [0, 440, 1, 1, 2, 0.1]);
::

In this case, the pattern is

[ controlIndex, value, controlIndex, value, controlIndex].

]
@section{section}
 To adjust a control

Use the 
@racketblock[.set:: message to change the value of a control while a synth is running.

]

@racketblock[
(
SynthDef("resetMyControls", { arg freq = 440, mul = 0.22;
	Out.ar(
		0,
		SinOsc.ar([freq, freq+1], 0, mul)
	)
}).add;
)

~aSynth = Synth("resetMyControls", [\freq, 440, \mul, 0.06]);
~aSynth.set(\freq, 600, \mul, 0.25);
::

]
@section{section}
 Environment variables

The '~' character before aSynth in the previous example defines an environment variable. An advantage to using an environment variable is that it doesn't have to be declared explicitly, as in


@racketblock[
var aSynth; // variables without the '~' MUST first be declared!!
::

More precisely, the ~ character puts a variable named 'aSynth' into an instance of an object known as the currentEnvironment. For further information, see the link::Classes/Environment:: document in the SuperCollider help system.

In this usage, ~aSynth behaves like a global variable in other programming languages. By the strict definition, it isn't precisely emphasis::global::, but it may be used as such in SuperCollider.

]
@section{section}
 Lag times

Use an array of lag times to state how long it takes to glide smoothly from one control value to another. Write the lag times in an array and place it in the synthdef after the ugenGraphFunc, as in


@racketblock[
(
SynthDef("controlsWithLags", { arg freq1 = 440, freq2 = 443, mul = 0.12;
	Out.ar(
		0,
		SinOsc.ar([freq1, freq2], 0, mul)
	)
}, [4, 5]).add;
)

~aSynth = Synth("controlsWithLags", [\freq1, 550, \freq2, 344, \mul, 0.1]);
~aSynth.set(\freq1, 600, \freq2, 701, \mul, 0.05);
::

]
@section{section}
 SynthDef templates

The array of lagtimes means that the synthdef template with two components (discussed in link::Tutorials/Mark_Polishook_tutorial/07_SynthDefs::)


@racketblock[
// a template for a synthdef with two components
SynthDef(
	"aSynth",				// 1st argument is a name
	{ .... i am a ugenGraphFunc ... }	// 2nd argument is a ugenGraphFunc
)
::

can be revised to include three components.

]

@racketblock[
// a re-defined template for a synthdef _with an array of lagtimes
// the class definition for the lagtime array calls it 'rates'
SynthDef(
	"aSynth",				// name
	{ .... i am a ugenGraphFunc ... },	// ugenGraphFunc
	[ ... lagTimes ... ]			// rates
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/11_Test_functions::
]


