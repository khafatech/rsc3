#lang scribble/manual
@(require (for-label racket))

@title{Ref}
 a reference to a value@section{categories}
  Core

@section{description}


A Ref holds an object which may be retrieved and altered with the messages value and value_(obj).
The backquote 
@racketblock[ ` :: is a unary operator that is equivalent to calling ]

@racketblock[Ref.new(obj)::.

Refs are most commonly used to prevent multi-channel expansion in link::Classes/SynthDef::s and link::Classes/Pattern::s (see link::Classes/Klank:: for an example).
Refs can also be used to simplify the coding of co-routines used in EventStreams (see link::Classes/Prout:: for an example).

]

@racketblock[

x = Ref(nil);
z = obj.method(x);		// method puts something in reference
x.value.doSomething;	// retrieve value and use it

::

Ref is also used as a quoting device to protect against multi channel expansion in certain UGens that require Arrays.

]
@section{classmethods}
 

@section{method}
 new

create a Ref of an object.
@section{discussion}
 
Another syntax:


@racketblock[
`5
::

]
@section{instancemethods}
 

@section{method}
 dereference

Answer the value. This message is also defined in class Object where it just returns the receiver.  Therefore anything.dereference will remove a Ref if there is one. This is slightly different than the value message, because value will also cause functions to evaluate themselves whereas dereference will not.

@section{method}
 asRef

Answers the receiver. In class Object this message is defined to create a Ref of the object.

@section{method}
 value

Get or set the value.

@section{method}
 get

Returns value.

@section{method}
 set

Sets value.

@section{method}
 at

Returns 
@racketblock[value.at(index)::

]
@section{method}
 put
Executes value.put(index, value)

@section{method}
 seq

this method is used to return values from within a Routine definition

@section{discussion}
 

@racketblock[
{ this.value = output.embedInStream(this.value); }
::

]
@section{method}
 asUGenInput

Returns the Ref - this prevents multi-channel expansion in a SynthDef

@section{method}
 asControlInput

Returns the value - this is used when sending a Ref as a control value to a server Node.

@section{section}
 Typical uses of Ref:

@section{subsection}
 preventing multi-channel expansion

Consult link::Guides/Multichannel-Expansion:: for details on multi-channel expansion in SynthDefs.

Refs prevent multi-channel expansion in a SynthDef, so the array below defines one Klank UGen rather than three.


@racketblock[
{ Klank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;
::

Refs cannot be used reliably to suppress multi-channel expansion within Events and Patterns.
Instead, it is necessary to enclose the array of values in another array:

]

@racketblock[
(
	SynthDef(\multi, { | out, freq = #[100,200,300], amp = 0.1, pan = 0, sustain = 1|
		var audio, env;
		env = EnvGen.kr(Env.perc(0.01, sustain), doneAction: Done.freeSelf);
		audio = Mix(Saw.ar(freq));
		audio = Pan2.ar(audio * env, pan, amp);
		OffsetOut.ar(out, audio)
	}).add;

	( instrument: \multi, freq: [ [500, 501, 700] ], sustain: 2).play

)
::

]

@racketblock[
(
	Pbind(*[
		instrument: \multi,
		freq: 	Prand([
						[[100, 141, 103] ],
						[[100, 310, 190] ],
						[[100, 100.1, 110] ],
				], inf),
		dur: 0.2,
		sustain: 0.3
	]).play;
)
::


]
@section{method}
 multichannelExpandRef
This method is called internally on inputs to UGens that take multidimensional arrays, like link::Classes/Klank:: and it allows proper multichannel expansion even in those cases. For SequenceableCollection, this returns the collection itself, assuming that it contains already a number of Refs.

@section{argument}
 rank
The depth at which the list is expanded. For instance the Klank spec has a rank of 2.

@racketblock[
`([[[100, 200], 500], nil, [[[0.01, 0.3], 0.8]]]).multichannelExpandRef(2);
[`[[100, 200], nil, [0.2, 0.8]], `[[130, 202], nil, [0.2, 0.5]]].multichannelExpandRef(2);
::
]


