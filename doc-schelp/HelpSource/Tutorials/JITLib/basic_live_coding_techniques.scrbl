#lang scribble/manual
@(require (for-label racket))

@title{basic_live_coding_techniques}
 basic live coding techniques@section{categories}
  Libraries>JITLib>Tutorials, Tutorials>JITLib

basic live coding techniques ("object style") without the use of JITLib

more to come..

using a simple environment. this looks just like ProxySpace, but works differently. for the difference, see link::Tutorials/JITLib/jitlib_basic_concepts_01:: and link::Tutorials/JITLib/jitlib_basic_concepts_02::.


@racketblock[
d = (); // create a new environment
d.push; // push it to current

// this synthdef can be changed on the fly, but the synth will
// not change from this. use expression [1] for replacing a given synth
(
SynthDef(\x, { |freq=440|
	Out.ar(0,
		Ringz.ar(Dust.ar(40), freq, 0.1)
	)
}).send(s);
)

// send a first synth:
~s1 = Synth(\x);

// [1]
// now you can play around with these lines, as well as with the synth def above
~s1 = Synth.replace(~s1, \x, [\freq, 3000]);
~s1.set(\freq, 4000);

// add a bus:

~b1 = Bus.control(s);
~b1.set(350);
~s1.map(\freq, ~b1);

// set the bus to different values:

~b1.set(100);
~b1.xline(800, 5);

~s3 = { Out.kr(~b1, MouseX.kr(300, 900, 1)) }; // add some mouse control on the fly
~s3.free; // remove it again.



// finish:

~b1.free;
d.clear;
d.pop;
::
]


