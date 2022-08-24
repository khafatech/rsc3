#lang scribble/manual
@(require (for-label racket))

@title{FreeSelfWhenDone}
 Free the enclosing synth when a UGen is finished@section{related}
  Classes/Done, Classes/PauseSelfWhenDone, Classes/Done
@section{categories}
   UGens>Synth control

@section{description}


Some UGens set a 'done' flag when they are finished playing.
FreeSelfWhenDone will free the enclosing synth when this flag is set to true.

See link::Classes/Done:: for a complete list of these UGens.

Note that many of these UGens have doneActions, which are another way of accomplishing the same thing. See link::Classes/Done:: for more detail.

@section{note}
  One must be careful when using binary operations on UGens with done flags, as these will return a link::Classes/BinaryOpUGen::, and thus prevent the done flag from being accessible. See example below. ::

@section{classmethods}
 
@section{private}
  categories

@section{method}
 kr

@section{argument}
 src

the UGen to check for done.

@section{examples}
 

@racketblock[
s.boot;

// simple example
(
{ var env;
env = Line.kr(0, 1, 1);
FreeSelfWhenDone.kr(env); // free synth at end of line
SinOsc.ar(200, 0, 0.5) * env
}.play;
)

// the previous example works, because FreeSelfWhenDone operates on the Line
// this version won't work
(
{ var env, output;
env = Line.kr(0, 1, 1);
output = SinOsc.ar(200, 0, 0.5) * env;
output.postln; // output is a BinaryOpUGen, which has no 'done' flag
FreeSelfWhenDone.kr(output); // won't ever be done
output
}.play;
)

// record for four seconds
b = Buffer.alloc(s, 44100 * 4.0, 1);
(
SynthDef("help-RecordBuf",{ arg out=0,bufnum=0;
	var formant, recbuf;
	formant = Formant.ar(XLine.kr(400,1000, 4), 2000, 800, 0.125);
	recbuf = RecordBuf.ar(formant, bufnum, recLevel: Line.kr(1, 1), loop: 0);
	// The RecordBuf doesn't loop, so you can check it for 'done' status
	FreeSelfWhenDone.kr(recbuf);
}).play(s,[\out, 0, \bufnum, b]);
)

// play it back
(
SynthDef("help-RecordBuf play",{ arg out=0,bufnum=0;
	var playbuf;
	playbuf = PlayBuf.ar(1,bufnum);
	FreeSelfWhenDone.kr(playbuf); // frees the synth when the PlayBuf is finished
	Out.ar(out, playbuf);
}).play(s,[\out, 0, \bufnum, b]);
)
::

]


