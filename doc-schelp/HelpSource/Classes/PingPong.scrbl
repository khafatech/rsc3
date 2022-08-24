#lang scribble/manual
@(require (for-label racket))

@title{PingPong}
 Stereo ping-pong delay.@section{related}
  Classes/SinOsc
@section{categories}
   UGens>Delays>Buffer


@section{description}


Bounces sound between two outputs…  Like a ping-pong ball. PingPong is
actually a compound built upon  link::Classes/RecordBuf::  and
link::Classes/PlayBuf:: .


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 bufnum
First index of a multi channel buffer.

@section{argument}
 inputs
An array of audio inputs, the same size as your buffer.

@section{argument}
 delayTime
Delay time in seconds.

@section{argument}
 feedback
Feedback coefficient.

@section{argument}
 rotate

Which rotates the inputArray by one step. (left → right, right →
left). Rotation of 0 (or 2) would result in no rotation to the
inputArray.


@section{Examples}
 


@racketblock[

(
s = Server.local;
s.waitForBoot({

b = Buffer.alloc(s,44100 * 2, 2);

SynthDef("help-PingPong",{ arg out=0,bufnum=0,feedback=0.5,delayTime=0.2;
	var left, right;
	left = Decay2.ar(Impulse.ar(0.7, 0.25), 0.01, 0.25,
		SinOsc.ar(SinOsc.kr(3.7,0,200,500)));
	right = Decay2.ar(Impulse.ar(0.5, 0.25), 0.01, 0.25,
		Resonz.ar(PinkNoise.ar(4), SinOsc.kr(2.7,0,1000,2500), 0.2));

	Out.ar(0,
		PingPong.ar(bufnum, [left,right], delayTime, feedback, 1)
	)
}).play(s,[\out, 0, \bufnum, b.bufnum,\feedback,0.5,\delayTime,0.1]);
});
)


(
s = Server.local;
s.waitForBoot({

b = Buffer.alloc(s,44100 * 2, 2);

SynthDef("help-PingPong",{ arg out=0,bufnum=0;
	var left, right;
	left = Decay2.ar(Impulse.ar(0.7, 0.25), 0.01, 0.25,
		SinOsc.ar(SinOsc.kr(3.7,0,200,500)));
	right = Decay2.ar(Impulse.ar(0.5, 0.25), 0.01, 0.25,
		Resonz.ar(PinkNoise.ar(4), SinOsc.kr(2.7,0,1000,2500),
0.2));

	Out.ar(0,
		PingPong.ar(bufnum, [left,right] *  EnvGen.kr(Env([1, 1, 0], [2, 0.1])),
			0.1, 0.8, 1)
	)
}).play(s,[\out, 0, \bufnum, b.bufnum]);
});
)





(

Patch({ arg buffer,feedback=0.5,delayTime=0.2;
	var left, right;
	left = Decay2.ar(Impulse.ar(0.7, 0.25), 0.01, 0.25,
		SinOsc.ar(SinOsc.kr(3.7,0,200,500)));
	right = Decay2.ar(Impulse.ar(0.5, 0.25), 0.01, 0.25,
		Resonz.ar(PinkNoise.ar(4), SinOsc.kr(2.7,0,1000,2500), 0.2));

	PingPong.ar(buffer.bufnumIr, [left,right], delayTime, feedback, 1)

}).gui

)

::

]


