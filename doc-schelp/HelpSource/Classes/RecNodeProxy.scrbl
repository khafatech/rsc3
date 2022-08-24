#lang scribble/manual
@(require (for-label racket))

@title{RecNodeProxy}
 a NodeProxy that can record@section{categories}
  Libraries>JITLib>NodeProxy
@section{related}
  Classes/NodeProxy

@section{description}

this is also created from a link::Classes/NodeProxy::, or an link::Classes/Ndef:: with the message strong::record::.

@section{ClassMethods}
 

@section{method}
 new
see superclass

@section{method}
 audio
see superclass

@section{method}
 newFrom
instantiate a new proxy that listens to the in proxy.

@section{InstanceMethods}
 

@section{method}
 open
open new file and initialize buffer on server

@section{method}
 record
start the recording synth.

@section{argument}
 paused
if paused is false start recording immediately.

@section{argument}
 clock
optional - the clock to use for scheduling recording ...
@section{argument}
 quant
... if a non-nil quant is given.

@section{method}
 close
stop recording, close file

@section{method}
 isRecording
see if recording right now

@section{method}
 wakeUp
until the proxy is not used by any output ( either .play or .ar/.kr ) it is not running on the server. you can wake it up to force it playing.

@section{Examples}
 


@racketblock[
s.boot;

a = RecNodeProxy.audio(s, 2);
a.source = { SinOsc.ar([400,500], 0, 0.1) };
a.play; //monitor;
a.open("xproxySpace.aif");
a.record(false);

a.source = { SinOsc.ar([400,700], 0, 0.1) };
a.source = { SinOsc.ar([410,510], 0, 0.1) };
a.source = { SinOsc.ar([LFNoise1.kr(80, 100, 300),500], 0, 0.1) };

//stop recording and close file
a.close;

//monitor off
a.stop;
::

]
@section{subsection}
 recording from some bus


@racketblock[
a = Bus.audio(s, 2);

SynthDef("test", { arg out; Out.ar(out, { WhiteNoise.ar(0.1) }.dup(2)) }).add;
x = Synth("test", [\out, a]);


n = RecNodeProxy.audio(s, 2);
n.source = { InFeedback.ar(a, 2) };

n.play;//monitor
n.stop;//turn off monitor

n.open("noise.aif");
n.record;
n.unpause;

n.close;
::

]
@section{subsection}
 instance creation from an existent node proxy


@racketblock[
b = NodeProxy.audio(s, 2);
b.play; //listen to b
b.source = { SinOsc.ar([400,500], 0, 0.1) }; //play something

r = RecNodeProxy.newFrom(b);
r.open("recproxy514.aif"); //open file
r.record; //start recorder (paused)

r.unpause; //start recording

b.source = { SinOsc.ar([430,500], 0, 0.1) };
b.source = { SinOsc.ar([410,510], 0, 0.1) };
b.source = { SinOsc.ar([LFNoise1.kr(80, 100, 300), 500], 0, 0.1) };
r.pause;
b.source = { WhiteNoise.ar(0.01) };
r.unpause;
r.pause;


//stop recording and close file
r.close;
b.stop; //stop listen to b
::

]
@section{subsection}
 instance creation from an existent node proxy again


@racketblock[
b = NodeProxy.audio(s, 2);
b.play; //listen to b
b.source = { SinOsc.ar([400,500], 0, 0.1) }; //play something

r = b.record("recproxy101.aiff"); //start recorder (paused)
r.unpause; //start recording
r.close; //end recording, close file
b.stop;	//stop listen
::

]
@section{subsection}
 recording from other sources


@racketblock[
s.boot;

a = RecNodeProxy.audio(s, 2);
b = a.index; //get the bus index;
a.play;		//monitor;
a.open("xproxySpace.aif");
a.record;
a.unpause;

(
Routine({
	var id;
	loop({
		id = s.nextNodeID;
		s.sendMsg("/s_new", "default", id,0,0, \out, b, \freq, rrand(400, 800));
		0.2.wait;
		s.sendMsg("/n_set", id, \gate, 0);
		0.2.wait;
	})
}).play;
)


//stop recording and close file
a.close;

//monitor off
a.stop;
::
]


