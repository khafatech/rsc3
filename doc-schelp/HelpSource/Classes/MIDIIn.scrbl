#lang scribble/manual
@(require (for-label racket))

@title{MIDIIn}
 receive MIDI messages@section{related}
  Classes/MIDIClient, Classes/MIDIOut, Classes/MIDIFunc, Classes/MIDIdef, Guides/MIDI, Guides/UsingMIDI
@section{categories}
  External Control>MIDI

@section{description}

This document explains technical details of the MIDI hardware interface class, MIDIIn.

MIDIIn is a simple and direct interface. When MIDI events come into SuperCollider, MIDIIn evaluates simple handler functions.

@section{note}
 
For general programming, strong::users should not use the MIDIIn class directly::. See the link::Classes/MIDIFunc:: and link::Classes/MIDIdef:: classes for higher level event matching and more flexible handling of event handlers.
::

Certain MIDI messages are supported only through MIDIIn. These are: sysex, sysrt, smpte.

See the link::Guides/UsingMIDI:: helpfile for practical considerations and techniques for using MIDI in SC.

@section{subsection}
 The MIDIIn class

MIDIIn links MIDI input received from the operating system to a set of user defined functions. Only one set of MIDI input handling functions can be active at a time, they are stored in the following class variables:

	noteOff, noteOn, polytouch, control, program, touch, bend, sysex, sysrt, smpte

The first argument these functions receive is an unique identifier that specifies the source of the data.


@section{ClassMethods}
 

@section{private}
 prDispatchEvent, connectByUID, disconnectByUID

@section{method}
 findPort
searches for a connected link::Classes/MIDIEndPoint:: by name.

@racketblock[
//list connected ins:
MIDIClient.init;
MIDIClient.sources;
::

]
@section{method}
 addFuncTo
Add a link::Classes/Function:: or similar object to be evaluated whenever a particular MIDI message is received. This method is preferable to the setters below, since it will not overwrite any existing functions.

@section{argument}
 what
A link::Classes/Symbol:: indicating the message type to wait for, one of 
@racketblock['noteOn':: ]

@racketblock['noteOff'::, ]

@racketblock['polytouch'::, ]

@racketblock['control'::, ]

@racketblock['program'::, ]

@racketblock['touch'::, ]

@racketblock['bend'::, ]

@racketblock['sysex'::, ]

@racketblock['sysrt'::, ]

@racketblock['smpte'::, or ]

@racketblock['invalid':: (for invalid sysex messages).

]
@section{argument}
 func
A link::Classes/Function:: or similar object to be evaluated when a message of the specified type is received. See the setters below for the arguments which will be passed at evaluation time.

@section{method}
 removeFuncFrom
Remove a link::Classes/Function:: or similar object from the list of those to be evaluated whenever a particular MIDI message is received. This method is preferable to the setters below, since it will leave any existing functions in place.

@section{argument}
 what
A link::Classes/Symbol:: indicating the message type, one of 
@racketblock['noteOn':: ]

@racketblock['noteOff'::, ]

@racketblock['polytouch'::, ]

@racketblock['control'::, ]

@racketblock['program'::, ]

@racketblock['touch'::, ]

@racketblock['bend'::, ]

@racketblock['sysex'::, ]

@racketblock['sysrt'::, ]

@racketblock['smpte'::, or ]

@racketblock['invalid':: (for invalid sysex messages).

]
@section{argument}
 func
The link::Classes/Function:: or similar object to be removed.

@section{method}
 replaceFuncTo
Replace a link::Classes/Function:: or similar object in the list to be evaluated whenever a particular MIDI message is received with another one. This method is preferable to the setters below, since it will not overwrite any existing functions.

@section{argument}
 what
A link::Classes/Symbol:: indicating the message type to wait for, one of 
@racketblock['noteOn':: ]

@racketblock['noteOff'::, ]

@racketblock['polytouch'::, ]

@racketblock['control'::, ]

@racketblock['program'::, ]

@racketblock['touch'::, ]

@racketblock['bend'::, ]

@racketblock['sysex'::, ]

@racketblock['sysrt'::, ]

@racketblock['smpte'::, or ]

@racketblock['invalid':: (for invalid sysex messages).

]
@section{argument}
 func
The link::Classes/Function:: or similar object to be replaced.

@section{argument}
 newFunc
A link::Classes/Function:: or similar object to be evaluated when a message of the specified type is received. See the setters below for the arguments which will be passed at evaluation time.


@section{method}
 noteOnZeroAsNoteOff

By default this flag is 
@racketblock[true:: and SuperCollider interprets incoming MIDI noteOn message with velocity 0 as noteOff messages. In case you do not want this automatic translation, you can set this flag to ]

@racketblock[false::


]
@section{method}
 connectAll

Connect to all possible MIDI inputs.

@section{argument}
 verbose

If set to true (default) it will print out the ports found in MIDIClient.init.

@section{method}
 disconnectAll

Disconnect from all MIDI inputs.


@section{subsection}
  Getter/Setters for Specific Message Types

The methods below allow you to register a function to respond to a particular message type.

@section{note}
 It is preferable to use the link::#*addFuncTo::, link::#*removeFuncFrom:: and link::#*replaceFuncTo:: methods above instead of these methods, as they will not overwrite any functions added by system objects.::

@section{method}
 noteOn
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI noteOn message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## keyNumber || 0 - 127
## velocity || 0 - 127
::

@section{method}
 noteOff
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI noteOff message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## keyNumber || 0 - 127
## velocity || 0 - 127 (typically 64 unless noteOff velocity is supported)
::

@section{method}
 polytouch
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI polytouch message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## keyNumber || 0 - 127
## pressure || 0 - 127
::

@section{method}
 control
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI control change message (CC) is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## controllerNumber || 0 - 127
## value || 0 - 127
::

@section{method}
 program
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI program change message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## programNumber || 0 - 127
::

@section{method}
 touch
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI after-touch message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## pressure || 0 - 127
::

@section{method}
 bend
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI pitch wheel change message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## MIDIchannel || ranges from 0 to 15
## bend || 0 - 16383 (14bits, the midpoint is 8192)
::

@section{method}
 sysex
@section{note}
 
The current implementation assembles a complete system exclusive packet before evaluating the function.
::
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI System Exclusive message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## data || an link::Classes/Int8Array:: (includes f0 and f7). See manufacturer references for details.
::

@section{method}
 sysrt
@section{table}
 
## strong::index:: || strong::data:: || strong::message::
## 2 || 14bits || song pointer
## 3 || 7bits || song select
## 8 || || midiclock
## 10 || || start
## 11 || || continue
## 12 || || stop
::
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI System Real-Time message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## index || ranges from 0 to 15
## data ||
::

@section{method}
 smpte
Over MIDI, SMPTE is transmitted at 1/4 frame intervals four times faster than the frame rate.
@section{table}
 
## strong::index:: || strong::data::
## 0 || frames low nibble
## 1 || frames hi nibble
## 2 || seconds low nibble
## 3 || seconds hi nibble
## 4 || minutes low nibble
## 5 || minutes hi nibble
## 6 || hours low nibble
## 7 || hours hi emphasis::bit:: OR'ed with frameRate
@section{list}
 
## 0 -> 24 fps
## 2 -> 25 fps
## 4 -> 30 fps drop frame
## 6 -> 30 fps
::
::
Nibbles are sent in ascending order.
@section{argument}
 value
a link::Classes/Function:: evaluated whenever a MIDI smpte message is received. It is passed the following arguments:
@section{definitionList}
 
## uid || unique identifier of the MIDI port
## index || ranges from 0 to 7
## data || 0 - 15 (4bits)
::

@section{Examples}
 

@section{subsection}
 Quick start for 1 port

@racketblock[
(
MIDIIn.connect;	// init for one port midi interface
// register functions:
~noteOff = { arg src, chan, num, vel;	[chan,num,vel / 127].postln; };
~noteOn = { arg src, chan, num, vel;	[chan,num,vel / 127].postln; };
~polytouch = { arg src, chan, num, vel;	[chan,num,vel / 127].postln; };
~control = { arg src, chan, num, val;	[chan,num,val].postln; };
~program = { arg src, chan, prog;		[chan,prog].postln; };
~touch = { arg src, chan, pressure;	[chan,pressure].postln; };
~bend = { arg src, chan, bend;		[chan,bend - 8192].postln; };
~sysex = { arg src, sysex;		sysex.postln; };
~sysrt = { arg src, chan, val;		[chan,val].postln; };
~smpte = { arg src, chan, val;		[chan,val].postln; };
MIDIIn.addFuncTo(\noteOn, ~noteOn);
MIDIIn.addFuncTo(\noteOff, ~noteOff);
MIDIIn.addFuncTo(\polytouch, ~polytouch);
MIDIIn.addFuncTo(\control, ~control);
MIDIIn.addFuncTo(\program, ~program);
MIDIIn.addFuncTo(\touch, ~touch);
MIDIIn.addFuncTo(\bend, ~bend);
MIDIIn.addFuncTo(\sysex, ~sysex);
MIDIIn.addFuncTo(\sysrt, ~sysrt);
MIDIIn.addFuncTo(\smpte, ~smpte);
)

//cleanup
(
MIDIIn.removeFuncFrom(\noteOn, ~noteOn);
MIDIIn.removeFuncFrom(\noteOff, ~noteOff);
MIDIIn.removeFuncFrom(\polytouch, ~polytouch);
MIDIIn.removeFuncFrom(\control, ~control);
MIDIIn.removeFuncFrom(\program, ~program);
MIDIIn.removeFuncFrom(\touch, ~touch);
MIDIIn.removeFuncFrom(\bend, ~bend);
MIDIIn.removeFuncFrom(\sysex, ~sysex);
MIDIIn.removeFuncFrom(\sysrt, ~sysrt);
MIDIIn.removeFuncFrom(\smpte, ~smpte);
)
::

]
@section{subsection}
 Quick start for 2 or more ports

@racketblock[
(
	var inPorts = 2;
	var outPorts = 2;
	MIDIClient.init(inPorts,outPorts);	// explicitly intialize the client
	inPorts.do({ arg i;
		MIDIIn.connect(i, MIDIClient.sources.at(i));
	});
)
::

]
@section{subsection}
 example with sound

@racketblock[
MIDIIn.connect;
s.boot;

(
SynthDef("sik-goo", { arg freq=440,formfreq=100,gate=0.0,bwfreq=800;
	var x;
	x = Formant.ar(
		SinOsc.kr(0.02, 0, 10, freq),
		formfreq,
		bwfreq
	);
	x = EnvGen.kr(Env.adsr, gate,Latch.kr(gate,gate)) * x;
	Out.ar(0, x);
}).add;
)

x = Synth("sik-goo");

//set the action:
(
~noteOn = {arg src, chan, num, vel;
	x.set(\freq, num.midicps / 4.0);
	x.set(\gate, vel / 200 );
	x.set(\formfreq, vel / 127 * 1000);
};
MIDIIn.addFuncTo(\noteOn, ~noteOn);

~noteOff = { arg src,chan,num,vel;
	x.set(\gate, 0.0);
};
MIDIIn.addFuncTo(\noteOff, ~noteOff);

~bend = { arg src,chan,val;
	//(val * 0.048828125).postln;
	x.set(\bwfreq, val * 0.048828125 );
};
MIDIIn.addFuncTo(\bend, ~bend);
)

//cleanup
MIDIIn.removeFuncFrom(\noteOn, ~noteOn);
MIDIIn.removeFuncFrom(\noteOff, ~noteOff);
MIDIIn.removeFuncFrom(\bend, ~bend);
::

]
@section{subsection}
 writing to the bus rather than directly to the synth

@racketblock[
//i used this and got acceptable latency for triggering synths live.
//The latency might actually be less than sc2, but i haven't used it enough
//to tell for sure yet.
//Powerbook G4, 512mb ram.
//- matrix6k@somahq.com

s.boot;

(
SynthDef("moto-rev", { arg ffreq=100;
	var x;
	x = RLPF.ar(LFPulse.ar(SinOsc.kr(0.2, 0, 10, 21), [0,0.1], 0.1),
		ffreq, 0.1)
		.clip2(0.4);
	Out.ar(0, x);
}).add;
)

b = Bus.control(s);

x = Synth("moto-rev");

// map the synth's first input (ffreq) to read
// from the bus' output index
x.map(0, b);


MIDIIn.connect;
//set the action:
(
~noteOn = {arg src, chan, num, vel;
	b.value = num.midicps.postln;
};
MIDIIn.addFuncTo(\noteOn, ~noteOn);

~control = {arg src, chan, num, val;
	[chan,num,val].postln;
};
MIDIIn.addFuncTo(\control, ~control);

~bend = {arg src, chan, val;
	val.postln;
};
MIDIIn.addFuncTo(\bend, ~bend);
)

// cleanup
x.free;
b.free;
MIDIIn.removeFuncFrom(\noteOn, ~noteOn);
MIDIIn.removeFuncFrom(\control, ~control);
MIDIIn.removeFuncFrom(\bend, ~bend);
::

]
@section{subsection}
 Keyboard Split for two voices

@racketblock[
//pbend to cutoff, mod to rez, 7 to amp
//- matrix6k@somahq.com

s.boot;
(
SynthDef("funk",{ arg freq = 700, amp = 0.2, gate = 1, cutoff = 20000, rez = 1, lfospeed=0;
	var e,x,env,range,filterfreq;
	e = Env.new([0, 0.1, 0.1, 0], [0, 0.1, 0.1], 'linear', 2);
	env=Env.adsr(0.3,1,1,1);
	range = cutoff -1;
	filterfreq = SinOsc.kr(lfospeed,0, range, cutoff).abs;
	x = RLPF.ar(Mix.ar([
			Mix.arFill(2, {Saw.ar(freq *2 + 0.2.rand2, amp)}),
			Mix.arFill(2, {Saw.ar(freq *4+ 0.2.rand2, amp)})
		]),
		EnvGen.kr(env,gate)*filterfreq,
		rez);
	Out.ar([0,1],x * EnvGen.kr(e, gate, doneAction: Done.freeSelf))
}).add;

SynthDef("strings",{ arg freq = 700, amp = 0.2, gate = 1;
	var x,enve;
	enve = Env.new([0, 0.1, 0.1, 0], [2, 0.1, 1], 'linear', 2);
	x = RLPF.ar(Mix.ar([
			Mix.arFill(2, {Saw.ar(freq +2.rand2,0.6)}),
			Mix.arFill(2, {Saw.ar(freq *0.5 + 2.rand2,0.6)})
		]),
		6000,1);
	Out.ar([0,1],x * EnvGen.kr(enve, gate, doneAction: Done.freeSelf))
}).add;
)

(
var keys, cutspec, cutbus, rezspec, rezbus, lfospec, lfobus;
keys = Array.newClear(128);

MIDIClient.init;
MIDIIn.connect(0, MIDIClient.sources.at(0));

g = Group.new;

cutspec = ControlSpec(100,10000,\linear,0.001);
cutbus = Bus.new(\control,1,1,s);
cutbus.value = 10000;

rezspec = ControlSpec(1,0,\linear,0.001);
rezbus = Bus.new(\control,2,1,s);
rezbus.value = 1.0;

lfospec = ControlSpec(0,50,\linear,0.001);
lfobus = Bus.new(\control,3,1,s);

~control = {arg src, chan, num, val;
	if(num == 1,{
		rezbus.value = rezspec.map(val/127.0);
	});
	if(num == 7,{
		lfobus.value = lfospec.map(val/127.0).postln;
	});
};
MIDIIn.addFuncTo(\control, ~control);

~bend = {arg src, chan, val;
	cutbus.value = cutspec.map(val/16383.0);
};
MIDIIn.addFuncTo(\bend, ~bend);

~noteOn = {arg src, chan, num, vel;
	var node;
	if(num < 60, {
		node = Synth.tail(g, "funk", [\freq, num.midicps, \amp, vel/255]);
		node.map("cutoff",1,"rez",2,"lfospeed",3);
//		node = Synth.basicNew("funk",s);
//		s.sendBundle(nil,
//			node.addToTailMsg(g,[\freq, num.midicps, \amp, vel/255]),
//			node.mapMsg("cutoff",1,"rez",2,"lfospeed",3)
//		);
		keys.put(num, node)
	},{
		node = Synth.tail(g, "strings", [\freq, num.midicps, \amp, vel/255]);
		keys.put(num, node)
	});
};
MIDIIn.addFuncTo(\noteOn, ~noteOn);

~noteOff = {arg src, chan, num, vel;
	var node;
	node = keys.at(num);
	if (node.notNil, {
		keys.put(num, nil);
		s.sendMsg("/n_set", node.nodeID, "gate", 0);
		// or node.release
		// then free it ... or get the NodeWatcher to do it
	});
};
MIDIIn.addFuncTo(\noteOff, ~noteOff);
)

//cleanup
MIDIIn.removeFuncFrom(\noteOn, ~noteOn);
MIDIIn.removeFuncFrom(\control, ~control);
MIDIIn.removeFuncFrom(\bend, ~bend);

::
]


