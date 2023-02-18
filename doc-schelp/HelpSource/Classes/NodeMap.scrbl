#lang scribble/manual
@(require (for-label racket))

@title{NodeMap}
 store control values and bus mappings@section{categories}
  Libraries>JITLib>NodeProxy, Server>Nodes, Server>Abstractions
@section{related}
  Classes/Bus

@section{description}

Object to store control values and bus mappings independently of a specific node.


@racketblock[
a = NodeMap.new;
a.set(\freq, [446, 662], \amp, 0.2, \out, Bus.audio(s));
a.asOSCArgArray;
::

]
@section{InstanceMethods}
 

@section{method}
 set
set arguments of a node

@section{method}
 unset
remove settings

@section{method}
 unmap
remove mappings

@section{method}
 at
return setting at that key.

@section{method}
 sendToNode
apply a setting to a node by sending a bundle

@section{method}
 send
apply a setting to a node by sending a bundle

@section{method}
 addToBundle
add all my messages to the bundle

@section{method}
 addToEvent
add all my values to the event

@section{method}
 asOSCArgArray
returns the arguments for an OSC message.

@section{method}
 unmapArgsToBundle
returns the arguments for an OSC message to unmap any mapped controls.

@section{method}
 setMsg
returns the OSC message for setting a synth
@section{argument}
  target
a group, synth, or server to use as a nodeID to set.

@section{method}
 setn, map, mapn
These are kept for backward compatibility. They redirect to link::#-set::.

@section{method}
 get
Kept for backward compatibility.

@section{method}
 clear
Remove all settings and clear cache


@section{private}
 updateArgs, upToDate

@section{Examples}
 


@racketblock[

s.boot;

(
SynthDef("modsine",
	{ arg freq=320, amp=0.2;
		Out.ar(0, SinOsc.ar(freq, 0, amp));
	}).add;
SynthDef("lfo",
	{ arg rate=2, busNum=0;
		Out.kr(busNum, LFPulse.kr(rate, 0, 0.1, 0.2))
	}).add;
)

//start nodes
(
b = Bus.control(s,1);
x = Synth("modsine");
y = Synth.before(x, "lfo", [\busNum, b]);
)

//create some node maps
(
h = NodeMap.new;
h.set(\freq, 800);
h.map(\amp, b);

k = NodeMap.new;
k.set(\freq, 400);
k.unmap(\amp);
)

//apply the maps

h.sendToNode(x); //the first time a new bundle is made
k.sendToNode(x);

h.sendToNode(x); //the second time the cache is used
k.sendToNode(x);

h.set(\freq, 600);

h.sendToNode(x); //when a value was changed, a new bundle is made

//free all
x.free; b.free; y.free;
::
]


