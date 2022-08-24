#lang scribble/manual
@(require (for-label racket))

@title{BusPlug}
 a listener on a bus@section{categories}
  Libraries>JITLib>NodeProxy, Live Coding
@section{related}
  Classes/Bus, Classes/Monitor, Classes/InBus, Classes/NodeProxy, Classes/Ndef

@section{description}

A BusPlug is represents a listener on a private link::Classes/Bus:: that makes it easy to play back to multiple channels. It is mainly in use as a superclass of NodeProxy, but it can be used for general channel routings as well. Most methods are documented in the link::Classes/NodeProxy:: helpfile.


@racketblock[
s.boot;
z = Bus.control(s, 16);
z.setn({ |i| (i * 5).nthPrime } ! 16 * 5 + 100);
a = BusPlug.for(z);
{ Splay.ar(SinOsc.ar(a.kr(3, MouseX.kr(0, 25)))) * 0.1 }.play; // selectively play 3 channel of the 16, modulate offset
::

]
@section{ClassMethods}
 



@section{method}
 new
Create a new (neutral) instance on the given server

@section{method}
 for
Create an instance with a given link::Classes/Bus::.

@section{method}
 audio
Create a new audio rate instance on the given server

@section{method}
 control
Create a new audio rate instance on the given server

@section{method}
 defaultNumAudio
Default number of channels when initializing in audio rate and no specific number is given (default: 2).

@section{method}
 defaultNumControl
Default number of channels when initializing in control rate and no specific number is given (default: 1).

@section{method}
 defaultReshaping
default reshaping behaviour for BusPlug and its sublass NodeProxy. See: link::#reshaping::

@section{InstanceMethods}
 

@section{private}
 makeBusArg, prepareForProxySynthDef, wakeUpToBundle, playNToBundle, playToBundle, newMonitorToBundle, clock

@section{method}
 server
Return the server that the BusPlug runs on.

@section{method}
 clear
Free the bus, end the monitor


@section{subsection}
 Making copies

@section{method}
 copy
copies the hidden internal state to make the new BusPlug independent of the old, running on a new link::Classes/Bus::. By design, the link::Classes/Monitor:: is copied, but is not running (use play to start it in the same configuration). If needed, you can also copy the link::Classes/Monitor:: only (see: link::Classes/NodeProxy#copy::).

@section{method}
 copyState
Copy the internal settings of one proxy into another. Old state is cleared, the bus is freed.

@section{argument}
 proxy
The object whose internal state is being copied.


@section{method}
 reshaping
Determines how to behave when link::#initBus:: is called.
Current options:
@section{list}
 
## teletype::nil:: Once initialized, keep the same bus - this is the default
## teletype::\static:: Same as nil, but allows you to override the default in instances
## teletype::\elastic:: On a change, shrink and grow according to need, replace bus. Monitoring is adjusted.
## teletype::\expanding:: On a change, only grow according to need, replace bus. Monitoring is adjusted.
::

@section{subsection}
 In UGen graphs and Patterns

@section{method}
 ar, kr
Return a link to numChannels of my output. If uninitialized, creates a matching bus. Normally, strong::ar defaults to stereo, kr to mono::. This can be set in the classvars: link::#*defaultNumAudio::, link::#*defaultNumControl::

For consistency, it always returns an array of signals when no numChannels are given. To make it a single ugen output, use 
@racketblock[cumChannels = 1:: . See also: link::Classes/InBus::.

]
@section{argument}
 numChannels
Number of channels returned. If the receiver is neutral or reshaping is elastic, initialize it to this number. If this is more than the available channels, use the clip behaviour (below). If set to 
@racketblock[1::, it will return an instance of link::Classes/InBus::, otherwise an Array of one or more instances of InBus.

]
@section{argument}
 offset
Channel offset when reading a bus that has more channels than numChannels, cross fading between adjacent channels.
"/Users/wanninger/Library/Application Support/SuperCollider/Help/Overviews/Methods.html"
@section{argument}
 clip
If set to 'wrap', exceeding channels will wrap around, if set to 'clip', repeat the last one.


@section{method}
 asUGenInput
Returns the appropriate output to read from the BusPlug bus (an link::Classes/InBus:: UGen)

@racketblock[
b = BusPlug.new;
{ Blip.ar(b + 5) }.play;
b.bus.set(12);
::

]
@section{method}
 embedInStream
Returns the map argument for the bus, if the bus has multiple channels, it will return an array of map args.

@racketblock[
b = BusPlug.new;
x = Pbind(\z, b).asStream;
x.next(()); // returns the map argument for the bus
b.defineBus(\audio, 3);
x.next(()); // returns map arguments for the audio rate bus
::

]
@section{method}
 asControlInput
Returns the map argument for the bus, just like link::#-embedInStream::

@section{subsection}
 Monitoring and Routing

@section{method}
 isPlaying
Returns true if server is running and bus not nil. link::Classes/NodeProxy:: this returns true if the group is playing.

@section{method}
 isMonitoring
Returns true if monitor is playing

@section{method}
 play
Play from a bus index with a number of channels to another index with a number of channels, within a link::Classes/Group:: (i.e. a target group or server).

@section{argument}
 out
bus index

@section{argument}
 numChannels
number of channels to output. If BusPlug is neutral or reshaping is elastic, initialize it to this number. If this is more than the available channels, wrap around. If this is not given, and reshaping is elastic, it will automatically expand.

@section{argument}
 group
target link::Classes/Group:: or link::Classes/Server:: in which to play the monitor synths.

@section{argument}
 multi
keep old playback links and add new one

@section{argument}
 vol
overall volume  at which to monitor

@section{argument}
 fadeTime
fade in / fade out time

@section{argument}
  addAction
Where in the node tree to play the monitor synths




@section{method}
 playN
Play back on non-contiguous channels. See: link::Classes/Monitor:: and link::Reference/playN::

@section{argument}
 outs
array of destination channels (or single value)


@section{argument}
 amps
array of amplitudes for each channel (or single value)

@section{argument}
 ins
array of source channel offsets within the bus (or single value)

@section{argument}
  vol
Overall volume (multiplied by amps)

@section{argument}
  fadeTime
array of fadeTimes (or single value) for fade in / fade out

@section{argument}
  group
target link::Classes/Group:: or link::Classes/Server:: in which to play the monitor synths.

@section{argument}
  addAction
Where in the node tree to play the monitor synths



@section{method}
 stop
stop to play out public channels.

@section{argument}
 fadeTime
decay time for this action

@section{argument}
 reset
if set to true, reset all monitor state. Otherwise, the previous play arguments are kept.


@section{method}
 monitor
returns the current monitor (see link::Classes/Monitor::)




@section{subsection}
 Bus changes
These methods are a little numerous, because they are important for implementing link::Classes/NodeProxy:: behavior. Mostly the methods link::#-bus:: and link::#-initBus:: will be sufficient in normal use.

@section{note}
 The old bus is freed when a new bus is made.::

@section{method}
 isNeutral
Returns true if no bus has been initialized so far.

@section{method}
 bus
Set or get the bus. If BusPlug monitor is playing, restart the monitor to adequately play back the new bus.

@section{method}
 setBus
set the bus object by passing a link::Classes/Bus::.
@section{note}
 you have to stop and play explicitly::

@section{method}
 defineBus
make a new bus for the BusPlug with a given rate and number of channels.

@section{method}
 initBus
Make a new bus only if necessary. This depends on the current bus and the link::#-reshaping:: mode.

@section{returns}
 Boolean (true if successful).






@section{Examples}
 


@racketblock[
// using as a control bus listener

s.boot;
z = Bus.control(s, 16);
a = BusPlug.for(z);

m = { Mix(SinOsc.ar(a.kr(16), 0, 0.1)) }.play;

z.setn(Array.rand(16, 300, 320).put(16.rand, rrand(500, 1000)));
z.setn(Array.rand(16, 300, 320).put(16.rand, rrand(500, 1000)));
z.setn(Array.rand(16, 300, 320).put(16.rand, rrand(500, 1000)));

m.free;


m = { SinOsc.ar(a.kr(2, MouseX.kr(0, 19)), 0, 0.1) }.play; // modulate channel offset

z.setn(Array.rand(16, 300, 1320).put(16.rand, rrand(500, 1000)));


m.free; z.free;

// using as a audio monitor

p = BusPlug.audio(s,2);
d = { Out.ar(p.index, PinkNoise.ar([0.1, 0.1])) }.play;


p.play; // monitor whatever plays in p (the execution order does not matter)



d.free;
d = { Out.ar(p.index, PinkNoise.ar([0.1, 0.1])) }.play;

p.stop;
p.play;

// also p can play to another bus:

p.stop;
p.play(12);

// listen to that bus for a test:
x = { InFeedback.ar(12,2) }.play;
x.free;
::
]


