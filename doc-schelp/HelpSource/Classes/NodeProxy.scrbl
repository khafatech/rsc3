#lang scribble/manual
@(require (for-label racket))

@title{NodeProxy}
 a reference on a server@section{categories}
  Libraries>JITLib>NodeProxy, Live Coding
@section{related}
  Classes/ProxySpace

@section{description}

Generally a strong::proxy:: is a placeholder for something. A node proxy is a placeholder strong::for something playing on a server:: and writes to a limited number of busses (usually a synth, but also an event stream that makes synths). NodeProxy objects can be replaced and recombined while they play. Also they can be used to build a larger structure which is used and modified later on. Overview: link::Overviews/JITLib::.

NodeProxy is used internally in link::Classes/ProxySpace:: and it is a superclass of link::Classes/Ndef::, allowing to easily access and combine a large number of placeholders.

Graphical editor for a node proxy: link::Classes/NdefGui::.

@section{note}
 
NodeProxy plays on a emphasis::private bus::. If you want to strong::hear:: the output, use link::#-play:: and link::#-stop::. To free inner players and stop listen: link::#-end::. Entirely removing all inner settings: link::#-clear::
::

@section{subsection}
 Further reading

@section{list}
 
## link::Tutorials/JITLib/the_lazy_proxy::
## link::Tutorials/JITLib/jitlib_efficiency::
## link::Tutorials/JITLib/jitlib_fading::
## link::Tutorials/JITLib/jitlib_asCompileString::
## link::Reference/NodeProxy_roles::
::

@section{subsection}
 First Example


@racketblock[
s.boot;

a = NodeProxy.new.play; // play to hardware output.
a.fadeTime = 2; // fadeTime specifies crossfade
// set the source
a.source = { SinOsc.ar([350, 351.3], 0, 0.2) };
a.source = { Pulse.ar([350, 351.3] / 4, 0.4) * 0.2 };
a.source = Pbind(\dur, 0.03, \freq, Pbrown(0, 1, 0.1, inf).linexp(0, 1, 200, 350));

b = NodeProxy.new;
a.source = { Ringz.ar(b.ar, [350, 351.3] * 8, 0.2) * 4 };
b.source = { Impulse.ar([5, 7]/2, [0, 0.5]) };

a.clear(3); // clear after 3 seconds
b.clear(3);
::

]
@section{ClassMethods}
 

@section{subsection}
 Creation

@section{method}
 new
Return a new instance of NodeProxy.


@racketblock[
// new node proxy
a = NodeProxy(s, \audio, 4);
a.numChannels;
a.clear; // remove bus.
a.numChannels; // nil.
::

]
@section{argument}
 server
The server on which to run and where the bus is allocated (default: 
@racketblock[Server.default::)

]
@section{argument}
 rate
If given, proxy is initialized to this rate

@section{argument}
 numChannels
If given, proxy is initialized to this number of channels

@section{argument}
 inputs
If given, proxy is initialized with the given inputs as objects on subsequent slots.

@section{copymethod}
  BusPlug *audio
@section{copymethod}
  BusPlug *control
@section{copymethod}
  BusPlug *for

@section{subsection}
 Accessing Class Variables

@section{copymethod}
  BusPlug *defaultNumAudio
@section{copymethod}
  BusPlug *defaultNumControl

@section{private}
 buildProxy, buildProxyControl, addChild, addNodeMapControlNames, asCode, cleanNodeMap, getBundle, getFamily, getStructure, internalKeys, loadToBundle, moveBeforeMsg, prepareOtherObjects, prepareToBundle, putNewObject, reallocBusIfNeeded, rebuildDeepToBundle, rebuildToBundle, removeToBundle, sendAllToBundle, sendEachToBundle, sendObjectToBundle, serverQuit, shouldAddObject, stopAllToBundle, supplementNodeMap, typeStr, unsetToBundle, wakeUpParentsToBundle, wakeUpToBundle

@section{InstanceMethods}
 

@section{private}
 prFadeTime, linkNodeMap, generateUniqueName, prepareOutput, addToChild

@section{subsection}
 Listening to the output

@section{copymethod}
  BusPlug -play

@section{copymethod}
  BusPlug -playN

@section{copymethod}
  BusPlug -stop

@section{method}
 end
releases the synths and stops playback.

@section{argument}
 fadeTime
cross fade time for this action.

@section{argument}
 reset
if set to true, reset all monitor state. Otherwise, the previous play arguments are kept.


@section{subsection}
 Embedding and Combining the proxy

@section{copymethod}
  BusPlug -ar, kr


@section{method}
 <--
Usage: strong::proxyA <-- proxyB::.  Set proxyA source to proxyB and play proxyA. If proxyB was playing, fade it out. This is convenient in the following situation:


@racketblock[
b = NodeProxy.new.play;
b.source = { PinkNoise.ar(0.2.dup) };
// now I want to filter b through a new proxy.
a = NodeProxy.new;
a <-- b; a.source = { HPF.ar(b.ar, 7000) };
a.source = { HPF.ar(b.ar, 3000) };// changing the source
a.clear; b.clear;
::

]
@section{method}
 <<>
Chaining. Usage: strong::proxyA <<> proxyB	<<> proxyC <<> ...:: . Map proxyC source to proxyB 
@racketblock[\in:: argument, and proxyB to proxyA's in argument.

]

@racketblock[
a = NodeProxy.new.play;
a.source = { RLPF.ar(\in.ar(0!2), [4600, 7000], 0.1) };
b = NodeProxy.new.source_ { Impulse.ar([5, 7] / 2) };
a <<> b;
::

]
@section{method}
 <>>
Inverse of the above. Usage: strong::proxyA <>> proxyB	<>> proxyC <>> ...:: .


@section{subsection}
 Setting the source

NodeProxy keeps a number of slots which can be sources and are mixed on the same bus.. The default source is the slot 0. All objects can be exchanged while running, and also before and after. Normally, the source is active immediately. If sources are to be exchanged "quietly", set the node proxy to sleep (awake = false), or use the message prime().

See the list under section link::#Supported sources::

@section{method}
 source
Play a new synth through proxy and release/remove any old ones.

@racketblock[
a = NodeProxy(s);
a.play;
a.source = { Pulse.ar(130, Saw.kr(0.3)) * 0.1 }; // change this line while running
::

]
@section{argument}
 obj
can be one of the supported inputs (see link::#Supported sources::)

@section{note}
 
When reshaping is set, e.g. to \elastic, setting the source can change the number of channels of the proxy. This means that its bus changes, and that child proxies, which read signals from it, may also change. See the example in link::#routing::.
::

@section{subsection}
 Routing
Signals can be routed between any number of node proxies.

See also: link::Classes/BusPlug#ar::, link::Classes/BusPlug#value::, link::#reshaping::.

Here is a simple example, using Ndef (NodeProxy works similarly):


@racketblock[
Ndef(\maus, { MouseX.kr });
Ndef(\haus, { Pan2.ar(Blip.ar(Ndef.kr(\maus, 1) * 70), SinOsc.kr(Ndef.ar(\maus, 1) * 5)) }).play;
::

]
@section{note}
 
If you don't specify the number of channels, an expression like 
@racketblock[Ndef.kr(\maus):: will return an Array. Subsequently, e.g. by a ]

@racketblock[Pan2:: UGen, you may get the message:

]

@racketblock[WARNING: Synth output should be a flat array.::

If you intend the output to be really mono, make it explicit by writing: ]

@racketblock[Ndef.kr(\maus, 1)::
::

With elastic reshaping, you can use the parent to expand the child, if you omit the number of channels in the routing:

]

@racketblock[
Ndef(\maus, { MouseX.kr });
Ndef(\haus, { Blip.ar(Ndef.kr(\maus) * 70) }).play;
Ndef(\haus).reshaping = \elastic;
Ndef(\maus).reshaping = \elastic;
Ndef(\maus, { LFNoise0.kr(1 ! 8) + 1 }); // now 8 parallel channels of audio are audible.
::




]
@section{subsection}
 Making copies

@section{method}
 copy
copies the hidden internal state to make the new proxy independent of the old, but will keep the reference to the source object. The rendered SynthDef is cached, which makes this method more efficient than simply assigning the same function to a new proxy. By design, the monitor is copied, but is not running (use play to start it in the same configuration).


@racketblock[
a = NodeProxy(s);
a.source = { |freq = 444| Blip.ar(freq * [1, 1.03], 200) * 0.1 };
a.play;
a.set(\freq, 555);
b = a.copy;
b.play;
b.set(\freq, 222);
::

]
@section{note}
 
If needed, you can also copy parts of a proxy, such as the link::Classes/Monitor:: (which usually routs the output back onto hardware busses) and the link::Classes/NodeMap::, which keeps a mapping and setting scope.
::


@racketblock[
s.scope(4);
a = NodeProxy(s);
a.source = { |freq = 234| Blip.ar(freq * [1, 1.03], 20) * 0.5 };
a.playN([0, 1, 3], [0.04, 0.3, 0.1], vol: 1); // some complicated routing
a.set(\freq, 123);

b = NodeProxy(s);
b.source = { |freq = 1000| SinOsc.ar(freq * 3 * {{ rrand(0.9, 1.1) }.dup(8) }.dup(2)).mean * 0.5 };
b.monitor = a.monitor.copy;
b.nodeMap = a.nodeMap.copy;
b.play;
a.stop;
::


]
@section{method}
 copyState
Copy the internal settings of one proxy into another. Old state is cleared.

@section{argument}
 proxy
The proxy whose internal state is copied.



@section{subsection}
 Reshaping

@section{method}
 reshaping
Determines how to behave when a new source is added.
Current options:

@section{table}
 
## nil || Once initialized, keep the same bus - this is the default
## \elastic || On a change, shrink and grow according to need, replace bus. The monitor and child proxies are adjusted.
## \expanding || On a change, only grow according to need, replace bus. The monitor and child proxies are adjusted.
::


@racketblock[
a = NodeProxy(s);
a.reshaping = \elastic;
a.play(0, 2); // play stereo
a.source = { BPF.ar(Dust.ar(8 ! 8), (0..5).nthPrime * 300, 0.01) * 20 };
a.numChannels; // 8
::

]
@section{method}
 mold
Adjust the proxy to a given rate / numChannels. If there are any child proxies that have elastic link::#-reshaping::, they are adjusted accordingly.


@racketblock[
a = NodeProxy(s);
a.play;
a.source = { BPF.ar(Impulse.ar({ rrand(1.0, 2.0) } ! a.numChannels), (0..5).nthPrime * 300, 0.01) * 20 };
a.numChannels; // 2
a.mold(8);
a.numChannels; // 8
::

]
@section{subsection}
 Other ways to set or change the sources

@section{method}
 prime
Set source without starting the synth. To start it, link::#-send:: can be used later. Running synths are released and proxy is initialized if still neutral.

@section{method}
 add
Add a new source to the present ones

@section{method}
 removeAt
Remove the object at index i and its synths, if they exist. If no index is supplied, remove them all.

@section{method}
 removeLast
Remove the last object and its synths, if they exist.


@section{method}
 put
Set the source by index. Objects can be inserted at any index, only the order of indices is relevant. Internally, NodeProxy uses an link::Classes/Order:: to access the sources.

@section{argument}
 index
where the object should be placed in the internal order. if 
@racketblock[-1::, all objects are freed

]
@section{argument}
 obj
A valid source (see link::#Supported sources::).

@section{argument}
 channelOffset
using a multichannel setup it can be useful to set this, when the objects numChannels is smaller than the proxy

@section{argument}
 extraArgs
Arguments that can be sent with the object directly (not cached)

@section{argument}
 now
if set to false, only prepare the source and do not start the object (see link::#-prime::)


@racketblock[
// put can be used with the array indexing syntax:
a = NodeProxy.new.play;
a[0] = { SinOsc.ar(Rand(200, 899)) * 0.1.dup };
a[2] = { SinOsc.ar(Rand(200, 899)) * 0.1.dup };
a.sources.do(_.postcs);
// using multiple index expands into multiple objects
a[0..5] = { SinOsc.ar(Rand(200, 899)) * 0.1.dup };
a.sources.do(_.postcs);
a.send; // exchange synths, using the sources as definitions
a.clear;
::

]
@section{subsection}
 Controlling the running processes

@section{method}
 pause
Pause all objects and set proxy to paused

@section{method}
 resume
If paused, start all objects

@section{method}
 rebuild
Rebuild all SynthDefs from sources.

@section{method}
 orderNodes
Arrange the order of groups from this to the last. This can be important when external input is filtered in order to strong::minimize latency::. Note that if a link::#-parentGroup:: was provided, the nodes must be in the same parentGroup.


@section{subsection}
 Release and cleaning up

@section{method}
 free
Release all running synths and the group. If patterns are playing, stop them.

@section{argument}
 fadeTime
decay time for this action

@section{argument}
 freeGroup
a boolean

@section{method}
 release
release running synths. If patterns are playing, stop them.

@section{argument}
 fadeTime
decay time for this action

@section{method}
 clear
reset everything to nil, neutralizes rate/numChannels

@section{argument}
 fadeTime
if a fadeTime is given, first fade out, then clear.

@section{subsection}
 Accessing Instance Variables

@section{method}
 sources
Returns an array of all sources

@section{method}
 source
Returns the first source.

@section{method}
 server
The node proxy's server (a link::Classes/Server::).

@section{method}
 bus
The node proxy's private bus (a link::Classes/Bus::). Because it has a private bus, it is not audible directly - monitoring it by (.play or playN) routs it to the hardware output channels.

@section{method}
 rate
The bus rate (default: nil) The rate and number of channels is determined either when the instance is created (.control/.audio) or by lazy initialisation from the first source (see link::Tutorials/JITLib/the_lazy_proxy::)

@section{method}
 numChannels
The bus numChannels (default: nil)

@section{method}
 isNeutral
true if the proxy has no initialized bus.

@section{method}
 group
The node proxy's group (a link::Classes/Group::). This is maintained by the proxy and serves as a context in which all synths are placed.

@section{method}
 parentGroup
Access the parentGroup (default: nil), which can be set to run the proxy's group in another group. This group has to be maintained (kept playing etc.) externally.

@section{method}
 clock
A clock, which can be set to account for different timing schemes, such as beat accurate replacement of sources.

@section{method}
 quant
A quant value, to specify quantizes replacement of sources. Compatible with the general use of quant in SuperCollider.

@section{method}
 quantize
Synchronize the proxies by resending and adjusting to quant.

@section{method}
 monitor
Access the link::Classes/Monitor:: object, which plays back the output of the proxy's private bus.

@section{method}
 loaded
Returns true if the object has been initialized on the server, e.g. a synthDef has been stored.

@section{method}
 paused
Returns true if the processes are paused.

@section{method}
 awake
If set to false (default: true), a change of the source does not start a new synth immediately. This is useful when synths are triggered by link::#-spawn::, and a change of sound should not duplicate sends.

@section{method}
 fadeTime
set the crossfade time. See: link::Tutorials/JITLib/jitlib_fading:: .

@section{subsection}
 Setting synth controls

@section{method}
 set, map, setn, mapn
NodeProxy behaves like its link::Classes/NodeMap:: and very similar to a link::Classes/Synth::.

@section{note}
 Now the methods map, setn, mapn, and xmap are there just for backward compatibility reasons. Everything is done by set.::


@racketblock[
a = NodeProxy(s); a.play;
a.source = { Splay.ar(Blip.ar(\freq.kr(18 ! 4))) * 0.2 };
a.set(\freq, [1, 3, 56, 13]);
a.source = { Splay.ar(SinOsc.ar(LFSaw.ar(\freq.kr(18 ! 4)) * 250 + 400)) * 0.2 };
b = NodeProxy(s); b.source = { MouseX.kr(0, 3) *  [1, 3, 56, 13] };
a.set(\freq, b);
a.clear; b.clear;
::

]
@section{argument}
 ... args

An array of pairs: strong::key, value, key, value, ...::

The unique key (a link::Classes/Symbol::) specifies the control name to be set.

For value anything can be specified that responds to "asControlInput", in particular:
@section{definitionList}
 
## number || set the control to that number
## array of numbers || set the subsequent control channels to the corresponding value
## node proxy || map subsequent control channels to the corresponding proxy output channel
::



@section{method}
 unset, unmap
Remove specified settings and unmap or unset the synths.

@section{method}
 xset
set/map with a crossfade into the new setting. The crossfadeT time is the NodeProxy link::#-fadeTime::.

@section{method}
 lag
set the lag values of these args (identical to link::#-setRates::). To remove these settings, use: 
@racketblock[lag(\key1, nil, key2, nil, ...)::

]
@section{method}
 setRates
set the default rate (\tr, \ir, numerical) for synthDef arg. A rate of nil removes setting.

@section{method}
 controlNames
Returns the link::Classes/ControlName:: objects of all slots, strong::except:: the names of this list (default: 
@racketblock[[\out, \i_out, \gate, \fadeTime]:: , which are used internally).

]
@section{method}
 controlKeys
Returns the keys (symbols) of all control names objects of all slots, strong::except:: the names of this list. (default: none).

@section{argument}
 except
list of names

@section{argument}
 noInternalKeys
If noInternalKeys is true (default: true), it ignores the keys 
@racketblock[[\out, \i_out, \gate, \fadeTime]:: .

]
@section{method}
 getKeysValues
Get all key value pairs from both link::Classes/NodeMap:: (the settings) and default arguments.

@section{method}
 controlKeysValues
Get all key value pairs from default arguments.

@section{subsection}
 Sending synths to server explicitly

Normally, processes (usually synths) are started when their respective source is added to the proxy. The processes can also be restarted, however, or the proxy can be used while asleep and the processes can then be started explicitly.

@section{method}
 send
Send a new synth without releasing the old one. If the source is a stream or a pattern, it starts a new one.

@section{argument}
 extraArgs
Arguments used to set the synth. The argument list is applied to the synth only. Arguments specified here override settings in the node map, but leave them untouched.

@section{argument}
 index
What slot to send a new synth with. If nil, uses all. (default: nil)

@section{argument}
 freeLast
if to free the last synth at that index or not (default: true)

@section{method}
 sendAll
Send all synths, or restart all objects.

@section{argument}
 extraArgs
Arguments used to set the synth. the argument list is applied to the synth only.

@section{argument}
 freeLast
if to free the last synth at that index or not (default: true)

@section{method}
 sendEach
Like send, just iterating separately over the objects.

@section{method}
 wakeUp
Until the proxy is not used by any output ( either .play or .ar/.kr ) it is not running on the server. you can wake it up to force it playing. Normally this is not needed.

@section{subsection}
 GUI

@section{method}
 edit
Returns a new instance of link::Classes/NodeProxyEditor:: for this proxy.


@racketblock[
a = NodeProxy.new;
a.edit;

(
a.source = { |freq = 440, rate = 2|
	SinOsc.ar(freq * [1, 1.625]) * SinOsc.kr(rate).max(0) * 0.2
}
);
::

]
@section{section}
 Supported sources

@section{definitionList}
 
## link::Classes/NodeProxy:: || played by reading from the other NodeProxy bus.
## link::Classes/Function:: || interpreted as ugen function, and plays a link::Classes/Synth::, similar to Function.play.
## link::Classes/SimpleNumber:: || write this value to the bus continuously, overwriting previous bus value.
## link::Classes/Array:: of numbers || write the values to the bus continuously overwriting previous bus value.
## link::Classes/Bus:: || read the signal on the bus.
## link::Classes/SynthDef:: || plays a link::Classes/Synth:: using the SynthDef.
## link::Classes/Symbol:: || plays a link::Classes/Synth:: from the SynthDef with this name.
## link::Classes/Pattern:: || played as event pattern (using link::Classes/Pbind:: or other event patterns).
## link::Classes/Stream:: || played as event stream (a stream returning events).
## nil || link::Classes/Nil:: removes all objects.
## link::Classes/Pdef::, link::Classes/EventPatternProxy:: || played like a stream.
## link::Classes/Task:: || played, no output is assigned.
## link::Classes/Tdef:: || played like Task
## link::Classes/Event:: || played like in a pattern.

## Associations (link::Reference/NodeProxy_roles::): ||
@section{definitionList}
 
## (\filter -> func) || filter previous input (with post control)
## (\filterIn -> func) || filter previous input (with pre control)
## (\set -> event pattern) || set controls with the event keys of the pattern
## (\setbus -> event pattern) || set bus with an event pattern. Bus value is the \value key of each event.
## (\setsrc -> event pattern) || set the source with an event pattern. source is the \source key of event.
## (\control -> array or number) || prepare an efficient way to set values by index
## (\mix -> func) || mix audio
::

## crucial library: ||
@section{definitionList}
 
## AbstractPlayer || started in a separate bus, mapped to this bus
## Instr || converted to player and started
::
::

Definitions for other sources can be added - see: link::Reference/NodeProxy_roles::

@section{Examples}
 

For more, see link::Classes/ProxySpace::


@racketblock[
///////////////////// using node proxy with ugen functions /////////////////////

s.boot;

a = NodeProxy.audio(s, 2);
a.play; // play to hardware output, return a group with synths

// setting the source
a.source = { SinOsc.ar([350, 351.3], 0, 0.2) };

// the proxy has two channels now:
a.numChannels.postln;
a.source = { SinOsc.ar([390, 286] * 1.2, 0, 0.2) };

// exceeding channels wrap:
a.source = { SinOsc.ar([390, 286, 400, 420, 300] * 1.2, 0, 0.2) };

// other inputs
a.source = { WhiteNoise.ar([0.01,0.01]) };
a.source = 0;
a.source = \default; // synthDef on server
a.source = SynthDef("w", { arg out=0; Out.ar(out,SinOsc.ar([Rand(430, 600), 600], 0, 0.2)) });
a.source = nil; // removes any object

// feedback
a.source = { SinOsc.ar(a.ar * 7000 * LFNoise1.kr(1, 0.3, 0.6) + 200, 0, 0.1) };
a.source = { SinOsc.ar(a.ar * 6000 * MouseX.kr(0, 2) + [100, 104], 0, 0.1) };

// fadeTime
a.fadeTime = 2.0;
a.source = { SinOsc.ar([390, 286] * ExpRand(1, 3), 0, 0.2) };


// adding nodes
a.add({ SinOsc.ar([50, 390]*1.25, 0, 0.1) });
a.add({ BrownNoise.ar([0.02,0.02]) });

// setting nodes at indices:
a[0] = { SinOsc.ar( 700 * LFNoise1.kr(1, 0.3, 0.6) + 200, 0, 0.1) };
a[1] = { LFPulse.kr(3, 0.3) * SinOsc.ar(500, 0, 0.1) };
a[2] = { LFPulse.kr(3.5, 0.3) * SinOsc.ar(600, 0, 0.1) };
a[3] = { SinOsc.ar([1,1.25] * 840, 0, 0.1) };

// filtering: the first argument is the previous bus content. more args can be used as usual.
a[3] = \filter -> { arg in; in * SinOsc.ar(Rand(100,1000)) };
a[2] = \filter -> { arg in; in * MouseY.kr(0,1) };
a[8] = \filter -> { arg in; in * MouseX.kr(0,1) };
a[4] = \filter -> { arg in; in * SinOsc.ar(ExpRand(1,5)).max(0) };



// setting controls
a.fadeTime = 2.0;
a.source = { arg f=400; SinOsc.ar(f * [1,1.2] * rrand(0.9, 1.1), 0, 0.1) };
a.set(\f, rrand(900, 300));
a.set(\f, rrand(1500, 700));
a.xset(\f, rrand(1500, 700)); // crossfaded setting
a.source = { arg f=400; RLPF.ar(Pulse.ar(f * [1,1.02] * 0.05, 0.5, 0.2), f * 0.58, 0.2) };

// control lags
a.lag(\f, 0.5); // the objects are built again internally and sent to the server.
a.set(\f, rrand(1500, 700));
a.lag(\f, nil);
a.set(\f, rrand(1500, 700));

a.fadeTime = 1.0;

// mapping controls to other node proxies

c = NodeProxy.control(s, 2);
c.source = { SinOsc.kr([10,20] * 0.1, 0, 150, 1300) };
a.map(\f, c);
a[0] = { arg f=400; RHPF.ar(Pulse.ar(f * [1,1.2] * 0.05, 0.5, 0.2), f * 0.58, 0.2) };
c.source = { SinOsc.kr([10,16] * 0.02, 0, 50, 700) };
c.source = { Line.kr(300, 1500, 10) + SinOsc.kr(20 * [1,2], 0, 100) };
a[1] = { arg f; LFPar.ar(f % MouseX.kr(1, 40, 1) * 4 + 360, 0, 0.2) };

// map multiple channels of one proxy to multiple controls of another
// recently changed behaviour!

a.source = { arg f=#[400, 400]; LPF.ar(Pulse.ar(f[0] * [0.4,1], 0.2, 0.2), f[1] * 3) };
a.map(\f, c); // multichannel proxy c is mapped to multichannel control of a
a.source = { arg f=#[400, 400]; LPF.ar(Pulse.ar(f, 0.2, 0.2), f[1]) };
a.source = { arg f=#[400, 400]; Formant.ar(140, f * 1.5, 100, 0.1) };
c.source = { SinOsc.kr([Line.kr(1, 30, 10), 1], 0, [100, 700], [300, 700]) };
c.source = 400;


c.fadeTime = 5.5;
c.source = { LFNoise0.kr([2.3, 1.0], [100, 700], [300, 1700]) };
c.source = { SinOsc.kr([2.3, 1.0], 0, [100, 700], [300, 1700]) };
c.source = 400;


// behave like a sc2 plug
c.gate(1400, 0.1);
c.gate(1000, 0.1);
c.line(1000, 1);

// direct access
a.lineAt(\f, 300, 2);
a.xlineAt(\f, 600, 0.3);
a.gateAt(\f, 1600, 0.3);


// changing nodeMaps
a.unmap(\f);
n = a.nodeMap.copy;
n.set(\f, 700);
a.fadeToMap(n);
n = a.nodeMap.copy;
n.set(\f, 400);
a.fadeTime = 1.0;
a.fadeToMap(n, [\f]); // linear interpolation to new map: experimental
a.map(\f, c); // restore mapping


// sending envelopes (up to 8 levels)
w = Env.new(Array.rand(3, 400, 1000),Array.rand(2, 0.3, 0.001), -4);
c.env(w);
c.env(w);
w = Env.new(Array.rand(8, 400, 1000),Array.rand(7, 0.03, 0.1));
c.env(w);
c.env(w);

// stop synthesis, then wake up proxies:

a.stop; // stop the monitor
a.play; // start the monitor
a.end;	// release the synths and stop the monitor
c.free;	// free the control proxy c
::

]

@racketblock[

///////////////////// channel offset/object index /////////////////////


a = NodeProxy.audio(s,2);
a.play;
a[0] = { Ringz.ar(Impulse.ar(5, 0, 0.1), 1260) };
a.put(1, { Ringz.ar(Impulse.ar(5.3, 0, 0.1), 420) }, 1);
a.put(0, { Ringz.ar(Dust.ar([1,1]*15.3, 0.1), 720) }, 1);
a.put(1, { Ringz.ar(Impulse.ar(5.3, 0, 0.1), 420) }, 1);
a.end;
::

]

@racketblock[

///////////////////// beat accurate playing /////////////////////




a = NodeProxy.audio(s,2);
a.play;

a.clock = TempoClock(2.0).permanent_(true); // round to every 2.0 seconds
a.source = { Ringz.ar(Impulse.ar(0.5, 0, 0.3), 3000, 0.01) };
a[1] = { Ringz.ar(Impulse.ar([0.5, 1], 0, 0.3), 1000, 0.01) };
a[2] = { Ringz.ar(Impulse.ar([3, 5]/2, 0, 0.3), 8000, 0.01) };
a[3] = { Ringz.ar(Impulse.ar([3, 5]*16, 0, 0.3), 5000, 0.01) * LFPulse.kr(0.5, 0, 0.05) };

a.removeLast;
a.removeAt(2);

a.clear;
::

]

@racketblock[

///////////////////// using patterns - event streams /////////////////////


(
// must have 'out' or 'i_out' argument to work properly
SynthDef("who", { arg freq, gate=1, out=0, ffreq=800, amp=0.1;
	var env;
	env = Env.asr(0.01, amp, 0.5);
	Out.ar(out, Pan2.ar(
		Formant.ar(freq, ffreq, 300, EnvGen.kr(env, gate, doneAction: Done.freeSelf)), Rand(-1.0, 1.0))
	)
}).add;

)


(
s.boot;
a = NodeProxy.audio(s, 2);
a.fadeTime = 2;
b = NodeProxy.audio(s,2);
b.fadeTime = 3;
)

a.play; // monitor output

// play the pattern silently in b
b.source = Pbind(\instrument, \who, \freq, 500, \ffreq, 700, \legato, 0.02);

// play b out through a:
a.source = b;

// filter b with ring modulation:
a.source = { b.ar * SinOsc.ar(SinOsc.kr(0.2, 300, 330)) }; // filter the input of the pattern
a.source = { b.ar * LFCub.ar([2, 8], add: -0.5) }; // filter the input of the pattern

a.source = b;

// map b to another proxy
c = NodeProxy.control(s, 1).fadeTime_(1);
c.source = { SinOsc.kr(2, 0, 400, 700) };


// now one can simply embed a control node proxy into an event pattern.
// (this works not for \degree, \midinote, etc.)
// embedding in other patterns it will still return itself.


b.source = Pbind(\instrument, \who, \freq, 500, \ffreq, c, \legato, 0.02);

c.source = { SinOsc.kr(SinOsc.kr(0.2, 0, 10, 10), 0, 400, 700) };

c.source = { LFNoise1.kr(5, 1300, 1500) };
c.source = { MouseX.kr(100, 5500, 1) };

(
b.source = Pbind(
			\instrument, \who,
			\freq, Pseq([600, 350, 300],inf),
			\legato, 0.1,
			\ffreq, Pseq([c, 100, c, 100, 300, 600], inf), // use proxy in a pattern
			\dur, Pseq([1, 0.5, 0.75, 0.25] * 0.4, inf),
			\amp, Pseq([0.2, 0.2, 0.1, 0.1, 0.2], inf)
		);
)



b[2] = Pbind(\instrument, \who, \freq, 620, \ffreq, Prand([500,c],inf), \legato, 0.1, \dur, 0.1);
b[3] = Pbind(\instrument, \who, \ffreq, 5000, \freq, Pseq([720, 800],inf), \legato, 0.1, \dur, 0.1, \amp, 0.01);
b[4] = Pbind(\instrument, \who, \freq, Pseq([700, 400],inf), \legato, 0.1, \ffreq, 200);
b[1] = { WhiteNoise.ar([0.01,0.01]) };
b[4] = { arg ffreq=800; Resonz.ar(WhiteNoise.ar([1,1]), ffreq, 0.05) };


b.map(\ffreq, c); // map the control to the proxy
b.removeLast;
b.removeLast;
a.source = { b.ar * WhiteNoise.ar(0.1, 1) };
a.source = { b.ar * WhiteNoise.ar(0.1, 1) + (b.ar * SinOsc.ar(SinOsc.kr(0.01, 0, 50, 330))) };

c.source = { XLine.kr(1900, 10, 10) };

a.clear(10); b.clear(10); c.clear(10); // fade out and clear all (free bus, group and synths)
::
]


