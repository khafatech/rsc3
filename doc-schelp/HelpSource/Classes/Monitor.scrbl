#lang scribble/manual
@(require (for-label racket))

@title{Monitor}
 link between busses@section{categories}
  Libraries>JITLib>NodeProxy, Live Coding
@section{related}
  Reference/playN, Classes/NodeProxy, Classes/Bus

@section{description}

A general purpose class for monitoring or crosslinking between busses. It supports multichannel expansion and crossfading between settings. It provides optimizations for playing contiguous channels to other contiguous busses (link::#play::) and for more complex routings, such as splitting, spreading etc to multiple channels (link::#playN::). Monitor uses the existing set of  link::Classes/SystemSynthDefs:: to do this.


@racketblock[
{ Out.ar(87, SinOsc.ar(MouseX.kr(240, 1000, 1) * [1, 2, 3], 0, 0.2)) }.play; // play three sine tone on channels 87, 88, and 89
x = Monitor.new; // create a new monitor
x.play(fromIndex: 87, fromNumChannels: 3, toIndex:0, toNumChannels:2); // play them back to the stereo hardware channels
::

]
@section{ClassMethods}
 

@section{private}
 warnPlayN


@section{InstanceMethods}
 

@section{method}
 play
Plays from a bus index with a number of channels to another index with a number of channels, within a target group, or a server.

@section{argument}
 fromIndex
bus index from which to read

@section{argument}
 fromNumChannels
number of channels to read from.

@section{argument}
 toIndex
bus index to which to write.

@section{argument}
 toNumChannels
number of channels to write. If this number is larger or smaller than fromNumChannels, wrap around. If nothing is given, uses fromNumChannels.

@section{argument}
 target
where to send the synths to (default: server default group)

@section{argument}
 multi
keep old links and add new one: this allows you to add layer after layer, otherwise free ther previous mapping (false by default).

@section{argument}
 volume
volume at which to monitor

@section{argument}
 fadeTime
specifies the fade in and fade out time

@section{argument}
 addAction
where, relative to the target to place the monitor group.


@racketblock[
s.boot;
s.scope(16);

{ Out.ar(87, SinOsc.ar(MouseX.kr(40, 10000, 1) * [1, 2, 3], 0, 0.2)) }.play;
x = Monitor.new;
x.play(87, 3, 1, 2);
x.out = 0;
x.stop(3.0);
x.play(87, 1, 0, 1); // in > out : now mixes down (wrapping)
x.play(89, 1, 0, 2); // in < out : now distributes to 2 channels
x.stop;

// multiple play
x.play(87, 1, 0, 2, multi:true);
x.play(88, 1, 0, 2, multi:true);
x.play(89, 1, 0, 2, multi:true);
x.stop;
::



]
@section{method}
 playN
Plays from an array of bus indices to another array of bus indices with an array of amplitudes, within a target group, or a server.

@section{note}
 
The arguments strong::out::, strong::amp:: and strong::in:: can be nested arrays. see also link::Reference/playN::

The three arguments out, amp, and in will wrap if they do not have the same size, like this:

@racketblock[  [[0, 1], [0.1], [3, 4, 5]].flop ::
::

]
@section{argument}
 out
array of destination channels.

@section{argument}
 amp
array of amplitudes for each channel

@section{argument}
 in
array of source channels

@section{argument}
 vol
global scaling value for amplitudes

@section{argument}
 fadeTime
specifies the fade in and fade out time

@section{argument}
 target
where to play (default: server default group)

@section{argument}
 addAction
where, relative to the target to place the monitor group.

@section{argument}
 multi
keep old links and add new one: this allows you to add layer after layer, otherwise free ther previous mapping (false by default).


@racketblock[
// examples: args are // outs, amps, ins, vol, fadeTime

{ Out.ar(87, SinOsc.ar(MouseX.kr(40, 10000, 1) * [1, 2, 3], 0, 0.2)) }.play;
x = Monitor.new;

(
x.playN(
	[0, 1, 4], 			// to these outs
	[0.1, 0.4, 0.3], 	// with these volumes
	[87, 88, 89]		// from these ins
);
)
(
x.playN(
	[0, [1, 3, 2], 4], 		// outs can be nested: 87 -> 0, 88 -> [1, 3, 2], 89 -> 4
	[0.1, [0.4, 0.2, 0.1], 0.3],	// with nested volumes 0.1, [0.4, 0.2, 0.1], and 0.3
	[87, 88, 89]); 			// reading from these ins
)
// can also set global volume and fadetime
x.playN(vol: 0.0, fadeTime:4);
::


]
@section{method}
 stop
Stops within the fadeTime.
@section{note}
 this keeps all the settings, so when using 
@racketblock[play:: next time, it will play in the same configuration, overriding only values provided.::
]

@racketblock[
{ Out.ar(87, SinOsc.ar(MouseX.kr(340, 1000, 1) * [1, 2, 3], 0, 0.2)) }.play;
x = Monitor.new.play(87, 3, 0, fadeTime: 3);
x.stop;
x.play;
::

]
@section{argument}
 argFadeTime
The time for fading out all routing synths.

@section{method}
 clear
Stops within the fadeTime.
@section{note}
 unlike 
@racketblock[stop::, this removes all the settings.::


]
@section{method}
 vol
Set the volume.

@racketblock[
{ Out.ar(87, SinOsc.ar(MouseX.kr(340, 1000, 1) * [1, 2, 3], 0, 0.2)) }.play;
x = Monitor.new.play(87, 3, 0, fadeTime: 3);
x.vol = 0.3;
x.stop;
::


]
@section{method}
 out
Set or get the first output index.

@section{method}
 outs
Set or get the array of output bus indices.

@section{method}
 ins
Set or get the array of input bus indices.

@section{method}
 amps
Set the array of amplitudes.

@section{method}
 fadeTimes
Set or get the array of fadeTimes.

@section{method}
 fadeTime
Set one single fadeTime for the next transition (may be a stop or a new play).

@section{method}
 isPlaying
Returns true if the group is still playing.

@section{method}
 group
Return the group in which all mapping synths are running.

@section{method}
 numChannels
Return the number of input channels.

@section{method}
 copy
Return a copy of the receiver, with the same channel setting, but not running. You can run it with the settings by sending it the link::#play:: message, and pass in any modifications you want to make.


@section{method}
 playToBundle
Adds all playing osc messages to a bundle, passed as an argument. The bundle object should implement the method strong::.add::

@section{private}
 hasSeriesOuts, newGroupToBundle, playNBusToBundle, playNToBundle, stopToBundle, updateDefault, usedPlayN

