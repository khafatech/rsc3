#lang scribble/manual
@(require (for-label racket))

@title{JITLib Changes in 3.7}
@section{categories}
  Libraries>JITLib, Live Coding
 A Changelog for JITLib

@section{section}
  Overview
In general, the changes in this version try to to strengthen the ideal of proxies being true placeholders that behave in many ways like their source objects:

@section{list}
 
## Pattern proxies now should behave more like their source pattern
## Node proxies can adjust their number of channels to the size of the source ugen function if you let them
::

This added a little complexity, but for various reasons, other aspects could be simplified, so that there are fewer classes and a bit more readable code. The system is easier to recover from failure. Some lesser known (or unknown) bugs are fixed, too.

As many parts of the code have been touched, it is possible that there are still bugs in it. Below, you find a long series of (manual) tests that should also reveal a little bit about what you should be able to expect from the system.

Classes that have changed:

@section{list}
 
## link::Classes/NodeProxy::, link::Classes/BusPlug::, link::Classes/Ndef::
## link::Classes/PatternProxy::, link::Classes/Pdefn::, link::Classes/EventPatternProxy::, link::Classes/Pdef::, link::Classes/ProxySpace::
## link::Classes/Monitor::, link::Classes/NodeMap::
::

@section{note}
  examples are given with Ndef, but apply to ProxySpace and NodeProxy: 
@racketblock[Ndef(\x, 5);:: is the same as ]

@racketblock[ ~x = 5;:: in ProxySpace.::

Perhaps one of the greatest chages in terms of behavior is the elastic strong::reshaping:: of node proxies (See: link::#NodeProxy elastic behavior setting::). By default it is off. If you want to test it as a default, you can add this to your startup file:

]

@racketblock[
BusPlug.defaultReshaping = \elastic;
::


]
@section{section}
  Behavior Changes

@section{subsection}
  Changes in pattern proxies (Pdef/Pdefn/Tdef and respective parent classes)

@section{list}
 
## When a none-stream/non-pattern object is the source of the proxy, return the object only once per stream (do not loop by default)
## In the pattern system, the role of a non-pattern object is ambiguous: in some cases, when used as an input of a pattern, it will return itself endlessly (e.g. in a link::Classes/Pbind:: value). In other cases (e.g. as an element of a link::Classes/Pseq:: list), they are used only once and give way to the next element. Pattern proxies should actually behave the same dependent on their input. This means that asStream and embedInStream have to be implemented slightly differently: all pattern proxies now behave to asStream and embedInStream like their respective source objects. E.g. 
@racketblock[Pdefn(\x, 800); Pbindef(\freq, Pdefn(\x)):: will loop 800, while ]

@racketblock[Pseq([Pdefn(\x), 900]):: will return first 800, then 900.

::

]
@section{subsection}
  Changes in node proxy (NodeProxy, Ndef, BusPlug)

@section{list}
 
## controls are now set at synth creation time, not in an extra set message. This means that i-rate controls can be set in the nodemap.
## send and spawn extraArgs now override any existing nodeMap settings for each synth
## when setting the source of a node proxy to a number or an array of numbers, the bus is set by a precompiled synth def, which is much more efficient and consistent than building one. This means the source can be now set to a number/array liberally at higher rates (e.g. Ndef(\x, [100, 200])).
## For implementation reasons, it is now not possible anymore to add up numbers in a single node proxy. One slot will overwrite the next. E.g. 
@racketblock[Ndef(\x)[0] = 100; Ndef(\x)[1] = 200;:: the output will be 200 and not (as ist used to be) 300.
## play (Monitor) can now map from any number of channels to any number of channels
## play and playN  (Monitor) previously passed arguments are kept and can be partially overridden by new ones (the message clear removes them).
## nodeProxy.value(n) (e.g. ]

@racketblock[SinOsc.ar(Ndef(\x)::) will keep the rate the proxy already has).
## using ]

@racketblock[x.ar(n):: and ]

@racketblock[x.kr(n):: will automatically expand when ]

@racketblock[n:: is nil. It remains fixed if n is given.
## ]

@racketblock[nodeProxy.ar(n):: / ]

@racketblock[nodeProxy.kr(n):: if no ]

@racketblock[n:: is given, they will now always return an array, ]

@racketblock[nodeProxy.value(x):: returns an array, if no input is given. This is for general consistency: a few ugens expect an array as an input, and as the numChannels of a proxy can change now, this has to be specified. In order to have a UGen and not an array, use ]

@racketblock[nodeProxy.ar(1)::, or ]

@racketblock[nodeProxy.kr(1)::, respectively.
## unlike it was before, ]

@racketblock[x.ar(n, offset):: will now wrap around the available proxy bus channels. There is a third argument (wrap), when set to false, it will clip instead
## a third argument, clip, was added to ar/kr: ]

@racketblock[x.ar(n, offset, clip)::. When set to \wrap it will extend the output size of the existing channels by wrapping (the default is \wrap). Set it to \clip if you want to keep it in range by repeating the last value.
## InBus is more flexible and logical in terms of multichannel expansion now and has a helpfile
::

]
@section{subsection}
  New functionality

@section{list}
 
## the message link::Classes/NodeProxy#-mold:: allows you to change the number of channels and rate of the proxy at runtime. The proxy's children are automatically remapped, both source and monitor will crossfade between the respective states.
## Note that when rebuilding children, the timing (clock, quant and fadeTime) of the parent is used for all others. If you need different time bases of related proxies, mold dosn't keep their timing. Set the molded proxy's quant to a smallest common multiple of the two quants in this case.
## You can now use NodeMap directly as arguments in Synth objects.
## NodeProxy can be used as argument in a Synth directly, it will either return its bus index or by asMap its bus mapping argument. Note that when reshaping is allowed, the synth won't update automatically. You can register a dependency on \bus.
## node proxies and pattern proxies can be copied now: this will copy the hidden internal state to make the copy independent, but will keep the reference to the source object. In NodeProxy, the rendered SynthDef is cached, which makes the copy efficient. A ProxySpace or LazyEnvir can be copied and then modified independently.
::

@section{subsection}
  NodeProxy elastic behavior setting

@section{list}
 
## NodeProxy is now able to reshape according to its source. When doing so, it recursively updates all other node proxies that depend on it. Both source and monitor will crossfade between the respective states.
## reshaping can be set on three levels:
@section{list}
 
## as a global default, e.g.: 
@racketblock[BusPlug.defaultReshaping = \elastic::.
## for a ProxySpace, e.g.: ]

@racketblock[p = ProxySpace.push; p.reshaping = \elastic::.
## for an individual NodeProxy/BusPlug e.g.: ]

@racketblock[Ndef(\x).reshaping = \elastic::.
::
## by default, reshaping is ]

@racketblock[nil:: (no automatic reshaping). Apart from this, currently, there is ]

@racketblock[\elastic::: both shrink and grow and change rate, and ]

@racketblock[\expanding::: only grow, but also adjust rate.
::

]
@section{subsection}
  A few minor changes

@section{list}
 
## An empty function / or a function returning nil to a node proxy will not fail, but simply play a synth without output
## when trying to play control rate proxy, just warn and don't throw an error
## the source of a proxy is built when it is inserted, not only when the server is running. This allows to know its number of channels independently of the server running.
## bus is freed after fadeTime as to avoid a too early reuse.
## proxy is passed into events produced by event streams (
@racketblock[~proxy::).
::


]
@section{section}
  Implementation Changes

@section{list}
 
## NodeProxy and BusPlug code has been refactored to make it easier to read and more consistent in behavior (see below).
## NodeMap is now simply a Dictionary (NodeMapSetting classes have been removed). You can use NodeMap for many purposes.
## A node proxy keeps track of any other node proxies that use its output so that it can rebuild them if the bus changes ("children").
## Some methods, like setSourceLikeInPbind and prFadeTime were not necessary anymore.
::

@section{section}
  Bugs fixed

@section{list}
 
## Pattern Proxies sometimes didn't embed correctly when quant was not nil (fixed)
## The node order of node proxies relative to each other was wrong when starting them from each other (fixed)
## setting the source of a NodeProxy many times at once failed to release the respective synths (fixed)
## using group-dependent event patterns (like Pfxb) in NodeProxy assigned them the wrong group (fixed)
## Cleanup sometimes didn't work in Pdef. Fixes issue #107.
::


@section{section}
  Example: Elastic NodeProxy


@racketblock[

s.boot;
Ndef.clear;
Ndef.defaultReshaping = \elastic;
Ndef(\x).play(0, 2);
Ndef(\x).fadeTime = 1.3;
Ndef(\y, { Dust.ar(115, 0.1) });
Ndef(\x, { Resonz.ar(Ndef.ar(\y), [700, 720], 0.001, 300) });
Ndef(\y, { Dust2.ar([3, 115], [1, 0.1]) });
Ndef(\x, { Resonz.ar(Ndef.ar(\y), [600, 1720], 0.01, 100) });
Ndef(\x, { Splay.ar(Resonz.ar(Ndef(\y), [600, 1720, 820, 1000] * 1.5, 0.01, 100)) });
Ndef(\y, { Dust.ar([3, 115, 5, 101] * 0.1, [1, 0.1]) });
Ndef(\y, { Dust.ar([3, 115, 5, 101] * 10, [1, 0.1]/2) });
Ndef(\y, { Dust.ar([1013, 415, 1115, 101] * (Ndef.kr(\aa) + 1), [0.1, 0.1]/2) });

Ndef(\aa, { Blip.ar([0.03, 0.05, 0.1], 3).exprange(0.1, 100) });
Ndef(\y, { Dust.ar([1013, 415, 1115, 101] * (Ndef.ar(\aa) + 1), [0.1, 0.1]/2) });

Ndef(\x, { Splay.ar(Resonz.ar(Ndef(\y), [600, 720, 320, 780] * (Ndef.ar(\aa).lag(0.01) * 0.3 + 1), 0.01, 100)).distort });
Ndef(\aa, { Blip.ar([1, 0.5, 0.4], 30).exprange(0.1, 10) });


Ndef(\y, { Impulse.ar([1, 2/3, 4/6, 2] * 100 * DelayC.ar(Ndef(\aa) + 1, 0.5, 0.5, 1, Ndef(\aa)), [0.1, 0.1]/2) });
Ndef(\y, { Impulse.ar([1, 2/3, 4/6, 2] * 4 * DelayC.ar(Ndef(\aa) + 1, 0.5, 0.5, 1, Ndef(\aa)), [0.1, 0.1]/2) });


Ndef(\y, { Impulse.ar([1, 2/3, 4/6, 2, 3] * 100 * DelayC.ar(Ndef.ar(\aa) + 1, 0.5, LFNoise1.kr(0.1 ! 4).range(0, 0.5), 1, Ndef.ar(\aa)), [0.1, 0.1]/2) });

Ndef(\y, { Impulse.ar([1, 2/3, 4/6, 2, 9/8] * 100 * DelayC.ar(Ndef.ar(\aa) + 1, 0.5, LFNoise1.kr(3 ! 2).range(0, 0.5), 1, Ndef(\aa)), [0.1, 0.1]/2) });

Ndef(\x, { Splay.ar(Resonz.ar(SplayAz.ar(4, Ndef.ar(\y)), [600, 720, 320, 780] * (Ndef.ar(\aa).lag(0.01) * 0.3 + 1), 0.01, 100)).distort });

::

]
@section{section}
  Tests for NodeProxy and Monitor


@racketblock[



// some tests for node proxy.
// equalities should always hold true

////////////////////////////////////// build and load separation ////////////////////////////////////////////
// testing build and load with and without server running

s.quit;
Ndef.clear;
x = nil;
Ndef(\x, { x = "rebuilt"; SinOsc.ar([661.1, 877.1, 551.1]) });
Ndef(\x).rate == \audio;
Ndef(\x).numChannels == 3;
Ndef(\x).loaded == false;
Ndef(\x).isPlaying == false;
x == "rebuilt";
x = nil;


s.boot;
Ndef(\x).loaded == false;
Ndef(\x).send;
x == nil;
Ndef(\x).isPlaying;
Ndef(\x).loaded == true;
Ndef(\x).rebuild;
x == "rebuilt";
Ndef(\x).loaded == true;

s.quit;
x = nil;
Ndef(\x).rebuild;
x == "rebuilt";
Ndef(\x).loaded == false;
Ndef(\x).send;
Ndef(\x).loaded == true;

// fail safe for large inits
s.options.numWireBufs = 64 * (2**7);
s.reboot;
Ndef.clear;
Ndef(\x).ar(s.options.numAudioBusChannels * 2);
Ndef(\x).isNeutral;
Ndef(\x).reshaping = \elastic;
Ndef(\x, { DC.ar(0 ! (s.options.numAudioBusChannels * 2)) });
Ndef(\x).isNeutral;
Ndef(\x).ar(8);
Ndef(\x).numChannels == 8;
Ndef(\x, { DC.ar(0 ! (s.options.numAudioBusChannels * 2)) });
Ndef(\x).numChannels == 8;


// testing fadeTime
s.reboot;
Ndef(\x).fadeTime = 2;
Ndef(\x).reshaping = \elastic;

Ndef(\x, { Out.kr(122, -1); SinOsc.ar([200, 300]) * 0.1 });
Ndef(\x, { Out.kr(122, -100); SinOsc.ar([200, 300, 400]) * 0.1 });

// wait a little
s.getControlBusValue(122) == -100;
Ndef(\x).clear(1); fork { 1.01.wait; Ndef(\x).isNeutral.postln };



////////////////////////////////////// new node map ////////////////////////////////////////////
// general test
s.reboot;
a = NodeMap.new;
a.upToDate == false;
a.setMsg == nil;
a.set(\freq, 700, \amp, [0.1, 0.2], \bus, Bus.audio(s));
a.setMsg;
a.asOSCArgArray;
a.addToEvent(x = (g: 9)); x == ( 'bus': 4, 'g': 9, 'freq': 700, 'amp': [ 0.1, 0.2 ] );

// listen
b = Bus.control(s, 1);
{ Out.kr(b, SinOsc.kr(3).exprange(300, 1000)) }.play;
a.set(\freq, b.asMap);
Synth(\default, a);


// node map with proxy
a = ProxyNodeMap.new;
a.set(\x, Ndef(\x));
a.parents.includes(Ndef(\x));
a.set(\x, 800);
a.parents.asArray.includes(Ndef(\x)) == false;
a.set(\x, Ndef(\x));
Ndef(\x).children == nil;

Ndef(\y).set(\x, Ndef(\x));
Ndef(\x).children.includes(Ndef(\y));
Ndef(\y).nodeMap.parents.includes(Ndef(\x));
Ndef(\y).unset(\x);
Ndef(\x).children.includes(Ndef(\y)) == false;

s.boot;
Ndef.clear;
x = nil;
OSCdef(\x, { |msg| msg[3..].postln; x = msg[3]; }, "/tr");
Ndef(\x, { var val = \freq.kr(0 ! 4); SendReply.kr(Changed.kr(val), "/tr", val) });
Ndef(\x).set(\freq, [100, pi, 34, 1]);
x == 100;
Ndef(\z, 80);
Ndef(\x).map(\freq, Ndef(\z));
x == 80;
Ndef(\z).children.includes(Ndef(\x));
Ndef(\x).map(\freq, nil);
Ndef(\z).children.includes(Ndef(\x)).not;

////// send extraArguments override nodemap by coming after it.
// look:
s.waitForBoot({ s.dumpOSC(true)});
Ndef.clear;
Ndef(\x).ar(2);
Ndef(\x, \default);
Ndef(\x).set(\freq, 800, \x, 70);
Ndef(\x, \default);
Ndef(\x, { SinOsc.ar(\freq.ar(440), SinOsc.ar(100 * \x.kr(0 ! 2))) * 0.1 });
Ndef(\x).set(\freq, 800, \x, 70);
Ndef(\x).send([\freq, 1000]);

s.dumpOSC(false);


// node map and control names
Ndef(\x).clear;
Ndef(\x, { |freq = 770, amp| [freq, amp]});
Ndef(\x).controlNames.collect { |x| x.name }.as(Set) == Set[\freq, \amp]
Ndef(\x).set(\freq, 800, \zzz, 99);
Ndef(\x).nodeMap.setMsg.includes(\out);
Ndef(\x).controlNames.collect { |x| x.name }.as(Set) == Set[\freq, \amp, \zzz];
Ndef(\x).getKeysValues.shape == [3, 2];
Ndef(\x).getKeysValues.flop[0].as(Set) == Set[\freq, \amp, \zzz];

// children and nodeMap
Ndef.clear;
a = { NodeProxy.control(s) } ! 8;
Ndef(\x, a.sum);
a[0].children.includes(Ndef(\x));
Ndef(\x, 0);
a[0].children.includes(Ndef(\x)).not;

// updated?
Ndef.clear;
Ndef(\x).ar(2);
Ndef(\x).map(\g, Ndef(\y));
Ndef(\x).nodeMap.asOSCArgArray;
Ndef(\y).index;
Ndef(\y).mold(8);
Ndef(\x).nodeMap.asOSCArgArray; // look. how to test ?




////////////////////////////////////// internal bus ////////////////////////////////////////////


// mold tests
s.boot; // important
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x, { SinOsc.ar([277, 377, 663]) });
Ndef(\x).numChannels == 3;
Ndef(\x).mold(1, \audio);
Ndef(\x).numChannels == 1;
Ndef(\x).rebuild;
Ndef(\x).numChannels == 3;
Ndef(\x).mold(2, \control);
Ndef(\x, { SinOsc.kr([277, 377, 663]) });
Ndef(\x).numChannels == 3;
Ndef(\x).rate == \control;

Ndef.clear;
Ndef(\x).reshaping = nil;
Ndef(\x, { SinOsc.ar([277, 377, 663]) });
Ndef(\x, { SinOsc.ar([277, 377, 663, 1000]) });
Ndef(\x).numChannels == 3;
Ndef(\x).mold;
Ndef(\x).numChannels == 4;
Ndef(\x).mold(1, \audio);
Ndef(\x).numChannels == 1;

s.quit;
Ndef.clear;
Ndef(\x).reshaping = nil;
Ndef(\x, { SinOsc.ar([277, 377, 663]) });
Ndef(\x, { SinOsc.ar([277, 377, 663, 1000]) });
Ndef(\x).numChannels == 3;
Ndef(\x).mold;
Ndef(\x).numChannels == 4;
Ndef(\x).mold(1, \audio);
Ndef(\x).numChannels == 1;


Ndef(\x).rebuild;
Ndef(\x).numChannels == 1;
Ndef(\x).mold(2, \control);
Ndef(\x, { SinOsc.kr([277, 377, 663]) });
Ndef(\x).numChannels == 2;

// testing channel init
Ndef.clear;
Ndef(\x).bus;
Ndef(\x).isNeutral;
Ndef(\x).ar(3);
Ndef(\x).ar(4);
Ndef(\x).numChannels == 3;
Ndef(\x, [1, 2, 3, 4, 5]);
Ndef(\x).numChannels == 3;
Ndef.clear;

Ndef(\x).reshaping = \elastic;
Ndef(\x).ar(3);
Ndef(\x).ar(4);
Ndef(\x).numChannels == 3; // .ar/.kr shouldn't change the bus, unless neutral

Ndef(\x, [1, 2, 3, 4, 5]); // but setting the source should
Ndef(\x).numChannels == 5;
Ndef(\x, [1, 2]);
Ndef(\x).numChannels == 2;
Ndef(\x).children == nil;

// listen
s.boot;
Ndef.clear;
Ndef(\x, { Blip.ar([172, 177]/10) * 0.2 });
s.reboot;
Ndef(\x).bus
Ndef(\x).play;

// test
Ndef.clear;
Ndef(\x).ar(2);
Ndef(\x, Ndef(\y));  // nested proxies without channel specification
Ndef(\x).numChannels == 2;
Ndef(\y).numChannels == 2;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 2;

Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(2);
Ndef(\x, Ndef(\y)); // nested proxies without channel specification
Ndef(\x).numChannels == 2;
Ndef(\y).numChannels == 2;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 4;
Ndef(\y).numChannels == 4;

Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(2);
Ndef(\x, { Ndef.ar(\y, 2) }); // nested proxies with channel specification
Ndef(\x).numChannels == 2;
Ndef(\y).numChannels == 2;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 2; // x remains the same
Ndef(\y).numChannels == 4; // y changes


Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(2);
Ndef(\x, { Ndef.ar(\y, 3) }); // nested proxies with channel specification
Ndef(\x).numChannels == 3; // x changed
Ndef(\y).numChannels == 3;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 3; // x remains the same
Ndef(\y).numChannels == 4; // y changed


s.boot; // important!
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(2);
Ndef(\x, { Ndef.ar(\y) });
Ndef(\x).numChannels == 2;
Ndef(\y).numChannels == 2;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 4;
Ndef(\y).numChannels == 4;
Ndef(\y).children.includes(Ndef(\x));



s.quit; // important!
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(2);
Ndef(\x, { Ndef.ar(\y) });
Ndef(\x).numChannels == 2;
Ndef(\y).numChannels == 2;
Ndef(\x).bus;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\y).numChannels == 4;
Ndef(\x).numChannels == 4;
Ndef(\x).loaded == false;
Ndef(\x).bus;

s.boot;
Ndef(\x).play;
Ndef(\y, [1, 2, 3, 4]);
Ndef(\x).numChannels == 4;
Ndef(\y).numChannels == 4;
Ndef(\y).children.includes(Ndef(\x));


// timing of bus freeing
s.boot; // important
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\y).kr(1);
b = Ndef(\y).bus;
Ndef(\x).quant = 4;
//Ndef(\x).fadeTime = 4;
Ndef(\y, Ndef(\x));
(
c = Ndef(\x).clock ? TempoClock.default;
c.sched(c.timeToNextBeat(4, 0.1), {
	Ndef(\x, { DC.ar([0, 0, 0]) }); (Ndef(\y).bus != b).postln; (b.index != nil).postln;

});
fork { 1.wait; c.sched(c.timeToNextBeat(4), { c.sched(Ndef(\x).fadeTime, { (b.index == nil).postln; nil }); nil }) };
);


// binary ops
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\z).reshaping = \elastic;
Ndef(\z).kr(3);

Ndef(\x, Ndef(\y) * Ndef(\z)); // binary op proxy init
Ndef(\x).numChannels == 3;
Ndef(\y).numChannels == 3;
Ndef(\y).rate == \control;

Ndef(\z, (1..5));
Ndef(\x).numChannels == 5;
Ndef(\y).numChannels == 3;
Ndef(\z).numChannels == 5;

Ndef(\z, { DC.ar([0, 0, 0]) });
Ndef(\x).numChannels == 3;
Ndef(\y).numChannels == 3;
Ndef(\z).numChannels == 3;
Ndef(\z).rate == \audio;


Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\z).reshaping = \elastic;

Ndef(\x, { Ndef.ar(\y) * Ndef.ar(\z) * [1, 1, 1] });
Ndef(\y, 0);
Ndef(\x).numChannels == 3;
Ndef(\y).numChannels == 1;
Ndef(\z).numChannels == 2;
Ndef(\z, { DC.ar([0, 0, 0, 0, 0]) });
Ndef(\x).numChannels == 5;
Ndef(\y).numChannels == 1;
Ndef(\z).numChannels == 5;
Ndef(\x).rate == \audio;

Ndef.clear;
Ndef(\x).ar.numChannels == Ndef.defaultNumAudio;
Ndef(\y).kr.numChannels == Ndef.defaultNumControl;


Ndef.clear;
Ndef.defaultNumAudio = 8;
Ndef.defaultNumControl = 3;
Ndef(\x).ar.numChannels == Ndef.defaultNumAudio;
Ndef(\y).kr.numChannels == Ndef.defaultNumControl;

s.boot;
Ndef.clear;
Ndef(\x).play;
Ndef(\x).ar.numChannels == Ndef.defaultNumAudio;


Ndef.defaultNumAudio = 2;
Ndef.defaultNumControl = 2;
Ndef.clear;

// unary and binary op inits
Ndef.clear;
a = Ndef(\x) + Ndef(\y);
Ndef(\x).isNeutral;
a.rate == \scalar;
a.numChannels == nil;

a = Ndef(\x) + 1; // a number has numChannels 1
Ndef(\x).isNeutral;
a.rate == \scalar;
a.numChannels == 1;

a = 1 + Ndef(\x); // a number has numChannels 1
Ndef(\x).isNeutral;
a.rate == \scalar;
a.numChannels == 1;

a = Ndef(\x) + { 100 }; // function has no rate, numChannels 1
Ndef(\x).isNeutral;
a.rate == \scalar;
a.numChannels == 1;

a = Ndef(\x).abs; // unary op doesn't influence shape
a.rate == \scalar;
a.numChannels == nil;

Ndef.clear;
Ndef(\y).ar(3); // init y as 3 channel audio
a = Ndef(\x) + Ndef(\y); // binary op will influence operand
Ndef(\x).numChannels == nil;
a.rate == \audio;
a.numChannels == 3;

Ndef.clear;
Ndef(\y).ar(3); // init y as 3 channel audio
a =  Ndef(\y) + Ndef(\x); // inverse binary op will influence operand
Ndef(\x).numChannels == nil;
a.rate == \audio;
a.numChannels == 3;


// many things at once
s.boot;
Ndef(\x).clear;
Ndef(\x).reshaping = \elastic;
20.do { Ndef(\x, { { WhiteNoise.ar } ! rrand(3, 8) }) };
OSCFunc({ |msg| x = msg[3].postln }, "/tr");
Ndef(\x, { SendTrig.kr(TDelay.kr(Impulse.kr(0), 0.1), 0, NumRunningSynths.kr) });
x == 1;
//s.plotTree;

Ndef.defaultNumControl

// direct embedding
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\z).reshaping = \elastic;
Ndef(\x, { SinOsc.ar(Ndef(\y)) * 0.1 });
Ndef(\y).rate == \control;
Ndef(\y).numChannels == 1;
Ndef(\x).rate == \audio;
Ndef(\x).numChannels == 1;
Ndef(\y, { LFPulse.kr([0.8, 0.6]) });
Ndef(\y).numChannels == 2;
Ndef(\x).numChannels == 2;

Ndef(\x, { SinOsc.ar(Ndef(\y) + 300) * 0.1 });
Ndef(\y).numChannels == 2;
Ndef(\x).numChannels == 2;

Ndef(\x, Ndef(\z) + Ndef(\u));
Ndef(\z).numChannels
Ndef(\z, (1..9));
Ndef(\x).numChannels == 9;

// slot rebuild tests
s.quit;
Ndef.clear; x = nil;
Ndef(\x).reshaping = \elastic;
Ndef(\x)[0] = { x = "rebuilt"; SinOsc.ar([277, 377, 663]) };
x == "rebuilt";
x = nil;
Ndef(\x)[1] = { SinOsc.ar([277, 377, 663, 900]) };
x == "rebuilt";
Ndef(\x).numChannels == 4;
Ndef(\x)[0] = {  x = "rebuilt"; SinOsc.ar([100, 202]) };
Ndef(\x).numChannels == 2;
x = nil;
Ndef(\x).mold(5);
x == "rebuilt";
Ndef(\x).numChannels == 5;

// testing slot channel init: TODO: are they really rebuilt?

Ndef.clear;
x = 0;
Ndef(\x)[0] = { "rebuild two".postln; x = x + 1; SinOsc.kr([1, 2]) };
Ndef(\x)[1] = { "rebuild four".postln;  x = x + 1; SinOsc.kr([1, 2, 3, 4]) };
Ndef(\x)[2] = { "rebuild one".postln;  x = x + 1; SinOsc.kr(1) };
Ndef(\x).numChannels == 2;
x = 0;
Ndef(\x).mold;
Ndef(\x).numChannels == 4;
x == 3;

// scalar outputs are not returned
Ndef.clear;
Ndef(\x)[0] = { "rebuild two".postln; [1, 2] };
Ndef(\x)[1] = { "rebuild four".postln; [1, 2, 3, 4] }
Ndef(\x)[2] = { "rebuild one".postln; 1 };
Ndef(\x).rate == \control;
Ndef(\x).mold; // shouldn't do anything either. Well, I suppose that in this case it has to, unless we check the max rate of the whole set to sources. This is impossible if the sources themselves initialise the proxy.
Ndef(\x).isNeutral.not;
Ndef(\x).mold(4); // this should
Ndef(\x).numChannels == 4;
Ndef(\x).rate == \control;



// mapping and initialisation tests
Ndef.clear;
Ndef(\x).nodeMap.proxy == Ndef(\x);
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).map(\freq, Ndef(\y));
Ndef(\y).children.includes(Ndef(\x));
Ndef(\y, (1..6));
Ndef(\y).numChannels == 6;
Ndef(\x).numChannels == nil;
Ndef(\x, 8);
Ndef(\x).numChannels == 1;
Ndef(\y, { SinOsc.ar(400) });
Ndef(\x).numChannels == 1; // correct. mapping shouldn't change numChannels
Ndef(\x, { SinOsc.ar(\freq.kr(440 ! 2)) * 0.1 });
Ndef(\y).numChannels == 1;
Ndef(\x).numChannels == 2;
Ndef(\x).rate == \audio;
Ndef(\x).set(\freq, 500);
Ndef(\x).unmap(\freq);
Ndef(\x).unset(\freq);
Ndef(\y).children.includes(Ndef(\x)).not;



// bus args and reshaping tests
s.options.numOutputBusChannels = 2;
s.options.numInputBusChannels = 2;
s.reboot;

Ndef.clear;
Ndef(\x).ar(2);
Ndef(\x).asControlInput == ["a4", "a5"];
Ndef.clear;
Ndef(\x).mold(3, \audio);
Ndef(\x).rate == \audio;
Ndef(\x).numChannels == 3;

s.boot;
Ndef.clear;
a = 10;
Ndef(\x, { { DC.ar(1.0) } ! a });
a = 11;
Ndef(\x).rebuild;
Ndef(\x).numChannels == 10;
Ndef(\x).reshaping = \elastic;
Ndef(\x).rebuild;
Ndef(\x).numChannels == 11;


Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x).ar(8);
Ndef(\y, { Ndef(\x) });
Ndef(\y).numChannels == 8;
Ndef(\x).children.includes(Ndef(\y));
Ndef(\x).clear;
Ndef(\x, { SinOsc.ar([100, 200, 283]) });
Ndef(\y).numChannels == 8;
Ndef(\x).numChannels == 3;

// channelOffset: look

Ndef.clear;
Ndef(\x).ar(4); // channels must be correctly offset
Ndef(\x).put(0, { SinOsc.ar(440) }, channelOffset: 2);
{ Ndef.ar(\x, 4) }.plot; // look: should have a sine in the third channel

// listening tests
Ndef.clear;
Ndef(\y, { Splay.ar(Ringz.ar(Ndef.ar(\x, 8), ((0..7) * 2 + 72).midicps, 0.1)) }).play;
Ndef(\x).put(0, { Impulse.ar(1) }, 0);
Ndef(\x).put(0, { Impulse.ar(2) }, 5);
Ndef(\x).put(1, { Impulse.ar(2/3) }, 2);
Ndef(\x).put(2, { Impulse.ar(4) }, 4);
Ndef(\x).quant = 1.0;
Ndef(\x).rebuild;


// dynamic offset via pattern
Ndef.clear;
s.boot;
Ndef(\x).reshaping = \elastic;
Ndef(\y).reshaping = \elastic;
Ndef(\x, Pbind(\channelOffset, Pseries(0, 1, inf), \dur, 0.25, \legato, 0.1));
Ndef(\y, { Ndef.ar(\x).collect { |x| x * SinOsc.ar(exprand(3.0, 3000)) * LFPulse.kr(rrand(10, 50)) } }).play(0, 2);
Ndef(\x).numChannels == 2;
Ndef(\x).quant = 4;
Ndef(\x).mold(4);
Ndef(\x).mold(8);
Ndef(\x).mold(16);

////////////////////////////////////// Monitor ////////////////////////////////////////////
// testing simple play
s.quit;
Ndef.clear;
Ndef(\x).play;
Ndef(\x).isNeutral;

s.boot;
Ndef(\x).play;
Ndef(\x).numChannels == NodeProxy.defaultNumAudio;
Ndef(\x).rate == \audio;
Ndef(\x).clear;
Ndef(\x).monitor.isPlaying == false;
Ndef(\x, (1..5));
Ndef(\x).play; // for now: warn and do nothing. In future: convert rate (system_link_control2audio)

Ndef.clear;
Ndef(\x, { SinOsc.ar((1..6) * 200) * 0.1 });
Ndef(\x).play;
Ndef(\x).monitor.outs == (0..5);
Ndef(\x).mold(2);
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == (0..1);
Ndef(\x).stop;
Ndef(\x).play;
Ndef(\x).monitor.outs == (0..1);
Ndef(\x).mold;
Ndef(\x).monitor.outs == (0..5);

Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x, { SinOsc.ar((1..6) * 200) * 0.1 });
Ndef(\x).play;
Ndef(\x).monitor.outs == (0..5);
Ndef(\x, { SinOsc.ar([300, 439]) * 0.1 });
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == [0, 1];
Ndef(\x, { SinOsc.kr([300, 439]) * 0.1 });
Ndef(\x, { SinOsc.ar([300, 439]) * 0.1 }); // playing again.
Ndef(\x, { SinOsc.ar((1..6) * 200) * 0.1 });

Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x).play(4, 3); // from bus 4, 3 channels
Ndef(\x).monitor.outs == [4, 5, 6];
Ndef(\x, { SinOsc.ar((1..6) * 200) * 0.1 });
Ndef(\x).numChannels == 6;
Ndef(\x).monitor.outs == [4, 5, 6];
Ndef(\x).monitor.ins == (Ndef(\x).index + (0..5));
Ndef(\x, { SinOsc.ar([300, 439]) * 0.1 });
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == [4, 5, 6];
Ndef(\x).monitor.ins == (Ndef(\x).index + (0..1));
Ndef(\x, { SinOsc.kr([300, 439]) * 0.1 });
Ndef(\x, { SinOsc.ar([300, 439]) * 0.1 }); // playing again.
Ndef(\x, { SinOsc.ar((1..6) * 200) * 0.1 });


Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x).play;
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == [0, 1];
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1]);
Ndef(\x, { DC.ar([0, 0, 0]) });
Ndef(\x).numChannels == 3;
Ndef(\x).monitor.outs == [0, 1, 2];
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1, 2]);
Ndef(\x).stop;
Ndef(\x).play;
Ndef(\x).monitor.outs == [0, 1, 2];
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1, 2]);


Ndef.clear;
Ndef(\x).reshaping = nil;
Ndef(\y).reshaping = nil;
Ndef(\x).play;
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == [0, 1];
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1]);
Ndef(\x, { DC.ar([0, 0, 0]) });
Ndef(\x).numChannels == 2;
Ndef(\x).monitor.outs == [0, 1]; // OK? should it be like this, probably yes. (maybe depend on s.options.numOutputBusChannels)
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1]);
Ndef(\x).stop;
Ndef(\x).play;
Ndef(\x).monitor.outs == [0, 1]; // no. [0, 1, 2] OK? should it be like this, probably yes. (maybe depend on s.options.numOutputBusChannels)
Ndef(\x).monitor.ins == (Ndef(\x).index + [0, 1]);


Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x, { Dust.ar(10 ! 2) * 0.1 }).play;
Ndef(\out) <-- Ndef(\x);
//{ Trig.ar(InFeedback.ar(0, 2), 0.01) }.plot(0.2);
Ndef(\x).monitor.isPlaying == false;
Ndef(\out).monitor.isPlaying == true;
Ndef(\out, { Ringz.ar(Ndef.ar(\x), [8000, 2888, 3000], 0.1) });
Ndef(\out).numChannels == 2;
Ndef(\out).reshaping = \elastic;
Ndef(\out, { Ringz.ar(Ndef.ar(\x), [8000, 2888, 3000], 0.1) });
Ndef(\x).numChannels == 2;
Ndef(\out).numChannels == 3;
Ndef(\out).monitor.ins.size == 3;
Ndef(\out).monitor.outs == [0, 1, 2];

// monitor defaults are kept
Ndef.clear;
Ndef(\x).playN([0, 3, 5], [0.1, 1], [0, 2], 0.3, [1.0, 2.0]);
Ndef(\x).monitor.ins == ([0, 2] + Ndef(\x).index);
Ndef(\x).monitor.outs == [0, 3, 5];
Ndef(\x).monitor.amps == [0.1, 1];
Ndef(\x).monitor.vol == 0.3
Ndef(\x).monitor.fadeTime == [1.0, 2.0];
Ndef(\x).playN(fadeTime: [0.1, 0.2]);
Ndef(\x).monitor.fadeTime == [0.1, 0.2];
Ndef(\x).monitor.outs == [0, 3, 5];
Ndef(\x).monitor.amps == [0.1, 1];
Ndef(\x).monitor.vol == 0.3;
Ndef(\x).stop;
Ndef(\x).play;
Ndef(\x).monitor.fadeTime == [0.1, 0.2];
Ndef(\x).monitor.outs == [0, 3, 5];
Ndef(\x).monitor.amps == [0.1, 1];
Ndef(\x).monitor.vol == 0.3;
Ndef(\x).clear; // clear it
Ndef(\x).play;
Ndef(\x).monitor.fadeTime != [0.1, 0.2];
Ndef(\x).monitor.outs == [0, 1];
Ndef(\x).mold(3);
Ndef(\x).playN([0, 3, 5], [0.1, 1], [0, 2], 0.3, [1.0, 2.0]);
Ndef(\x).monitor.outs == [0, 3, 5];
Ndef(\x).mold(7);
Ndef(\x).monitor.outs == [0, 3, 5];
Ndef(\x).playN([0, 3, 6]);
Ndef(\x).monitor.outs == [0, 3, 6];
Ndef(\x).monitor.amps == [0.1, 1];
Ndef(\x).monitor.ins == ([0, 2] +  Ndef(\x).index);
// cmd-period
Ndef(\x).play;
Ndef(\x).monitor.outs == [0, 3, 6];
Ndef(\x).monitor.amps == [0.1, 1];
Ndef(\x).monitor.ins == ([0, 2] +  Ndef(\x).index);

Ndef(\x).playN(ins: [0, 1]);
Ndef(\x).monitor.ins == ([0, 1] +  Ndef(\x).index);
Ndef(\x).playN(ins: [0, 7]);
Ndef(\x).monitor.ins == ([0, 0] +  Ndef(\x).index); // clip at end
Ndef(\x).mold(9);
Ndef(\x).monitor.ins == ([0, 7] +  Ndef(\x).index); // unclipped now.

Ndef.clear;
s.boot;
Ndef(\x).play(4, 7); // play seven channels from bus four
Ndef(\x).monitor.outs == ((0..6) + 4);
Ndef(\x).monitor.ins == ((0..6) +  Ndef(\x).index);
Ndef(\x).playN(fadeTime: [0.1, 0.2]);
Ndef(\x).monitor.outs == ((0..6) + 4);
Ndef(\x).monitor.ins == ((0..6) +  Ndef(\x).index);
Ndef(\x).playN(outs: [0, 2, 3]);
Ndef(\x).monitor.outs == [0, 2, 3];
Ndef(\x).monitor.ins == ((0..6) +  Ndef(\x).index);


// changed messages
s.boot;
x = nil;
Ndef.clear;
f = { |...args| x = x ++ args.postln };
Ndef(\x).addDependant(f);
Ndef(\x, { SinOsc.ar });
x.includesAll([\source, \bus]);
x = nil;
Ndef(\x).set(\freq, 700);
x.includesAll([\set]);
Ndef(\x).map(\freq, 700);
x.includesAll([\set]); // set == map
Ndef(\x).unset(\freq, \rate);
x.includesAll([\unset]);
Ndef(\x).unmap(\freq, \rate);
x.includesAll([\unmap]);
Ndef(\x).reshaping = \elastic;
x = nil;
Ndef(\x, { Impulse.kr([1, 2, 3]) });
x.includesAll([\rebuild, \source]);
x = nil;
Ndef(\x).mold(7, \audio);
x.includesAll([\rebuild]);
x = nil;
Ndef(\x).mold(1, \audio);
x.includesAll([\rebuild, \bus]);
x = nil;
Ndef(\x).play;
x.includesAll([\play]);
Ndef(\x).clear;
Ndef(\x).removeDependant(f);

// defaultReshaping
Ndef.defaultReshaping = \elastic;
Ndef.clear;
Ndef(\u).reshaping == Ndef.defaultReshaping;
Ndef(\u, 8);
Ndef(\u).rate == \control;
Ndef(\u, { Blip.ar(1 ! 6) });
Ndef(\u).rate == \audio;
Ndef.clear;
Ndef(\err, { Plippp.ar(800) }); // create a deliberate error
Ndef(\err).isNeutral; // still unchanged

p = ProxySpace(s);
p.reshaping = \elastic;
p[\out].reshaping == \elastic;
p.reshaping = nil;
p[\out].reshaping == nil;

\control<\scalar;
\audio<\control;

// reshaping types: nil, elastic and expanding

// reshaping = nil
Ndef.clear;
Ndef(\x).reshaping = nil;
Ndef(\x).kr(1);
Ndef(\x).rate == \control;
Ndef(\x, { DC.ar(1) });
Ndef(\x).numChannels == 1;
Ndef(\x).rate == \control;
Ndef(\x, { DC.ar([1, 2]) });
Ndef(\x).numChannels == 1;
Ndef(\x, { DC.kr(1) });
Ndef(\x).numChannels == 1;
Ndef(\x).rate == \control;

// reshaping = \elastic
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x).kr(1);
Ndef(\x).rate == \control;
Ndef(\x, { DC.ar(1) });
Ndef(\x).numChannels == 1;
Ndef(\x).rate == \audio;
Ndef(\x, { DC.ar([1, 2]) });
Ndef(\x).numChannels == 2;
Ndef(\x, { DC.kr(1) });
Ndef(\x).numChannels == 1;
Ndef(\x).rate == \control;

// reshaping = \expanding
// for now, rate always triggers change.
Ndef.clear;
Ndef(\x).reshaping = \expanding;
Ndef(\x).kr(1);
Ndef(\x).rate == \control;
Ndef(\x, { DC.ar(1) });
Ndef(\x).numChannels == 1;
Ndef(\x).rate == \audio;
Ndef(\x, { DC.ar([1, 2]) });
Ndef(\x).numChannels == 2;
Ndef(\x, { DC.ar(1) });
Ndef(\x).numChannels == 2;
Ndef(\x).rate == \audio;
Ndef(\x).mold(1); // force it
Ndef(\x).numChannels == 1;


///////// BusPlug tests

s.boot;
a = BusPlug(s);
a.bus == nil;
a.initBus(\audio, 16);
a.bus.numChannels == 16;
a.initBus(\audio, 2) == true;
a.initBus(\audio, 17) == false;
a.reshaping = \elastic;
a.initBus(\audio, 17) == true;
a.bus.numChannels == 17;
a.clear;
a.isNeutral == true;


// extraArgs: sendEach vs sendAll
Ndef(\x).play;
Ndef(\x)[0] = { |freq = 900| Blip.ar(freq, 200) * 0.04 };
Ndef(\x)[1] = { |freq = 900| BPF.ar(PinkNoise.ar(19), freq, 0.01) };
Ndef(\x).send([\freq, { rrand(340.0, 500.0) } ]);
Ndef(\x).sendEach([\freq, { rrand(340.0, 500.0) } ]);


// testing mapping operators
Ndef.clear;
Ndef.defaultReshaping = \elastic;
Ndef.all.fadeTime = 3;

Ndef(\x) <<> Ndef(\y) <<> Ndef(\z);
Ndef(\x).numChannels == 2;
Ndef(\z).numChannels == 2;
Ndef(\z, { SinOsc.ar([100, 200, 300] + 520) });
Ndef(\z).numChannels == 3;
Ndef(\y).numChannels == 2;
Ndef(\y, { \in.ar(0 ! 3) * SinOsc.ar(441) });
Ndef(\y).numChannels == 3;
Ndef(\x, { \in.ar(0 ! 3) * SinOsc.ar(141) * 0.2 });
Ndef(\x).play(0, 2);
Ndef(\y).fadeTime = 2;
Ndef(\y, { \in.ar(0 ! 3) * SinOsc.ar(141 * 2, SinOsc.ar(Line.kr(1, 12, 10))) });

// busses as arguments for send
b = Bus.control(s, 1);
b.set(440);

Ndef(\x, { |freq = 100| SinOsc.ar(freq) * 0.1 }).play;
Ndef(\x).send([\freq, b.asMap]);

// proxies as arguments of synths
SynthDef(\x, { |z=440| Out.ar(0, Blip.ar(z) * 0.1) }).add;
Ndef(\f, { MouseX.kr(400, 700) });
(instrument: \x, z: Ndef(\f)).play;

// concatenate nodeMaps:
a = NodeMap.new;
a.set(\x, 800);
Ndef(\x).set(\y, 900);
Ndef(\x).nodeMap = Ndef(\x).nodeMap ++ a;
Ndef(\x).nodeMap;



////////////////////////////////////////////////// Other tests /////////////////////////////////////////////////


// mapping tests with synths.


a = { Out.kr(67, SinOsc.kr(1) * 500 + 600) }.play;

b = Synth(\default);
b.set(\freq, 800);
b.map(\freq, 67);
b.map(\freq, -1);

Ndef(\x, \default).play;
Ndef(\y, { SinOsc.kr(1) * 500 + 600 });
Ndef(\x).map(\freq, Ndef(\y));
Ndef(\x).unmap(\freq);
Ndef(\x).set(\freq, 800);

Ndef(\x).nodeMap


// direct embedding in UGens with asAudioRateInput

Ndef.clear;
Ndef(\x).ar(2);
{ CombL.ar(Ndef(\x)) }.play;
Ndef(\x, { Dust2.ar(Line.kr(0, 200, 10)) });

Ndef.clear;
Ndef(\x).kr(2);
{ CombL.ar(Ndef(\x)) }.play;
Ndef(\x, { Dust2.kr(Line.kr(0, 200, 10)) });


// copying

a = NodeProxy(s);
a.set(\gg, 910);
a.source = 6;
b = a.copy;
a.bus !== b.bus;
a.nodeMap.at(\gg) == b.nodeMap.at(\gg);
a.source == b.source;
b.source = 10;
a.source != b.source;
a.nodeMap.at(\gg) == b.nodeMap.at(\gg);
b.set(\gg, 723);
a.nodeMap.at(\gg) != b.nodeMap.at(\gg);

p = ProxySpace(s);
p.use { ~x = 891; ~y = 71; ~x.set(\freq, 661) };
q = p.copy;
f = { |x, func| x.envir.keys.as(Array).sort.collect { |key| func.(x.envir.at(key)) } };
f.(p, { |proxy| proxy.source }) == f.(q, { |proxy| proxy.source });
f.(p, { |proxy| proxy.nodeMap.at(\freq) }) == f.(q, { |proxy| proxy.nodeMap.at(\freq) });
f.(p, { |proxy| proxy.objects }) != f.(q, { |proxy| proxy.objects });
p.use { ~x.set(\freq, 0) };
f.(p, { |proxy| proxy.nodeMap.at(\freq) }) != f.(q, { |proxy| proxy.nodeMap.at(\freq) });
p.use { ~x = -100 };
f.(p, { |proxy| proxy.source }) == f.(q, { |proxy| proxy.source });

Ndef(\x).set(\freq, 900);
Ndef(\x, 7);
a = Ndef(\y);
Ndef(\x).copy(\y);
Ndef(\y) === a;
Ndef(\y).nodeMap.at(\freq) == 900;
Ndef(\y).source == 7;


// various objects
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x, (freq: 100, numChannels: 3));
Ndef(\x).rate == \audio;
Ndef(\x).numChannels == 3;
Ndef(\x, (freq: 100, numChannels: 2));
Ndef(\x).numChannels == 3;  // if proxy is initialized, it is user's responsibility (good?)

Ndef.clear;
Ndef(\x, Task { 2.do { "hwert".scramble.postln; 1.wait } });
Ndef(\x).isNeutral;

// crucial lib
Instr(\sin, { arg freq = 440, amp; SinOsc.ar(freq, 0.0, amp) });
Ndef.clear;
Ndef(\x).reshaping = \elastic;
Ndef(\x, Instr(\sin));
Ndef(\x).rate == \audio;
Ndef(\x).numChannels == 1;
Ndef(\x, Patch(\sin, [\freq, 440]));
Ndef(\x).play;

Instr(\sin, { arg freq, amp; SinOsc.ar(freq, BrownNoise.ar(1 ! 2), amp) });


Ndef(\x, Instr(\sin));// not correct yet, should be 2.

//Ndef(\c, StreamKrDur(Pseq([Prand([530, 600],1), 700, 400, 800, 500].scramble, inf) / 3, 0.2)); // still fails

::

]
@section{section}
 Tests for PatternProxy


@racketblock[
// general rule: proxy should behave (in streams and patterns) like the object it holds
(
g = { |f, source, firstSource|
	var n = 16;
	var a, b, proxy, pattern;
	proxy = PatternProxy.new;
	a = f.(proxy).asStream;
	if(firstSource.notNil) { proxy.source = firstSource; n.do { a.next } };
	proxy.source = source;
	b = f.(source).asStream;
	Array.fill(n, { a.next }).postln == Array.fill(n, { b.next }).postln
};
h = { |f, source|
	g.(f, source, Pseq([-1], inf));
}
)


g.( { |x| Pseq([x, 2, 3]) }, 1);
g.( { |x| Pseq([x, 2, 3]) }, Pseq([10], 2));
g.( { |x| x }, Pseq([10], 2));
g.( { |x| x }, 1);
g.( { |x| Pn(x, 3) }, 1);
g.( { |x| Pn(x, 3) }, Pseq([10], 2));
g.( { |x| Pswitch([10, 20, 30], x) }, Pseq([1, 2], 2));
g.( { |x| Pswitch([10, x, 30], Pseq([0, 1, 2], 2)) }, 80);
g.( { |x| Pswitch([10, x, 30], Pseq([0, 1, 2], 2)) }, Pseq([1, 2], 2));
g.( { |x| Pfin(6, x) }, Pseq([0, 1, 2], 2));
g.( { |x| Pfin(6, x) }, Pseq([0, 1, 2], 1));
g.( { |x| Pfin(6, x) }, 3);


h.( { |x| Pseq([x, 2, 3]) }, 1);
h.( { |x| Pseq([x, 2, 3]) }, Pseq([10], 2));
h.( { |x| x }, Pseq([10], 2));
h.( { |x| x }, 1);
h.( { |x| Pn(x, 3) }, 1);
h.( { |x| Pn(x, 3) }, Pseq([10], 2));
h.( { |x| Pn(Pfin(4, x)) }, Pseq([0, 1, 2], 2));
h.( { |x| Pn(Pfin(4, x)) }, Pseq([0, 1, 2], 1));
h.( { |x| Pn(Pfin(4, x)) }, 3);

(
g = { |f, source|
	var n = 16;
	var a, b, proxy, pattern;
	proxy = EventPatternProxy.new;
	proxy.source = source;
	a = Pevent(f.(proxy)).asStream;
	b = Pevent(f.(proxy.source)).asStream;
	Array.fill(n, { a.next }).postln == Array.fill(n, { b.next }).postln
};
)

a = Pcollect({ |event| event }, (zz: 4));
a = Pcollect({ |event| event }, Pn((zz: 4)));


a = Pcollect({ |event| (aa:9) }, Pbind.new);
a = Pcollect({ |event| (aa:9) }, (zz: 4));
x = a.asStream;
x.next(());

a = Pcollect({ |event| event.put(\test, -100) }, (zz: 4));
x = a.asStream;
x.next(());




x = (zz: 4).asStream;
(aa: 9).processRest( (zz: 4) );


a = Pcollect({ |event| event }, (zz: 4));
x = Pevent(a).asStream;
x.next;

a = Pcollect({ |event| event[\a] = 9; event }, (zz: 4));
x = Pevent(a).asStream;
x.next;

a = Pcollect({ |event| (aa:9) }, (zz: 4));
x = Pevent(a).asStream;
x.next;

(aa: 6).next(())


Pevent(Pcollect({ |event| event[\zz] = 9; event; }, (zz: 4))).asStream.nextN(8)


g.( { |x| Pseq([x, x]) }, (x: 9));
g.( { |x| Pseq([x, (y: 10)]) }, (x: 9));
g.( { |x| Pseq([x, (y: 10)]) }, Pbind(\x, Pseq([1, 2, 3])));
g.( { |x| Pswitch([(x: 10), (x:20), (x:30)], x) }, Pseq([0, 1, 2], 2), 10);
//g.( { |x| Pgate(Pseq([(x:1), (x:2), (x:3)], inf), \go) <> x }, Pseq([ (go: true), (), (), (go: false)], inf));
g.( { |x| Pselect({ |event| event[\zz].notNil }, x) }, Pseq([(), (), (zz: 300), (zz: 500)], 2));
//g.( { |x| Pselect({ |event| event[\zz].notNil }, x) }, (zz: 800));
g.( { |x| Pcollect({ |event| event[\zz] = 100 }, x) }, Pseq([(), (), (zz: 300), (zz: 500)], 2));
g.( { |x| Pfset({ ~gg = 8; ~zz = 9; }, x) }, Pbind.new);
//g.( { |x| Pfset({ ~gg = 8; ~zz = 9; }, x) },  (x: 9));
g.( { |x| Psetpre({ ~gg = 8; ~zz = 9; }, x) }, Pbind.new);
g.( { |x| Ppar([x, x]) }, Pbind.new);
g.( { |x| Ppar([x, x]) }, Pseq([(zz: 300), (zz: 500)], 1));
g.( { |x| Ppar([x, x], inf) }, Pseq([(zz: 300), (zz: 500)], 1));
g.( { |x| Pfin(3, x) }, Pseq([(zz: 300), (zz: 500)], 1));
g.( { |x| Pfin(3, x) }, Pseq([(zz: 300), (zz: 500)], 4));



// testing Patternproxy / Pdefn
Pdefn.clear;
Pdefn(\x, Pseq([1, 2, 3]));
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, nil];
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, nil];
Pdefn(\x, Pseq([1, 2, 3], inf));
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, 1];
Pdefn(\x, Pseq([1, 2, 3]));
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, nil];

Pdefn.clear;
Pdefn(\x, 7);
Pdefn(\x).asStream.nextN(2) == [7, 7];
Pdefn(\x).asStream.nextN(2) == [7, 7];
Pseq([Pdefn(\x), 8]).asStream.nextN(2) == [7, 8];
Pnsym(Pseq([\x, \y, \x])).asStream.nextN(4) == [7, Pdefn.default, 7, nil];
Pdefn(\zz).asStream.nextN(2) == Pdefn.default.dup(2);

Pdefn(\x, 7);
Pdefn(\x).asStream.nextN(2) == 7.dup(2);
Pdefn(\x, Pseq([1, 2, 3], inf));
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, 1];
Pdefn(\x, 8);
Pdefn(\x).asStream.nextN(2) == 8.dup(2);
Pnsym(Pseq([\x, \y, \x])).asStream.nextN(4) == [8, Pdefn.default, 8, nil];
Pdefn(\y, Pseq([1, 2, 3], 1));
Pnsym(Pseq([\x, \y, \x])).asStream.nextN(5) == [8, 1, 2, 3, 8];

Pdefn(\y, -1);
Pnsym(Pseq([\x, \y], 2)).asStream.nextN(5) == [8, -1, 8, -1, nil];
Pdefn(\y, 1);
Pnsym(Pseq([\x, \y], 2)).asStream.nextN(5) == [8, 1, 8, 1, nil];

Pdefn.clear;
Pdefn(\x, 7);
Pdefn(\y, Pseq([10, 20], inf));
Pnsym1(Pseq([\x, \y, \x, \y], inf)).asStream.nextN(7) == [7, 10, 7, 20, 7, 10, 7];
Pnsym1(Pseq([\x, \y, \x, \y], 1)).asStream.nextN(7) == [7, 10, 7, 20, nil, nil, nil];

Pdefn.clear;
Pdefn(\x, Set[1, 2, 3]);
Pdefn(\x).asStream.nextN(4) == Set[1, 2, 3].dup(4);
Pdefn(\x, { Pseq([1, 2, 3]) }); // functions expand
Pdefn(\x).asStream.nextN(4) == [1, 2, 3, nil];
Pdefn(\x).set(\first, 7);
Pdefn(\x, { |envir| Pseq([envir[\first], 2, 3]) }); // functions expand and take envir as arg
Pdefn(\x).asStream.nextN(4) == [7, 2, 3, nil];


// testing conditions: FAILS
Pdefn.clear;
Pdefn(\x, Pseq([1, 2, 3], inf));
x = Pdefn(\x).asStream;
x.next;
z = false;"";
Pdefn(\x).condition = { z };
Pdefn(\x, Pseq([10, 20, 30]));
x.nextN(6).postln == [2, 3, 1, 2, 3, 1];
z = true;
x.nextN(6).postln == [10, 20, 30, nil, nil, nil];

Pdefn.clear;
Pdefn(\x, Pseq([1, 2, 3]));
x = Pdefn(\x).asStream;
x.next;
Pdefn(\x).count(2); // update only every 2 items
Pdefn(\x, Pseq([10, 20, 30]));
x.nextN(6).postln == [2, 10, 20, 30, nil, nil];



// testing EventPatternProxy / Pdef

// testing _single events_ as sources, embedInStream vs. asStream

Pdef.clear;
// both play only once
(freq: 700).asStream.play;
Pdef(\x, (freq: 700)).play;

// "automatic" tests
Pdef.clear;
Pdef(\x).player == nil;

Pdef(\x, (freq: 700));
Pevent(Pdef(\x)).asStream.nextN(2) == [ ( 'freq': 700 ), ( 'freq': 700 ) ];
Pevent(Pdef(\x)).asStream.nextN(2) == [ ( 'freq': 700 ), ( 'freq': 700 ) ];
Pdef(\x, (freq: 800));
Pevent(Pdef(\x)).asStream.nextN(2) == [ ( 'freq': 800 ), ( 'freq': 800 ) ];

Pdef(\y, (freq: 100));
Pevent(Psym(Pseq([\x, \y]))).asStream.nextN(3) == [ ( 'freq': 800 ), ( 'freq': 100 ), nil ];
Pevent(Psym(Pseq([\x, \y]))).asStream.nextN(3) == [ ( 'freq': 800 ), ( 'freq': 100 ), nil ];
Pdef(\y, (freq: 200));
Pevent(Psym(Pseq([\x, \y]))).asStream.nextN(3) == [ ( 'freq': 800 ), ( 'freq': 200 ), nil ];
Pdef(\y, Pn((freq: 200)));
Pevent(Psym(Pseq([\x, \y]))).asStream.nextN(3) == [ ( 'freq': 800 ), ( 'freq': 200 ),  ( 'freq': 200 ) ];
Pdef(\y, (freq: 200));
Pevent(Psym(Pseq([\x, \y]))).asStream.nextN(3) == [ ( 'freq': 800 ), ( 'freq': 200 ), nil ];

// endless:
Pdef.clear;
Pdef.defaultValue == Event.silent;
Pdef(\x, (freq: 800));
x = Pevent(Pdef(\x).endless).asStream;
a = Pdef.defaultValue;
x.nextN(3) == [(freq: 800), a, a];
Pdef(\x, (freq: 900).loop);
x.nextN(3).collect(_[\freq]) == [ 900, 900, 900 ];

Pdefn.clear;
Pdefn.defaultValue == 1;
Pdefn(\x, 9);
x = Pdefn(\x).endless.asStream;
a = Pdefn.defaultValue;
x.nextN(3) == [9, a, a];
Pdefn(\x, Pseq([1, 2, 3]));
x.nextN(3) == [1, 2, 3];



// Pbindef
Pdef.clear;
Pbindef(\p, \z, 7);
Pevent(Pbindef(\p)).asStream.nextN(3) == [ ( 'z': 7 ), ( 'z': 7 ), ( 'z': 7 ) ];
Pbindef(\p, \z, Pn(7, 2));
Pevent(Pbindef(\p)).asStream.nextN(3) == [ ( 'z': 7 ), ( 'z': 7 ), nil ];
Pbindef(\p, \z, 7);
Pevent(Pbindef(\p)).asStream.nextN(3) == [ ( 'z': 7 ), ( 'z': 7 ), ( 'z': 7 ) ];
Pbindef(\p, \z, nil);
Pevent(Pbindef(\p)).asStream.nextN(3) == ().dup(3);

Pdefn.clear;
Pdefn(\x, 9);
Pdefn(\y, Pseq([1, 2, 3]));
Pdefn(\z, Pdefn(\x) + Pdefn(\y));
Pdefn(\z).asStream.nextN(4) == [ 10, 11, 12, nil ];
Pdefn(\x, -1);
Pdefn(\z).asStream.nextN(4) == [ 0, 1, 2, nil ];

// testing quant
Pdefn.clear;
Pdef.clear;
Pdefn(\m, 12);
Pdefn(\m).quant = 2;
Pbindef(\x,\midinote, Pseq([60,62,64,65,67,65,64,62], inf) + Pdefn(\m));

Pevent(Pbindef(\x)).asStream.nextN(3) == [ ( 'midinote': 72 ), ( 'midinote': 74 ), ( 'midinote': 76 ) ];
Pbindef(\x,\dur, 0.5);
Pevent(Pbindef(\x)).asStream.nextN(3) == [ ( 'dur': 0.5, 'midinote': 72 ), ( 'dur': 0.5, 'midinote': 74 ), ( 'dur': 0.5, 'midinote': 76 ) ];
Pdefn(\m, 11);
Pevent(Pbindef(\x)).asStream.nextN(3) == [ ( 'dur': 0.5, 'midinote': 71 ), ( 'dur': 0.5, 'midinote': 73 ), ( 'dur': 0.5, 'midinote': 75 ) ];

Pbindef(\x,\dur, 0.25).play(quant:4);
Pdefn(\m, -4); // change takes effect on 4-beat boundary...




// testing fades
Pdef.clear;
Pdef(\x).fadeTime = 10;
Pdef(\x, Pbind(\degree, Pseq((0..5), inf), \dur, 0.1, \legato, 0.2));
Pdef(\x).play;
Pdef(\x, Pbind(\degree, Pseq((0..5) + 12, inf), \dur, 0.1, \legato, 0.2));
Pdef(\x, Pbind(\degree, Pseq((0..5) + 6, inf), \dur, 0.1, \legato, 0.2));


// cleanup tests
(
SynthDef(\echo, { arg out=0, maxdtime=0.2, dtime=0.2, decay=2, gate=1;
    var env, in;
    env = Linen.kr(gate, 0.05, 1, 0.1, 2);
    in = In.ar(out, 2);
    XOut.ar(out, env, CombL.ar(in * env, maxdtime, dtime, decay, 1, in));
}, [\ir, \ir, 0.1, 0.1, 0]).add;

SynthDef(\distort, { arg out=0, pregain=40, amp=0.2, gate=1;
    var env;
    env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	XOut.ar(out, env, (In.ar(out, 2) * pregain).distort + Dust2.ar([4, 10, 100], [0.4, 0.01, 0.01]).sum * amp);
}, [\ir, 0.1, 0.1, 0]).add;

SynthDef(\wah, { arg out=0, gate=1, rate = 0.3, max = 8000, rq = 0.1;
    var env, in;
    env = Linen.kr(gate, 0.05, 1, 0.4, 2);
    in = In.ar(out, 2);
    XOut.ar(out, env, RLPF.ar(in, LinExp.kr(LFNoise1.kr(rate), -1, 1, 200, max), rq).softclip * 0.8);
}, [\ir, 0]).add;
)

(
Pdef.clear;

Pdef(\p, Pbind(\degree, Prand((0..7),12), \dur, 0.3 / Pseq([1, 1, 1, 2, 3, 1, 1, 3, 1, 1], inf), \legato, 0.2));

Pdef(\q, Pfxb(Pdef(\p), \echo, \dtime, 0.2, \decay, 3));

Pdef(\r, Pfxb(Pdef(\q), \distort, \pregain, 20, \amp, 0.25));

Pdef(\o, Pfxb(Pdef(\r), \wah, \rq, 0.2));

Pdef(\out, Pseq([Pdef(\p), Pdef(\q), Pdef(\r), Pdef(\o)], inf)).play;
)

Pdef(\o, Pfxb(Pdef(\r), \wah, \rate, 16, \max, 700));
Pdef(\p, Pbind(\degree, Pseq((0..5)*3, 1), \dur, 0.1, \legato, 0.2));
Pdef(\out, Pseq([Pdef(\o)], 2)).play;

Pdef(\p, Pbind(\degree, Pshuf([0, 3, 5, 11, 3, 2], 2), \dur, 0.1, \legato, 0.2));
Pdef(\out, Psym(Pseq("pqropqooqprppo", inf))).play;

Pdef(\o, Pfxb(Pdef(\r), \wah, \rate, 56, \max, 7000, \rq, 1.0.rand));
Pdef(\r, Pfxb(Pdef(\q), \distort, \pregain, 120, \amp, 0.1));
Pdef(\out).fadeTime = 10;

Pdef(\out, Psym(Pseq("ppprp", inf)));
Pdef(\out, Psym(Pshuf("ppprqrp", inf)));
Pdef(\out, Psym(Pshuf("o", inf)));
Pdef(\out, Pseq([Pdef(\o)], 2)).play;

Pdef(\out, Psym(Pseq("ppprp", inf)) <> (mtranspose: 3));
Pdef(\out, Psym(Pseq("popoprp", inf)) <> (mtranspose: [0, 3]));
Pdef(\out, Pseq([Pdef(\o)], 2)).play;
Pdef(\out).stop;



// in NodeProxy:
s.plotTree;
Ndef(\x, Pdef(\out)).play;
Pdef(\out, Pseq([Pdef(\o)], 2));
Ndef(\x).set(\mtranspose, 3);
Ndef(\x).clear;

// was a problem for 2 years (issue  #107) should be fixed now
Pdef(\foo, Pmono(\default, \dur, 1/8, \degree, Pseq([1,2,3,4,5],32))).play;
Pdef(\foo).quant == 1;
fork { 15.do { Pdef(\foo, Pmono(\default, \dur, 1/8, \degree, Pseq([5,4,3,2,1],inf))); 0.2.wait;}};




Pdef(\foo, Pmono(\default, \dur, 1/8, \degree, Pseq([1,2,3,4,5],32))).play;
Pdef(\foo).quant == 1;
fork { 15.do { Pdef(\foo, Pmono(\default, \dur, 1/8, \degree, Pseq([5,4,3,2,1],inf))); 0.2.wait;}};




Tdef(\x, { loop { 1.wait; "wesew".postln } }).play;
Tdef(\x, { loop { 0.1.wait; "wesew".postln } }).play;

// testing copy

// pattern proxy
a = PatternProxy({ |envir| Pseq([1, 2, 3], inf) + envir[\x] }).set(\x, 0);
b = a.copy;
f = { |x| x.asStream.nextN(8).postln };

f.(a) == f.(b);
a.set(\x, 16);
f.(a) != f.(b);

a = PatternProxy({ |envir| Pseq([1, 2, 3], inf) + envir[\x] }).set(\x, 0);
b = a.copy;
f.(a) == f.(b);
a.source = Pseq([1, 2, 3, 4], inf);
f.(a) != f.(b);
b.source = Pseq([1, 2, 3, 4], inf);
f.(a) == f.(b);

Pdefn(\a, { |envir| Pseq([1, 2, 3], inf) + envir[\x] }).set(\x, 0);
Pdefn(\a).copy(\b);
f = { |x| Pdefn(x).asStream.nextN(8).postln };
f.(\a) == f.(\b);
Pdefn(\a).set(\x, 16);
f.(\a) != f.(\b);

// event pattern proxy
a = EventPatternProxy({ Pbind(\note, Pseq([1, 2, 3], inf)) }).set(\x, 0);
b = a.copy;
f = { |x| Pevent(x).asStream.nextN(8).postln };
f.(a) == f.(b);
a.set(\x, 16);
f.(a) != f.(b);

b = a.copy;
f.(a) == f.(b);
a.source = Pbind(\note, 7);
f.(a) != f.(b);
b.source = Pbind(\note, 7);
f.(a) == f.(b);




::
]


