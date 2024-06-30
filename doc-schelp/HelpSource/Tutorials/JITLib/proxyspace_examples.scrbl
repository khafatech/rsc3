#lang scribble/manual
@(require (for-label racket))

@title{ProxySpace examples}
 Some ProxySpace examples@section{categories}
  Libraries>JITLib>Tutorials, Tutorials>JITLib
@section{related}
  Overviews/JITLib, Classes/ProxySpace

@section{section}
 main examples

@section{subsection}
 preparing the environment


@racketblock[
(
s.boot;
p = ProxySpace.push(s);
)
::

]
@section{subsection}
 playing and monitoring


@racketblock[
// play some output to the hardware busses, this could be any audio rate key.
~out.play;

~out = { SinOsc.ar([400, 408] * 0.8, 0, 0.2) };


// replacing the node. the crossfade envelope is created internally.
~out = { SinOsc.ar([443, 600 - Rand(0,200)], 0, 0.2) };
~out = { Resonz.ar(Saw.ar(40 + [0,0.2], 1), [1200, 1600], 0.1) + SinOsc.ar(60 * [1,1.1],0,0.2) };
~out = { Pan2.ar(PinkNoise.ar(0.1), LFClipNoise.kr(2)) };
::

]
@section{subsection}
 setting the node controls


@racketblock[
~out = { arg rate = 2; Pan2.ar(PinkNoise.ar(0.1), LFClipNoise.kr(rate)) };
~out.set(\rate, 30);
~out = { arg rate = 2; Pan2.ar(Dust.ar(2000, 0.2), LFClipNoise.kr(rate)) };
~out.set(\rate, 2);
::

]
@section{subsection}
 referencing between proxies


@racketblock[
~lfo = { LFNoise2.kr(30, 300, 500) };
~out = { SinOsc.ar(~lfo.kr, 0, 0.15) };
~out = { SinOsc.ar(~lfo.kr * [1, 1.2], 0, 0.1) * Pulse.ar(~lfo.kr * [0.1, 0.125], 0.5) };
~lfo = { LFNoise1.kr(30, 40) + SinOsc.kr(0.1, 0, 200, 500) };
~out = { SinOsc.ar(~lfo.kr * [1, 1.2], 0, 0.1) };
~lfo = 410;
::

]
@section{subsection}
 math


@racketblock[
// unary operators
~lfo2 = { SinOsc.kr(0.5, 0, 600, 100) };
~lfo = ~lfo2.abs;
~lfo2 = { SinOsc.kr(1.3, 0, 600, 100) };

// binary operators
~lfo3 = { LFTri.kr(0.5, 0, 80, 300) };
~lfo = ~lfo2 + ~lfo3;
~lfo = ~lfo3;
~lfo = (~lfo3 / 50).sin * 200 + 500 * { LFTri.kr(~lfo.kr * 0.0015, 0, 0.1 * ~lfo3.kr / 90, 1) };
~lfo3 = { Mix(~lfo2.kr * [1, 1.2]) };

currentEnvironment.free; // free all node proxies
~out.stop; // free the playback synth.
::

]
@section{subsection}
 waking up a network of proxies


@racketblock[
// hit cmd-. to stop all nodes
// start again
~out.play;
::

]
@section{subsection}
 feeding back

(one buffer size delay)


@racketblock[
~out = { SinOsc.ar([220, 330], ~out.ar(2).reverse * LFNoise2.kr(0.5, 4pi), 0.4) };

// there is no immediacy: hear the buffer size cycle
~out = { Impulse.ar(1 ! 2) + (~out.ar(2) * 0.99) };



// SuperCollider 'differential equations'

~out = { SinOsc.ar(Slope.ar(~out.ar) * MouseX.kr(1000, 18000, 1)) * 0.1 + SinOsc.ar(100, 0, 0.1) };

(
~out = { var z, zz;
	z = Slope.ar(~out.ar);
	zz = Slope.ar(z);
	SinOsc.ar(Rand(300,410), z) *
	SinOsc.ar(zz * 410)
	* 0.1 + Decay2.ar(Pan2.ar(Dust.ar(600), MouseX.kr(-1,1)), 0.01, 0.05);
}
)
::

]
@section{subsection}
 multiple control


@racketblock[
(
~out = { arg freqOffest;
	var ctl;
	ctl = Control.names(\array).kr(Array.rand(8, 400, 1000));
	Pan2.ar(Mix(SinOsc.ar(ctl + freqOffest, 0, 0.1 / 8)), LFNoise0.kr(2))
};
)

~out.setn(\array, Array.exprand(8, 400, 2000));
~out.set(\freqOffest, rrand(300,200));
~out.map(\freqOffest, ~lfo);

// a simpler short form for this is:
(
~out = { arg freqOffest=0, array = #[ 997, 777, 506, 553, 731, 891, 925, 580 ];
	Pan2.ar(Mix(SinOsc.ar(array + freqOffest, 0, 0.1 / 8)), LFNoise0.kr(2))
};
)
::

]
@section{subsection}
 mixing


@racketblock[
~out1 = { SinOsc.ar(600, 0, 0.1) };
~out2 = { SinOsc.ar(500, 0, 0.1) };
~out3 = { SinOsc.ar(400, 0, 0.1) };
~out = ~out2 + ~out1 + ~out3;

~out = ~out1 + ~out2;
~out = ~out1;

// another way is:
~out = { SinOsc.ar(600, 0, 0.1) };
~out.add({ SinOsc.ar(500, 0, 0.1) });
~out.add({ SinOsc.ar(400, 0, 0.1) });

// or with direct access:
~out[1] = { SinOsc.ar(500 * 1.2, 0, 0.1) };
~out[2] = { SinOsc.ar(400 * 1.2, 0, 0.1) };
::

]
@section{subsection}
 restoring / erasing


@racketblock[
~out.free; // this frees the group, not the play synth x
~out.send; // resends all synths
~out.free;
~out.send(nil, 1); // this sends at index 1 only
~out.send;

// removing:
~out.removeLast;
~out.removeAt(0);

// cleaning up, freeing the bus:
~out.clear; // this neutralizes the proxy, and frees its bus
::

for more on the proxy slots see: link::Tutorials/JITLib/jitlib_basic_concepts_03::

]
@section{subsection}
 garbage collecting


@racketblock[
// often there are proxies playing that are not used anymore - this is good,
// because they might be used again at any time.
// this shows how to free unused proxies, such as ~out1, ~out2.

~out.play;
~out = { Pan2.ar(SinOsc.ar(~lfo.kr, 0, 0.2), sin(~lfo.kr / 10)) }; // ~lfo is kept, as its parents.
~lfo = { LFNoise2.kr(3, 160, 400) };

p.keysValuesDo { arg key, proxy; [key, proxy.isPlaying].postln };
p.reduce; // all monitoring proxies (in this case ~out) are kept running.
// equivalent: p.reduce(to: [~out]);
p.keysValuesDo { arg key, proxy; [key, proxy.isPlaying].postln };

// to remove everything else:
p.postln;
p.clean; // all monitoring proxies (in this case ~out) are kept.
p.postln;

// after ~out is stopped, it is removed, too:
~out.stop; // stop monitor
p.clean;
p.postln; // empty space.
::

]
@section{subsection}
 execution order


@racketblock[
// you can .play .kr or .ar also a name that is not yet used.
// the rate is guessed as far as possible. on this topic see also: [the_lazy_proxy]

~myOut.play; // play some key (audio rate is assumed)

// the rate is determined from the first access:
// like this ~lfo becomes control rate

~myOut = { SinOsc.ar(~freq.kr * 2, 0, 0.1) };
~freq = 900;
~freq = { SinOsc.kr(115, 0, 70, 220) }

~myOut = { SinOsc.ar(~otherFreq.ar * 2, 0, 0.1) };
~otherFreq = { SinOsc.ar(115, 0, 70, 220) };

currentEnvironment.clear; // clear every proxy in this environment and remove them. (same: p.clear)
::

]
@section{subsection}
 setting the xfade time


@racketblock[
~out.play;

~out.fadeTime = 4;
~out = { SinOsc.ar(Rand(800, 300.0) * [1,1.1], 0, 0.1) };
~out = { SinOsc.ar(Rand(800, 300.0) * [1,1.1], 0, 0.1) };
~out.fadeTime = 0.01;
~out = { SinOsc.ar(Rand(800, 300.0) * [1,1.1], 0, 0.1) };
~out = { SinOsc.ar(Rand(800, 300.0) * [1,1.1], 0, 0.1) };

~out.free(3);	// release the synths and the group with a given fadeTime without changing proxy time
~out.stop;	// stop monitor
::

]
@section{subsection}
 setting and mapping arguments


@racketblock[
~out.play;

~out = { arg freq=500, ffreq=120; SinOsc.ar(freq*[1,1.1], SinOsc.ar(ffreq, 0, pi), 0.2) };
~out.set(\freq, 400 + 100.rand2);
~out.set(\freq, 400 + 100.rand2);
~out.set(\ffreq, 30 + 20.rand2);
~out.unset(\freq, \ffreq);	// remove the setting
~out.set(\ffreq, 30 + 10.rand2, \freq, 500 + 200.rand2);


// argument settings and mappings are applied to every new function
~out = { arg freq=100, ffreq=20; SinOsc.ar(freq, SinOsc.ar(SinOsc.ar(ffreq)*ffreq, 0, pi), 0.2) };

// mapping to other proxies
~lfo = { SinOsc.kr(0.3, 0, 80, 100) };
~out.map(\ffreq, ~lfo);

~out = { arg freq=300, ffreq=20; Pulse.ar(freq * [1, 1.1] + SinOsc.ar(ffreq, 0, freq), 0.3, 0.1) };
~out = { arg freq=300, ffreq=20; BPF.ar(LFSaw.ar(ffreq * [1, 1.1], 0, 1), freq, 0.2) };

~lfo = { FSinOsc.kr(0.3, 0, 30, 200) + FSinOsc.kr(10, 0, 10) };
~out = { arg freq=300, ffreq=20; SinOsc.ar(freq*[1,1.1], SinOsc.ar(ffreq, 0, pi), 0.1) };


// crossfaded setting and mapping: fadeTime is used
~out.fadeTime = 2;
~out.xset(\freq, 9000);
~out.xset(\freq, rrand(400, 700));

~lfo = { FSinOsc.kr(0.1, 0, 30, 100) };
~lfo2 = { LFClipNoise.kr(3, 100, 200) };
~lfo3 = StreamKrDur(Pseq([Prand([530, 600],1), 700, 400, 800, 500].scramble, inf) / 3, 0.2);

~out.xmap(\ffreq, ~lfo2);
~out.xmap(\ffreq, ~lfo);
~out.xmap(\ffreq, ~lfo3);

// argument rates: just like a synthdef has input 'rates' (like \ir or \tr), a nodeproxy control
// can be given a rate. this rate is used for each function passed into the proxy.

// trigger inputs
~out = { arg trig, dt=1; Decay2.kr(trig, 0.01, dt) * Mix(SinOsc.ar(7000 * [1.2, 1.3, 0.2])) }
~out.setRates(\trig, \tr);

// set the group, so the node proxy does not store the new value
~out.group.set(\trig, 0.1, \dt, 0.1);
~out.group.set(\trig, 0.4, \dt, 0.31);
~out.group.set(\trig, 0.13, \dt, 2);

// lagging controls:
~out.lag(\xfreq, 1); // equivalent to ~out.setRates(\xfreq, 1);
(
~out = { arg trig, dt=1, xfreq=700;
	Decay2.kr(trig, 0.01, dt) * Mix(SinOsc.ar(xfreq * [1.2, 1.3, 0.2]))
};
)
~out.group.set(\trig, 0.1, \dt, 1, \xfreq, rrand(2000,9000));
~out.group.set(\trig, 0.1, \dt, 0.5, \xfreq, rrand(2000,9000));
~out.group.set(\trig, 0.1, \dt, 1, \xfreq, rrand(2000,9000));

// changing the lag, the synth is reconstructed with the new lag:

~out.lag(\xfreq, 0.01);
~out.group.set(\trig, 0.1, \dt, 1, \xfreq, rrand(2000,9000));
~out.group.set(\trig, 0.1, \dt, 1, \xfreq, rrand(2000,9000));
~out.group.set(\trig, 0.1, \dt, 1, \xfreq, rrand(2000,9000));

// removing the trig rate:
~out.setRates(\trig, nil);

// note that the same works with the i_ and the t_ arguments, just as it does in SynthDef
::

]
@section{section}
 other possible inputs

@section{subsection}
 using a synthdef as input

for a more systematic overview see: link::Tutorials/JITLib/jitlib_fading::


@racketblock[
// you have the responsibility for the right number of channels and output rate
// you have to supply an 'out' argument so it can be mapped to the right channel.

~out.play;
~out = SynthDef("w", { arg out=0; Out.ar(out,SinOsc.ar([Rand(430, 600), 600], 0, 0.2)) });
~out = SynthDef("w", { arg out=0; Out.ar(out,SinOsc.ar([Rand(430, 600), 500], 0, 0.2)) });


// if you supply a gate it fades in and out. evaluate this several times
(
~out = SynthDef("w", { arg out=0, gate=1.0;
	Out.ar(out,
		SinOsc.ar([Rand(430, 800), Rand(430, 800)], 0, 0.2)
			* EnvGen.kr(Env.asr(1,1,1), gate, doneAction: Done.freeSelf)
	)
	});
)

// once the SynthDef is sent, it can be assigned by name.
// using this method, a gate argument should be
// provided that releases the synth. (doneAction: Done.freeSelf)
// this is very efficient, as the def is on the server already.

// if the synth def is in the synthdesc lib (.add) its gate is detected.

(
SynthDef("staub", { arg out, gate=1;
	Out.ar(out,
		Ringz.ar(Dust.ar(15), Rand(1, 3) * 3000*[1,1], 0.001)
			* EnvGen.kr(Env.asr, gate, doneAction: Done.freeSelf)
	)
}).add;
)

~out = \staub;



// if you supply an envelope that frees itself, no bundle is sent to free it
(
~out = SynthDef("w", { arg out, lfo, f0=430;
	Out.ar(out,
		SinOsc.ar([Rand(f0, 800), Rand(f0, 800)]+lfo, 0, 0.2)
			* EnvGen.kr(Env.perc(0.01, 0.03), doneAction: Done.freeSelf)
	)
	});
)

~out.spawn;
~out.spawn([\f0, 5000]);
fork { 5.do { ~out.spawn([\f0, 5000 + 1000.0.rand]); 0.1.wait; } }


// when the synth description in the SynthDescLib is found for the symbol,
// the proxy can determine whether to release or to free the synth.
// so if there is no 'gate' arg provided and the def has a desc, the synth is
// freed and not released.

(
SynthDef("staub", { arg out;
	Out.ar(out, Ringz.ar(WhiteNoise.ar(0.01), 1000 * [1,1], 0.001))
}).add; // store the synth def so it is added to the SynthDescLib
)


~out = \staub;
~out = \staub;	// watching the synth count shows that the old synth is freed.
~out = 0;	// now out plays continuous stream of zero.
~out = nil;	// removes object and stops it.
::

]
@section{subsection}
 using patterns


@racketblock[
// example

(
SynthDef(\who, { arg amp=0.1, freq=440, detune=0, gate=1, out=0, ffreq=800;
	var env;
	env = Env.asr(0.01, amp, 0.5);
	Out.ar(out, Pan2.ar(
		Formant.ar(freq + detune, ffreq, 30, EnvGen.kr(env, gate, doneAction: Done.freeSelf)), Rand(-1.0, 1.0))
	)
}).add;

)

~out.play;


~out = Pbind(\instrument, \who, \freq, [600, 601], \ffreq, 800, \legato, 0.02);


// embed a control node proxy into an event pattern:
// this does not work for indirect assignment as \degree, \midinote, etc.,
// because there is calculations in the event! if needed, these can be done in the SynthDef.

~lfo = { SinOsc.kr(2, 0, 400, 700) };
~out = Pbind(\instrument, \who, \freq, 500, \ffreq, ~lfo, \legato, 0.02);

~lfo = { SinOsc.kr(SinOsc.kr(0.2, Rand(0,pi), 10, 10), 0, 400, 700) };

~lfo = { LFNoise1.kr(5, 1300, 1500) };
~lfo = { MouseX.kr(100, 5500, 1) };

(
~out = Pbind(
		\instrument, \who,
		\freq, Pseq([500, 380, 300],inf),
		\legato, 0.1,
		\ffreq, Pseq([~lfo, 100, ~lfo, 100, 300, 550], inf), // use it in a pattern
		\dur, Pseq([1, 0.5, 0.75, 0.125]*0.4, inf)
	);
)

// note that when you use a proxy within a non-event pattern it gets embedded as an object,
// so this functionality is still standard

// works only with control rate proxies. multichannel control rate proxies cause
// multichannel expansion of the events:

~lfoStereo = { LFNoise1.kr([1, 1], 1300, 1500) }; // 2 channel control rate proxy
~out = Pbind(\instrument, \who, \ffreq, ~lfoStereo, \legato, 0.02).trace;
~lfoStereo = { [MouseX.kr(100, 15500, 1), SinOsc.kr(SinOsc.kr(0.2, 0, 10, 10), 0, 400, 700)] }

// btw: setting the clock will cause the pattern to sync:
p.clock = TempoClock.default;
p.clock.tempo = 2.0;
p.clock.tempo = 1.0


// patterns also crossfade, if an \amp arg is defined in the synthdef:
// (evaluate a couple of times)
~out.fadeTime = 3.0;
(
~out = Pbind(
		\instrument, \who,
		\freq, Pshuf([500, 380, 200, 510, 390, 300, 300],inf) * rrand(1.0, 2.0),
		\legato, 0.1,
		\ffreq, Pshuf([~lfo, 100, ~lfo, 100, 300, 550], inf),
		\dur, 0.125 * [1, 2, 3, 2/3].choose
	);
)
::

]
@section{subsection}
 using instruments and players

@section{note}
 
for the following to work you will need to have the strong::cruciallib:: quark installed.
::


@racketblock[
// pause and resume do not work yet.


// store an instrument
(
Instr(\test,
	{ arg dens=520, ffreq=7000; Ringz.ar(Dust.ar(dens, [1,1] * 0.1), ffreq, 0.02) }
	);
)

~out = Patch(\test, [10, rrand(5000, 8000)]);
~out.fadeTime = 3;


(
~out = InstrSpawner({ arg freq=1900,env,pan;
	Pan2.ar(SinOsc.ar(freq, 0.5pi, 0.3) * EnvGen.kr(env, doneAction: Done.freeSelf), pan)
},[
	Prand([1500, 700, 800, 3000] + 170.rand2, inf),
	Env.perc(0.002,0.01),
	Prand([-1,1],inf)
],0.125)
)

~out.clear;



// does not work (yet).
//~out.set(\dens, 120);
//~out.xset(\dens, 1030);
//~out.unmap(\ffreq);
//~out.set(\ffreq, 500);
::

]
@section{section}
 client side routines

@section{subsection}
 spawning


@racketblock[
~out.play;
~out.awake = false; // allow sound object assignment without immediate sending

// putting an synthdef into the node proxy without playing it right away
// the synthdef has an envelope that frees by itself.
(
~out = SynthDef("a", { arg out=0, freq=800, pmf=1.0, pan;
		var env, u;
		env = EnvGen.kr(Env.perc(0.001, 0.04, 0.4),doneAction: Done.freeSelf); // envelope
		u = SinOsc.ar(freq * Rand(0.9, 1.1), SinOsc.ar(pmf, 0, pi), env);
		Out.ar(out, Pan2.ar(u, pan))
	})
);


// create a task to repeatedly send grains
(
t = Task.new({
	loop({
		// starts a synth with the current synthdef at index 0
		~out.spawn([\pmf, [1, 20, 300].choose, \pan, [0, -1, 1].choose]);
		[0.1, 0.01, 0.25].choose.wait;
	})
});
)

t.start;
t.stop;
t.start;

// note: if you want to avoid using interpreter variables (single letter, like "t"),
// you can use Tdef for this. (see Tdef.help)

// set some argument
~out.set(\freq, 300);
~out.set(\freq, 600);
~out.map(\freq, ~lfo);
~lfo = { SinOsc.kr(0.1, 0, 3000, 4000) };
~lfo = { SinOsc.kr(0.1, 0, 600, 700) };
~lfo.add({ Trig.kr(Dust.kr(1), 0.1) * 3000 });
~lfo = 300;

// change the definition while going along
(
~out = SynthDef("a", { arg out, freq=800;
		var env;
		env = EnvGen.kr(Env.perc(0.01, 0.1, 0.3),doneAction: Done.freeSelf);
		Out.ar(out, Pulse.ar(freq * Rand([0.9,0.9], 1.1), 0.5, env) )
	});
)


t.stop;
~out.awake = true; // don't forget this
// free all synths in this current ProxySpace
currentEnvironment.clear;
::

]
@section{subsection}
 granular synthesis: efficient code

see also link::Tutorials/JITLib/jitlib_efficiency::


@racketblock[
~out.play;

(
SynthDef("grain", { arg i_out = 0, pan;
	var env;
	env = EnvGen.kr(Env.perc(0.001, 0.003, 0.2),doneAction: Done.freeSelf);
	Out.ar(i_out, Pan2.ar(FSinOsc.ar(Rand(1000,10000)), pan) * env)
}).send(s);
)

// a target for the grains
~someInput.ar(2); // initialize to 2 channels audio
~out = ~someInput;

(
t = Task({
	loop({
		s.sendMsg("/s_new","grain",-1,0,0,
			\i_out, ~someInput.index, // returns the bus index of the proxy
			\pan, [1, 1, -1].choose * 0.2
		);
		[0.01, 0.02].choose.wait;
	})
});
)
t.play;

// different filters;

~out.fadeTime = 1.0;

~out = { BPF.ar(~someInput.ar, MouseX.kr(100, 18000, 1), 0.1) };

~out = { CombL.ar(~someInput.ar * (LFNoise0.ar(2) > 0), 0.2, 0.2, MouseX.kr(0.1, 5, 1)) };

~out = { RLPF.ar(~someInput.ar, LFNoise1.kr(3, 1000, 1040), 0.05) };


t.stop;


// end

~out.stop;
currentEnvironment.clear;
ProxySpace.pop; // restore original environment
::

]
@section{subsection}
 using multiple proxyspaces

can be done while the server is not running: with p.wakeUp or p.play

the environment can be played back.


@racketblock[
// quit server:

s.quit;


// create two proxyspaces without a running server
(
p = ProxySpace(s);
q = ProxySpace(s);

p.use({
	~out = { Resonz.ar(~in.ar, ~freq.kr, 0.01) };
	~in = { WhiteNoise.ar(0.5) };
	~freq = { LFNoise2.kr(1, 1000, 2000) };
});

q.use({
	~in = { Dust.ar(20, 0.1) };
	~out = { Resonz.ar(~in.ar * 450, ~freq.kr, 0.005) };
	~freq = { LFNoise2.kr(1, 400, 2000) };
});
)

(
// wait for the booted server
s.waitForBoot({
	// play the proxy at \out
	p.play(\out);
	q.play; // out is the default output
});
)
::

]
@section{subsection}
 external access


@racketblock[
q[\in][1] = { Impulse.ar(2, 0, 0.5) }; // adding a synth at index 1

// equivalent to
q.at(\in).put(1, { Impulse.ar(7, 0, 0.5) });
::

]
@section{subsection}
 connecting two spaces

(must be on one server)


@racketblock[
(
q.use({
	~freq = 100 + p[\freq] / 2;
})
)
::

]
@section{subsection}
 recording output

(see also: link::Classes/RecNodeProxy::)


@racketblock[
r = p.record(\out, "proxySpace.aiff");

// start recording
r.unpause;

// pause recording
r.pause;

// stop recording
r.close;
::

]
@section{subsection}
 push/pop


@racketblock[
// make p the currentEnvironment
p.push;

~freq = 700;
~freq = 400;
~freq = { p.kr(\freq) + LFNoise1.kr(1, 200, 300) % 400 }; // feedback
~freq = 400;

p.pop; // restore environment


// make y the currentEnvironment
q.push;

~freq = 1000;
~in = { WhiteNoise.ar(0.01) };

q.pop; // restore environment

q.clear;
p.clear;
::

]
@section{section}
 some more topics

@section{subsection}
 nodeproxy with numbers as input


@racketblock[
p = ProxySpace.push(s.boot);


~out = { SinOsc.ar(~a.kr * Rand(1, 2), 0, 0.1) };
~out.play;

~a = 900;

// these add up:
~a[0] = 440;
~a[1] = 220;
~a[2] = 20;

~a.fadeTime = 2;

~a[0] = 300; // now there is a crossfade.
~a[1] = { SinOsc.kr(5, 0, 20) };
~a[2] = { SinOsc.kr(30, 0, 145) };



// internally a numerical input is approximately replaced by:
// (pseudocode)
SynthDef("name", { arg out, fadeTime;
	Out.kr(out,
		Control.kr(Array.fill(proxy.numChannels, { the number }))
			* EnvGate.new(fadeTime:fadeTime)
	)
});
::
]

