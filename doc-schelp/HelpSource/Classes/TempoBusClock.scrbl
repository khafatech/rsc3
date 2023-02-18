#lang scribble/manual
@(require (for-label racket))

@title{TempoBusClock}
 a clock that synchronizes its tempo with the server@section{categories}
  Libraries>JITLib>NodeProxy, Live Coding
@section{related}
  Classes/TempoClock

@section{ClassMethods}
 

@section{method}
 new
return a new instance.

@section{argument}
 control
can be anything that responds to the message 
@racketblock[set(key, val, ...):: e.g. a link::Classes/Synth:: or a link::Classes/NodeProxy::. The control key set is "tempo". otherwise TempoBusClock works just like a link::Classes/TempoClock::.
]
@section{argument}
 tempo
see link::Classes/TempoClock::
@section{argument}
 beats
see link::Classes/TempoClock::
@section{argument}
 seconds
see link::Classes/TempoClock::


@section{Examples}
 


@racketblock[
(
a = { |tempo=1| Ringz.ar(Impulse.ar(tempo), [501, 500], 1/tempo) }.play;
t = TempoBusClock(a);
Task { loop { "klink".postln; 1.wait } }.play(t);
);

t.tempo = 1.3;
t.tempo = 0.5;
t.tempo = 1.0;


// in ProxySpace, a TempoBusClock can be added together with a ~tempo NodeProxy:

p = ProxySpace.push(s);
p.makeTempoClock;
p.clock; // now the ProxySpace's clock is a TempoBusClock

~out.play;
~out = { Ringz.ar(Impulse.ar(~tempo.kr), [501, 500], 1/~tempo.kr) * 0.3 };
p.clock.tempo = 1.3;

// patterns and tasks are synchronized:

~out2.play;
~out2 = Pbind(\dur, 1, \note, Pwhite(0, 7, inf));

p.clock.tempo = 3;
p.clock.tempo = 1;
::
]


