#lang scribble/manual
@(require (for-label racket))

@title{the_lazy_proxy}
 the lazy proxy@section{categories}
  Libraries>JITLib>Tutorials
@section{related}
  Overviews/JITLib, Classes/NodeProxy, Classes/ProxySpace

The class link::Classes/NodeProxy:: (and link::Classes/BusPlug::) uses a lazy evaluation scheme to derive its appropriate rate and numChannels from the first meaningful input that is assigned to it. see link::Classes/NodeProxy:: and link::Classes/ProxySpace:: helpfiles for basic info. So as long as the source is not set, the proxy is strong::neutral:: :


@racketblock[
p = ProxySpace.push;
~x.isNeutral;
::

as soon as the first time the source is set, it derives its bus arguments from that input

]

@racketblock[
~x = { Array.fill(14, { SinOsc.kr(1.0.rand, 0, 100) }) }; //~x is now 14 channels control rate
~x;
::

in order to reset these properties, clear is used:

]

@racketblock[
~x.clear;
//note that no other proxy should be reading from ~x when this is done:
//for simplicity nodeproxy currently does not care for its children, only for its parents.
::

for a quick initialisation, also ]

@racketblock[defineBus:: can be used:

]

@racketblock[
~x.defineBus(\control, 5);
// or in another way:
~x.kr(5)
::

the properties are also set when some other proxy reads from it:

]

@racketblock[
~x = { LFPulse.kr * ~b.kr(7) }; //the first arg to kr / ar is the default number of channels
::

if no number of channels is passed in, the default value is used:

]

@racketblock[
~test.ar; // 2
~krtest.kr; // 1
::

the default can be set in the class NodeProxy:

]

@racketblock[
NodeProxy.defaultNumAudio = 3;
NodeProxy.defaultNumControl = 13;

~test3.ar; // 3
~krtest3.kr; // 13

// set them back:
NodeProxy.defaultNumAudio = 2;
NodeProxy.defaultNumControl = 1;
::

also if a proxy is used as a map source, control rate is assumed:

]

@racketblock[
~u;
~x.map(\zzz, ~u);
~u;
::

when unary or binary operations are performed, the highest rate / numChannels is used to initialize all uninitialized proxies:

]

@racketblock[
~x.clear;
~x.defineBus(\control, 5);
~x = ~e + ~f;

~x.clear; ~e.clear; ~f.clear;
~e.defineBus(\audio, 1);
~x = ~e + ~f.squared + ~r;
~x;

~x.clear; ~e.clear; ~f.clear;
~e.defineBus(\audio, 3);
~x = ~e;
::

if a rate-1 proxy is used as rate-2 input, the rate is converted and the channels are expanded in the usual multichannel expansion pattern:

]

@racketblock[
~f.defineBus(\control);
~f.ar(2);

~f.defineBus(\audio);
~f.kr(2);

// if the number of channels passed in is less, it only uses the first n channels
~f.defineBus(\audio, 8);
~f.ar(2);
::

an offset can be passed in as second argument to ar/kr

]

@racketblock[
//modulate offset:
p = ProxySpace.push(s.boot);

~out.play;
~src = { SinOsc.ar(Array.rand(5, 400, 500.0), SinOsc.ar(Array.exprand(5, 2.1, 500.0)), 0.1) };
~out = { ~src.ar(1, MouseX.kr(0, 5)) };
~out = { Mix(~src.ar(3, MouseX.kr(0, 5))) }; //the wrapping offset is moved accordingly
::
]


