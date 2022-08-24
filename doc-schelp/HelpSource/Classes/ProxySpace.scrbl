#lang scribble/manual
@(require (for-label racket))

@title{ProxySpace}
 an environment of references on a server@section{categories}
  Libraries>JITLib>Environments, Live Coding, Collections>Unordered
@section{related}
  Classes/NodeProxy, Classes/ProxyMixer, Overviews/JITLib

@section{description}

Generally a strong::proxy:: is a placeholder for something. A node proxy is a placeholder for something strong::playing on a server:: that writes to a limited number of busses (e.g. a synth or an event stream). NodeProxy objects can be replaced and recombined while they play. Also they can be used to build a larger structure which is used and modified later on. Overview: link::Overviews/JITLib::

When accessed, ProxySpace returns a link::Classes/NodeProxy::. A similar class without environment: link::Classes/Ndef::

For more examples see: link::Tutorials/JITLib/proxyspace_@section{examples}
 , link::Tutorials/JITLib/jitlib_basic_concepts_01::

For GUI overview, see link::Classes/ProxyMixer::. See link::Classes/NodeProxy:: for many relevant methods.

@section{subsection}
 First Example


@racketblock[
s.boot;

p = ProxySpace.new;
p.fadeTime = 2; // fadeTime specifies crossfade
p[\out].play; // monitor an empty placeholder through hardware output
// set its source
p[\out] = { SinOsc.ar([350, 351.3], 0, 0.2) };
p[\out] = { Pulse.ar([350, 351.3] / 4, 0.4) * 0.2 };
p[\out] = Pbind(\dur, 0.03, \freq, Pbrown(0, 1, 0.1, inf).linexp(0, 1, 200, 350));

// route one proxy through another:
p[\out] = { Ringz.ar(p[\in].ar, [350, 351.3] * 8, 0.2) * 4 };
p[\in] = { Impulse.ar([5, 7]/2, [0, 0.5]) };

a.clear(3); // clear after 3 seconds
b.clear(3);
::

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{subsection}
 Creation

@section{method}
 new

@section{argument}
 server
a link::Classes/Server:: object. Note that on remote computers the clock must be in sync.

@section{argument}
 name
a link::Classes/Symbol::. If a name is given, the proxy space is strong::stored:: in 
@racketblock[ProxySpace.all:: under this name.

]
@section{argument}
 clock
for event-based or beat-sync playing use a link::Classes/TempoClock::.

@section{method}
 push
replace the currentEnvironment with a new ProxySpace and strong::clear:: the current one, if it is a ProxySpace (this is to avoid piling up proxy spaces).

In order to move to another ProxySpace while keeping the current, use strong::pop:: and then strong::push:: a new one. To have multiple levels of proxy spaces, use strong::.new.push;::

@section{method}
 pop
restore the previous currentEnvironment

@section{method}
 clearAll
clear all registered spaces

@section{InstanceMethods}
 

@section{subsection}
 Play back and access

@section{method}
 play
returns a group that plays the link::Classes/NodeProxy:: at that strong::key::.

@section{argument}
 key
a link::Classes/Symbol::

@section{argument}
 out
output channel offset

@section{argument}
 numChannels
play this number of channels.

@section{method}
 record
returns a link::Classes/RecNodeProxy:: that records the NodeProxy at that key.

@section{method}
 ar, kr
returns a NodeProxy output that plays the NodeProxy at that key, to be used within a function used as input to a node proxy

@section{method}
 wakeUp
when the proxyspace is created without a running server this method can be used. To run it (internally this is done by link::#-play:: as well).

@section{method}
 fadeTime
set the fadetime of all proxies as well as the default fade time

@section{method}
 clock
set the clock of all proxies as well as the default clock.

@section{method}
 quant
set the quant of all proxies as well as the default quant.

@section{method}
 free
free all proxies (i.e. free also the groups, do not stop the monitors)

@section{method}
 release
release all proxies (i.e. keep the groups running)

@section{method}
 stop
stop all proxies (stop only monitors, do not stop synths)

@section{method}
 end
end all proxies (free and stop the monitors)

@section{method}
 clear
clear the node proxy and remove it from the environment. this frees all buses. If a fadeTime is given, first fade out, then clear.

@section{method}
 add
add the ProxySpace to the repository (name required)

@section{method}
 remove
remove the ProxySpace from the repository

@section{subsection}
 Setting the sources

The strong::rate:: and strong::numChannels:: of the link::Classes/NodeProxy:: determined in a lazy way from the first object put into this environment (see helpfile). Once it is created it can only be set to a function that returns the same rate and a number of channels equal to the intial one or smaller. For details, see link::Tutorials/JITLib/the_lazy_proxy::.

@section{method}
 put
Gets the NodeProxy at strong::key:: (if none exists, returns a new one) and sets its source to strong::obj::. For how this works, see also link::Classes/LazyEnvir:: and link::Classes/NodeProxy::.

@section{method}
 at
Return the proxy source object at that key.

@section{subsection}
 garbage collecting

@section{method}
 clean
free and remove all proxies that are not needed in order to play the ones passed in with 'exclude'. if none are passed in, all proxies that are monitoring (with the .play message) are kept as well as their parents etc.

@section{method}
 reduce
free all proxies that are not needed in order to play the ones passed in with 'to'. if none are passed in, all proxies that are monitored (with the play message) are kept as well as their parents etc.

@section{subsection}
 making copies

@section{method}
 copy
Copies the environment into a new one, with each proxy being copied as well (See: link::Classes/NodeProxy#-copy::). Also the instance variables that determine the ProxySpace behaviour are included, such as server, fadeTime, quant, reshaping (this happens in the 
@racketblock[copyState:: method).

]

@racketblock[
p = ProxySpace.push(s.boot);
p.reshaping = \elastic;
~out.play;
~out = { Blip.ar(~freq, ~numharm) };
~freq = 70;
~numharm = { MouseX.kr(2, 100, 1) };

q = p.copy; p.pop; q.push;

q.reshaping.postln; // also elastic
~out.play;
~freq = { MouseY.kr(2, 1000, 1) * [1, 1.2] };

p.end; q.end;
::


]
@section{subsection}
 Writing code

@section{method}
 document
creates a new document with the current proxyspace state. This does not allow open functions as proxy sources. see: link::Tutorials/JITLib/jitlib_asCompileString::.

@section{argument}
 keys
list of keys to document a subset of proxies

@section{argument}
 onlyAudibleOutput
a boolean.

@section{argument}
 includeSettings
a boolean.

@section{Examples}
 


@racketblock[
// ProxySpace returns instances of NodeProxy:
a = NodeProxy(s) 	is equivalent to ~a;
a.source = ...		is equivalent to ~a = ...
a[3] = ...		is equivalent to ~a[3] = ...

// the two expressions are equivalent:
~out = something;
currentEnvironment.put(\out, something);
::

]

@racketblock[
// examples

p = ProxySpace.push(s.boot); // use ProxySpace as current environment.

~out.play;

~out = { SinOsc.ar([400, 407] * 0.9, 0, 0.2) };

~out = { SinOsc.ar([400, 437] * 0.9, 0, 0.2) * LFPulse.kr([1, 1.3]) };

~out = { SinOsc.ar([400, 437] * 0.9, 0, 0.2) * ~x.kr(2) };

~x = { LFPulse.kr([1, 1.3] * MouseX.kr(1, 30, 1)) };

~out = { SinOsc.ar([400, 437] * Lag.kr(0.1 + ~x, 0.3), 0, 0.2) * ~x };

p.fadeTime = 5;

~out = { SinOsc.ar([400, 437] * 1.1, 0, 0.2) * ~x.kr(2) };

p.clear(8); // end and clear all in 8 sec.


p.pop; // move out.
::
]


