#lang scribble/manual
@(require (for-label racket))

@title{NdefMixerOld}
 mix control for all Ndefs on a given server@section{categories}
  Libraries>JITLib>GUI
@section{related}
  Classes/NdefMixer

@section{description}


For more details see: link::Classes/ProxyMixer::

@section{ClassMethods}
 

@section{subsection}
 Creation

@section{method}
 new
Return a new window for a given server.


@racketblock[
// ndef mxers for other servers
n = NdefMixer(\internal);
n = NdefMixer(\localhost);
n = NdefMixer(\trala);		// fails, no such server
::

]
@section{argument}
 server
Server object ( link::Classes/Server:: ) or server name ( link::Classes/Symbol:: )

@section{argument}
 nProxies
an Integer.

@section{argument}
 title
a String.

@section{argument}
 bounds
a Rect.

@section{InstanceMethods}
 

@section{method}
 proxyspace
Return the proxyspace.


@section{Examples}
 


@racketblock[
n = NdefMixer(s);		// for the default server
// make a new proxy
(
Ndef(\a, {
	Pan2.ar(
		Ringz.ar(
			Impulse.ar(exprand(0.5, 4)),
			exprand(300, 3000),
			0.02
		),
	1.0.rand2,
	0.2)
})
);



n.proxyspace;

Ndef(\duster, { Dust.kr(4) });

Ndef(\a).ar;
Ndef(\a).fadeTime = 2;
Ndef(\a).end;
::
]


