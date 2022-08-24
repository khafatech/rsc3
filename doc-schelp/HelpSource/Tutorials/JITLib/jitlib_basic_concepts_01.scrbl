#lang scribble/manual
@(require (for-label racket))

@title{jitlib_basic_concepts_01}
 some placeholders in SuperCollider@section{categories}
  Libraries>JITLib>Tutorials, Tutorials>JITLib
@section{related}
  Overviews/JITLib, Tutorials/JITLib/jitlib_basic_concepts_02

This helpfile explains some basic concepts of interactive programming with SuperCollider and proxy space.

@section{section}
 What is a proxy?

A proxy is a place holder that is often used to operate on something that does not yet exist. For example, an emphasis::OutputProxy:: is used to represent multiple outputs of a ugen, even if only one ugen is created eventually. Any object can have proxy behaviour (it may delegate function calls to different objects for example) but specially functions and references can be used as operands while they keep their referential quality.

See also: link::Classes/OutputProxy::, link::Classes/Function::, link::Classes/Ref::

Further reading: link::Classes/NodeProxy::, link::Classes/ProxySpace::, link::Classes/Ndef::

@section{subsection}
 using a Ref as a proxy


@racketblock[
// reference example

// create a new Ref object
y = `(nil);

// you can start to calculate with y, even if its value is not yet given:
z = y + 10; // returns a function

// now the source can be set:
y.value = 34;

// the function z can be evaluated now.
z.value


// the same without a reference does not work:

y = nil; // empty y first

z = y + 10; // this fails.

// also an array does not provide this referentiality:

y = [nil]; // array with nil as element

z = y + 10; // this fails.

// an environment without sufficient defaults has the same problem:

currentEnvironment.postln; // anEnvironment
~x; // access the environment: there is nothing stored: nil
~x = 9; // store something
~x; 	// now 9 is stored
~x + 100; // calculate with it

currentEnvironment.postln; // the value is stored in the environment

~y + ~x; // cause an error: ~y is nil.
~y = -90; // set ~y

~y + ~x; // this works.
::

]
@section{subsection}
 using a Function as a proxy


@racketblock[
// a function can serve the same purpose

y = nil; // empty y first
z = { y } + 10;	// this does not fail, instead it creates a new function, which
		// does not fail when evaluating it after y is set to 34.

y = 34;
z.value;
::

see also client side proxies like link::Classes/Tdef::, link::Classes/Pdefn::, link::Classes/Pdef::, link::Classes/Fdef::

]
@section{section}
 NodeProxy, ProxySpace, Ndef

For interactive programming it can be useful to be able to use something before it is there - it makes evaluation order more flexible and allows to postpone decisions to a later moment. Some preparations have to be done usually - like above, a reference has to be created. In other situations this sort of preparation is not enough, for example if one wants to apply mathematical operations on signals of running processes on the server.

@section{note}
  Wherever examples are given with Ndef, they apply to ProxySpace and NodeProxy: 
@racketblock[Ndef(\x, 5);:: is the same as:  ]

@racketblock[ ~x = 5;:: in ProxySpace, and ]

@racketblock[a = NodeProxy.new; a.source = 5;::::

]

@racketblock[
// boot the server
s.boot;

// two proxies on a server. calculation rate is audio rate, number of channels is 2
y = NodeProxy.audio(s, 2);
z = NodeProxy.audio(s, 2);

// use them in calculation
z.play;
z.source = y.sin * 0.2;


// set its source now.
y.source = { Saw.ar([300, 301], 4*pi) };

// the source can be of various type, one of them would be a number:
y.source = 0.0;

// post the source
y.source;

// end them, free their bus number
y.clear;
z.clear;
::

In order to provide a simple way of creating node proxies, a proxy space can be used.
So the above reads like this:

]

@racketblock[
p = ProxySpace.push(s.boot); // store proxy space in p so it can be accessed easily.
~z.play;


~z = ~y.sin * 0.2;


~y = { Saw.ar([300, 301], 4*pi) };


// clear the space (all proxies)
p.clear;

// move out of the proxyspace.
p.pop;
::

Another, very common way to access node proxies is link::Classes/Ndef:: (this is the same as the above, just written with Ndef):

]

@racketblock[
Ndef(\z).play;

Ndef(\z, Ndef(\y).sin * 0.2);

Ndef(\y, { Saw.ar([300, 301], 4 * pi) });

Ndef.clear;
::

The class Ndef uses link::Classes/ProxySpace:: internally to store its place holders.


Further reading: link::Classes/NodeProxy::, link::Classes/ProxySpace::, link::Classes/Ndef::

next: link::Tutorials/JITLib/jitlib_basic_concepts_02::
]


