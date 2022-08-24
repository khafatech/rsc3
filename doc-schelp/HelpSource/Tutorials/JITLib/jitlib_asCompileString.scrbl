#lang scribble/manual
@(require (for-label racket))

@title{jitlib_asCompileString}
 asCompileString in JITLib@section{categories}
  Libraries>JITLib>Tutorials


@racketblock[
{ 10 + 6 * ~harry }.asCompileString;
::

many objects understand strong::.storeOn::, which a way to post their string that is needed to reproduce them by compilation. sometimes one wants to store a certain configuration of a proxy space, which can be done
if all functions used are closed functions.

]

@racketblock[
// an example how ProxySpace can document its current state:

p = ProxySpace.push(s);


(
~ctl1 = {
	var z = 1;
	4.do { |i| z = z * SinOsc.kr(i.sqrt, i+[0,0.2]) };
	z
};

~ctl2[0] = { LFNoise2.kr([20,20],20) };
~ctl2[1] = {
	LFNoise2.kr([20,20],20) * LFNoise0.kr([20,20],20)
};

~out = {
	SinOsc.ar(~freq.kr, 0, 0.1)
};

~freq[0] = { ~ctl1.kr(2) + ~ctl2.kr(2) + 400 };
~freq[5] = ~ctl1.wrap2(~ctl2) * ~ctl1 / (~ctl2 + ~ctl1);

~pat = Pbind(\freq, Pfunc({ 1.2.rand }));
~z = 9;
~out.set(\freq, 760, \ffreq, 20);
)

p.asCompileString;

// the document message creates a new document which it posts the code into

p.document;		// document everything
p.document([\out]); 	// document all dependants of ~out
p.document([\ctl1]);	// document all dependants of ~ctl1
::
]


