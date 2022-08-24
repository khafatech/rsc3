#lang scribble/manual
@(require (for-label racket))

@title{FFTTrigger}
 Outputs the necessary signal for FFT chains, without doing an FFT on a signal@section{categories}
  UGens>FFT

@section{classmethods}
 
@section{private}
  categories

@section{method}
  new
@section{argument}
  buffer
a buffer to condition for FFT use
@section{argument}
  hop
the hop size for timing triggers (defaults to 0.5)
@section{argument}
  polar
a flag. If 0.0, the buffer will be prepared for complex data, if > 0.0, polar data is set up.

@section{examples}
 

@racketblock[
(
s.waitForBoot({
	b = Buffer.alloc(s, 512);
});
)
// Reminder: This isn't the intended typical usage! It's OK to do this though.
(
x = {
	var mags, phases, chain, sig;
	// Create simple undulating magnitudes
	mags = {FSinOsc.kr(ExpRand(0.1, 1)).range(0, 1)}.dup(100);
	// Then give them a "rolloff" to make the sound less unpleasant
	mags = mags  * ((1, 0.99 .. 0.01).squared);
	// Let's turn the bins on and off at different rates, I'm *sure* that'll sound interesting
	mags = mags * {LFPulse.kr(2 ** IRand(-3, 5)).range(0, 1)}.dup(100);
	// Let's ignore phase for now
	phases = 0.dup(100);
	chain = FFTTrigger(b, 0.5);
	// Now we can do the packing
	chain = PackFFT(chain, 512, [mags, phases].flop.flatten, 0, 99, 1);
	sig = IFFT(chain);
	Out.ar(0, sig.dup);
}.play(s);
)
x.free;
b.free;
::

]


