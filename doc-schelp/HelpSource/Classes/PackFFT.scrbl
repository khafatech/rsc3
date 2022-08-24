#lang scribble/manual
@(require (for-label racket))

@title{PackFFT}
 Pack separate demand-rate FFT bin streams into an FFT chain buffer@section{categories}
  UGens>FFT
@section{related}
  Classes/UnpackFFT

@section{description}

Takes an array of magnitudes and phases, and packs them into an FFT buffer ready for transforming back into time-domain audio using IFFT.

Most people won't need to use this directly - instead, use pvcollect, pvcalc, or pvcalc2 methods from the link::Classes/PV_ChainUGen:: base class.

@section{classmethods}
 
@section{private}
  categories

@section{method}
  new

@section{argument}
  chain
The link::Classes/FFT:: chain

@section{argument}
  bufsize
FFT buffer size

@section{argument}
  magsphases
The input data should be a flat array containing magnitude and phase of all bins in ascending order.
e.g. 
@racketblock[ [mag0, phase0, mag1, phase1, mag2, phase2, ... magN, phaseN] ::
This input is typically demand-rate.

]
@section{argument}
  frombin
restricts the frequency band

@section{argument}
  tobin
restricts the frequency band

@section{argument}
  zeroothers
set to 1 to zero all the magnitudes outside the restricted frequency band

@section{discussion}
 
This is technically similar to Demand or Duty in that it calls demand-rate UGens further up the graph to process the values, eventually calling UnpackFFT. These two ends of the process must in most cases see the same chain...! Otherwise behaviour is undefined and, who knows, possibly unpleasant.

Optional parameters: frombin and tobin allow you to fill the supplied data only into a subset of the FFT bins (i.e. a single delimited frequency band), and if you do this, you can also optionally set zeroothers to 1 to zero all the magnitudes outside this band (otherwise they stay intact).

@section{examples}
 
Here's an unusual example which uses PackFFT without using UnpackFFT first - essentially creating our FFT data from scratch.

@racketblock[
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
	// We need to create an FFT chain to feed our data in to.
	// The easiest way is to do an FFT on some signal which we then ignore!
	chain = FFT(LocalBuf(512), FSinOsc.ar);
	// Now we can do the packing
	chain = PackFFT(chain, 512, [mags, phases].flop.flatten, 0, 99, 1);
	sig = IFFT(chain);
	Out.ar(0, sig.dup);
}.play(s);
)
x.free;
::

]


