#lang scribble/manual
@(require (for-label racket))

@title{PV_ChainUGen}
 Base class for UGens that alter FFT chains@section{categories}
  UGens>FFT

@section{description}

"PV_ChainUGen" is an abstract class - in other words, a class that you do not use directly. Instead, use one of its subclasses. It represents phase-vocoder UGens - i.e. UGens which apply some kind of transformation to the frequency-domain signal produced by FFT.

(Strictly, this class encompasses all units whose output is an FFT chain. This is why FFT is in this group but IFFT is not - the IFFT unit outputs ordinary time-domain audio.)

For more info on using these units, see link::Guides/FFT-Overview::.

@section{classmethods}
 
@section{private}
  categories

@section{instancemethods}
 
@section{method}
  pvcalc
Process the bins of an FFT chain
@section{discussion}
 

@racketblock[
chain = chain.pvcalc(numframes, func, frombin, tobin, zeroothers)
::
pvcalc applies function ]

@racketblock[func:: to the frequency-domain data of an FFT chain.
]

@racketblock[func:: should be a function that takes two arrays as inputs (magnitude, and phase) and returns a resulting pair of arrays ]

@racketblock[[magnitude, phase]::.

frombin, tobin, and zeroothers are optional arguments which limit the processing to a specified integer range of FFT bins. If zeroothers is set to 1 then bins outside of the range being processed are silenced.

See ]

@racketblock[pvcollect:: below for discussion of efficiency considerations. See also ]

@racketblock[pvcalc2:: below, and link::Classes/UnpackFFT::.

]
@section{method}
 pvcalc2
Process the bins of two FFT chains
@section{discussion}
 

@racketblock[
chain = chain.pvcalc2(chain2, numframes, func, frombin, tobin, zeroothers)
::
pvcalc2 is just like pvcalc but can combine two FFT chains together. See ]

@racketblock[pvcalc:: above for more information.

]

@racketblock[func:: should be a function that takes four arrays as inputs (magnitudes1, phases1, magnitudes2, phases2) and returns a resulting pair of arrays ]

@racketblock[[magnitude, phase]::.

]
@section{method}
  pvcollect
Process each bin of an FFT chain, separately
@section{discussion}
 

@racketblock[
chain = chain.pvcollect(numframes, func, frombin, tobin, zeroothers)
::
pvcollect applies function ]

@racketblock[func:: to each bin of an FFT chain. func should be a function that takes ]

@racketblock[ magnitude, phase, bin, index :: as inputs and returns a resulting ]

@racketblock[[magnitude, phase]::.

The "bin" is the integer bin number, starting at 0 for DC, while "index" is the iteration number, always starting with 0. You can optionally ignore the phase and only return a single (magnitude) value, in which case the phase is assumed to be left unchanged.

frombin, tobin, and zeroothers are optional arguments which limit the processing to a specified integer range of FFT bins. If zeroothers is set to 1 then bins outside of the range being processed are silenced.

Note that this procedure can be relatively CPU-heavy, depending on how you use it.
Using pvcollect (or its components, UnpackFFT & PackFFT) is usually less efficient than using a single "PV_" unit generator to process an FFT chain, because it involves the creation of quite a large graph of demand-rate unit generators.

If you wish to reduce the CPU impact of using this approach, try the following:
]
@section{list}
 
## Use the frombin and tobin arguments to limit the number of FFT bins that will be included in the calculation. Often the lower FFT bins contain the loudest and/or most relevant information, so perhaps your effect sounds very similar if you ignore the higher-up bins (either leave them unprocessed, or discard them by setting the zeroothers argument to 1, which has the effect of a band-pass frequency-domain filter).
## Use a smaller FFT buffer size.
## Avoid creating ugens inside your calculation function if at all possible. For example, a deterministic ugen such as LFPar.kr(0.5, 0, 1) will be replicated once for each bin if specified inside the function, despite the fact that the output is always the same. Define it outside the calculation function and then reference it by variable name.
## Avoid unused calculations! For example, uncommenting all the different lines in the above will waste effort because many values will be calculated but not used. This cannot be optimised away during compilation. It is particularly important because all calculations are duplicated (once for each bin) so can have a significant impact on efficiency.
## If you find yourself calling pvcollect on an FFT chain more than once in series, you should definitely try to combine your processing into a single pvcollect function, to avoid unnecessary unpacking-then-packing-then-unpacking-then-packing.
::

@section{Examples}
 

@section{subsection}
  pvcalc

@racketblock[
(
s.waitForBoot({
	c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
})
)

(
x = {
	var in, chain, v;
	in = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(LocalBuf(1024), in);

	chain = chain.pvcalc(1024, {|mags, phases|
//////// Try uncommenting each of these lines in turn and re-running the synth:
		[mags * {1.5.rand}.dup(mags.size), phases + {pi.rand}.dup(phases.size)]; // Arbitrary filter, arbitrary phase shift
		//[mags.reverse, phases.reverse]; // Upside-down!
		//[mags.differentiate, phases.differentiate]; // Differentiate along frequency axis
		//[mags[30..] ++ mags[..30], phases[30..] ++ phases[..30]]; // ".rotate" doesn't work directly, but this is equivalent
	}, frombin: 0, tobin: 250, zeroothers: 0);

	Out.ar(0, 0.5 * IFFT(chain).dup);
}.play(s);
)
x.free;
::

]
@section{subsection}
  pvcalc2

@racketblock[
(
s.waitForBoot({
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
})
)

(
x = {
	var fftsize = 1024;
	var in, chain, in2, chain2, out;
	in = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(LocalBuf(fftsize), in);

	// in2 = PlayBuf.ar(1, e, BufRateScale.kr(e), loop: 1);
	// JMcC babbling brook
	in2 = ({
		RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 14)
			* 400 + 500, 0.03, 0.003)}!2)
			+ ({RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 20)
			* 800 + 1000, 0.03, 0.005)}!2
		)
			* 4;
	chain2 = FFT(LocalBuf(fftsize), in2);

	chain = chain.pvcalc2(chain2, fftsize, {|mags, phases, mags2, phases2|
		[mags * mags2 / 10, phases2 + phases]
	}, frombin: 0, tobin: 125, zeroothers: 0);

	out = IFFT(chain);
	Out.ar(0, 0.5 * out.dup);
}.play(s);
)
x.free;
::

]
@section{subsection}
  pvcollect

@racketblock[
(
s.waitForBoot({
c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
})
)

(
x = {
	var in, chain, v;
	in = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(LocalBuf(1024), in);

	v = LFPar.kr(0.5).range(0.1, 1);

	chain = chain.pvcollect(1024, {|mag, phase, index|
//////// Try uncommenting each of these lines in turn and re-running the synth:
		//mag;
		//[mag, phase];
		//[mag, phase] / 3;
		//[mag, phase].sqrt;
		//[mag, 3.14.rand];
		//[mag, LFNoise0.kr.range(0, 3.14)];
		//[mag * Dseq([1, 0, 0, 1, 1, 0, 1, 0].stutter(8), 999999999999)]; // Can even use Demand ugens! One val demanded each frame
		//[mag.sqrt, 3.14.rand];
		//if(index % 7 == 0, mag, 0); // Comb filter
		//if(LFNoise0.kr(10) > 0.5, mag, 0);
		//mag + DelayN.kr(mag, 1, v); // Spectral delay
		if((index-LFPar.kr(0.1).range(2, 1024/20)).abs < 10, mag, 0); // Swept bandpass
	}, frombin: 0, tobin: 250, zeroothers: 0);

	Out.ar(0, 0.5 * IFFT(chain).dup);
}.play(s);
)
x.free;
::
]


