#lang scribble/manual
@(require (for-label racket))

@title{FFT}
 Fast Fourier Transform@section{related}
  Classes/IFFT, Guides/FFT-Overview
@section{categories}
  UGens>FFT

@section{description}


The fast fourier transform analyzes the frequency content of a signal, which can be useful for audio analysis or for frequency-domain sound processing (phase vocoder).

@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer
A buffer to store spectral data. The buffer's size must
correspond to a power of 2. LocalBuf is useful here, because processes should not share data between synths. (Note: most PV UGens operate on this data in place. Use PV_Copy for parallel processing.)

@section{argument}
 in
The signal to be analyzed. The signal's rate determines the rate at which the input is read.

@section{argument}
  hop
The amount of offset from the beginning of one FFT analysis frame to the next, measured in multiples of the analysis frame size. This can range between 1.0 and values close to (but larger than) 0.0, and the default is 0.5 (meaning each frame has a 50% overlap with the preceding/following frames).

@section{argument}
  wintype
Defines how the data is windowed:
@section{table}
 
## -1 || strong::rectangular:: windowing, simple but typically not recommended;
## 0 || (the default) strong::Sine:: windowing, typically recommended for phase-vocoder work;
## 1 || strong::Hann:: windowing, typically recommended for analysis work.
::

@section{argument}
  active
A simple control allowing FFT analysis to be active (>0) or inactive (<=0). This is mainly useful for signal analysis processes which are only intended to analyse at specific times rather than continuously

@section{argument}
  winsize
The windowed audio frames are usually the same size as the buffer. If you wish the FFT to be zero-padded then you can specify a window size smaller than the actual buffer size (e.g. window size 1024 with buffer size 2048). Both values must still be a power of two. Leave this at its default of zero for no zero-padding.

@section{returns}
 
The FFT chain

@section{discussion}
 
Only the first two arguments are required. The remaining arguments allow for custom FFT analyses for specialised situations.

FFT uses a local buffer for holding the buffered audio. The buffer size must be a multiple of the control block size as well as being a power of two.

Note that for phase-vocoder usage, changing the hop or wintype settings from their defaults will typically result in unnatural sound when used in combination with IFFT, due to windowing artifacts. (A hop of 0.25, with Hann windowing, can be a useful combination for phase-vocoder work.)


@section{Examples}
 


@racketblock[
(
{
	var in, chain;
	in = WhiteNoise.ar(0.1);
	chain = FFT(LocalBuf(2048), in);
	IFFT(chain) // inverse FFT
}.play;
)

// inspecting it, we see that the chain is an FFT:
(
{
	var in, chain;
	in = WhiteNoise.ar(0.1);
	chain = FFT(LocalBuf(2048), in);
	chain.inspect; 
	IFFT(chain) // inverse FFT
}.play;
)

(
{
	var in, chain;
	in = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08,0,6,6.2).squared, 0, 100, 800));
	chain = FFT(LocalBuf(2048), in);
	IFFT(chain)
}.play;
)

(
{
	var in, chain;
	in = SinOsc.ar(SinOsc.kr(SinOsc.kr(0.08,0,6,6.2).squared, 0, 100,800));
	//in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_MagAbove(chain, 310);
	0.5 * IFFT(chain);
}.play;
)

(
{
	var in, chain;
	in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_BrickWall(chain, SinOsc.kr(0.1));
	IFFT(chain)
}.play;
)

(
{
	var in, chain;
	in = WhiteNoise.ar(0.8);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_RandComb(chain, 0.95, Impulse.kr(0.4));
	IFFT(chain)
}.play;
)

(
{
	var in, chain;
	in = WhiteNoise.ar(0.2);
	chain = FFT(LocalBuf(2048), in);
	chain = PV_RectComb(chain, 8, LFTri.kr(0.097,0,0.4,0.5),
		LFTri.kr(0.24,0,-0.5,0.5));
	IFFT(chain)
}.play;
)

(
{
	var in, chain;
	in = SinOsc.ar(LFNoise1.kr(5.2,250,400));
	chain = FFT(LocalBuf(2048), in);
	// moves in and out of freeze
	chain = PV_MagFreeze(chain, SinOsc.kr(0.2) );
	0.5 * IFFT(chain);
}.play;
)


// stereo example:

(
{
	var in, chain;
	in = SinOsc.ar(LFNoise1.kr([5.2, 3.3],250,400));
	chain = FFT({ LocalBuf(2048) } ! 2, in); // we need two buffers for stereo input.
	// moves in and out of freeze
	chain = PV_MagFreeze(chain, SinOsc.kr([0.2, 0.3]) );
	0.5 * IFFT(chain);
}.play;
)

::

]


