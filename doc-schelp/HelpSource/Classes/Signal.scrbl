#lang scribble/manual
@(require (for-label racket))

@title{Signal}
Sampled audio buffer@section{related}
 Classes/Wavetable
@section{categories}
  Collections>Ordered

@section{description}

A Signal is a FloatArray that represents a sampled function of time buffer.  Signals support math operations.

@section{CLASSMETHODS}
 

@section{method}
 sineFill
Fill a Signal of the given size with a sum of sines at the given amplitudes and phases. The Signal will be normalized.

@racketblock[
Signal.sineFill(1000, 1.0/[1, 2, 3, 4, 5, 6]).plot;
::
]
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 amplitudes
an Array of amplitudes for each harmonic beginning with the fundamental.
@section{argument}
 phases
an Array of phases in radians for each harmonic beginning with the fundamental.

@section{method}
 chebyFill
Fill a Signal of the given size with a sum of Chebyshev polynomials at the given amplitudes. For eventual use in waveshaping by the Shaper ugen; see link::Classes/Shaper:: helpfile and link::Classes/Buffer#-cheby#Buffer:cheby:: too.
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 amplitudes
an link::Classes/Array:: of amplitudes for each Chebyshev polynomial beginning with order 1.
@section{argument}
 normalize
a link::Classes/Boolean:: indicating whether to normalize the resulting Signal. If the zeroOffset argument is true, the normalization is done for use as a transfer function, using link::Classes/Signal#-normalizeTransfer#normalizeTransfer::, otherwise it just uses link::Classes/Signal#-normalize#normalize:: to make the absolute peak value 1.  Default is true.
@section{argument}
 zeroOffset
a link::Classes/Boolean:: indicating whether to offset the middle of each polynomial to zero. If true, then a zero input will always result in a zero output when used as a link::Classes/Shaper##waveshaper::. If false, then the "raw" (unshifted) Chebyshev polynomials are used. Default is false.
@section{discussion}
 
@section{note}
 
In previous versions, chebyFill always offset the curves to ensure the center value was zero. The zeroOffset argument was added in version 3.7, and the default behavior was changed, so that it no longer offsets.
::

@racketblock[
Signal.chebyFill(1000, [1]).plot;

// shifted to avoid DC offset when waveshaping a zero signal
Signal.chebyFill(1000, [0, 1], zeroOffset: true).plot;

// normalized sum of (unshifted) Chebyshev polynomials (the default)
Signal.chebyFill(1000, [0, 1, 0, 0, 0, 1], normalize: true, zeroOffset: false).plot;

Signal.chebyFill(1000, [0, 0, 1]).plot;
Signal.chebyFill(1000, [0.3, -0.8, 1.1]).plot;


// This waveshaping example uses two buffers, one with zero offset and
// the other not.
//
// 1. The offset version gives zero output (DC free) when waveshaping an
// input signal with amplitude of zero (e.g. DC.ar(0)).
//
// 2. The non-offset version makes better use of the full (-1 to 1) range
// when waveshaping a varying signal with amplitude near 1, but (if even
// Chebyshev polynomial degrees are used) will have a DC offset when
// waveshaping a signal with amplitude of zero.
//
// 3. Wrapping the non-offset Shaper in a LeakDC (the third signal in the
// example) cancels out any DC offsets (third version), while making full use
// of the -1 to 1 range.
(
s.waitForBoot({
	var amplitudes = [0, 1, 1, -2, 1];
	var sigs = [
		Signal.chebyFill(256+1, amplitudes, normalize: true, zeroOffset: true),
		Signal.chebyFill(256+1, amplitudes, normalize: true, zeroOffset: false)
	];
	b = sigs.collect{ arg sig; Buffer.loadCollection(s, sig.asWavetableNoWrap) };
	s.sync;
	x = {
		var in = SinOsc.ar(100, 0, SinOsc.kr(0.1, 0, 0.5, 0.5));
		Shaper.ar(b, in ) ++ LeakDC.ar(Shaper.ar(b[1], in))
	}.scope;
})
)
x.free; b.do(_.free); b = nil
::

]
@section{method}
 hanningWindow
Fill a Signal of the given size with a Hanning window.

@racketblock[
Signal.hanningWindow(1024).plot;
Signal.hanningWindow(1024, 512).plot;
::
]
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 pad
the number of samples of the size that is zero padding.

@section{method}
 hammingWindow
Fill a Signal of the given size with a Hamming window.

@racketblock[
Signal.hammingWindow(1024).plot;
Signal.hammingWindow(1024, 512).plot;
::
]
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 pad
the number of samples of the size that is zero padding.

@section{method}
 welchWindow
Fill a Signal of the given size with a Welch window.

@racketblock[
Signal.welchWindow(1024).plot;
Signal.welchWindow(1024, 512).plot;
::
]
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 pad
the number of samples of the size that is zero padding.

@section{method}
 rectWindow
Fill a Signal of the given size with a rectangular window.

@racketblock[
Signal.rectWindow(1024).plot;
Signal.rectWindow(1024, 512).plot;
::
]
@section{argument}
 size
the number of samples in the Signal.
@section{argument}
 pad
the number of samples of the size that is zero padding.

@section{method}
 fftCosTable
Fourier Transform: Fill a Signal with the cosine table needed by the FFT methods. See also the instance methods link::#-fft:: and link::#-ifft::.

@racketblock[
Signal.fftCosTable(512).plot;
::

]
@section{INSTANCEMETHODS}
 

@section{private}
 performBinaryOpOnSignal, performBinaryOpOnComplex, performBinaryOpOnSimpleNumber

@section{method}
 plot
Plot the Signal in a window. The arguments are not required and if not given defaults will be used.

@racketblock[
Signal.sineFill(512, [1]).plot;
Signal.sineFill(512, [1]).plot("Signal 1", Rect(50, 50, 150, 450));
::
For details, see link::Reference/plot::

]
@section{method}
 play
Loads the signal into a buffer on the server and plays it. Returns the buffer so you can free it again.

@racketblock[
b = Signal.sineFill(512, [1]).play(true, 0.2);
b.free;	// free the buffer again.
::
]
@section{argument}
 loop
A link::Classes/Boolean:: whether to loop the entire signal or play it once. Default is false.
@section{argument}
 mul
volume at which to play it, 0.2 by default.
@section{argument}
 numChannels
if the signal is an interleaved multichannel file, number of channels, default is 1.
@section{argument}
 server
the server on which to load the signal into a buffer.

@section{method}
 waveFill
Fill the Signal with a function evaluated over an interval.

@section{argument}
 function
a function that should calculate the value of a sample.

@racketblock[
(
a = Signal.newClear(256);
a.waveFill({ arg x, old, i; sin(x)}, 0, 3pi);
a.waveFill({ arg x, old, i; old * sin(11 * x + 0.3) }, 0, 3pi);
a.waveFill({ arg x, old, i; old * (x % 4) }, 0, 3pi);

a.plot;
)
::


The function is called with three arguments:
]
@section{definitionList}
 
## x || the value along the interval.
## old || the old value (if the signal is overwritten)
## i || the sample index.
::
As arguments, three values are passed to the function: the current input value (abscissa), the old value (if the signal is overwritten), and the index.

@racketblock[
(
a = Signal.newClear(16);
a.waveFill({ arg x, prev, i; [x, prev, i].postln; sin(x).max(0) }, 0, 3pi);
a.plot;
)
::



]
@section{argument}
 start
the starting value of the interval.
@section{argument}
 end
the ending value of the interval.

@section{method}
 asWavetable
Convert the Signal into a Wavetable.

@racketblock[
Signal.sineFill(512, [1]).asWavetable.plot;
::

]
@section{method}
 fill
Fill the Signal with a value.

@racketblock[
Signal.newClear(512).fill(0.2).plot;
::

]
@section{method}
 scale
Scale the Signal by a factor strong::in place::.

@racketblock[
a = Signal[1, 2, 3, 4];
a.scale(0.5); a;
::

]
@section{method}
 offset
Offset the Signal by a value strong::in place::.

@racketblock[
a = Signal[1, 2, 3, 4];
a.offset(0.5); a;
::

]
@section{method}
 peak
Return the peak absolute value of a Signal.

@racketblock[
Signal[1, 2, -3, 2.5].peak;
::

]
@section{method}
 normalize
Normalize the Signal strong::in place:: such that the maximum absolute peak value is 1.

@racketblock[
Signal[1, 2, -4, 2.5].normalize;
Signal[1, 2, -4, 2.5].normalize(0, 1);	// normalize only a range
::

]
@section{method}
 normalizeTransfer
Normalizes a transfer function so that the center value of the table is offset to zero and the absolute peak value is 1. Transfer functions are meant to be used in the link::Classes/Shaper:: ugen.

@racketblock[
Signal[1, 2, 3, 2.5, 1].normalizeTransfer;
::

]
@section{method}
 invert
Invert the Signal strong::in place::.

@racketblock[
a = Signal[1, 2, 3, 4];
a.invert(0.5); a;
::

]
@section{method}
 reverse
Reverse a subrange of the Signal strong::in place::.

@racketblock[
a = Signal[1, 2, 3, 4];
a.reverse(1, 2); a;
::

]
@section{method}
 fade
Fade a subrange of the Signal strong::in place::.

@racketblock[
a = Signal.fill(10, 1);
a.fade(0, 3);		// fade in
a.fade(6, 9, 1, 0);	// fade out
::

]
@section{method}
 integral
Return the integral of a signal.

@racketblock[
Signal[1, 2, 3, 4].integral;
::

]
@section{method}
 overDub
Add a signal to myself starting at the index. If the other signal is too long only the first part is overdubbed.

@racketblock[
a = Signal.fill(10, 100);
a.overDub(Signal[1, 2, 3, 4], 3);

		// run out of range
a = Signal.fill(10, 100);
a.overDub(Signal[1, 2, 3, 4], 8);

a = Signal.fill(10, 100);
a.overDub(Signal[1, 2, 3, 4], -4);

a = Signal.fill(10, 100);
a.overDub(Signal[1, 2, 3, 4], -1);

a = Signal.fill(10, 100);
a.overDub(Signal[1, 2, 3, 4], -2);

a = Signal.fill(4, 100);
a.overDub(Signal[1, 2, 3, 4, 5, 6, 7, 8], -2);
::

]
@section{method}
 overWrite
Write a signal to myself starting at the index. If the other signal is too long only the first part is overdubbed.

@racketblock[
a = Signal.fill(10, 100);
a.overWrite(Signal[1, 2, 3, 4], 3);

		// run out of range
a = Signal.fill(10, 100);
a.overWrite(Signal[1, 2, 3, 4], 8);

a = Signal.fill(10, 100);
a.overWrite(Signal[1, 2, 3, 4], -4);

a = Signal.fill(10, 100);
a.overWrite(Signal[1, 2, 3, 4], -1);

a = Signal.fill(10, 100);
a.overWrite(Signal[1, 2, 3, 4], -2);

a = Signal.fill(4, 100);
a.overWrite(Signal[1, 2, 3, 4, 5, 6, 7, 8], -2);
::

]
@section{method}
 blend
Blend two signals by some proportion.

@racketblock[
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0.2);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 0.4);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 1);
Signal[1, 2, 3, 4].blend(Signal[5, 5, 5, 0], 2);
::

]
@section{subsection}
 Fourier Transform

@section{method}
 fft
Perform an FFT on a real and imaginary signal in place. See also the class method link::#*fftCos@section{Table}
 .

@racketblock[
(
var size = 512, real, imag, cosTable, complex;

real = Signal.newClear(size);
		// some harmonics
real.sineFill2([[8], [13, 0.5], [21, 0.25], [55, 0.125, 0.5pi]]);
		// add a little noise
real.overDub(Signal.fill(size, { 0.2.bilinrand }));

imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

complex = fft(real, imag, cosTable);
[real, imag, (complex.magnitude) / 100 ].flop.flat
	.plot("fft", Rect(0, 0, 512 + 8, 500), numChannels: 3);
)
::

]
@section{method}
 ifft
Perform an inverse FFT on a real and imaginary signal in place. See also the class method link::#*fftCos@section{Table}
 .

@racketblock[
(
var size = 512, real, imag, cosTable, complex, ifft;

real = Signal.newClear(size);
		// some harmonics
real.sineFill2([[8], [13, 0.5], [21, 0.25], [55, 0.125, 0.5pi]]);
		// add a little noise
real.overDub(Signal.fill(size, { 0.2.bilinrand }));

imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

complex = fft(real, imag, cosTable).postln;
ifft = complex.real.ifft(complex.imag, cosTable);

[real, ifft.real].flop.flat
	.plot("fft and back", Rect(0, 0, 512 + 8, 500), numChannels: 2);
)
::

]
@section{subsection}
 Unary Messages

Signal will respond to unary operators by returning a new Signal.

@racketblock[
x = Signal.sineFill(512, [0, 0, 0, 1]);
[x, x.neg, x.abs, x.sign, x.squared, x.cubed, x.asin.normalize, x.exp.normalize, x.distort].flop.flat
	.plot(numChannels: 9);
::

]
@section{method}
 neg, abs, sign, squared, cubed, sqrt, exp, log, log2, log10, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, distort, softclip, isPositive, isNegative, isStrictlyPositive

@section{subsection}
 Binary Messages

Signal will respond to binary operators by returning a new Signal.

@racketblock[
(
x = Signal.fill(512, { rrand(0.0, 1.0) });
y = Signal.fill(512, { |i| (i * pi / 64).sin });
[x, y, (x + y) * 0.5, x * y, min(x, y), max(x, y) ].flop.flat
	.plot(numChannels: 6);
)
::
]
@section{method}
 +, -, *, /, div, pow, mod, min, max, ring1, ring2, ring3, ring4, difsqr, sumsqr, sqrdif, absdif, amclip, scaleneg, clip2, wrap2, excess
@section{method}
  %, **
@section{method}
  <!


@racketblock[
// these fail for Signal but work for FloatArray:
Signal[0, 0.5, 1] pow: 2;
Signal[0, 0.5, 1] ** 2;
Signal[0, 0.5, 1] mod: 0.15;
Signal[0, 0.5, 1] % 0.15;

FloatArray[0, 0.5, 1] % 0.15;
::
]


