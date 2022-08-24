#lang scribble/manual
@(require (for-label racket))

@title{Pitch}
 Autocorrelation pitch follower@section{categories}
  UGens>Analysis>Pitch

@section{description}

This is a better pitch follower than link::Classes/ZeroCrossing::, but more costly of CPU. For most purposes the default settings can be used and only 
@racketblock[in:: needs to be supplied. Pitch returns two values (via an link::Classes/Array:: of OutputProxys, see the link::Classes/OutputProxy:: help file), a ]

@racketblock[freq:: which is the pitch estimate and ]

@racketblock[hasFreq::, which tells whether a pitch was found. Some vowels are still problematic, for instance a wide open mouth sound somewhere between a low pitched short 'a' sound as in 'sat', and long 'i' sound as in 'fire', contains enough overtone energy to confuse the algorithm.

]
@section{classmethods}
 
@section{private}
  categories
@section{method}
  kr
@section{argument}
 in
@section{argument}
 initFreq
@section{argument}
 minFreq
@section{argument}
 maxFreq
@section{argument}
 execFreq
@section{argument}
 maxBinsPerOctave
@section{argument}
 median
@section{argument}
 ampThreshold
@section{argument}
 peakThreshold
@section{argument}
 downSample
@section{argument}
 clar

@section{discussion}
 
The pitch follower executes periodically at the rate specified by 
@racketblock[execFreq:: in cps. ]

@racketblock[execFreq:: is clipped to be between ]

@racketblock[minFreq:: and ]

@racketblock[maxFreq::. First it detects whether the input peak to peak amplitude is above the ]

@racketblock[ampThreshold::.
If it is not then no pitch estimation is performed, ]

@racketblock[hasFreq:: is set to zero and ]

@racketblock[freq:: is held at its previous value. It performs an autocorrelation on the input and looks for the first peak after the peak around the lag of zero that is above ]

@racketblock[peakThreshold:: times the amplitude of the peak at lag zero.

If the ]

@racketblock[clar:: argument is greater than zero (it is zero by default) then ]

@racketblock[hasFreq:: is given additional detail. Rather than simply being 1 when a pitch is detected, it is a "clarity" measure in the range between zero and one. (Technically, it's the height of the autocorrelation peak normalised by the height of the zero-lag peak.)
It therefore gives a kind of measure of "purity" of the pitched signal.

Using a ]

@racketblock[peakThreshold:: of one half does a pretty good job of eliminating overtones, and finding the first peak above that threshold rather than the absolute maximum peak does a good job of eliminating estimates that are actually multiple periods of the wave.

The autocorrelation is done coarsely at first using a maximum of ]

@racketblock[maxBinsPerOctave:: lags until the peak is located. Then a fine resolution search is performed until the peak is found. (Note that maxBinsPerOctave does NOT affect the final pitch resolution; a fine resolution search is always performed.
Setting maxBinsPerOctave larger will cause the coarse search to take longer, and setting it smaller will cause the fine search to take longer.)

The three values around the peak are used to find a fractional lag value for the pitch. If the pitch frequency is higher than ]

@racketblock[maxFreq::, or if no peak is found above ]

@racketblock[minFreq::, then ]

@racketblock[hasFreq:: is set to zero and ]

@racketblock[freq:: is held at its previous value.

It is possible to put a median filter of length ]

@racketblock[median:: on the output estimation so that outliers and jitter can be eliminated. This will however add latency to the pitch estimation for new pitches, because the median filter will have to become half filled with new values before the new one becomes the median value.
If median is set to one then that is equivalent to no filter, which is the default.

When an in range peak is found, it is inserted into the median filter, a new pitch is read out of the median filter and output as ]

@racketblock[freq::, and ]

@racketblock[hasFreq:: is set to one.

It is possible to down sample the input signal by an integer factor ]

@racketblock[downSample:: in order to reduce CPU overhead. This will also reduce the pitch resolution.

Until Pitch finds a pitch for the first time, it will output ]

@racketblock[initFreq::.

None of these settings are time variable.

]
@section{instancemethods}
 
@section{private}
  init

@section{examples}
 
(use headphones!)


@racketblock[

(
SynthDef("pitchFollow1", {
	var in, amp, freq, hasFreq, out;
	in = Mix.new(SoundIn.ar([0,1]));
	amp = Amplitude.kr(in, 0.05, 0.05);
	# freq, hasFreq = Pitch.kr(in, ampThreshold: 0.02, median: 7);
	//freq = Lag.kr(freq.cpsmidi.round(1).midicps, 0.05);
	out = Mix.new(VarSaw.ar(freq * [0.5,1,2], 0, LFNoise1.kr(0.3,0.1,0.1), amp));
	6.do({
		out = AllpassN.ar(out, 0.040, [0.040.rand,0.040.rand], 2)
	});
	Out.ar(0,out);
}).play(s);
)
::

]

@racketblock[
(
SynthDef("pitchFollow2", {
	var in, amp, freq, hasFreq, out;
	in = Mix.new(SoundIn.ar([0,1]));
	amp = Amplitude.kr(in, 0.05, 0.05);
	# freq, hasFreq = Pitch.kr(in, ampThreshold: 0.02, median: 7);
	out = CombC.ar(LPF.ar(in, 1000), 0.1, (2 * freq).reciprocal, -6).distort * 0.05;
	6.do({
		out = AllpassN.ar(out, 0.040, [0.040.rand,0.040.rand], 2)
	});
	Out.ar(0,out);
}).play(s);
)

/*

RM octaver

inSignal is RingModulated by a sinusoidal tone with half frequency.
The resulting spectrum is given by all the components of inSignal with
half freqs.
This means that the new spectrum is a one 8ve below version of the input signal's one,
with only odd partials.
As a consequence, if inSignal is added again, even partials are
recovered.

See:
Miller Puckette, The Theory and Technique of Electronic Music, p. 126
http://crca.ucsd.edu/~msp/techniques/latest/book.pdf
http://crca.ucsd.edu/~msp/techniques/latest/book-html/node77.html#sect5.ringmod

andreavalle

*/

(
SynthDef.new(\RmOctaver, { var in, out = 0, freq, hasFreq;
	in = SoundIn.ar(0);
	# freq, hasFreq = Pitch.kr(in);
	Out.ar(out, SinOsc.ar(freq: freq*0.5)*in+in);
}).add;
)

Synth.new(\RmOctaver);

::
]


