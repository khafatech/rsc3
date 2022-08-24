#lang scribble/manual
@(require (for-label racket))

@title{Onsets}
 Onset detector@section{categories}
  UGens>Analysis
@section{related}
  Classes/BeatTrack, Classes/Loudness, Classes/MFCC, Classes/Pitch, Classes/KeyTrack

@section{description}

An onset detector for musical audio signals - detects the beginning of notes/drumbeats/etc. Outputs a control-rate trigger signal which is 1 when an onset is detected, and 0 otherwise.

For more details of all the processes involved, the different onset detection functions, and their evaluation, see:

D. Stowell and M. D. Plumbley. Adaptive whitening for improved real-time audio onset detection. emphasis::Proceedings of the International Computer Music Conference (ICMC2007)::, Copenhagen, Denmark, August 2007. See
http://www.elec.qmul.ac.uk/digitalmusic/papers/2007/StowellPlumbley07-icmc.pdf.

@section{classmethods}
 
@section{private}
  categories

@section{method}
  kr

@section{argument}
  chain
an FFT chain.
@section{argument}
  threshold
the detection threshold, typically between 0 and 1, although in rare cases you may find values outside this range useful.
@section{argument}
  odftype
chooses which emphasis::onset detection function:: is used. In many cases the default will be fine. More choices are listed below.

The remaining args are all tweak factors, explained below in section Advanced features:

@section{argument}
  relaxtime
@section{argument}
  floor
@section{argument}
  mingap
@section{argument}
  medianspan
@section{argument}
  whtype
@section{argument}
  rawodf

@section{Discussion}
 

	The following choices are available for 
@racketblock[odftype:: :

]
@section{definitionlist}
 
## 
@racketblock[\power:: || generally OK, good for percussive input, and also very efficient
## ]

@racketblock[\magsum:: || generally OK, good for percussive input, and also very efficient
## ]

@racketblock[\complex:: || performs generally very well, but more CPU-intensive
## ]

@racketblock[\rcomplex:: || performs generally very well, and slightly more efficient than ]

@racketblock[\complex::
## ]

@racketblock[\phase:: || generally good, especially for tonal input, medium efficiency
## ]

@racketblock[\wphase:: || generally very good, especially for tonal input, medium efficiency
## ]

@racketblock[\mkl:: || generally very good, medium efficiency, pretty different from the other methods
::


For the FFT chain, you should typically use a frame size of 512 or 1024 (at 44.1 kHz sampling rate) and 50% hop size (which is the default setting in SC). For different sampling rates choose an FFT size to cover a similar time-span (around 10 to 20 ms).

The onset detection should work well for a general range of monophonic and polyphonic audio signals. The onset detection is purely based on signal analysis and does not make use of any "top-down" inferences such as tempo.

Which onset detection function should you choose? The differences aren't large, so I'd recommend you stick with the default ]

@racketblock[\rcomplex:: unless you find specific problems with it. Then maybe try ]

@racketblock[\wphase::. The ]

@racketblock[\mkl:: type is a bit different from the others so maybe try that too. They all have slightly different characteristics, and in tests perform at a similar quality level.

]
@section{subsection}
  Advanced features

Further options are available, which you are welcome to explore if you want. They are numbers that modulate the behaviour of the onset detector:

@section{list}
 
## strong::relaxtime:: and strong::floor:: are parameters to the whitening process used, a kind of normalisation of the FFT signal. (Note: in \mkl mode these are not used.)
	@section{list}
 
	## strong::relaxtime:: specifies the time (in seconds) for the normalisation to "forget" about a recent onset. If you find too much re-triggering (e.g. as a note dies away unevenly) then you might wish to increase this value.
	## strong::floor:: is a lower limit, connected to the idea of how quiet the sound is expected to get without becoming indistinguishable from noise. For some cleanly-recorded classical music with wide dynamic variations, I found it helpful to go down as far as 0.000001.
	::
## strong::mingap:: specifies a minimum gap (in FFT frames) between onset detections, a brute-force way to prevent too many doubled detections.
## strong::medianspan:: specifies the size (in FFT frames) of the median window used for smoothing the detection function before triggering.
::

@section{examples}
 

@racketblock[
(
s.waitForBoot({
	// Prepare the buffers
	b = Buffer.alloc(s, 512);
	// Feel free to load a more interesting clip!
	// a11wlk01 is not an ideal example of musical onsets.
	d = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
});
)

////////////////////////////////////////////////////////////////////////////////////////////////
// Move the mouse to vary the threshold
(
x = {
	var sig, chain, onsets, pips;

	// A simple generative signal
	sig = LPF.ar(Pulse.ar(TIRand.kr(63, 75, Impulse.kr(2)).midicps), LFNoise2.kr(0.5).exprange(100, 10000)) * Saw.ar(2).range(0, 1);
	// or, uncomment this line if you want to play the buffer in
	//sig = PlayBuf.ar(1, d, BufRateScale.kr(d), loop: 1);

	chain = FFT(b, sig);

	onsets = Onsets.kr(chain, MouseX.kr(0,1), \rcomplex);

	// You'll hear percussive "ticks" whenever an onset is detected
	pips = WhiteNoise.ar(EnvGen.kr(Env.perc(0.001, 0.1, 0.2), onsets));
	Out.ar(0, Pan2.ar(sig, -0.75, 0.2) + Pan2.ar(pips, 0.75, 1));
}.play;
)
x.free; // Free the synth

////////////////////////////////////////////////////////////////////////////////////////////////
// Or we could expand this multichannel, run a series of different thresholds at the same time,
// to sonify the effect of the threshold value.
// A little hard to listen to at first: try and identify a pitch at which the best sort of
// detection is happening.
// You'll hear "bobbling" at low pitches where the threshold is definitely too low.

(
var threshes = (0.1, 0.2 .. 1);
x = {
	var sig, chain, onsets, pips;

	// A simple generative signal
	sig = LPF.ar(Pulse.ar(TIRand.kr(63, 75, Impulse.kr(2)).midicps), LFNoise2.kr(0.5).exprange(100, 10000)) * Saw.ar(2).range(0, 1);
	// or, uncomment this line if you want to play the buffer in
	//sig = PlayBuf.ar(1, d, BufRateScale.kr(d), loop: 1);

	chain = FFT(b, sig);

	onsets = Onsets.kr(chain, threshes, \rcomplex);

	// Generate pips at a variety of pitches
	pips = SinOsc.ar((threshes).linexp(0, 1, 440, 3520), 0, EnvGen.kr(Env.perc(0.001, 0.1, 0.5), onsets)).mean;
	Out.ar(0, Pan2.ar(sig, -0.75, 0.2) + Pan2.ar(pips, 0.75, 1));
}.play;
)

x.free; // Free the synth
[b, d].do(_.free); // Free the buffers
::

]


