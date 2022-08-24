#lang scribble/manual
@(require (for-label racket))

@title{Loudness}
 Extraction of instantaneous loudness in sones@section{categories}
  UGens>Analysis>Amplitude
@section{related}
  Classes/BeatTrack, Classes/MFCC, Classes/Onsets, Classes/Pitch, Classes/KeyTrack

@section{description}

A perceptual loudness function which outputs loudness in sones; this is a variant of an MP3 perceptual model, summing excitation in ERB bands. It models simple spectral and temporal masking, with equal loudness contour correction in ERB bands to obtain phons (relative dB), then a phon to sone transform. The final output is typically in the range of 0 to 64 sones, though higher values can occur with specific synthesised stimuli.


@section{classmethods}
 
@section{method}
  kr

@section{argument}
  chain
[fft] Audio input to track, which has been pre-analysed by the link::Classes/FFT:: link::Classes/UGen::; see examples below for the expected FFT size.

@section{argument}
  smask
[sk] Spectral masking param: lower bins mask higher bin power within ERB bands, with a power falloff (leaky integration multiplier) of smask per bin.

@section{argument}
  tmask
[sk] Temporal masking param: the phon level let through in an ERB band is the maximum of the new measurement, and the previous minus tmask phons.

@section{examples}
 

@racketblock[
// assumes hop of half fftsize, fine
b = Buffer.alloc(s, 1024, 1); // for sampling rates 44100 and 48000
//b = Buffer.alloc(s, 2048, 1); // for sampling rates 88200 and 96000
d = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

// analyse loudness and poll result
(
{
var in, fft, loudness;

in = PlayBuf.ar(1, d, BufRateScale.kr(d), 1, 0, 1);

fft = FFT(b, in);

loudness = Loudness.kr(fft).poll(50);

Out.ar(0, Pan2.ar(in));
}.play
)


// TESTS
// sones = 2 ** ((phon - 40) / 10)
// sine of 40 dB = 40 phon at 1000 kHz = 1 sone
// full amp = 100 dB
// -60.dbamp = 0.001 = 1 sone
// -40.dbamp = 0.01 = 4 sone
// -20.dbamp= 0.1 = 16 sone
// 0.dbamp= 1 = 64 sone
(
{
var in, fft, loudness;

in = SinOsc.ar(1000, 0, 0.001); //should be 1 sone
//in = SinOsc.ar(1000, 0, 0.01); //should be 4 sone
//in = SinOsc.ar(1000, 0, 0.1); //should be 16 sone
//in = SinOsc.ar(1000, 0, 1); //should be 64 sone
//in = Saw.ar * SinOsc.ar(4);
//in = WhiteNoise.ar;
//in = Silent.ar; // should be small, around 2 ** ((0 - 40) / 10) = 2 ** (-4) = 0.0625
//in = DC.ar(1);
//in = SinOsc.ar(22050, pi * 0.5, 1);
// fade ins
//in = SinOsc.ar(1000, 0, Line.kr(0, 1, 2));
//in = SinOsc.ar(1000, 0, Line.kr(0, 1, 2) ** 2);
//in = WhiteNoise.ar(Line.kr(0, 1, 2));
//in = PlayBuf.ar(1, d, BufRateScale.kr(d), 1, 0, 1);

fft = FFT(b, in);

loudness = Loudness.kr(fft, 0.25, 6).poll(50);

Out.ar(0, Pan2.ar(in));
K2A.ar(loudness * 0.016)
}.plot(2.0)
)
::

Research note: This link::Classes/UGen:: is an informal juxtaposition of perceptual coding, and a Zwicker and Glasberg/Moore/Stone loudness model.
]


