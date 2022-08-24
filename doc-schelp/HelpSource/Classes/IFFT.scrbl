#lang scribble/manual
@(require (for-label racket))

@title{IFFT}
 Inverse Fast Fourier Transform@section{related}
  Classes/FFT, Guides/FFT-Overview
@section{categories}
   UGens>FFT


@section{description}

The inverse fast fourier transform converts from frequency content to a
signal.

The fast fourier transform analyzes the frequency content of a signal. The IFFT UGen converts this emphasis::frequency-domain:: information back into emphasis::time-domain:: audio data. Most often this is used as the end of a process which begins with link::Classes/FFT::, followed by frequency-domain processing using PV (phase-vocoder) UGens, followed by IFFT.

@section{classmethods}
 

@section{method}
 new, ar, kr
returns a time domain signal from converting the FFT frequency domain signal chain. The *new method is equivalent to the *ar message returns an audio rate signal.

@section{argument}
 buffer

The FFT "chain" signal coming originally from an FFT UGen, perhaps via other PV UGens.

@section{argument}
  wintype
Defines how the data is windowed:
@section{table}
 
## -1 || strong::rectangular:: windowing, simple but typically not recommended;
## 0 || (the default) strong::Sine:: windowing, typically recommended for phase-vocoder work;
## 1 || strong::Hann:: windowing, typically recommended for analysis work.
::

@section{argument}
  winsize
Can be used to account for zero-padding, in the same way as the link::Classes/FFT:: UGen.

@section{returns}
 
The emphasis::time-domain:: audio signal.

@section{discussion}
 
The IFFT UGen converts the FFT data in-place (in the original FFT buffer) and overlap-adds the result to produce a continuous signal at its output.

@section{Examples}
 


@racketblock[

// without any modification, convert FFT chain (frequency domain signal) back to audio (time domain signal)
(
{   var in, chain;
    in = WhiteNoise.ar;
    chain = FFT(LocalBuf(2048), in);
    IFFT.ar(chain) * -20.dbamp // inverse FFT
}.play;
)

::

]


