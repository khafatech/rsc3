#lang scribble/manual
@(require (for-label racket))

@title{BEQSuite}
 Base class for B Equalization Suite@section{categories}
  UGens>Filters>BEQSuite

@section{description}


@section{definitionList}
 
## link::Classes/BLowPass:: || 12dB/oct rolloff - 2nd order resonant Low Pass Filter
## link::Classes/BLowPass4:: || 24dB/oct rolloff - 4th order resonant Low Pass Filter (pseudo UGen)
## link::Classes/BHiPass:: || 12dB/oct rolloff - 2nd order resonant Hi Pass Filter
## link::Classes/BHiPass4:: || 24dB/oct rolloff - 4th order resonant Hi Pass Filter (pseudo UGen)
## link::Classes/BPeakEQ:: || Parametric Equalizer
## link::Classes/BLowShelf:: || Low resonant Shelf
## link::Classes/BHiShelf:: || High resonant Self
## link::Classes/BBandPass:: || Band Pass Filter
## link::Classes/BBandStop:: || Band Reject Filter
## link::Classes/BAllPass:: || All Pass Filter
::

The B equalization suite is based on the Second Order Section (link::Classes/SOS::) biquad UGen.

SC3 port and coding - blackrain 06.17.2005

SC3 plugin UGens - Joshua Parmenter 06.22.2005

Some ideas from emphasis::"Cookbook formulae for audio EQ biquad filter coefficients":: by Robert Bristow-Johnson.



