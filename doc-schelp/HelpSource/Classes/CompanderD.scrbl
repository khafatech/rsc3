#lang scribble/manual
@(require (for-label racket))

@title{CompanderD}
 Compressor, expander, limiter, gate, ducker.@section{related}
  Classes/Amplitude, Classes/Compander, Classes/Normalizer, Classes/Limiter
@section{categories}
   UGens>Dynamics


@section{description}


CompanderD passes the signal directly to the control input, but
adds a delay to the process input so that the lag in the gain clamping
will not lag the attacks in the input sound.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in
The signal to be compressed / expanded / gated.

@section{argument}
 thresh
Control signal amplitude threshold, which determines the break point between slopeBelow and slopeAbove. Usually 0..1. The control signal amplitude is calculated using RMS.

@section{argument}
 slopeBelow
Slope of the amplitude curve below the threshold. If this slope > 1.0, the amplitude will drop off more quickly the softer the control signal gets; when the control signal is close to 0 amplitude, the output should be exactly zero -- hence, noise gating. Values < 1.0 are possible, but it means that a very low-level control signal will cause the input signal to be amplified, which would raise the noise floor.

@section{argument}
 slopeAbove
Same thing, but above the threshold. Values < 1.0 achieve compression (louder signals are attenuated); > 1.0, you get expansion (louder signals are made even louder). For 3:1 compression, you would use a value of 1/3 here.

@section{argument}
 clampTime
The amount of time it takes for the amplitude adjustment to kick in fully. This is usually pretty small, not much more than 10 milliseconds (the default value). I often set it as low as 2 milliseconds (0.002).

@section{argument}
 relaxTime
The amount of time for the amplitude adjustment to be released. Usually a bit longer than clampTime; if both times are too short, you can get some (possibly unwanted) artifacts.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Discussion}
 
If any of this is confusing, see http://en.wikipedia.org/wiki/Audio_level_compression



