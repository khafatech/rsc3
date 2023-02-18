#lang scribble/manual
@(require (for-label racket))

@title{SendPeakRMS}
 Track peak and power of a signal for GUI applications.@section{related}
  Classes/Peak, Classes/PeakFollower, Classes/OSCFunc
@section{categories}
   UGens>Analysis>Amplitude


@section{description}


The SendPeakRMS unit generator computes peak and power of a signal and sends the
computed values back to the clients. It does not produce any output.


@section{CLASSMETHODS}
 

@section{private}
  new1


@section{method}
 ar, kr

Unlike with other unit generators, the 
@racketblock[ar:: and ]

@racketblock[kr:: methods do not
specify the rate of the computation, but the granularity. When the SendPeakRMS ugen
is instantiated with ]

@racketblock[kr::, the reply rate id quantized to control-rate
blocks.

]
@section{argument}
 sig

The input signal.

@section{argument}
 replyRate

Float or Integer. Specifies the number of replies that are sent to the clients
per second.

@section{argument}
 peakLag

Float or Integer. Lag time, which is applied to the peak values. This option is
commonly used for GUI VU meters.

@section{argument}
 cmdName

Symbol or String. Address pattern for reply message.

@section{argument}
 replyID

Integer ID (similar to link::Classes/SendTrig::).

@section{INSTANCEMETHODS}
 
@section{private}
  numOutputs, writeOutputSpecs


@section{EXAMPLES}
 


@racketblock[
(
{
	SendPeakRMS.kr(Dust.ar(20), 20, 3, "/replyAddress")
}.play;
)

(
o = OSCFunc({ |msg|
	"peak: %, rms: %".format(msg[3], msg[4]).postln
}, '/replyAddress');
)
o.free;
::

]


