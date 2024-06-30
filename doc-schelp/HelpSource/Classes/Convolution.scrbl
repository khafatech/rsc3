#lang scribble/manual
@(require (for-label racket))

@title{Convolution}
 Real-time convolver.@section{related}
  Classes/Convolution2, Classes/Convolution2L, Classes/Convolution3
@section{categories}
   UGens>FFT, UGens>Convolution


@section{description}


Strict convolution of two continuously changing inputs. Also see
link::Classes/Convolution2::  for a cheaper CPU cost alternative for the
case of a fixed kernel which can be changed with a trigger message.


See also  link::http://www.dspguide.com/ch18.htm::  by Steven W.
Smith.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in

Processing target.


@section{argument}
 kernel

Processing kernel.


@section{argument}
 framesize

Size of FFT frame, must be a power of two (512, 1024, 2048, 4096 are standard choices).
Convolution uses twice this number internally.
Note that the convolution gets progressively more expensive to run for higher powers!
The maximum value you can use is 2^16=16384.
(This upper limit is half of "SC_FFT_MAXSIZE" defined in the SC source code.)
Larger convolutions than this can be done using link::Classes/PartConv::.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
(

	{ var input, kernel;

	input=SoundIn.ar(0);
	kernel= Mix.ar(LFSaw.ar([300,500,800,1000]*MouseX.kr(1.0,2.0),0,1.0));

	//must have power of two framesize
	Out.ar(0,Convolution.ar(input,kernel, 1024, 0.5));
	 }.play;

)

(
//must have power of two framesize- FFT size will be sorted by Convolution to be double this
//maximum is currently a=8192 for FFT of size 16384
a=2048;
s = Server.local;
//kernel buffer
g = Buffer.alloc(s,a,1);
)

(
//random impulse response
g.set(0,1.0);
100.do({arg i; g.set(a.rand, 1.0.rand)});


	{ var input, kernel;

	input=SoundIn.ar(0);
	kernel= PlayBuf.ar(1,g.bufnum,BufRateScale.kr(g.bufnum),1,0,1);

	Out.ar(0,Convolution.ar(input,kernel, 2*a, 0.5));
	 }.play;

)
::

]

