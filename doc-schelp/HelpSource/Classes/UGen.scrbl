#lang scribble/manual
@(require (for-label racket))

@title{UGen}
 Abstract superclass of all unit generators@section{categories}
  UGens, Server>Abstractions
@section{related}
  Browse#UGens, Guides/Tour_of_UGens, Guides/UGens-and-Synths

@section{description}


UGens represent calculations with signals. They are the basic building blocks of synth definitions on the server, and are used to generate or process both audio and control signals. The many subclasses of UGen are the client-side representations of unit generators, and are used to specify their parameters when constructing synth definitions (see link::Classes/SynthDef::).

@section{subsection}
  Interface
All UGens respond to one or more of the following class methods:
@section{list}
 
## 
@racketblock[ ar(arg1, arg2, ... ) ::
## ]

@racketblock[ kr(arg1, arg2, ... ) ::
## ]

@racketblock[ ir(arg1, arg2, ... ) ::
::

They return a new instance of UGen that calculates at audio/control rate or at initialization only (ir). Some UGens, like link::Classes/Rand::, use the ]

@racketblock[*new:: method instead. These methods are implemented in subclasses, where argument names and their meaning depend on the case.

If any argument is an array, they return an array of UGens ( see: link::Guides/Multichannel-Expansion:: ). If the combination of rates between arguments and ugen are not allowed, calling the methods will throw an error. This method adds the UGen to the current SynthDef, so it only fully works inside a UGen function.
]

@racketblock[
{ Blip.ar(Blip.kr(4, 5, 500, 60), 59, 0.1) }.play;
::

]
@section{subsection}
  Documentation of mul and add arguments

A great number of UGens take arguments for 
@racketblock[mul:: and ]

@racketblock[add:: in their ]

@racketblock[*ar:: and ]

@racketblock[*kr:: methods. Because these arguments are so ubiquitous, they are not general documented in the individual help files.
Mul and add simply refer to a constant or signal by which to multiply the output of the UGen, and a constant or signal to add to the output of the UGen. (mul happens before add.) They thus correspond in many cases to scaling the amplitude of the UGen signal in the case of mul, and adding a constant or DC offset in the case of add.
In most cases the defaults for mul and add are 1 and 0 respectively, and they are commonly implemented using a automatically generated link::Classes/MulAdd:: UGen for efficiency. See also the ]

@racketblock[range:: and ]

@racketblock[madd:: methods below.

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
  buildSynthDef
@section{Returns}
  the SynthDef in which the UGen is situated.
@section{Discussion}
 

@racketblock[
{ UGen.buildSynthDef.dump; Silent.ar }.play;
::

]
@section{subsection}
  Internally used class methods

@section{method}
  multiNew
These methods are responsible for multichannel expansion. They call 
@racketblock[*new1(rate, ...args):: for each parallel combination. Most ]

@racketblock[*ar/*kr:: methods delegate to link::#*multiNew]
@section{List}
 .
@section{argument}
  ... args
The first argument is rate, then the rest of the arguments. 
@racketblock[(rate, ...args)::

]
@section{method}
  multiNewList
See link::#*multiNew::.
@section{argument}
  args
An array where the first argument is rate, then the rest of the arguments. 
@racketblock[([rate, ...args])::

]
@section{method}
  new1
This method returns a single instance of the UGen, not multichannel expanded. It is called inside multiNewList, whenever a new single instance is needed.

@section{method}
  methodSelectorForRate
@section{argument}
  rate
A link::Classes/Symbol::, 
@racketblock[ \audio, \control, \scalar ::
]
@section{Returns}
 
An appropriate message selector ( link::Classes/Symbol:: like 
@racketblock[ \ar, \kr, \ir :: ) for the given rate.

]
@section{method}
  replaceZeroesWithSilence
@section{Returns}
 
A new link::Classes/Array::, where every zero is replaced by a link::Classes/Silent:: UGen.
@section{discussion}
 
This is required internally sometimes for UGens like link::Classes/Out::.

@section{instancemethods}
 

@section{subsection}
  Convenience Methods

@section{method}
  scope

Displays the output of this UGen in an individual link::Classes/Stethoscope:: window.
@section{argument}
 name
The name of the window
@section{argument}
 bufsize
Buffer size
@section{argument}
 zoom
Zoom factor

@section{discussion}
 

@racketblock[
s.boot

{ Ringz.ar(PinkNoise.ar([0.1, 0.2]).scope(\pink), 2000, 1, 0.25) }.play; // multichannel works
s.scope; // can still separately scope the output of the server
::

]
@section{method}
  poll

Polls the output of this UGen every interval seconds, and posts the result.

@section{argument}
 trig
Trig frequency
@section{argument}
 label
A symbol to label the output
@section{argument}
 trigid
Numerical ID
@section{discussion}
 
The default trig is 10, which converts to 10 triggers per second (or every 0.1 seconds). See link::Classes/Poll:: for more info on polling.

@racketblock[
{ SinOsc.ar(LFNoise0.ar(2).range(420, 460).poll(label: \LFNoise), 0, 0.2) }.play;

// multichannel polling:
(
{
    var freqs = SinOsc.ar([0.2, 0.3]).range(420, 460);
    freqs.poll(label: [\freq1, \freq2]);
    SinOsc.ar(freqs, 0, 0.2);
}.play;
)
::


]
@section{method}
  dpoll

Like 
@racketblock[poll::, only that ]

@racketblock[dpoll:: is used for Demand ugens. See link::Classes/Poll:: for more info on polling.

]
@section{method}
  range

Scales the output of this UGen to be within the range of 
@racketblock[lo:: and ]

@racketblock[hi::.
]
@section{discussion}
 
Note that 
@racketblock[range:: expects the default output range, and thus should not be used in conjunction with mul and add arguments.
]

@racketblock[
{ SinOsc.ar(SinOsc.ar(0.3).range(440, 660), 0, 0.5) * 0.1 }.play;
::

]
@section{method}
  exprange

Maps the output of this UGen exponentially to be within the range of 
@racketblock[lo:: and ]

@racketblock[hi:: using a link::Classes/LinExp:: UGen.
]
@section{discussion}
 

@racketblock[lo:: and ]

@racketblock[hi:: should both be non-zero and have the same sign. Note that ]

@racketblock[exprange:: expects the default output range, and thus should not be used in conjunction with mul and add arguments.
]

@racketblock[
{ SinOsc.ar(SinOsc.ar(0.3).exprange(440, 6600), 0, 0.5) * 0.1 }.play;
::

]
@section{method}
  curverange

Scales the output of this UGen to be within the range of 
@racketblock[lo:: and ]

@racketblock[hi:: using a curve factor of ]

@racketblock[curve::.
]
@section{discussion}
 
Note that 
@racketblock[curverange:: expects the default output range, and thus should not be used in conjunction with mul and add arguments.
]

@racketblock[
{ SinOsc.ar(SinOsc.ar(0.3).curve(440, 660, -3), 0, 0.5) * 0.1 }.play;
::



]
@section{method}
  unipolar

Scales the output of this UGen to be between 
@racketblock[(0..mul):: range (default 1).
]
@section{discussion}
 
Note that 
@racketblock[unipolar:: expects the default output range, and thus should not be used in conjunction with mul and add arguments.
]

@racketblock[
{ SinOsc.ar(300, 0, 0.5) * SinOsc.kr(2).unipolar * 0.1 }.play;
::

]
@section{method}
  bipolar

Scales the output of this UGen to be between 
@racketblock[(-mul..mul):: range (default 1).
]
@section{discussion}
 
Note that 
@racketblock[bipolar:: expects the default output range, and thus should not be used in conjunction with mul and add arguments.
]

@racketblock[
{ SinOsc.ar(500 + LFPulse.ar(4).bipolar(40), 0, 0.5) * 0.1 }.play;
::

]
@section{method}
  clip
Wraps the receiver in a link::Classes/Clip:: UGen, clipping its output at 
@racketblock[lo:: and ]

@racketblock[hi::.

]
@section{method}
  fold
Wraps the receiver in a link::Classes/Fold:: UGen, folding its output at 
@racketblock[lo:: and ]

@racketblock[hi::.

]
@section{method}
  wrap
Wraps the receiver in a link::Classes/Wrap:: UGen, wrapping its output at 
@racketblock[lo:: and ]

@racketblock[hi::.

]
@section{method}
  blend
Blends 
@racketblock[this:: with ]

@racketblock[that:: by wrapping the receiver in an link::Classes/XFade2:: (if ]

@racketblock[this:: or ]

@racketblock[that:: are audio-rate UGens) or link::Classes/LinXFade2:: UGen.
]
@section{note}
  The 
@racketblock[blendFrac:: argument is between 0 and 1::


]
@section{method}
  lag
Wraps the receiver in a link::Classes/Lag:: UGen, smoothing its output by 
@racketblock[t1:: seconds lagtime. If a second argument is given, it wraps it in a link::Classes/LagUD:: UGen.

]
@section{method}
  lag2
Wraps the receiver in a link::Classes/Lag2:: UGen, smoothing its output by 
@racketblock[t1:: seconds lagtime. If a second argument is given, it wraps it in a link::Classes/Lag2UD:: UGen.

]
@section{method}
  lag3
Wraps the receiver in a link::Classes/Lag3:: UGen, smoothing its output by 
@racketblock[t1:: seconds lagtime. If a second argument is given, it wraps it in a link::Classes/Lag3UD:: UGen.

]
@section{method}
  lagud
Wraps the receiver in a link::Classes/LagUD:: UGen, smoothing its output by 
@racketblock[lagtimeU:: and ]

@racketblock[lagtimeD::.

]
@section{method}
  lag2ud
Wraps the receiver in a link::Classes/Lag2UD:: UGen, smoothing its output by 
@racketblock[lagtimeU:: and ]

@racketblock[lagtimeD::.

]
@section{method}
  lag3ud
Wraps the receiver in a link::Classes/Lag3UD:: UGen, smoothing its output by 
@racketblock[lagtimeU:: and ]

@racketblock[lagtimeD::.

]
@section{method}
  varlag
Wraps the receiver in a link::Classes/VarLag:: UGen, smoothing its output by 
@racketblock[time:: seconds.

]
@section{method}
  slew
Wraps the receiver in a link::Classes/Slew:: UGen, limiting the slope of its output.

@section{method}
  degreeToKey

Wraps the receiver in a link::Classes/DegreeToKey:: UGen.

@section{method}
  minNyquist

Wraps the receiver in a 
@racketblock[min:: link::Classes/BinaryOpUGen::, such that the lesser of the receiver's output and the Nyquist frequency is output. This can be useful to prevent aliasing.

]
@section{method}
  linlin
Wraps the receiver so that a linear input range is mapped to a linear output range.

@section{discussion}
 
The clip argument can be one of the four:
@section{table}
 
## 
@racketblock[nil:: || do not clip at outMin or outMax
## ]

@racketblock[\minmax:: || clip at outMin or outMax
## ]

@racketblock[\min:: || clip at outMin
## ]

@racketblock[\max:: || clip at outMax
::
Example:
]

@racketblock[
{ Line.ar(-1, 5, 0.1).linlin(0, 3, -1, 1) }.plot(0.1);

// modulate some values
(
{ Line.ar(-1, 5, 0.1).lincurve(SinOsc.ar(100), SinOsc.ar(130) + 3, -1, 1, clip: nil) }
    .plot(0.1, minval: -15, maxval: 5)
)
::

]
@section{method}
  linexp

Wraps the receiver so that a linear inputrange is mapped to an exponential output range.
@section{discussion}
 
outMin and outMax must be nonzero and of the same sign. For clip argument, see 
@racketblock[linlin:: above.
]

@racketblock[
{ Line.ar(-1, 5, 0.1).linexp(0, 3, 0.01, 1) }.plot(0.1);
::

]
@section{method}
  explin

Wraps the receiver so that an exponential inputrange is mapped to a linear output range.
@section{discussion}
 
inMin and inMax must be nonzero and of the same sign. For clip argument, see 
@racketblock[linlin:: above.
]

@racketblock[
{ Line.ar(1, 5, 0.1).explin(1, 3, -1, 1) }.plot(0.1);
::

]
@section{method}
  expexp

Wraps the receiver so that an exponential inputrange is mapped to an exponential output range.
@section{discussion}
 
outMin, outMax, inMin and inMax must be nonzero and of the same sign. For clip argument, see 
@racketblock[linlin:: above.
]

@racketblock[
{ Line.ar(1, 5, 0.1).expexp(1, 3, 0.01, 1) }.plot(0.1);
::

]
@section{method}
  lincurve

Wraps the receiver so that a linear inputrange is mapped to a curve-like exponential output range.
@section{discussion}
 
outMin and outMax may be zero and of the different sign. For clip argument, see 
@racketblock[linlin:: above.
]

@racketblock[
{ Line.ar(-1, 5, 0.1).lincurve(0, 3, -1, 1, curve: -4) }.plot(0.1);

// modulate the curve. Unlike with numbers and CurveSpec, the curve absolute value
// should not be much smaller than 0.5.
{ SinOsc.ar(100).lincurve(-1, 1, -1, 1, XLine.kr(-3, -100, 0.1)) * 0.1 }.plot(0.1);
::

]
@section{method}
  curvelin
Wraps the receiver so that a  curve-like exponential inputrange is mapped to a linear output range.
@section{discussion}
 
inMin and inMax may be zero and of the different sign. For clip argument, see 
@racketblock[linlin:: above.
]

@racketblock[
{ Line.ar(-1, 5, 0.1).curvelin(0, 3, -1, 1, curve: -4) }.plot(0.1);
::


]
@section{method}
  bilin
Map the receiver from two assumed linear input ranges (inMin..inCenter) and (inCenter..inMax) to two linear output ranges (outMin..outCenter) and (outCenter..outMax). If the input exceeds the input range, the following behaviours are specified by the clip argument.
@section{argument}
 inCenter
@section{argument}
 inMin
assumed input minimum
@section{argument}
 inMax
assumed input maximum
@section{argument}
 outCenter
@section{argument}
 outMin
output minimum
@section{argument}
 outMax
output maximum
@section{argument}
 clip
nil (don't clip)
\max (clip ceiling)
\min (clip floor)
\minmax (clip both - this is default).
@section{discussion}
 

@racketblock[
{Line.kr(0, 10, 0.1).bilin(1, 0, 10, 0.6, 0, 1)}.plot(0.1)
::


]
@section{method}
  prune
Limits the receiver range to one of the four clip modes, see 
@racketblock[linlin:: above.

]
@section{method}
  checkBadValues
Wraps the receiver in a link::Classes/CheckBadValues:: UGen with the corresponding 
@racketblock[id:: and ]

@racketblock[post:: flag.

]
@section{method}
  if
Outputs trueUGen when the receiver outputs 1, falseUGen when the receiver outputs 0. If the receiver outputs a value between 0 and 1, a mixture of both will be played.
@section{discussion}
 
This is implemented as: 
@racketblock[(this * (trueUGen - falseUGen)) + falseUGen)::

Note that both trueUGen and falseUGen will be calculated regardless of whether they are output, so this may not be the most efficient approach.
]

@racketblock[
// note different syntax in these two examples
{ if( LFNoise1.kr(1.0, 0.5, 0.5) , SinOsc.ar, Saw.ar ) * 0.1 }.play;

{ Trig1.ar(Dust.ar(3), 0.2).lag(0.1).if(FSinOsc.ar(440), FSinOsc.ar(880)) * 0.1 }.play;
::

]
@section{method}
  @
Dynamic geometry support. Returns 
@racketblock[Point(this, y)::.
]
@section{discussion}
 

@racketblock[
{ (SinOsc.ar(1001) @ SinOsc.ar(1207)).rho }.scope;
::

]
@section{method}
  asComplex
Complex math support. Returns 
@racketblock[Complex(this, 0.0)::.

]
@section{method}
  dumpArgs
Posts a list of the arguments for this UGen and their values.


@section{subsection}
  Other Instance Methods

The following methods and instance variables are largely used in the construction of synth definitions, synth descriptions (see link::Classes/SynthDesc::), UGen class definitions, etc., and are usually not needed for general use.
Users should not attempt to set any of these values in general code.

@section{method}
  synthDef
The SynthDef which contains the UGen.

@section{method}
  inputs
The array of inputs to the UGen.

@section{method}
  rate
The output rate of the UGen which is one of the link::Classes/Symbol::s 
@racketblock[\audio::, or ]

@racketblock[\control::.

]
@section{method}
  signalRange
@section{Returns}
  A symbol indicating the signal range of the receiver. Either 
@racketblock[\bipolar:: or ]

@racketblock[\unipolar::.

]
@section{method}
  numChannels
@section{Returns}
  The number of output channels.
@section{discussion}
 
For a UGen, this will always be 1, but link::Classes/Array:: also implements this method, so multichannel expansion is supported. See link::Guides/Multichannel-Expansion::.

@section{method}
  numInputs
@section{Returns}
  The number of inputs for this UGen.

@section{method}
  numOutputs
@section{Returns}
  The number of outputs for this UGen.

@section{method}
  name
@section{Returns}
  The Class name of the receiver as a link::Classes/String::.

@section{method}
  madd
Wraps the receiver in a link::Classes/MulAdd:: UGen.
@section{discussion}
 
This is for the most part only used in UGen class definitions in order to allow efficient implementation of 
@racketblock[mul:: and ]

@racketblock[add:: arguments.

]
@section{method}
  isValidUGenInput
@section{Returns}
  true

@section{method}
  asUGenInput
@section{Returns}
  the receiver
@section{discussion}
 
This method is implemented in a number of classes in order to allow objects like link::Classes/Node::s, link::Classes/Bus::ses, and link::Classes/Buffer::s to be passed directly as UGen inputs and link::Classes/Synth:: args.

@section{method}
  copy
@section{Returns}
  the receiver.
@section{discussion}
 
Thus UGen-dup effectively returns a reference to the original and is a convenient way to copy a mono signal to multiple channels.

@racketblock[
{ SinOsc.ar(Rand(200, 4000), 0, 0.2).dup }.plot // this is the same UGen
::
Function-dup evaluates that function multiple times, thus potentially returning distinct UGens.
]

@racketblock[
{ { SinOsc.ar(Rand(200, 4000), 0, 0.2) }.dup }.plot // these are different UGens
::


]
@section{subsection}
  Internally used instance methods

@section{method}
  methodSelectorForRate
See link::#*methodSelectorForRate::

@section{method}
  init
By default, this method stores the inputs (e.g. the arguments to 
@racketblock[*ar:: and ]

@racketblock[*kr::) in the UGen.
]
@section{discussion}
 
This may be overridden to do other initialisations, as long as the inputs are set correctly.



