#lang scribble/manual
@(require (for-label racket))

@title{LevelIndicator}
 a level indicator GUI widget@section{categories}
  GUI>Views
@section{related}
  Classes/RangeSlider

@section{description}

A level indicator view, suitable for use as a level or peak meter, etc.


@section{CLASSMETHODS}
 
@section{private}
 key

@section{INSTANCEMETHODS}
 
@section{private}
 valueAction

@section{METHOD}
  value
Get or set the current level of the view.

@section{argument}
  val
A link::Classes/Float:: between 0 and 1.

@section{returns}
  A link::Classes/Float::

@section{METHOD}
  warning
@section{METHOD}
  critical
Set the warning and critical thresholds. If meter value is above either threshold, link::#-warningColor or link::#-criticalColor will be shown, respectively (by default, yellow and red).
If link::#-drawsPeak:: is true warning color will be displayed based on link::#-peakLevel:: rather than value.

@section{argument}
  val
A link::Classes/Float::.


@racketblock[
a = LevelIndicator(bounds:Rect(10, 10, 20, 160)).front;
a.value = 0.5;
a.warning = 0.6; a.critical = 0.9;
a.value = 0.7;
a.value = 0.9;
::

]
@section{METHOD}
  style
Sets the style of the view.

@section{argument}
  val
An link::Classes/QLevelIndicatorStyle:: \continuous or \led (see link::#-stepWidth::)


@racketblock[
(
w = Window().front.layout_(
	HLayout(
		LevelIndicator().style_(\continuous).value_(1/3),
		LevelIndicator().style_(\led).value_(2/3),
)
)
)
::

]
@section{METHOD}
  stepWidth
The width of each led light, for \led.

@section{argument}
  val
An positive link::Classes/Integer::.


@racketblock[
(
w = Window().front.layout_(HLayout(
	LevelIndicator().style_(\led).value_(0.8).stepWidth_(1),
	LevelIndicator().style_(\led).value_(0.8).stepWidth_(3),
	LevelIndicator().style_(\led).value_(0.8).stepWidth_(10),
	LevelIndicator().style_(\led).value_(0.8).stepWidth_(50),
));
)
::

]
@section{METHOD}
  numSteps
The number of steps used in \led style.

@section{argument}
  val
An positive link::Classes/Integer::.


@racketblock[
(
a = LevelIndicator(bounds:Rect(10, 10, 80, 400)).front();
a.value = 1;
a.style = \led;
a.numSteps = 4;
)
::

]
@section{METHOD}
  image
@section{note}
  Not yet implemented in Qt GUI ::

@section{argument}
  image
An link::Classes/@section{Image}
 . The default image is the SC cube.

@section{METHOD}
  numTicks
The number of ticks to display in the view's scale.

@section{argument}
  number
An link::Classes/Integer:: >= 0.


@racketblock[
(
w = Window(bounds:100@400).front().background_(Color.black);
w.layout_(HLayout(
	LevelIndicator()
		.numTicks_(16)
		.value_(0.75)
))
)
::

]
@section{METHOD}
  numMajorTicks
The number of ticks in the view's scale which will be large sized.

@section{argument}
  number
An link::Classes/Integer:: >= 0.


@racketblock[
(
w = Window(bounds:100@400).front().background_(Color.black);
w.layout_(HLayout(
	LevelIndicator()
		.numMajorTicks_(16)
		.numTicks_(16)
		.value_(0.75)
))
)
::

]
@section{METHOD}
  drawsPeak
Determines whether the view draws a separate peak display. This can be useful for displaying both peak and RMS values. If drawsPeak is true link::#-@section{warning}
  and link::#-critical:: will be displayed based on link::#-peakLevel:: rather than value.

@section{argument}
  bool
A link::Classes/Boolean::. By default the peak is not drawn.


@racketblock[
(
w = Window().front().layout_(HLayout(
	LevelIndicator().style_(\continuous).value_(0.75).drawsPeak_(true).peakLevel_(0.9),
	LevelIndicator().style_(\led).value_(0.75).drawsPeak_(true).peakLevel_(0.9)
))
)
::

]
@section{METHOD}
  peakLevel
Sets the level of the peak display. (See link::#-drawsPeak::.)

@section{argument}
  val
A link::Classes/Float::.


@racketblock[
(
w = Window().front().layout_(HLayout(
	LevelIndicator().style_(\continuous).value_(0.1).drawsPeak_(true).peakLevel_(0.3),
	LevelIndicator().style_(\continuous).value_(0.1).drawsPeak_(true).peakLevel_(0.5),
	LevelIndicator().style_(\continuous).value_(0.1).drawsPeak_(true).peakLevel_(0.7),
	LevelIndicator().style_(\continuous).value_(0.1).drawsPeak_(true).peakLevel_(0.9),
))
)
::

]
@section{METHOD}
  meterColor
@section{METHOD}
  warningColor
@section{METHOD}
  criticalColor
Sets the color of the meter, as well as the warning and critical colors.

@section{argument}
  color
A link::Classes/Color::.


@racketblock[
(
l = LevelIndicator(bounds:Rect(100, 100, 100, 400)).front().value_(1).style_(\led);
l.meterColor = Color.blue(0.9);
l.warningColor = Color.blue(0.7);
l.criticalColor = Color.blue(0.5);
)
::
]

@racketblock[
(
// inverse
l.background = Color.blue;
l.meterColor = Color.black.alpha_(1);
l.warningColor = Color.black.alpha_(1);
l.criticalColor = Color.black.alpha_(0.3);
)
::


]
@section{EXAMPLES}
 


@racketblock[
(
// something to meter
s.waitForBoot({
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

	x = {
		var colum, noise, imp, delimp, mul = 1;
	imp = Impulse.kr(10);
	delimp = Delay1.kr(imp);
	colum = PlayBuf.ar(1, b, BufRateScale.kr(b), loop: 1) * mul;
	// measure rms and Peak
	SendReply.kr(imp, '/levels', [Amplitude.kr(colum), K2A.ar(Peak.ar(colum, delimp).lag(0, 3))]);
	colum;
}.play;

	a = LevelIndicator(bounds:Rect(100, 100, 100, 400)).front;
	a.onClose_({ x.free; o.free; });
o = OSCFunc({arg msg;
	{
		a.value = msg[3].ampdb.linlin(-40, 0, 0, 1);
		a.peakLevel = msg[4].ampdb.linlin(-40, 0, 0, 1);
	}.defer;
}, '/levels', s.addr);
})
)

(
a.warning = -6.dbamp;
a.critical = -3.dbamp;
)
// optionally show peak level
a.drawsPeak = true;

(
a.style = \led;
a.stepWidth = 3;
)

// different colors
(
a.meterColor = Color.blue(0.9);
a.warningColor = Color.blue(0.8);
a.criticalColor = Color.blue(0.6);
)
// all styles can have ticks
(
a.background = Color.clear;
a.numTicks = 11; // includes 0;
)

// Single blinking square level indicator
(
a.style = \led;
a.numTicks = 0;
a.drawsPeak = false;
a.bounds = a.bounds.resizeTo(90, 90);
a.numSteps = 1;
)


::
]


