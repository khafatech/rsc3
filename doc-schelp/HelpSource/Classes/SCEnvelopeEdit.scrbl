#lang scribble/manual
@(require (for-label racket))

@title{SCEnvelopeEdit}
 An envelope editor view@section{categories}
  GUI>Kits>Cocoa
@section{related}
  Classes/EnvelopeView

@section{description}

An editable Envelope view.

@section{subsection}
  Some Important Issues Regarding SCEnvelopeEdit

The breakpoints are color coded as follows:
@section{table}
 
## blue || normal
## red || sustain node
## green || loop node
::


@section{classmethods}
 

@section{method}
  new
@section{argument}
  parent
The parent view.
@section{argument}
  bounds
An instance of link::Classes/Rect::, or a link::Classes/Point:: indicating 
@racketblock[width@height::.
]
@section{argument}
  env
The envelope. An instance of link::Classes/Env::.
@section{argument}
  pointsPerSegment
The resolution in points per segment. Default value is 10.

@section{method}
  paletteExample
@section{argument}
  parent
@section{argument}
  bounds

@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
  viewClass

@section{instancemethods}
 

@section{method}
  refresh
If the link::Classes/Env:: object is modified directly, this needs to be called to update the GUI.

maxLevel
Changes maximum level shown in editor.
@section{argument}
  level
An instance of link::Classes/Float::.

@section{method}
  minLevel
Changes minimum level shown in editor.
@section{argument}
  level
An instance of link::Classes/Float::.

@section{method}
  minTime
Changes minimum time (sec) shown in editor. Negative times are okay because link::Classes/Env:: uses inter-node durations.
@section{argument}
  sec
An instance of link::Classes/Float::. Seconds.

@section{method}
  maxTime
Changes maximum time (sec) shown in editor.
@section{argument}
  sec
An instance of link::Classes/Float::. Seconds.


@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
  defaultMouseDownAction
@section{argument}
  x
@section{argument}
  y
@section{argument}
  modifiers
@section{argument}
  buttonNumber
@section{argument}
  clickCount


@section{method}
  env
@section{argument}
  e

@section{method}
  addBreakPoint
@section{argument}
  level

@section{method}
  insertAtTime
@section{argument}
  time
@section{argument}
  level

@section{method}
  pointsPerSegment

@section{method}
  initSCEnvelopeEdit
@section{argument}
  argEnv
@section{argument}
  argPPS
@section{argument}
  setMinMax

@section{method}
  redraw

@section{method}
  updateAll

@section{method}
  updateSegment
@section{argument}
  segNum

@section{method}
  clear

@section{examples}
 

Make a basic editor:

@racketblock[
(
e = Env([1, 2], [10]);
w = Window("Env Editor", Rect(200, 200, 300, 200));
v = SCEnvelopeEdit(w, w.view.bounds.moveBy(20, 20).resizeBy(-40, -40), e, 20).resize_(5);
w.front;
)

v.addBreakPoint;

(
v.clear;
v.redraw;
v;
)

v.maxLevel_(2); // to give more headroom
v.maxTime_(2); // to increase release point
v.minTime_(-1); // to increase attack time

e.curves_('sin'); // env object is changed
v.refresh; // must refresh editor
::

Controlling a Synth
]

@racketblock[
s.boot;

(
e = Env([0, 1, 0.7, 0.9, 0], [0.03, 0.03, 0.03, 0.03], 'sin');
f = Env([0, 1, 0.7, 0.9, 0], [0.03, 0.03, 0.03, 0.03], 'sin');
w = Window("Shards", Rect(100, 100, 500, 400));
v = SCEnvelopeEdit(w, w.view.bounds.resizeBy(-20, -200), e, 10).resize_(2);
StaticText(w, v.bounds).string_(" amplitude").resize_(2);
x = SCEnvelopeEdit(w, v.bounds.moveBy(0, 200), f, 10).resize_(2);
StaticText(w, x.bounds).string_(" frequency").resize_(2);
w.front;
)

(
SynthDef("sineBlip", {
	arg freq = 440, vol = 0.1, la0, la1, la2, la3, la4, ta0, ta1, ta2, ta3, crva,
		lf0, lf1, lf2, lf3, lf4, tf0, tf1, tf2, tf3, crvf;
	var signal, fenv, aenv;
	fenv = EnvGen.ar(Env([lf0, lf1, lf2, lf3, lf4], [tf0, tf1, tf2, tf3], crvf));
	aenv = EnvGen.ar(Env([la0, la1, la2, la3, la4], [ta0, ta1, ta2, ta3], crva), doneAction: Done.freeSelf);
	signal = SinOsc.ar([freq, freq*2] * fenv) * aenv * vol;
	Out.ar(0, signal.dup);
}).add;
)

(
Routine({
	var par, indices;
	indices = (2..21);
	loop({
		par = (indices +++ (
			v.env.levels ++
			v.env.times ++
			v.env.curves ++
			x.env.levels ++
			x.env.times ++
			x.env.curves)).flatten;
		s.sendBundle(s.latency, [\s_new, "sineBlip", -1, 1, 1, \freq, exprand(4e3,11e3)] ++ par);
		0.04.wait;
	});
}).play;
)
::
]


