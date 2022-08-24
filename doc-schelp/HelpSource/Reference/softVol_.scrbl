#lang scribble/manual
@(require (for-label racket))

@title{softVol_}
 set a nodeproxy's vol conditionally@section{categories}
  Libraries>JITLib>GUI
@section{related}
  Reference/softSet, Reference/softPut

@section{description}

Extension method to link::Classes/NodeProxy:: to set vol conditionally.

@section{method}
  softVol
setter.

@section{argument}
  val
the volume value to set to
@section{argument}
  within
the normalized range within which the set is accepted
@section{argument}
  pause
a flag whether to pause the nodeproxy when volume is 0.
waits for 0.2 seconds for volume to go down first.
@section{argument}
  lastVal
the previous value that the controller has set - can be
@section{argument}
  spec
a link::Classes/ControlSpec:: can be passed in. if nil, 
@racketblock[\amp.asSpec:: is used.

]
@section{Examples}
 

@racketblock[
Ndef(\test, { |freq=200| Splay.ar(SinOsc.ar(freq * Array.rand(12, 0.95, 1.05))) });
Ndef(\test).play(vol: 0.1);

	// example of softSet, softSet which knows lastVal,
	// softVol_ and softVol_ which knows lastVol:
(
w = Window("softVol", Rect(500, 200, 400, 240)).front;
w.view.addFlowLayout;
NdefGui(Ndef(\test), 2, w);

	// same for volume - not too safe
EZSlider(w, 340@30, \softVol, \amp, { |sl|
	Ndef(\test).softVol_(sl.value, 0.05)
});
	// safer
EZSlider(w, 340@30, \knowLastV, \amp, Routine { |sl|
	var newVal, lastVal;
	loop {
		newVal = sl.value;
		Ndef(\test).softVol_(sl.value, 0.05, lastVal: lastVal);
		lastVal = newVal;
		\dummy.yield;
	}
});
)
::
]


