#lang scribble/manual
@(require (for-label racket))

@title{EnvirGui}
 display the contents of an environment for editing@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/EZText, Classes/TdefGui, Classes/PdefGui

@section{description}

EnvirGui displays all keys and values of an environment, so one can change them flexibly. Single number get displayed with an link::Classes/EZSlider::, pairs of numbers with an link::Classes/EZRanger::, and anything else is shown as an link::Classes/EZText:: (a text field).

@section{ClassMethods}
 

@section{subsection}
 Creation

@section{method}
 new
create a new EnvirGui
NdefParamGui

@racketblock[
// simple example
g = EnvirGui.new(nil, 5);    // empty with 5 slots
g.parent.alwaysOnTop_(true);
g.object_((a: 1, b: \werty, freq: [500, 2000]));	// put some things in
g.envir.put(\karl1, \otto1);				// one more
g.envir.put(\caesar, \julius);				// one more

g.envir.putAll((b: -12, r: 1, s: 2, t: 3, u: 4, v: 5))

g.object_((x: 2));	// put another object in

g.envir.putAll((b: -12, r: 1, s: 2, t: 3, u: 4, v: 5))

g.envir.removeAt(\b)
g.envir.removeAt(\r)
g.envir.removeAt(\s)
g.envir.removeAt(\t)
g.envir.removeAt(\u)
g.envir.removeAt(\v)
g.close;
::

]
@section{argument}
 object
the envir to display

@section{argument}
 numItems
the number of items to display. If an envir is given, and no num, num is envir.size.

@section{argument}
 parent
the parent view to display in; if none is given, a new window is created.

@section{argument}
 bounds
the bounds within which to display; if none is given, bounds are calculated.

@section{argument}
 makeSkip
flag whether to make a skipjack to manage updates of the envirgui.

@section{argument}
 options
configuration options

@section{InstanceMethods}
 

@section{subsection}
 Instance Variables

@section{method}
 numItems
how many envir items to display

@section{method}
 envir
the envir displayed - actually an alias for object.

@section{method}
 zone
the composite view the envirgui makes for itself


@section{method}
 paramViews
the paramViews that display the values:
@section{list}
 
## Single numbers appear in an link::Classes/EZSlider::,
## pairs of numbers will be shown in an link::Classes/EZRanger::,
## all other values are shown as compileStrings in an link::Classes/EZText::.
::
See link::Classes/ParamView:: for details.

@section{method}
 specs
a local dictionary of the specs used for display ranges of numerical parameters by the paramViews of this envirgui. See the link::#-getSpec:: method for details.

@section{method}
 editKeys
the keys of the currently displayed items in the dict.

@section{method}
 keysRotation
if the size of envir exceeds numItems, the keys displayed can be rotated: e.g. with 10 keys displayed on 5 paramViews, keysRotation 0 means show keys (0..4), keysRotation 2 means show keys (2..6), etc.

strong::gui elements present if requested in options::
@section{method}
  docBut, knowBut, parentBut, protoBut

@section{subsection}
 Some Methods

@section{method}
 object
set the environment to show

@section{argument}
 obj
can be nil, a dictionary, an environment, or an event.


@racketblock[
g = EnvirGui((freq: 120, \amp: 0.2, \pan: -0.5), 12, nil, bounds: Rect(20, 400, 220, 100));
g.object_((a: 1, b: [2, 3], c: \symbol, d: [4, 5, 6], f: { "boing".postln }))
::

]
@section{method}
 envir
same as object_(obj)

@section{method}
 name
if in its own window, set the window's name


@racketblock[
g.name_("Yoohoo");
::

]
@section{method}
 getSpec
For editing, numerical parameters need control specs for the ranges on the gui. These can be set locally in the EnvirGui, or global specs will be looked up. If no local or global specs exist for that parameter name, getSpec makes a usable guess for them. (With JITLibExtensions, one can also look up specs attached to an object such as a proxy.)


@racketblock[
// inline example
g = EnvirGui.new; g.parent.alwaysOnTop_(true);
g.getSpec(\freq, 400);		// \freq exists as global spec, so use that
g.object_((freq: 150));

g.getSpec(\iFrek, 500);		// no global spec, so make a new one:
				// exponential from val * 0.05 to val * 20;
g.specs;			// and keep it here
g.envir.put(\iFrek, 500);
::

]
@section{argument}
 key
the parameter name for which to find a spec

@section{argument}
 value
the current value of that param, which is used when guessing specs.

@section{method}
 putSpec
add a spec for a given key, or (if it is a global key) override a global spec with a local one:


@racketblock[
// set a desired range and warp:
g.putSpec(\iFrek, [10, 1000, \exp]);
g.putSpec(\freq, [10, 1000, \exp]);
g.specs;
g.putSpec(\freq, \freq);

// specs are remembered when object changes
g.putSpec(\freq, [10, 1000, \exp]);
g.object_((freq: 200, iFrek: 20));

// if needed, clear specs by hand when switching objects
g.specs.clear; g.object_((freq: 200, iFrek: 20));

g.close
::

]
@section{method}
 replaceKeys, addReplaceKey, removeReplaceKey, showKeyFor
keys with technical names can be replaced in the display with clearer names.

e = (longFreq: 123, amp: 0.25);
g = EnvirGui(e); g.parent.alwaysOnTop_(true);

g.editKeys;
g.addReplaceKey(\longFreq, \freq1, [50, 500, \exp]);
g.viewForParam(\longFreq).visible_(false);
g.viewForParam(\freq1).visible_(true);

e.put(\z, 345);
e.put(\a, 34);

g.replaceKeys
g.removeReplaceKey(\longFreq)

@section{method}
 highlight, unhighlight, colorizeArea
methods for visually emphasizing areas.

g.highlight(0, ">>_", Color.red(1, 0.5));
g.unhighlight(0);

@section{subsection}
 Some internal methods


@section{method}
 setByKeys
update the widgets for the current keys


@section{method}
 showFields
show (num) active fields, make others invisible.

@section{method}
 useRanger
set and get whether arrays of 2 number values should be displayed with EZRangers.


strong::methods that make gui elements: ::
@section{method}
 makeViews, makeNameView, makeOptionalViews, makeKnowBut, makeDocBut, makeClrBut, makeProtoBut, makeParentBut

strong::standard JITGui methods: ::
@section{method}
 accepts, setDefaults, getState, checkUpdate, updateButtons

@section{private}
  clearFields, updateViewSpecs, viewForParam

@section{Examples}
 


@racketblock[
	// Setting envir variables in a Tdef:
(
Tdef(\text).set(\note, [0, 2, 7], \dur, { [0.1, 0.2, 0.4].choose }, \pan, 0, \amp, 0.1);

w = Window("EZTexts", Rect(200, 400, 304, 120)).front;
w.addFlowLayout;

TdefGui(Tdef(\text), 0, parent: w);

e = EnvirGui(Tdef(\text).envir, 4, parent: w);

Tdef(\text, { |ev|
	var mydur;
	loop {
		mydur = ev.dur;
		(note: ev.note, dur: mydur, amp: ev.amp, pan: ev.pan).postln.play;
		mydur.wait;
	}
}).play;
)

	// or equivalently, use the built-in EnvirGui in TdefGui:
TdefGui(Tdef(\text), 8);

Tdef(\text).set(\yuhu, Prand([2, 3, 5, 8, 13], inf), \magic, [\abra, \cadabra]);

Tdef(\text).clear;
Tdef(\text).envir.clear;
::
]


