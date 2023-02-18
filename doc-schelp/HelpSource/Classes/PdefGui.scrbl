#lang scribble/manual
@(require (for-label racket))

@title{PdefGui}
 a line of editing controls for a Pdef, and optionally its envir@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/PdefAllGui, Classes/TdefGui, Classes/TdefAllGui, Classes/EnvirGui

@section{description}


A gui showing the link::Classes/Pdef::'s name, playing state, source state, and envir state. Optionally, its envir can also be edited.

@section{subsection}
 First example


@racketblock[
g = PdefGui();			// make a PdefGui
g.object = Pdef(\a);		// show when a Pdef is put in
Pdef(\a, Pbind(\note, 12)); 	// show whether it has a source
Pdef(\a).play; 			// show whether playing, stopped, or ended, and pausable
Pdef(\a).set(\dur, 0.25); 	// show whether the Pdef has an envir
g.close;

g = PdefGui(Pdef(\a), 3);	// with an envirgui for 3 items
Pdef(\a).set(\lofreq, [1, 10], \str, "someString", \oops, \oneSymbolTooMany);
Pdef(\a).clear;
Pdef(\a).envir.clear;
g.close;

(				// put it in an existing window - margin is 0@0
w = Window("my win", Rect(200, 200, 300, 200)).front;
w.addFlowLayout;
PdefGui(Pdef(\a), 0, w);
PdefGui(Pdef(\a), 3, w);
)
::

]
@section{subsection}
 Details on the GUI elements

@section{definitionList}
 
## name button
|| when selected, typing the delete key will delete its Pdef.
## play/stop button
|| indicates whether the Pdef is playing:
@section{table}
 
## " >" || if stopped,
## " _" || if playing and active,
## " |" || if it is playing, but the stream has ended.
::
## pause/resume button
|| only visible if one can pause or resume the Pdef, i.e. while it is playing.
@section{table}
 
## "paus" || shown when you can pause it,
## "rsum" || shown when you can resume it.
::
## src button
|| opens a document to edit the source (function) of the Pdef.
@section{table}
 
## green || a source exists,
## white || the source is nil.
::
## env button
|| opens a document to edit the environment of the Pdef, which is where one can keep all variables the Pdef uses for easy access.
@section{table}
 
## green || the Pdef has an envir,
## white || the envir is nil.
::
::

@section{ClassMethods}
 

@section{subsection}
 Creation Methods

@section{method}
 new
Create a new link::Classes/JITGui:: that will be watching an object and display its state.

@section{argument}
 object
the object to watch

@section{argument}
 numItems
the number of display items to use, e.g. how many fields for text, or how many EZSliders for single-number parameters.

@section{argument}
 parent
a parent view on which to display. If nil, a new window is created; strong::parent:: can also be an existing window or a composite view.

@section{argument}
 bounds
a desired size and position where to display a JITGui. can be nil, a link::Classes/Point::, or a link::Classes/Rect::. JITGuis know their minimum size ( strong::minSize:: ), and if bounds is nil, minSize is used. if bounds is a link::Classes/Point:: or link::Classes/Rect::, it will be set to at least minSize. With a rect one can also supply a position where to display. If a point,shown size is the maximum of bounds and minSize.

@section{argument}
 makeSkip
A flag whether to make a skipjack.

@section{argument}
 options
a list of additional information, e.g. flags about optional buttons. (this is used is some subclasses)

@section{InstanceMethods}
 

@section{method}
 object
a link::Classes/Pdef::, or nil

@section{method}
 numItems
the number of items in the envirGui

@section{method}
 parent
the parent view

@section{method}
 bounds
the bounds of the link::#-zone::

@section{method}
 zone
the link::Classes/CompositeView:: within which the PdefGui is shown

@section{method}
 nameBut, playBut, pauseBut, srcBut, envBut
the buttons

@section{method}
 envirGui
the gui for the Pdef's envir - nil if numItems is 0.

@section{method}
 object
put an object in the gui.

@section{method}
 moveTo
(if the jitGui is in its own window)

move it to some specific location.

@section{method}
 clear
(if the jitGui is in its own window)

set the PdefGui's object to nil

@section{method}
 close
(if the jitGui is in its own window)

and close its window.

@section{subsection}
 Internal methods

@section{method}
 srcString
a compileString that recreates the Pdef.


@racketblock[
// assume g from above is still there
g.srcString;
::

]
@section{method}
 editString
a compileString that recreates the Pdef's envir at edKey.

@section{method}
 editStrings
a compileString that recreates the Pdef's envir at edKeys.

@section{argument}
 edKeys
Default value is nil.


@racketblock[
// assume g from above is still there
g.editString;
Pdef(\a).set(\foo, \bar);
g.editString(\foo);

g.editStrings;
::

]
@section{method}
 getUsedKeys
the keys in use in the envir


@racketblock[
g.getUsedKeys;
::

]
@section{method}
 openDoc
open a document with some strings at some location


@racketblock[
g.openDoc(g.editStrings);
::

]
@section{method}
 makeEnvirGui
make an envirGui within zone - called internally.

@section{Examples}
 


@racketblock[
Pdef(\a, Pbind(\freq, Prand((1..16) * 55, inf)));
Pdef(\a).play;
t = PdefGui(Pdef(\a), 4);
Pdef(\a).set(\dur, 0.125, \amp, 0.05);

Pdef(\a).stop;
Pdef(\a).play;
Pdef(\a).pause;
Pdef(\a).resume;

t.object_(nil);
t.object_(Pdef(\a));

(
w = Window("put it in a selfmade window").front;
w.addFlowLayout;
w.view.decorator.shift(50, 50);
PdefGui(Pdef(\a), 12, w);
)

Pdef(\b, Pbind(\note, Pxrand((0..7), inf), \dur, 0.125));
Pdef(\b).play;
PdefGui(Pdef(\b));

	// see all Pdefs:
PdefAllGui(16);
::
]


