#lang scribble/manual
@(require (for-label racket))

@title{TdefGui}
 a line of editing controls for a Tdef, and optionally its envir@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/TdefAllGui, Classes/PdefGui, Classes/PdefAllGui, Classes/EnvirGui

@section{description}


A gui showing the link::Classes/Tdef::'s name, playing state, source state, and envir state. Optionally, its envir can also be edited.

@section{subsection}
 First example


@racketblock[
g = TdefGui();			// make a TdefGui
g.object = Tdef(\a);		// show when a Tdef is put in
Tdef(\a, { "boing".postln }); 	// show whether it has a source
Tdef(\a).play; 			// show whether playing, stopped, or ended, and pausable
Tdef(\a).set(\abc, 123); 	// show whether the tdef has an envir

g = TdefGui(Tdef(\a), 3);	// with an envirgui for 3 items
Tdef(\a).set(\a, 12, \lofreq, [1, 10], \str, "someString", \oops, \oneSymbolTooMany);

(				// put it in an existing window - margin becomes 0@0
w = Window().front; w.addFlowLayout;
TdefGui(Tdef(\a), 0, w);
TdefGui(Tdef(\a), 3, w);
)
::

]
@section{subsection}
 Details on the GUI elements

@section{definitionList}
 
## name button
|| when selected, typing the delete key will delete its Tdef.
## play/stop button
|| indicates whether the tdef is playing:
@section{table}
 
## " >" || if stopped,
## " _" || if playing and active,
## " |" || if it is playing, but the stream has ended.
::
## pause/resume button
|| only visible if one can pause or resume the Tdef, i.e. while it is playing.
@section{table}
 
## "paus" || shown when you can pause it,
## "rsum" || shown when you can resume it.
::
## src button
|| opens a document to edit the source (function) of the Tdef.
@section{table}
 
## green || a source exists,
## white || the source is nil.
::
## env button
|| strong::click:: opens a document to edit the envir of the Tdef, strong::option-click:: opens a new TdefGui with a big enough link::Classes/EnvirGui:: for the Tdef's envir.
@section{table}
 
## green || the Tdef has an envir,
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
a desired size and position where to display a JITGui. can be nil, a link::Classes/Point::, or a link::Classes/Rect::. JITGuis know their minimum size ( strong::minSize:: ), and if bounds is nil, minSize is used. if bounds is a point or rect, it will be set to at least minSize. With a rect one can also supply a position where to display. If a point,shown size is the maximum of bounds and minSize

@section{argument}
 makeSkip
A flag whether to make a skipjack. If one uses a TdefGui as part of a larger gui ensemble, one may want to call checkUpdate on all of them together, not with separate skipJacks.

@section{argument}
 options
a list of additional information, e.g. flags about optional buttons. (this is used is some subclasses)

@section{InstanceMethods}
 

@section{method}
 object
a link::Classes/Tdef::, or nil

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
the link::Classes/CompositeView:: within which the TdfGui is shown


@section{method}
 nameBut, playBut, pauseBut, srcBut, envBut
the buttons

@section{method}
 envirGui
the gui for the Tdef's envir - if numItems > 0.

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

set the TdefGui's object to nil

@section{method}
 close
(if the jitGui is in its own window)

and close its window.

@section{subsection}
 Internal methods

@section{method}
 srcString
a compileString that recreates the Tdef.


@racketblock[
// assume g from above is still there
g.srcString;
::

]
@section{method}
 editString
a compileString that recreates the Tdef's envir at edKey.

@section{method}
 editStrings
a compileString that recreates the Tdef's envir at edKeys.

@section{argument}
 edKeys
Default value is nil.


@racketblock[
// assume g from above is still there
g.editString;
Tdef(\a).set(\foo, \bar);
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
open a document with some strings at some location. used with src button, env button.


@racketblock[
g.openDoc(g.editStrings);
::

]
@section{method}
 makeEnvirGui
make an envirGui within zone.

@section{Examples}
 


@racketblock[
(
Tdef(\a, { |e| 100.do { |i| i.postln; 0.5.wait } });
t = TdefGui(Tdef(\a), 4);
Tdef(\a).set(\freq, 200, \dur, 0.1, \otto, 12, \ann, 1234);
)

Tdef(\a).stop;
Tdef(\a).play;
Tdef(\a).pause;
Tdef(\a).resume;

t.object_(nil);
t.object_(Tdef(\a));

(
w = Window("put it in a selfmade window").front;
w.addFlowLayout;
w.view.decorator.shift(50, 50);
TdefGui(Tdef(\a), 12, w)
)

Tdef(\b, { |e| 100.do { |i| Tdef(\a).set(\otto, 8.rand); exprand(0.1, 3.0).wait } });
Tdef(\b).play;
TdefGui(Tdef(\b));

	// see all Tdefs:
TdefAllGui(16);
::
]


