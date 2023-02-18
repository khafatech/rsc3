#lang scribble/manual
@(require (for-label racket))

@title{MonitorGui}
 display and control a Monitor@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/NodeProxy, Classes/Ndef, Classes/JITGui, Classes/NdefGui

@section{description}


MonitorGui displays the state of a link::Classes/NodeProxy::'s link::Classes/Monitor::. It is used in link::Classes/NdefGui::, link::Classes/ProxyMixer::, and link::Classes/NdefMixer::.

@section{subsection}
 First examples


@racketblock[
s.boot;
Ndef(\a).ar;
Ndef(\k).kr;

	// make a MonitorGui with all bells and whistles
m = MonitorGui.new(bounds: 500@40, options: [\name, \level, \fade]);

	// when it has a kr proxy, it is visible, but disabled
m.object_(Ndef(\k));
	// with an ar proxy, it is enabled
m.object_(Ndef(\a));


	// show its play state
Ndef(\a).play
	// and volume
Ndef(\a).vol_(0.25);

	// NOTE: shift-clicking the play button opens a playN dialog!

Ndef(\a).stop;
Ndef(\a).play(0);
::

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{subsection}
 Creation

@section{method}
 new


@racketblock[
g = MonitorGui(Ndef(\a));	// barebones
(
w = Window.new.front;
w.addFlowLayout;
g = MonitorGui(Ndef(\a), w, 300@40);
)

	// bounds
MonitorGui.new(Ndef(\a), bounds: Rect(100, 100, 400, 30))
MonitorGui.new(Ndef(\a), bounds: 400@24)

	// level name and numerical value
MonitorGui.new(Ndef(\a), options: [\level])

	// a nameView and a fadeTime setter box
MonitorGui.new(Ndef(\a), options: [\name, \fade])

	// all of 'em
MonitorGui.new(Ndef(\a), options: [\level, \name, \fade])
::

]
@section{argument}
 object
the nodeproxy whose monitor state will be shown, or nil.

@section{argument}
 parent
a parent view where MonitorGui is to be shown. If nil, a window is made.

@section{argument}
 bounds
bounds where the view (or window) will be shown.

@section{argument}
 makeSkip
a flag whether to create and start a link::Classes/SkipJack:: for auto-updating.

@section{argument}
 options
an array of symbols for options of what to display.

@section{subsection}
 Class Variables

@section{method}
 lastOutBus
the highest outbus number to allow. Default is 99.

@section{InstanceMethods}
 

@section{subsection}
 Instance Variables

@section{method}
 config
some information on what to display

@section{method}
 ampSl
an link::Classes/EZSlider:: for link::Classes/Monitor:: volume

@section{method}
 playBut
a play button. Shift-click opens a dialog window for playN output routing by code

@section{method}
 setOutBox
a numberbox to set output routing

@section{method}
 fadeBox
a numberbox for setting monitor fadeTime.

@section{subsection}
 Some Methods

strong::Making various gui elements: ::

@section{method}
 makeViews
@section{method}
 makeVol
@section{method}
 makeNameView
@section{method}
 makePlayOut
@section{method}
 makeFade

strong::Standard JITGui methods: ::

@section{method}
 setDefaults
create default layout sizes

@section{method}
 accepts
accepts nil or NodeProxy

@section{method}
 getState
get the object's current state

@section{method}
 checkUpdate
compare previous state with current state, and update gui elements.



