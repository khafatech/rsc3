#lang scribble/manual
@(require (for-label racket))

@title{NdefGui}
 a gui for a NodeProxy or Ndef@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/JITGui, Classes/MonitorGui, Classes/NdefParamGui, Classes/NdefMixer, Classes/ProxyMixer

@section{description}


NdefGui provides controls for handling and editing a link::Classes/NodeProxy:: or link::Classes/Ndef::, and its monitors. NdefGui replaces link::Classes/NodeProxyEditor::. It provides:

@section{list}
 
## sliders for numerical settings
## mapping of kr proxies to parameters
## optional controls for playing / monitoring
::

Both link::Classes/NodeProxy:: and link::Classes/Ndef:: implement a strong::.gui:: message, which returns a NdefGui for that NodeProxy. Overview: link::Overviews/JITLib::.


@racketblock[
(
s.boot;
Ndef(\a, { |freq=300, dens=20, amp=0.1, pan|
	Pan2.ar(Ringz.ar(Dust.ar(dens, amp / (dens.max(1).sqrt)), freq, 0.2), pan)
}).gui;
)
::

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{subsection}
 Creation

@section{method}
 new

@section{argument}
 object
the nodeproxy to be shown and edited, or nil.

@section{argument}
 numItems
How many sliders or textviews for parameters to provide. Default value is 0.

@section{argument}
 parent
a parent view where NdefGui is to be shown. If nil, a window is made.

@section{argument}
 bounds
bounds where the view (or window) will be shown.

@section{argument}
 makeSkip
a flag whether to create and start a link::Classes/SkipJack:: for auto-updating.

@section{argument}
 options
an array of symbols for options of what to display. See list below.

@section{subsection}
  Preset options lists which can be used in *new:

@section{method}
 big
two lines of controls for a big NdefGui, usually in its own window

@section{method}
 full
two lines of controls for a very big NdefGui

@section{method}
 audio
for ar proxies, used in ProxyMixer left hand side

@section{method}
 audioSm
for ar proxies, used in ProxyMixer.small on left hand side

@section{method}
 control
for kr proxies, used in ProxyMixer mid section


@section{subsection}
 Class Variables

@section{method}
 buttonSizes
a dict for the sizes of the different gui elements.

@section{method}
 buttonFuncs
a lookup dict for making the different buttons.

@section{method}
 makeButFuncs
not a class var, but the method that inits buttonFuncs.



@section{InstanceMethods}
 

@section{subsection}
 Variables

See link::Classes/JITGui:: for more instance methods.

Various views the NdefGui has if they were present in the options:

@section{method}
 nameView, typeView, monitorGui, paramGui, fadeBox, pauseBut, sendBut, edBut, wakeBut

@section{subsection}
 Basic Methods

@section{method}
 edits
the paramGui's widgets (usually, EZSliders)
@section{method}
 editKeys
the currently used param names

@section{method}
 highlight, unhighlight
highlight and unhighlight a single slider by index

@section{method}
 highlightName, unhighlightName
highlight and unhighlight the nameView

@section{method}
 highlightParams
highlight a contiguous group of sliders;
used for showing assigned MIDI faderboxes etc.

@section{method}
 addReplaceKey, removeReplaceKey
editKeys with technical names can be replaced with
more user-friendly ones.

@section{method}
 proxy
an alias to method object, object_


@section{subsection}
 Standard JITGui Methods

@section{method}
  setDefaults
@section{method}
  accepts
@section{method}
  getState, checkUpdate

@section{subsection}
 GUI Element Creation

@section{method}
  makeViews
creates all the views given in the options list. Internally this calls the following methods:

@section{method}
  makeNameView, makeTypeView, makeClrBut, makeWakeBut, makeResetBut, makeScopeBut, makeDocBut, makeEndBut, makeFadeBox, makePauseBut, makeSendBut, makeEdBut, makeRipBut, makePollBut
@section{method}
  makeMonitor


@section{Examples}
 


@racketblock[
	// some preparation - make an ar and a kr nodeproxy.
s.boot;
(
Ndef(\lfo, { |lofreq| SinOsc.kr(lofreq) });
Ndef(\a, { |freq=300, dens=20, amp=0.1, pan|
	Pan2.ar(Ringz.ar(Dust.ar(dens, amp / (dens.max(1).sqrt)), freq, 0.2), pan)
});
)

    // make an NdefGui. By default, this has a lot of the options on.
n = NdefGui.new;
n.object_(Ndef(\lfo));
Ndef(\lfo).set(\lofreq, 12);

n.object_(Ndef(\a));
Ndef(\a).set(\freq, 120);
::

]
@section{subsection}
 Some configuration options


@racketblock[
	// some preparation - make an ar and a kr nodeproxy.
s.boot;
(
Ndef(\lfo, { |lofreq| SinOsc.kr(lofreq) });
Ndef(\a, { |freq=300, dens=20, amp=0.1, pan|
	Pan2.ar(Ringz.ar(Dust.ar(dens, amp / (dens.max(1).sqrt)), freq, 0.2), pan)
});
)

    // make an NdefGui. By default, this has a lot of the options on.
n = NdefGui.new;
n.object_(Ndef(\lfo));
Ndef(\lfo).set(\lofreq, 12);

n.object_(Ndef(\a));
Ndef(\a).set(\freq, 120);
::

]
@section{subsection}
 Some configuration options


@racketblock[
	// numItems - sliders for setting parameters
n = NdefGui(Ndef(\a), 8);

	// 4 sets of configurations are provided:
n = NdefGui(Ndef(\a), 0, options: NdefGui.control);
n = NdefGui(Ndef(\a), 0, options: NdefGui.audio);
n = NdefGui(Ndef(\a), 0, options: NdefGui.big);
n = NdefGui(Ndef(\a), 0, options: NdefGui.full);


NdefGui.control;	// used for control proxies in ProxyMixer/NdefMixer
->	[ \name, \pausR, \sendR, \poll, \ed ]

NdefGui.audio;		// used for audio proxies in ProxyMixer/NdefMixer
->	[ \monitor, \playN, \name, \pausR, \sendR, \ed ]

NdefGui.big;		// used for the big NdefGui in ProxyMixer/NdefMixer
->	[ \name, \type, \CLR, \reset, \scope, \doc, \end, \fade,
		\monitor, \playN, \pausR, \sendR, \poll ]

			// all of 'em
NdefGui.full;
->	[ 	name, \type, \CLR, \reset, \scope, \doc, \end, \fade, \rip,
		monitor, \playN, \pausR, \sendR, \poll, \ed ]
/*
	// the choice of elements is:
	\name		a dragboth for the proxy's name
	\type		a view for the proxy's type (ir, ar + numChans, kr + numChans)
	\CLR 		button to clear proxy
	\reset		button to reset proxy nodemap
	\scope		button to scope proxy
	\doc			button to document proxy as code
	\end			button to end proxy
	\fade		EZNumber for setting proxy fadetime

	\monitor		MonitorGui for audio proxies
	\playN		a button for editing playN settings (within the MonitorGui)

	\rip		(^)	button to open a new editor on the proxy (used in ProxyMixer/NdefMixer)

	\pausR		a button to toggle proxy pause/resume
	\sendR		a button to re-send; alt-click tells the proxy to rebuild
	\poll		poll the proxy

    * preset storage in JITLibExtensions, see ProxyPreset / NdefPreset

*/

// 	add your own functions to add your own elements, such as:
NdefGui.buttonSizes.put(\zoink, 60);
NdefGui.buttonFuncs.put(\zoink, { |ndgui| Button(ndgui.zone, 60@20).states_([["zoink"]]).action_({ ndgui.object.zoink }) });

n = NdefGui(Ndef(\a), 4, options: NdefGui.big ++ [\zoink]);


	// make one and watch how the elements change
n = NdefGui(Ndef(\a), 4, options: NdefGui.big);

Ndef(\a).stop;
Ndef(\a).play;
Ndef(\a).vol_(0.3);
Ndef(\a).stop;

Ndef(\a).playN([2, 5]);   // does not display fully on outNumberBox
Ndef(\a).playN([2, 5], vol: 0.34);


	// as in ProxyMixer, left side
n = NdefGui(Ndef(\a), options: NdefGui.audio);

// as in ProxyMixer control zone
n = NdefGui(Ndef(\a), 4, options: NdefGui.control);

    // NdefGui default
n = NdefGui(options: NdefGui.big);

    // and a few more
n = NdefGui(bounds: 400@20, options: NdefGui.full);

	// put in a window - then no margin is added
(
w = Window().front;
w.addFlowLayout;
n = NdefGui(Ndef(\a), 4, w, options: NdefGui.big);
)


Ndef(\a, { |freq = 10| Blip.ar(freq) }).set(\freq, 200)
Ndef(\a, { |freq = 10, amp = 0.1| Blip.ar(freq) * amp })
Ndef(\a).set(\freq, 220)
Ndef(\a).set(\harm, 20)
Ndef(\a, { |freq = 10, amp = 0.1, harm = 20| Blip.ar(freq, harm) * amp })
::

]
@section{subsection}
 Test - drag and drop proxies between NdefGuis

This seems broken in 3.7.0 - drags are sticky and can't be dropped.


@racketblock[
(
	p = ProxySpace.push(s.boot);

l = NdefGui(nil, 3).moveTo(10, 120);
m = NdefGui(nil, 3).moveTo(10, 240);
n = NdefGui(nil, 3).moveTo(10, 360);
o = NdefGui(nil, 3).moveTo(10, 480);

Spec.add(\dens, [0.1, 300, \exp]);

	// make 3 kinds of proxies: using tilde/proxyspace, Ndef, and unnamed.
~spacy = {|dens=5| Formlet.ar(Dust.ar(dens ! 2), LFDNoise0.kr(20 ! 2).lag(0.1).linexp(-1, 1, 300, 5000), 0.003, 0.03) };
Ndef(\ndeffy, { GrayNoise.ar( 0.1 ! 2) });
c = NodeProxy.audio.source_({ PinkNoise.ar(0.1 ! 2) });

	// put one in each editor
l.object_(~spacy);
m.object_(Ndef(\ndeffy));
n.object_(c);
)
	// One should also be able to drag and drop text into the drag,
    // but this is also not working yet.
(	Ndef(\a)	)
::

]
@section{subsection}
 Test - replacing keys

This is used in ProxyChain (JITLibExtensions).


@racketblock[
(
Ndef(\a, { |freq=300, dens=20, amp=0.1, pan|
	Pan2.ar(Ringz.ar(Dust.ar(dens, amp / (dens.max(1).sqrt)), freq, 0.2), pan)
});
n = NdefGui(Ndef(\a));
)
n.addReplaceKey(\freq, \myFreak);
	// ATM needs an extra update:
x = n.object; n.object_(nil); n.object_(x);
::
]


