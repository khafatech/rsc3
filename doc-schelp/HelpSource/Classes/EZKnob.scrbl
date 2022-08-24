#lang scribble/manual
@(require (for-label racket))

@title{EZKnob}
 Wrapper class for label, knob, number box@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/Knob, Classes/NumberBox, Classes/StaticText, Classes/CompositeView, Classes/EZGui

@section{description}

EZKnob is wrapper class which creates an (optional) link::Classes/StaticText::, and a link::Classes/Knob:: plus a link::Classes/NumberBox::. If the parent is nil, then EZKnob will create its own window. See link::Classes/EZGui:: more options.

@section{subsection}
  Some Important Issues Regarding NumberBox

@section{warning}
 
EZKnob replaces the EZKnob Quark, which is now called EZKnobOld.  It is encouraged to update your code. The two classes have different creation methods and approaches, particularly concerning the strong::dimensions:: (now strong::bounds::). To make the conversion process easier,  EZKnobOld has an instance method called convert which will post the equivalent creation code for the new EZKnob.
::

@section{note}
 
Bounds: Make certain to choose bounds that are large enough to encompass the knob, the number box, and the label (if you use one), otherwise you may get confusing results. See the examples below.
::

@section{classmethods}
 

@section{method}
  new

@section{argument}
  parent
The parent view or window. If the parent is nil, then EZKnob will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

@section{argument}
  bounds
An instance of link::Classes/Rect:: or link::Classes/Point::. Default value is 
@racketblock[160@20::.  Make certain to choose bounds that are large enough to encompass the knob, the number box, and the label (if you use one), otherwise you may get confusing results. See the examples below.

]
@section{argument}
  label
The label. Default value is nil. If nil, then no link::Classes/StaticText:: is created.

@section{argument}
  controlSpec
The link::Classes/ControlSpec:: for scaling the value.  If the 
@racketblock[minVal + maxVal:: of the spec is 0, then ]

@racketblock[centered:: will be set to true automatically.

]
@section{argument}
  action
A link::Classes/Function:: called when the value changes. The function is passed the EZKnob instance as its argument.

@section{argument}
  initVal
The value to initialize the knob and number box with. If nil, then it uses the link::Classes/ControlSpec::'s default value.

@section{argument}
  initAction
A link::Classes/Boolean:: indicating whether the action function should be called when setting the initial value. The default is false.

@section{argument}
  labelWidth
Number of pixels width for the label. default is 60. This is only valid for the 
@racketblock[\horz:: layout.

]
@section{argument}
  knobSize
An instance of link::Classes/Point::.  It will adjust itself to maximize the space use of 
@racketblock[width/height::. By default, it uses the maximum available height, and adjusts the width accordingly.

]
@section{argument}
  unitWidth
Number of pixels width for the unit label. Default is 0. If 0, then no unitLabel is created.

@section{argument}
  labelHeight
Default is 20.

@section{argument}
  layout

@racketblock[\vert::, ]

@racketblock[vert2::,  ]

@racketblock[\line2::, or ]

@racketblock[\horz::. default is ]

@racketblock[\vert::.

]
@section{argument}
  gap
A link::Classes/Point::. By default, the view tries to get its parent's gap, otherwise it defaults to 
@racketblock[2@2::. Setting it overrides these.

]
@section{argument}
  margin
A link::Classes/Point::. This will inset the bounds occupied  by the subviews of view.

@section{discussion}
 

@racketblock[
(
w=Window.new.front;
g = EZKnob( w,        // parent
            50@90,    // bounds
            " test ", // label
            \freq,    // controlSpec
            { |ez| (ez.value.asString ++" is the value of " ++ ez).postln } // action
);
g.setColors(Color.grey, Color.white)
);

// Simplest version, no parent view, so a window is created
(
	g = EZKnob(label:" test ");
	g.action_({ |ez| (ez.value.asString ++" is the value of " ++ ez).postln });
);
::
The contained views can be accessed via the EZKnob instance variables: ]

@racketblock[labelView::, ]

@racketblock[knobView::, ]

@racketblock[numberView::.


]
@section{instancemethods}
 

@section{subsection}
  Accessing Instance Variables

@section{method}
  numberView
@section{Returns}
  the 
@racketblock[numberView::

]
@section{method}
  knobView
@section{Returns}
  the 
@racketblock[knobView::

]
@section{method}
  labelView
Set/get the 
@racketblock[labelView::

]
@section{method}
  action
A function to be evaluated when the value changes. Te first argument will be the EZKnob.
@section{argument}
  arg1
An instance of link::Classes/Function:: or link::Classes/Function@section{List}
 . Default value is 
@racketblock[nil::.

]
@section{method}
  value
The value of the knob

@section{method}
  centered
Sets/gets whether the knob is in centered mode. See link::Classes/Knob::.

@section{method}
  round
Rounds the values in the number box.

@section{method}
  controlSpec
An instance of link::Classes/ControlSpec:: for scaling the values.

@section{method}
  value
Gets/sets the list/menu to the index at value. Does not perform the action.
@section{argument}
  val
An link::Classes/Integer::.

@section{method}
  doAction
Performs the action at the current index and the global action.

@section{method}
  set
Set the args after creation. You can only set the label if it was not nil from the beginning.

@section{method}
  visible
Sets/gets if the component views are visible.
@section{argument}
  bool
An instance of link::Classes/Boolean::. Default is 
@racketblock[true::.

]
@section{subsection}
  Changing Appearance

@section{method}
  setColors
@section{argument}
  stringBackground
An instance of link::Classes/Color::. The 
@racketblock[background:: of the label and unit views.
]
@section{argument}
  stringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the label and unit views.
]
@section{argument}
  numBackground
An instance of link::Classes/Color::. The 
@racketblock[numColor:: of the number view.
]
@section{argument}
  numStringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the number view.
]
@section{argument}
  numNormalColor
An instance of link::Classes/Color::. The 
@racketblock[normalColor:: of the number view.
]
@section{argument}
  numTypingColor
An instance of link::Classes/Color::. The 
@racketblock[typingColor:: of the number view.
]
@section{argument}
  knobColors
An instance of link::Classes/Color::. The 
@racketblock[knobColors:: of the knob view.
]
@section{argument}
  background
An instance of link::Classes/Color::. The 
@racketblock[background:: of the enclosing view.

]
@section{method}
  font
Set the Font used by all the views.
@section{argument}
  font
An instance of link::Classes/Font::.

@section{examples}
 


@racketblock[
(	// basic use
	w=Window.new.front;
	g=EZKnob(w, 50@90," test  ", \freq,{|a| a.value.postln});
	g.setColors(Color.grey,Color.white);
);


// lots of knobs on on view
(
w=Window.new.front;
w.view.decorator=FlowLayout(w.view.bounds);
w.view.decorator.gap=2@2;

20.do{
	EZKnob(w, 180@24," Freq ", \freq,unitWidth:30,initVal:6000.rand,layout:\horz)
	.setColors(Color.grey,Color.white)
	.font_(Font("Helvetica",11));

};
);

Window.closeAll  // use this to close all the windows

/////////////////////////////////////////////////////////////////
////////// click these parentheses to see all features and layouts

(
m=nil;
m=2@2;		// comment this for no margin

/////////////////
/// Layout \line2

(		// all features, small font
		g=EZKnob(nil, 128@40," freq  ", \freq,unitWidth:20,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,50);
		g.font_(Font("Helvetica",10));
);

(		// no unitView
		g=EZKnob(nil, 118@40," freq  ", \freq,unitWidth:0,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -40);
);
(		// no label, so use window name as label
		g=EZKnob(nil, 118@30, nil, \freq,labelWidth:100, unitWidth:20,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -130);
		g.window.name="Freq";
);

/////////////////
/// Layout \horz


(		// all features
		g=EZKnob(nil, 200@28," freq  ", \freq,unitWidth:30,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(0,50);
);

(		// no unitView
		g=EZKnob(nil, 160@28," freq  ", \freq,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(0, -30);
);
(		// no label, so use window name as label
		g=EZKnob(nil, 120@28, nil, \freq ,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(0, -110);
		g.window.name="Freq";
);



/////////////////
/// Layout \vert

(		// all features
		g=EZKnob(nil, 82@82," freq  ", \freq,unitWidth:18,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.font_(Font("Helvetica", 10));
		g.window.bounds = g.window.bounds.moveBy(220,50);
);

(		// no unitView, with label
		g=EZKnob(nil, 70@90," freq  ", \freq,unitWidth:0,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(220,-90);
);

(		// no label
		g=EZKnob(nil, 120@60,nil, \freq, unitWidth:30,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(220,-230);
		g.window.name="Freq";
);

(		// no lablel, so use window name as label
		g=EZKnob(nil, 120@60,nil, \freq,unitWidth:0,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(220,-340);
		g.window.name="Freq";
);


/////////////////
/// Layout \vert2

(		// all features
		g=EZKnob(nil, 82@82," freq  ", \freq,unitWidth:18,layout:\vert2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.font_(Font("Helvetica", 10));
		g.window.bounds = g.window.bounds.moveBy(350,50);
);

(		// no unitView, with label
		g=EZKnob(nil, 70@90," freq  ", \freq,unitWidth:0,layout:\vert2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(350,-90);
);

(		// no label
		g=EZKnob(nil, 120@60,nil, \freq, unitWidth:30,layout:\vert2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(350,-230);
		g.window.name="Freq";
);

(		// no lablel, so use window name as label
		g=EZKnob(nil, 120@60,nil, \freq,unitWidth:0,layout:\vert2, margin: m);
		g.setColors(Color.grey,Color.white,Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(350,-340);
		g.window.name="Freq";
);


)




///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////


// Sound example
(
// start server
s.waitForBoot({

var w, startButton, noteControl, cutoffControl, resonControl;
var balanceControl, ampControl;
var node, cmdPeriodFunc;

// define a synth
SynthDef("window-test", { arg note = 36, fc = 1000, rq = 0.25, bal=0, amp=0.4, gate = 1;
		var x;
		x = Mix.fill(4, {
			LFSaw.ar((note + {0.1.rand2}.dup).midicps, 0, 0.02)
		});
		x = RLPF.ar(x, fc, rq).softclip;
		x = RLPF.ar(x, fc, rq, amp).softclip;
		x = Balance2.ar(x[0], x[1], bal);
		x = x * EnvGen.kr(Env.cutoff, gate, doneAction: Done.freeSelf);
		Out.ar(0, x);
	}, [0.1, 0.1, 0.1, 0.1, 0.1, 0]
).add;




// make the window
w = Window("another control panel", Rect(20, 400, 230, 250));
w.front; // make window visible and front window.
w.view.decorator = FlowLayout(w.view.bounds);
w.view.decorator.gap=2@2;

// add a button to start and stop the sound.
startButton = Button(w, 75 @ 20);
startButton.states = [
	["Start", Color.black, Color.green(0.7)],
	["Stop", Color.white, Color.red(0.7)]
];
startButton.action = {|view|
		if (view.value == 1) {
			// start sound
			node = Synth( "window-test", [
				"note", noteControl.value,
				"fc", cutoffControl.value,
				"rq", resonControl.value,
				"bal", balanceControl.value,
				"amp", ampControl.value.dbamp ]);
		} {
			// set gate to zero to cause envelope to release
			node.release; node = nil;
		};
};

// create controls for all parameters
w.view.decorator.nextLine;
noteControl = EZKnob(w, 220 @ 32, "Note ", ControlSpec(24, 60, \lin, 1, 36, \note),
	{|ez| node.set( "note", ez.value )}, unitWidth:30,layout:\horz)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
cutoffControl = EZKnob(w, 220 @ 32, "Cutoff ", ControlSpec(200, 5000, \exp,0.01,1000,\Hz),
	{|ez| node.set( "fc", ez.value )}, unitWidth:30,layout:\horz)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
resonControl = EZKnob(w, 220 @ 32, "Reson ", ControlSpec(0.1, 0.7,\lin,0.001,0.2,\rq),
	{|ez| node.set( "rq", ez.value )}, unitWidth:30,layout:\horz)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
balanceControl = EZKnob(w, 220 @ 32, "Balance ", \bipolar,
	{|ez| node.set( "bal", ez.value )},  unitWidth:30,layout:\horz)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
ampControl = EZKnob(w, 220 @ 32, "Amp ", \db,
	{|ez| node.set( "amp", ez.value.dbamp )}, -6, unitWidth:30,layout:\horz)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);


// set start button to zero upon a cmd-period
cmdPeriodFunc = { startButton.value = 0; };
CmdPeriod.add(cmdPeriodFunc);

// stop the sound when window closes and remove cmdPeriodFunc.
w.onClose = {
	node.free; node = nil;
	CmdPeriod.remove(cmdPeriodFunc);
};
});
)


//////////////////////////////
// more examples
// these mimick the original  EZKnob layout and colors

(
w = Window("EZKnob", Rect(380,400,300,180)).front;
w.view.decorator = FlowLayout(w.view.bounds);
k = EZKnob(w, 42 @ 74, "Knob", action: { arg knb; knb.value.postln; }, margin:2@2, labelHeight:16);
k.view.background_(Color.grey.alpha_(0.4));
)
k.centered_(true)
k.value=0.5;
k.visible_(false)
k.visible_(true)

k.enabled_(false)
k.value = 0.1
k.enabled
k.enabled_(true)
k.value = 0.25

(
w = Window("EZKnob", Rect(380,400,300,180)).front;
w.view.decorator = FlowLayout(w.view.bounds, gap: 1@1);
StaticText(w, (42 * 4 + 3) @ 16).string_("EZKnob Cluster").background_(Color.blue(0.1,0.1));
w.view.decorator.nextLine;
a = [
		EZKnob(w, 42 @ 74, "knob 1", margin:2@2, labelHeight:16),
		EZKnob(w, 42 @ 74, "knob 2", controlSpec: \freq, margin:2@2, labelHeight:16),
		EZKnob(w, 42 @ 74, "knob 3", controlSpec: \pan, margin:2@2, labelHeight:16).round_(0.001),
		EZKnob(w, 42 @ 74, "knob 4", controlSpec: \rq, margin:2@2, labelHeight:16)
	];
a.do{arg a;a.view.background_(Color.grey.alpha_(0.4))};
)
// a now holds the array of knobs
a
a[0].value
a[3].value_(0.5)
a.collect(_.value );
::
]


