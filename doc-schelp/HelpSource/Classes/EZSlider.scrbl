#lang scribble/manual
@(require (for-label racket))

@title{EZSlider}
 Wrapper class for label, slider, number box@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/Slider, Classes/NumberBox, Classes/StaticText, Classes/CompositeView, Classes/EZGui

@section{description}

EZSlider is wrapper class which creates an (optional) link::Classes/StaticText::, and a link::Classes/Slider:: plus a link::Classes/NumberBox::. If the parent is 
@racketblock[nil::, then EZSlider will create its own window. See link::Classes/EZGui:: more options.

]
@section{subsection}
  Scrolling and Arrow Keys
EZSlider's number box scrolls by default, using the step size of the link::Classes/ControlSpec::. If the controlSpec's step is set to 0, or is not set, then the the stepping and scrolling will be guessed according to the 
@racketblock[minval:: and ]

@racketblock[maxval:: values of the spec on creation of the view.  Unlike the step variable of a regular link::Classes/NumberBox::, ]

@racketblock[controlSpec.step:: is also the smallest possible increment for the link::Classes/NumberBox::. By default, the shift-key modifier will allow you to step by 100x ]

@racketblock[controlSpec.step::, while the ctrl-key will give you 10x ]

@racketblock[controlSpec.step::.  Since the alt-key would give you 0.1 of the minimum step, it is disabled by default, but you can change that by setting ]

@racketblock[numberView.alt_step:: to any value you like. Accordingly you can customize the other modifiers to fit your needs. See link::Classes/NumberBox:: and link::Classes/Slider::. This also effects the arrow keys for the slider.

]
@section{classmethods}
 

@section{subsection}
  Creation / Class Methods

@section{method}
  new
@section{argument}
  parent
The parent view or window. If the parent is 
@racketblock[nil::, then EZSlider will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

]
@section{argument}
  bounds
An instance of link::Classes/Rect:: or link::Classes/Point::. Default value is 
@racketblock[160@20::.

]
@section{argument}
  label
The label. Default value is 
@racketblock[nil::. If ]

@racketblock[nil::, then no link::Classes/StaticText:: is created.

]
@section{argument}
  controlSpec
The link::Classes/ControlSpec:: for scaling the value.

@section{argument}
  action
A link::Classes/Function:: called when the value changes. The function is passed the EZSlider instance as its argument.

@section{argument}
  initVal
The value to initialize the slider and number box with. If 
@racketblock[nil::, then it uses the link::Classes/ControlSpec::'s default value.

]
@section{argument}
  initAction
A link::Classes/Boolean:: indicating whether the action function should be called when setting the initial value. The default is false.

@section{argument}
  labelWidth
Number of pixels width for the label. The default is 60.

@section{argument}
  numberWidth
Number of pixels width for the number box. The default is 45.

@section{argument}
  unitWidth
Number of pixels width for the unit label. The default is 0. If 0, then no unitLabel is created.

@section{argument}
  labelHeight
The default is 20;

@section{argument}
  layout

@racketblock[\vert::, ]

@racketblock[\line2::, or ]

@racketblock[\horz::. The default is ]

@racketblock[\horz::.

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
w = Window.new.front;
g = EZSlider( w,         // parent
              390@20,    // bounds
              " test ",  // label
              \freq,     // controlSpec
              { |ez| (ez.value.asString ++" is the value of " ++ ez).postln } // action
);
g.setColors(Color.grey,Color.white)
);

// Simplest version, no parent view, so a window is created
(
	g = EZSlider(label:" test ");
	g.action_({ |ez| (ez.value.asString ++" is the value of " ++ ez).postln });
);
::
The contained views can be accessed via the EZSlider instance variables: ]

@racketblock[labelView::, ]

@racketblock[sliderView::, ]

@racketblock[numberView::.

]
@section{instancemethods}
 

@section{subsection}
  Accessing Instance and Class Variables

@section{method}
  numberView
Returns the numberView.

@section{method}
  action
A link::Classes/Function:: or link::Classes/Function@section{List}
  to be evaluated when the value changes. The first argument will be the EZSlider.

@section{method}
  value
The value of the slider.

@section{method}
  round
Rounds the values in the number box.

@section{method}
  controlSpec
An instance of ControlSpec for scaling the values.

@section{method}
  value
Gets/sets the list/menu to the index at value. Does not perform the action.
@section{argument}
  val
An link::Classes/Integer::.

@section{method}
  valueAction
Sets the value and performs the action at the index value and the global action.
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
Sets/gets it the component views are visible.
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
  sliderBackground
An instance of link::Classes/Color::. The slider 
@racketblock[background::.
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
  knobColor
An instance of link::Classes/Color::. The 
@racketblock[knobColor:: of the knob view.
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
	g=EZSlider(w, 400@16," test  ", \freq,unitWidth:30, numberWidth:60,layout:\horz);
	g.setColors(Color.grey,Color.white);
);
g.view.enabled=false
// lots of sliders on on view
(
w=Window.new.front;
w.view.decorator=FlowLayout(w.view.bounds);
w.view.decorator.gap=2@2;

20.do{
	EZSlider(w, 392@16," Freq ", \freq,unitWidth:30,initVal:6000.rand, numberWidth:60,layout:\horz)
	.setColors(Color.grey,Color.white)
	.font_(Font("Helvetica",11));

};
);

Window.closeAll  // use this to close all the windows

/////////////////////////////////////////////////////////////////
////////// click these parentheses to see all features and layouts

(

m=nil;
//m=2@2;		// uncomment this for margin

/////////////////
/// Layout \horz

(		// all features, small font
		g=EZSlider(nil, 400@14," freq  ", \freq,unitWidth:30, numberWidth:60,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,50);
		g.font_(Font("Helvetica",10));
);

(		// no unitView
		g=EZSlider(nil, 400@16," freq  ", \freq,unitWidth:0, numberWidth:60,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -20);
);
(		// no label, so use window name as label
		g=EZSlider(nil, 400@16, nil, \freq,unitWidth:0, numberWidth:60,layout:\horz, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -90);
		g.window.name="Freq";
);

/////////////////
/// Layout \line2

(		// all features
		g=EZSlider(nil, 300@42," freq  ", \freq,unitWidth:30, numberWidth:60,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,-160);
);

(		// no unitView, with label
		g=EZSlider(nil, 300@42," freq  ", \freq,unitWidth:0, numberWidth:60,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,-260);
);

(		// no label
		g=EZSlider(nil, 300@42,nil, \freq, unitWidth:30, numberWidth:60,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,-360);
		g.window.name="Freq";
);

(		// no lablel, so use window name as label
		g=EZSlider(nil, 150@42,nil, \freq,unitWidth:0, numberWidth:60,layout:\line2, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,-460);
		g.window.name="Freq";
);

/////////////////
/// Layout \vert

(		// all features, small font
		g=EZSlider(nil, 45@300," Vol  ", \db.asSpec.step_(0.01),unitWidth:30, numberWidth:60,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(250,50);
		g.font_(Font("Helvetica",10));
);
(		// no label, small font
		g=EZSlider(nil, 45@300, nil, \db.asSpec.step_(0.01),unitWidth:30, numberWidth:60,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(310,50);
		g.font_(Font("Helvetica",10));
);
(		// no Units small font
		g=EZSlider(nil, 45@300, " Vol", \db.asSpec.step_(0.01),unitWidth:0, numberWidth:60,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(370,50);
		g.font_(Font("Helvetica",10));
);
(		// no unitView, no Units small font
		g=EZSlider(nil, 45@300, nil, \db.asSpec.step_(0.01),unitWidth:0, numberWidth:60,layout:\vert, margin: m);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,
			Color.white, Color.yellow,nil,nil, Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(430,50);
		g.font_(Font("Helvetica",10));
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
w = Window("another control panel", Rect(20, 400, 440, 180));
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
noteControl = EZSlider(w, 430 @ 20, "Note ", ControlSpec(24, 60, \lin, 1, 36, \note),
	{|ez| node.set( "note", ez.value )}, unitWidth:30)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
cutoffControl = EZSlider(w, 430 @ 20, "Cutoff ", ControlSpec(200, 5000, \exp,0.01,1000,\Hz),
	{|ez| node.set( "fc", ez.value )}, unitWidth:30)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
resonControl = EZSlider(w, 430 @ 20, "Reson ", ControlSpec(0.1, 0.7,\lin,0.001,0.2,\rq),
	{|ez| node.set( "rq", ez.value )}, unitWidth:30)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
balanceControl = EZSlider(w, 430 @ 20, "Balance ", \bipolar,
	{|ez| node.set( "bal", ez.value )},  unitWidth:30)
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);

w.view.decorator.nextLine;
ampControl = EZSlider(w, 430 @ 20, "Amp ", \db,
	{|ez| node.set( "amp", ez.value.dbamp )}, -6, unitWidth:30)
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




// a variant of the above example so one can
// add new parameters and more views are created automatically

(
// start server
s.waitForBoot({

var w, startButton, sliders;
var node, cmdPeriodFunc;
var params, specs;

// define a synth
SynthDef("window-test", { arg note = 36, fc = 1000, rq = 0.25, bal = 0, amp=0.4, width=0, gate = 1;
		var x;
		x = Mix.fill(4, {
			VarSaw.ar((note + {0.1.rand2}.dup).midicps, 0, width, 0.02)
		});
		x = RLPF.ar(x, fc, rq).softclip;
		x = RLPF.ar(x, fc, rq, amp).softclip;
		x = Balance2.ar(x[0], x[1], bal);
		x = x * EnvGen.kr(Env.cutoff, gate, 5, doneAction: Done.freeSelf);
		Out.ar(0, x);
	}, [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0]
).add;


params = ["note", "fc", "rq", "bal", "amp", "width"];
specs = [
	ControlSpec(24, 60, \lin, 1, 36, \note),
	ControlSpec(200, 5000, \exp,0.01,1000,\Hz),
	ControlSpec(0.1, 0.7,\lin,0.001,0.2,\rq),
	ControlSpec(-1, 1, \lin, 0, 0, \pan),
	ControlSpec(0.0001, 2, \exp, 0, 0.3, \vol), // db spec acts weird, so use self made one
	ControlSpec(0, 1, \lin, 0, 0.3, \width),
];

// make the window
w = Window("another control panel", Rect(20, 400, 440, 180));
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
		var args;
		if (view.value == 1) {
			// start sound
			params.do { |param, i|
				args = args.add(param);
				args = args.add(sliders[i].value)
			};
			node = Synth("window-test", args.postcs);
		} {
			// set gate to zero to cause envelope to release
			node.release; node = nil;
		};
};

// create controls for all parameters
w.view.decorator.nextLine;
sliders = params.collect { |param, i|
	EZSlider(w, 430 @ 20, param, specs[i], {|ez| node.set( param, ez.value )})
		.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);
};
// set start button to zero upon a cmd-period
cmdPeriodFunc = { startButton.value = 0; };
CmdPeriod.add(cmdPeriodFunc);

// stop the sound when window closes and remove cmdPeriodFunc.
w.onClose = {
	node.free; node = nil;
	CmdPeriod.remove(cmdPeriodFunc);
};

})
)
::
]


