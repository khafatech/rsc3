#lang scribble/manual
@(require (for-label racket))

@title{EZNumber}
 Wrapper class for label and number box@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/NumberBox

@section{description}

EZNumber is wrapper class which creates an (optional) link::Classes/StaticText::, and a link::Classes/NumberBox::.

@section{subsection}
  Some Important Issues Regarding NumberBox
If the parent is 
@racketblock[nil::, then EZNumber will create its own window. See link::Classes/EZGui:: more options.

]
@section{subsection}
  Scrolling and Arrow Keys
EZNumber scrolls by default, using the step size of the link::Classes/ControlSpec::. If the controlSpec's step is set to 0, or is not set, then the the stepping and scrolling will be guessed according to the 
@racketblock[minval:: and ]

@racketblock[maxval:: values of the spec on creation of the view.  Unlike the step variable of a regular link::Classes/NumberBox::, ]

@racketblock[controlSpec.step:: is also the smallest possible increment for the EZNumber. By default, the shift-key modifier will allow you to step by 100 x ]

@racketblock[controlSpec.step::, while the ctrl-key will give you 10x ]

@racketblock[controlSpec.step::. Since the alt-key would give you 0.1 of the minimum step, it is disabled by default, but you can change that by setting ]

@racketblock[numberView.alt_step:: to any value you like. Accordingly you can customize the other modifiers to fit your needs. See link::Classes/NumberBox::.

]
@section{classmethods}
 
@section{method}
  new

@section{argument}
  parent
The parent view or window. If the parent is nil, then EZNumber will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

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
A link::Classes/Function:: called when the value changes. The function is passed the EZNumber instance as its argument.

@section{argument}
  initVal
The value to initialize the slider and number box with. If 
@racketblock[nil::, then it uses the link::Classes/ControlSpec::'s default value.

]
@section{argument}
  initAction
A link::Classes/Boolean:: indicating whether the action function should be called when setting the initial value. The default is 
@racketblock[false::.

]
@section{argument}
  labelWidth
Number of pixels width for the label. The  default is 60. In the 
@racketblock[\horz:: layout, if you specify the ]

@racketblock[numberWidth::, then the ]

@racketblock[labelWidth:: is ignored and is set to the ]

@racketblock[bounds.width - unitWidth - numberWidth::.

]
@section{argument}
  numberWidth
Number of pixels width for the number box. The  default is 45. In \line2 layout, numberWidth defaults to the bounds.width minus the unitWidth.

@section{argument}
  unitWidth
Number of pixels width for the unit label. The default is 0.  If 
@racketblock[unitWidth:: is 0, then no ]

@racketblock[unitLabel:: is created. In the ]

@racketblock[\line2:: layout, if you specify the ]

@racketblock[numberWidth::, then the ]

@racketblock[unitWidth:: is ignored and is set to the ]

@racketblock[bounds.width - unitWidth - numberWidth::.

]
@section{argument}
  labelHeight
Default is 20;

@section{argument}
  layout

@racketblock[\line2::, or ]

@racketblock[\horz::. The default is ]

@racketblock[\horz::; ]

@racketblock[\line2:: is a two line version.

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
g = EZNumber(w,        // parent
             150@20,   // bounds
             " test ", // label
             \freq,    // controlSpec
             { |ez| (ez.value.asString ++" is the value of " ++ ez).postln }, // action
             330,      // initValue
             true      // initAction
);
g.setColors(Color.grey,Color.white);
);


// Simplest version, no parent view, so a window is created
(
	g = EZNumber(label:" test ", controlSpec: \amp.asSpec.step_(0.01) );
	g.action_({ |ez| (ez.value.asString ++" is the value of " ++ ez).postln });
);
::

The contained views can be accessed via the EZNumber instance variables: ]

@racketblock[labelView::, ]

@racketblock[numberView::.

]
@section{instancemethods}
 

@section{method}
  numberView
Returns the numberView

@section{method}
  action
A function to be evaluated when the value changes. Te first argument will be the EZNumber.
@section{argument}
  arg1
An instance of link::Classes/Function:: or link::Classes/Function@section{List}
 . Default value is nil.

@section{method}
  value
The value of the slider

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
Set the args after creation.

@section{method}
  enabled
Sets/gets if the list is enabled.
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
@racketblock[background:: of the number view.
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
  background
An instance of link::Classes/Color::. The 
@racketblock[background:: of the enclosing view.

]
@section{method}
  font
Set the link::Classes/Font:: used by all the views.
@section{argument}
  font
An instance of link::Classes/Font::.


@section{examples}
 

@racketblock[
// Simplest version
(		// basic use
		w=Window.new.front;
		g=EZNumber(w, 170@16," test  ", \freq,unitWidth:30, numberWidth:60,layout:\horz);
		g.setColors(Color.grey,Color.white);
);


// lots of numberviews on on view
(
w=Window.new.front;
w.view.decorator=FlowLayout(w.view.bounds);
w.view.decorator.gap=2@2;

40.do{
		g=EZNumber(w, 170@16," test  ", \freq,unitWidth:30, numberWidth:60,layout:\horz);
		g.setColors(Color.grey,Color.white, Color.grey(0.8));
};
);


// click these parentheses to see all features and layouts
(

m=nil;
m=2@2;		//comment for no margin

/////////////////
/// Layout \horz

(		// all features
		g=EZNumber(nil, 170@20," freq  ", \freq,unitWidth:30, numberWidth:60,layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,50);
);

(		// no unitView
		g=EZNumber(nil, 170@20," freq  ", \freq,unitWidth:0, numberWidth:60,layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -20);
);

(		// no label, with unit. use window name as label
		g=EZNumber(nil, 120@20,nil, \freq,unitWidth:30, numberWidth:60,layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -90);
		g.window.name="Freq";
);


(		// no units, no label; use window name as label;
		g=EZNumber(nil, 120@20, nil, \freq,unitWidth:0, numberWidth:60,layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -160);
		g.window.name="Freq";
);

/////////////////
/// Layout \line2

(		// all features
		g=EZNumber(nil, 120@42," freq  ", \freq,unitWidth:30, numberWidth:60,layout:\line2,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(100,50);
);

(		// no unitView, with label
		g=EZNumber(nil, 170@42," freq  ", \freq,unitWidth:0, numberWidth:60,layout:\line2,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(100, -50);
);
(		// no unitView, no label; use window name as label
		g=EZNumber(nil, 170@20,nil, \freq,unitWidth:0, numberWidth:60,layout:\line2,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(100,-150);
		g.window.name="Freq";
);


)



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
	w = Window("another control panel", Rect(200, 400, 300, 180));
	w.front; // make window visible and front window.
	w.view.decorator = FlowLayout(w.view.bounds);

	w.view.background = Color.rand;

	// add a button to start and stop the sound.
	startButton = Button(w, 75 @ 20);
	startButton.states = [
		["Start", Color.black, Color.green],
		["Stop", Color.white, Color.red]
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
	noteControl = EZNumber(w, 160 @ 20, "Note ", ControlSpec(24, 60, \lin, 1),
		{|ez| node.set( "note", ez.value )}, 36);

	w.view.decorator.nextLine;
	cutoffControl = EZNumber(w, 160 @ 20, "Cutoff ", ControlSpec(200, 5000, \exp),
		{|ez| node.set( "fc", ez.value )}, 1000);

	w.view.decorator.nextLine;
	resonControl = EZNumber(w, 160 @ 20, "Reson", ControlSpec(0.1, 0.7),
		{|ez| node.set( "rq", ez.value )}, 0.2);

	w.view.decorator.nextLine;
	balanceControl = EZNumber(w, 160 @ 20, "Balance ", \bipolar,
		{|ez| node.set( "bal", ez.value )}, 0);

	w.view.decorator.nextLine;
	ampControl = EZNumber(w, 160 @ 20, "Amp ", \db,
		{|ez| node.set( "amp", ez.value.dbamp )}, -6);


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
::
]


