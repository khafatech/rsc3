#lang scribble/manual
@(require (for-label racket))

@title{EZRanger}
 A wrapper class for a label, a rangeslider, and numberboxes@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/EZGui, Classes/StaticText, Classes/Slider, Classes/NumberBox

@section{description}

EZRanger is wrapper class which creates an (optional) link::Classes/StaticText::, and a link::Classes/Slider:: plus a link::Classes/NumberBox::. If the parent is 
@racketblock[nil::, then EZRanger will create its own window. See link::Classes/EZGui:: more options.

]
@section{subsection}
  Scrolling and Arrow Keys
EZRanger's number boxs scroll by default, using the step size of the link::Classes/ControlSpec::. If the controlSpec's step is set to 0, or is not set, then the the stepping and scrolling will be guessed according to the 
@racketblock[minval:: and ]

@racketblock[maxval:: values of the spec on creation of the view.  Unlike the step variable of a regular link::Classes/NumberBox::, ]

@racketblock[controlSpec.step:: is also the smallest possible increment for the link::Classes/NumberBox::s.  By default, the shift-key modifier will allow you to step by 100x ]

@racketblock[controlSpec.step::, while the ctrl-key will give you 10x ]

@racketblock[controlSpec.step::. Since the alt-key would give you 0.1 of the minimum step, it is disabled by default, but you can change that by setting ]

@racketblock[numberView.alt_step:: to any value you like. Accordingly you can customize the other modifiers to fit your needs. See link::Classes/NumberBox:: and link::Classes/Slider::. This also effects the arrow keys for the slider.

]
@section{classmethods}
 

@section{method}
  new

@section{argument}
  parent
The parent view or window. If the parent is nil, then EZRanger will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

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
A link::Classes/Function:: called when the value changes. The function is passed the EZRanger instance as its argument.

@section{argument}
  initVal
An instance of link::Classes/Array:: 
@racketblock[[lo, hi]::. If ]

@racketblock[nil::, then it uses the link::Classes/ControlSpec::'s default value.

]
@section{argument}
  initAction
A link::Classes/Boolean:: indicating whether the action function should be called when setting the initial value. The default is 
@racketblock[false::.

]
@section{argument}
  labelWidth
Number of pixels width for the label. default is 60.

@section{argument}
  numberWidth
Number of pixels width for the number box. default is 45.

@section{argument}
  unitWidth
Number of pixels width for the unit label. The default is 0. If 0, then no 
@racketblock[unitLabel:: is created.

]
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
g = EZRanger(w, 400@16," test  ", \freq, { |v| v.value.postln }, [50,2000], unitWidth:30)
)

// Simplest version, no parent view, so a window is created
(
EZRanger(nil, 400@16," test  ", \freq, { |v| v.value.postln }, [50,2000])
)
::
The contained views can be accessed via the EZRanger instance variables: ]

@racketblock[rangeSlider::, ]

@racketblock[hiBox::, ]

@racketblock[loBox::, ]

@racketblock[unitView::, ]

@racketblock[labelView::.

]
@section{instancemethods}
 

@section{subsection}
  Accessing Instance and Class Variables

@section{method}
  unitView
The units label. Only appears if 
@racketblock[unitWidth:: was set to > 0.

]
@section{method}
  controlSpec
An instance of ControlSpec for scaling the values.

@section{method}
  loBox
The 
@racketblock[lo:: value link::Classes/NumberBox::.

]
@section{method}
  action
Set/get a link::Classes/Function:: or link::Classes/Function@section{List}
  to be evaluated when the value changes. The first argument will be the EZRanger.

@section{method}
  rangeSlider
The link::Classes/RangeSlider:: link::Classes/View::

@section{method}
  lo
Set/get the low value.

@section{method}
 hi
Set/get the high value

@section{method}
  hiBox
The hi value link::Classes/NumberBox::.

@section{method}
  round
Rounds the values in the number boxes.

@section{subsection}
  Doing Some Task (optional)

@section{method}
  doAction
Performs the action at the current index and the global action.

@section{method}
  value
Gets/sets the 
@racketblock[lo:: and ]

@racketblock[hi:: values.
]
@section{argument}
  vals
An instance of link::Classes/Array:: 
@racketblock[ [lo, hi] ::.

]
@section{method}
  valueAction
Sets the value and performs the action at the index value and the global action.
@section{argument}
  vals
An instance of link::Classes/Array:: 
@racketblock[ [lo, hi] ::.

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
  sliderColor
An instance of link::Classes/Color::. The slider 
@racketblock[background::.
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
  knobColor
An instance of link::Classes/Color::. The 
@racketblock[knobColor:: of the slider view.
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
	g=EZRanger(w, 400@16," test  ", \freq,{|v| v.value.postln},[50,2000],unitWidth:30);
	g.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow);
);

// lots of range sliders on a view
(
w=Window.new.front;
w.view.decorator=FlowLayout(w.view.bounds);
w.view.decorator.gap=1@1;

20.do{
	g=EZRanger(w, 400@16," test  ", \freq,{|v| v.value.postln},[50.rand,50+20000.rand],unitWidth:30)
	.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey,Color.white, Color.white, Color.yellow)
	.font_(Font("Helvetica",11));

};
);

Window.closeAll

/////////////////////////////////////////////////////////////////
////////// click these parentheses to see all features and layouts
(

m=nil;
m=2@2; // comment for no margin


/////////////////
/// Layout \horz

(		// all features, small font
		g=EZRanger(nil, 400@16," freq  ", \freq,
			initVal:[100.rand,200+2000.rand],unitWidth:30, numberWidth:60,layout:\horz, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180,50);
		g.font_(Font("Helvetica",10));
);

(		// no unitView
		g=EZRanger(nil, 400@16," freq  ", \freq,initVal:[100.rand,200+2000.rand],
			unitWidth:0, numberWidth:60,layout:\horz, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180, -20);
		g.font_(Font("Helvetica",10));
);
(		// no label, so use window name as label
		g=EZRanger(nil, 400@16, nil, \freq,initVal:[100.rand,200+2000.rand],
			unitWidth:0, numberWidth:60,layout:\horz, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180, -90);
		g.window.name="Freq";
		g.font_(Font("Helvetica",10));
);

/////////////////
/// Layout \line2

(		// all features
		g=EZRanger(nil, 300@42," freq  ", \freq,initVal:[100.rand,200+2000.rand],
			unitWidth:30, numberWidth:60,layout:\line2, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180,-160);
);

(		// no unitView, with label
		g=EZRanger(nil, 300@42," freq  ", \freq,initVal:[100.rand,200+2000.rand],
			unitWidth:0, numberWidth:60,layout:\line2, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180,-260);
);

(		// no label
		g=EZRanger(nil, 300@42,nil, \freq, initVal:[100.rand,200+2000.rand],
			unitWidth:30, numberWidth:60,layout:\line2, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180,-360);
		g.window.name="Freq";
);

(		// no label, so use window name as label
		g=EZRanger(nil, 150@42,nil, \freq,initVal:[100.rand,200+2000.rand],
			unitWidth:0, numberWidth:60,layout:\line2, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white));
		g.window.bounds = g.window.bounds.moveBy(-180,-460);
		g.window.name="Freq";
);

/////////////////
/// Layout \vert

(		// all features, small font
		g=EZRanger(nil, 45@300," Vol  ", \db.asSpec.step_(0.01),initVal:[-3-15.rand,-2.rand],
			unitWidth:30, numberWidth:60,layout:\vert, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white,\h));
		g.window.bounds = g.window.bounds.moveBy(250,50);
		g.font_(Font("Helvetica",9));
);
(		// no label, small font
		g=EZRanger(nil, 45@300, nil, \db.asSpec.step_(0.01),initVal:[-3-15.rand,-2.rand],
			unitWidth:30, numberWidth:60,layout:\vert, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white,\h));
		g.window.bounds = g.window.bounds.moveBy(310,50);
		g.font_(Font("Helvetica",9));
);
(		// no Units small font
		g=EZRanger(nil, 45@300, " Vol", \db.asSpec.step_(0.01),initVal:[-3-15.rand,-2.rand],
			unitWidth:0, numberWidth:60,layout:\vert, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white,\h));
		g.window.bounds = g.window.bounds.moveBy(370,50);
		g.font_(Font("Helvetica",9));
);
(		// no unitView, no units small font
		g=EZRanger(nil, 45@300, nil, \db.asSpec.step_(0.01),initVal:[-3-15.rand,-2.rand],
			unitWidth:0, numberWidth:60,layout:\vert, margin:2@2);
		g.setColors(Color.grey,Color.white, Color.grey(0.7),
			Color.grey, Color.white, Color.yellow, background:Color.grey(0.7),
				knobColor: HiliteGradient(Color.grey, Color.white,\h));
		g.window.bounds = g.window.bounds.moveBy(430,50);
		g.font_(Font("Helvetica",9));
);

)

/////////////////

////Sound Example


(	// example to explore a synthesis idea:
p = ProxySpace.push(s.boot);

q = q ? ();
q.freqRange = [200, 2000];
q.ampRange = [0.1, 1];
q.ringRange = [0.1, 10];
q.numRange = [3, 30];

q.soundfunc = { |dens=5|
	Splay.ar(
		Array.fill(exprand(q.numRange[0], q.numRange[1]).asInteger, {
			Ringz.ar(
				Dust.ar(dens),
				exprand(q.freqRange[0], q.freqRange[1]),
				exprand(q.ringRange[0], q.ringRange[1]),
				exprand(q.ampRange[0], q.ampRange[1])
			)
		})
	).distort
};
)
~plong.play;

~plong.fadeTime = 3;
~plong = q[\soundfunc];

(
w = Window("cow herd").front;
w.view.decorator_(FlowLayout(w.bounds.copy.moveTo(0, 0)));

Spec.add(\ring, [0.03, 30, \exp]);
Spec.add(\num, [3, 30, \exp, 1]);

EZRanger(w, 390@20, "numRange", \num, { |sl| q.numRange = sl.value; }, labelWidth: 65)
	.round_(1);

EZRanger(w, 390@20, "freqRange", \freq, { |sl| q.freqRange = sl.value; }, q.freqRange, labelWidth: 65)
	.round_(0.1);
EZRanger(w, 390@20, "ringRange", \ring, { |sl| q.ringRange = sl.value; }, q.ringRange, labelWidth: 65)
	.round_(0.0001);
EZRanger(w, 390@20, "ampRange", \amp, { |sl| q.ampRange = sl.value; }, q.ampRange, labelWidth: 65)
	.round_(0.0001);
Button(w, 190@20).states_([[\newSound]]).action_({~plong = q[\soundfunc] });
)
::
]


