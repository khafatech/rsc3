#lang scribble/manual
@(require (for-label racket))

@title{EZText}
 Wrapper class for a label, a text field and a value@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/StaticText, Classes/TextField

@section{description}

EZText is a wrapper class which creates an (optional) link::Classes/StaticText::, and a link::Classes/TextField::. The value is displayed as a compileString in the text field for editing.

@section{subsection}
  Some Important Issues Regarding EZText
If the parent is 
@racketblock[nil::, then EZText will create its own link::Classes/Window::. See link::Classes/EZGui:: for more options.

]
@section{classmethods}
 

@section{subsection}
  Creation / Class Methods

@section{method}
  new

@section{argument}
  parent
The parent view or window. If the parent is nil, then EZText will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

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
  action
A link::Classes/Function:: called when the value changes. The function is passed the EZText instance as its argument.

@section{argument}
  initVal
The value to initialize the EZText with.

@section{argument}
  initAction
A link::Classes/Boolean:: indicating whether the action function should be called when setting the initial value. The default is false.

@section{argument}
  labelWidth
Number of pixels width for the label. The default is 60. In the 
@racketblock[\horz:: layout, if you specify the ]

@racketblock[textWidth::, then the ]

@racketblock[labelWidth:: is ignored and is set to the ]

@racketblock[bounds.width - textWidth::.

]
@section{argument}
  textWidth
Number of pixels width for the number box. The default is 45. In 
@racketblock[\vert:: layout, ]

@racketblock[textWidth:: defaults to the ]

@racketblock[bounds.width::.

]
@section{argument}
  labelHeight
Default is 20.

@section{argument}
  layout

@racketblock[\vert::, or ]

@racketblock[\horz::. The default is ]

@racketblock[\horz::; ]

@racketblock[\vert:: is a two line version.

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
 
Example:

@racketblock[
(
w = Window("EZText", Rect(300, 300, 260, 60)).front;
g = EZText( w,          // parent
            250@50,     // bounds
            "testing",  // label
            { |ez| (ez.value.asString ++" is the value of " ++ ez).postln }, // action
            [1, 2, 3],  // initValue
            true        // initAction
);
g.setColors(Color.grey,Color.white);
);

// Simplest version, no parent view, so a window is created
(
	g = EZText(label:" test ");
	g.action_({ |ez| (ez.value.asString ++" is the value of " ++ ez).postln });
);

(
	g = EZText(bounds: Rect( 100, 200, 150, 50), label:" test ", layout: \vert);
	g.action_({ |ez| (ez.value.asString ++" is the value of " ++ ez).postln });
);
::
The contained views can be accessed via the EZText instance variables: ]

@racketblock[labelView::, ]

@racketblock[textField::.

]
@section{instancemethods}
 

@section{method}
  textField
Returns the textField.

@section{method}
  action
A link::Classes/Function:: to be evaluated when the value changes. Typical use is to type in a new value, and interpret it by hitting the evaluation shortcut. The first argument to the function will be the EZText.

@section{method}
  value
Gets/sets the value of the ezText. Does not perform the action.
@section{argument}
  inval
Any object.

@section{method}
  valueAction
Sets the value and performs the action.
@section{argument}
  val
Any object.

@section{method}
  doAction
Performs the action.

@section{method}
  enabled
Sets/gets whether the textfield is enabled.
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
  textBackground
An instance of link::Classes/Color::. The 
@racketblock[background:: of the textField.
]
@section{argument}
  textStringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the textField.
]
@section{argument}
  textNormalColor
An instance of link::Classes/Color::. The 
@racketblock[normalColor:: of the textField.
]
@section{argument}
  textTypingColor
An instance of link::Classes/Color::. The 
@racketblock[typingColor:: of the textField.
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
// Simplest version
(		// basic use
		w=Window("ez", Rect(300, 300, 300, 50)).front;
		g=EZText(w, 290@40," test  ", textWidth: 220,layout:\horz);
		g.setColors(Color.grey,Color.white);
);


// lots of textFields on one window
(
w=Window.new.front;
w.view.decorator=FlowLayout(w.view.bounds);
w.view.decorator.gap=2@2;

40.do{
		g=EZText(w, 170@16," test  ", textWidth: 120,layout:\horz);
		g.setColors(Color.grey, Color.white, Color.grey(0.8));
};
);


// click these parentheses to see three variants
(

m=nil;
m=2@2;		//comment for no margin

/////////////////
/// Layout \horz

(		// with label
		g=EZText(nil, 170@20," freq  ", textWidth:120,layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180,50);
);

(		// no label. use window name as label
		g=EZText(nil, 120@20, layout:\horz,margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(-180, -90);
		g.window.name="Freq";
);

/////////////////
/// Layout \vert

(		// all features
		g=EZText(nil, 120@60," freq  ", textWidth: 120,layout: \vert, margin:m);
		g.setColors(Color.grey,Color.white,background: Color.grey(0.7));
		g.window.bounds = g.window.bounds.moveBy(100,50);
);

)



// Simplest sound example
(
Tdef(\text).set(\note, [0, 2, 7], \dur, { [0.1, 0.2].choose });

w = Window("EZTexts", Rect(200, 400, 304, 120)).front;
w.addFlowLayout;

TdefGui(Tdef(\text), 0, w);
Tdef(\text).envir.keysValuesDo { |k, v|
	EZText(w, Rect(0,0,300,40), k, { |ez|
		Tdef(\text).envir.put(*[k, ez.value].postcs);
	}, v);
};

Tdef(\text, { |ev|
	var mydur;
	loop {
		mydur = ev.dur;
		(note: ev.note, dur: mydur).postln.play;
		mydur.wait;
	}
}).play;
)

// type these or similar functions into dur and note fields and evaluate:

{ [0.1, 0.2, 0.3].choose }
{ [ 0, 2, 7, 10 ].scramble.keep(rrand(0, 4)) }
::
]


