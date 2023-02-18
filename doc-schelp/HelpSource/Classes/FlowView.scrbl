#lang scribble/manual
@(require (for-label racket))

@title{FlowView}
 CompositeView with a FlowLayout as decorator@section{categories}
  GUI>Views
@section{related}
  Classes/FlowLayout, Classes/CompositeView

@section{description}

In the simplest respect this is a lazy contraction of this:

@racketblock[
w = GUI.window.new;
w.view.decorator = FlowLayout.new(w.bounds);
w.front;
::

link::Classes/FlowView:: add some features to this setup.
]

@racketblock[
(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

// the StartRow will be fixed at this point in the children array
f.startRow;

GUI.slider.new(f, Rect(0,0,100,100));
f.startRow;

GUI.slider.new(f, Rect(0,0,100,100));
)
::


]
@section{CLASSMETHODS}
 

@section{METHOD}
  new

@section{argument}
  parent
Parent widget.

@section{argument}
  bounds
An instance of link::Classes/Rect::, or a link::Classes/Point:: indicating width@height.

@section{argument}
  margin
...
@section{argument}
  gap
...
@section{argument}
  windowTitle
Title of the window.


@section{INSTANCEMETHODS}
 

@section{METHOD}
  startRow
Start a new row.

@section{METHOD}
  indentedRemaining
The maximum space that is left, starting at the current cursor position.

@racketblock[
(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

GUI.slider.new(f, f.indentedRemaining)
	.background = Color.blue(alpha:0.2)
)

(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

f.startRow; // new row

GUI.slider.new(f, f.indentedRemaining)
	.background = Color.blue(alpha:0.2)
)

(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

GUI.slider.new(f, f.indentedRemaining)
	.background = Color.blue(alpha:0.2)
)
::

]
@section{METHOD}
  used
The area used so far, rounded up to the nearest rectangle plus margin.

@racketblock[
(
w = GUI.window.new;
w.front;
f = FlowView.new(w);
f.background = Color.blue(alpha:0.1);

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

f.used.postln;

// overlaid
GUI.compositeView.new(w,f.used)
	.background = Color.red(alpha: 0.1);
)

(
w = GUI.window.new;
w.front;
f = FlowView.new(w);
f.background = Color.blue(alpha:0.1);

GUI.slider.new(f, Rect(0,0,100,100));
GUI.slider.new(f, Rect(0,0,100,100));

f.startRow; // new row

GUI.slider.new(f, Rect(0,0,100,100));

f.used.postln;

// overlaid
GUI.compositeView.new(w,f.used)
	.background = Color.red(alpha: 0.1);
)
::

]
@section{METHOD}
  flow
Insert a sub flow view into the current view.

@racketblock[
(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));

// flow within a flow
g = f.flow({ arg g;
	ActionButton(g,"a");
	GUI.slider.new(g,Rect(0,0,100,100)).background_(Color.rand);
}).background_(Color.black); // shrinks to fit the contents afterwards
)
::

]
@section{argument}
  func
(describe argument here)

@section{argument}
  bounds
(describe argument here)

@section{METHOD}
  comp
Insert a sub composite view into the current view.

@racketblock[
(
f = FlowView.new;

GUI.slider.new(f, Rect(0,0,100,100));

// SuperCollider composite view
g = f.comp({ arg g;
	GUI.slider.new(g, Rect(50,30,50,100)).background_(Color.rand);
	GUI.slider.new(g, Rect(120,30,50,100)).background_(Color.rand);
},Rect(0, 0, 200, 200)).background_(Color.black);

f.startRow;
"Back to flowing".gui(f);
)
::

]
@section{argument}
  func
(describe argument here)

@section{argument}
  bounds
(describe argument here)


@section{EXAMPLES}
 


@racketblock[
// note: some of the following examples use ActionButton from the crucialib

// tests
(
FlowView.new.flow({ arg f;
//	b = ActionButton(f,"hi",minWidth:140)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	b = ActionButton(f,"hi",minWidth:140);
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	b = GUI.slider.new(f,Rect(0,0,100,100));
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		//b = ActionButton(f,"hi",minWidth:140)
	}).background_(Color.white)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		b = ActionButton(f,"hi",minWidth:140)
	}).background_(Color.white)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
		b = ActionButton(f,"hi",minWidth:140);
	}).background_(Color.white)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
		b = ActionButton(f,"hi",minWidth:140);
	}).background_(Color.white)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		b = ActionButton(f,"hi",minWidth:140);
		f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
	}).background_(Color.white)
}).background_(Color.grey)
)

(
FlowView.new.flow({ arg f;
	g = f;
	f.flow({ arg f;
		b = GUI.slider.new(f,Rect(0,0,140,20));
		f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
	}).background_(Color.white)
}).background_(Color.grey)
)


(
FlowView.new.flow({ arg f;
		b = GUI.slider.new(f,Rect(0,0,140,20));
		f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
}).background_(Color.grey)
)


(
a = FlowView.new.flow({ arg f;
	g = f;
	w = f.flow({ arg f;
		b = f.flow({ arg f;
			ActionButton(f,"hello",minWidth:100);
		}).background_(Color.blue);
		ActionButton(f,"hi",minWidth:140);
	}).background_(Color.white)
}).background_(Color.grey)

)

b.remove(true);
w.resizeToFit(true,true);


// add something big back in
ActionButton(w,"i'm back",minWidth: 200);
w.resizeToFit(true,true);
// slightly wrong size at the bottom
::
]


