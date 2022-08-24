#lang scribble/manual
@(require (for-label racket))

@title{DragBoth}
 A simple drag-and-drop source and receiver@section{categories}
  GUI>Views
@section{related}
  Classes/DragSink, Classes/DragSource

@section{description}


link::Classes/DragSource::, link::Classes/DragSink:: and link::Classes/DragBoth:: are a set of view classes intended as simple-to-use drag-and-drop sources and destinations. They are graphically represented as a simple rectangle, and their specialty is that they emphasis::do not require the Cmd/Ctrl key to be held down to initiate dragging::.

Akin to link::Classes/StaticText:: they can store arbitrary content in the link::Classes/StaticText#-object#-object:: variable, and display it using link::Classes/Object#-asString::. You can set the displayed text separately using link::Classes/StaticText#-string#-string::, and keep it independent of the content if you set link::Classes/StaticText#-setBoth#-setBoth:: to 
@racketblock[false::.

strong::DragBoth::, specifically, strong::accepts any:: dropped data and stores it into the strong::-object:: variable, as well as gives that variable as data strong::for dragging::.

See: link::Classes/View#Drag and drop:: for a general description of the drag and drop mechanism.



]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  key



@section{INSTANCEMETHODS}
 

@section{METHOD}
  defaultGetDrag
	@section{RETURNS}
  The link::Classes/StaticText#-object#-object::.

@section{METHOD}
  defaultCanReceiveDrag
	@section{RETURNS}
  Always True.

@section{METHOD}
  defaultReceiveDrag
	Sets the link::Classes/StaticText#-object#-object:: to the current drag data.



@section{EXAMPLES}
 

@racketblock[
(
w = Window.new.front;
w.addFlowLayout;
// store various kinds of objects in the drag source

// a string source
a = DragBoth(w, Rect(10, 10, 150, 20)).align_(\center).background_(Color.rand);
a.object = "drag us around";

a = DragBoth(w, Rect(10, 10, 150, 20)).align_(\center).background_(Color.rand);
a.object="SUPERCOLLIDER";

8.do{
a = DragBoth(w, Rect(10, 10, 150, 20)).align_(\center).background_(Color.rand);
a.receiveDragHandler = { arg obj; obj.object = View.currentDrag.scramble };
}
)
::
]


