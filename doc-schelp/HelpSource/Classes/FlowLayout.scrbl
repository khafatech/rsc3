#lang scribble/manual
@(require (for-label racket))

@title{FlowLayout}
 A view decorator which autowraps the view contents@section{categories}
  GUI>Layout
@section{related}
  Classes/SCContainerView, Classes/CompositeView

@section{description}

FlowLayout is a decorator which automatically arranges views inside a container view in a row, and starts a new row if there is not enough space left for the next view. link::Classes/Window:: and link::Classes/CompositeView:: both have 
@racketblock[addFlowLayout:: methods which assign FlowLayout to their view decorators and return the decorator.

]
@section{classmethods}
 

@section{method}
  new
@section{argument}
  bounds
An instance of link::Classes/Rect::. Normally set to the 
@racketblock[parent.bounds::.
]
@section{argument}
  margin
An instance of link::Classes/Point::. The horizontal and vertical inner margins, within which the parent's subviews are placed.
@section{argument}
  gap
An instance of link::Classes/Point::. The horizontal and vertical layout gap between the subviews.

@section{discussion}
 
Example:

@racketblock[
(
w = Window.new.front;
//change the gaps and margins to see how they work
w.view.decorator = FlowLayout( w.view.bounds, 10@10, 20@5 );
16.do{ Slider2D( w.view,80@80 ).background_( Color.rand ) };
)
::
You can also write:
]

@racketblock[
(
w = Window.new.front;
w.addFlowLayout( 10@10, 20@5 ); // a shortcut method, see SCContainerView
16.do{ Slider2D( w.view,80@80 ).background_( Color.rand ) };
)
::

]
@section{instancemethods}
 

@section{subsection}
  Accessing Instance Variables

@section{method}
  nextLine
Forces the decorator to start a new line:

@racketblock[
(
w = Window.new;
q = w.addFlowLayout( 10@10, 20@5 );
Slider2D( w.view,140@140 ).background_( Color.rand );
q.nextLine;
Slider2D( w.view,140@140 ).background_( Color.rand );
w.front;
)
::

]
@section{method}
  indentedRemaining
Returns and instance of link::Classes/Rect::. This is a very useful method which tells you how much space is left in a row, before the next row starts. The height of 
@racketblock[indentedRemaining::, is the full height remaining in the FlowLayout.
]

@racketblock[
(
//normally you will only use the width of indentedRemaining
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,150@150 ).background_( Color.rand );
Slider2D( w.view,150@150 ).background_( Color.rand );
Slider( w.view, d.indentedRemaining.width@150) //fits this view perfectly to the right innerBounds
	.background_( Color.rand );
w.front;
)
::
Compare this with:
]

@racketblock[
( //here the third view is fit to both the right and bottom innerBounds
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,140@140 ).background_( Color.rand );
Slider2D( w.view,140@140 ).background_( Color.rand );
d.nextLine;
Slider2D( w.view, d.indentedRemaining ).background_( Color.rand );
w.front;
)
::

]
@section{method}
  bounds
The outer bounds in which the decorator places the subviews in the parent view.
@section{argument}
  b
An instance of link::Classes/Rect::.

@section{method}
  innerBounds
Returns the bounds inset by margin.

@section{method}
  gap
The horizontal and vertical layout gap between the subviews.
@section{argument}
  arg1
An instance of link::Classes/Point::.

@section{method}
  margin
The horizontal and vertical inner margins, within which the parent's subviews are placed.
@section{argument}
  arg1
An instance of link::Classes/Point::.


@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
  left
Get the current left indentation or manually set it.
@section{argument}
  arg1
A number.
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,150@150 ).background_( Color.rand );
d.left_(220); //manually set the new indentation
Slider2D( w.view,150@150 ).background_( Color.rand );
w.front;
)
::

]
@section{method}
  top
Get the current top indentation or manually set it.
@section{argument}
  arg1
A number.
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,150@150 ).background_( Color.rand );
d.top_(50); //manually set the new indentation
Slider2D( w.view,150@150 ).background_( Color.rand );
Slider2D( w.view,150@150 ).background_( Color.rand );
w.front;
)
::

]
@section{method}
  shift
Set the current left and top indentation (see above).

@section{method}
  maxHeight
Get/set maximium height of the subviews in the current position.
@section{argument}
  arg1
A number.
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,100@160 ).background_( Color.rand );
Slider2D( w.view,150@150 ).background_( Color.rand );
"first row maxHeight: " ++ d.maxHeight.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
"second row maxHeight: " ++ d.maxHeight.postln;
w.front;
)
::

]
@section{method}
  maxRight
Get/set maximium right of the subviews in the current position.
@section{argument}
  arg1
A number.
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,100@160 ).background_( Color.rand );
"first row maxRight: " ++ d.maxRight.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
Slider2D( w.view,150@150 ).background_( Color.rand );
"second row maxRight: " ++ d.maxRight.postln;
w.front;
)
::

]
@section{method}
  currentBounds
Gets a link::Classes/Rect:: with 
@racketblock[bounds.width:: and ]

@racketblock[height = top + maxHeight::.
]
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 10@5 );
Slider2D( w.view,100@160 ).background_( Color.rand );
d.currentBounds.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
d.currentBounds.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
d.currentBounds.postln;
w.front;
)
::

]
@section{method}
  used
Gets a link::Classes/Rect:: with the space actually used.
@section{discussion}
 

@racketblock[
(
w = Window.new;
w.view.decorator = d = FlowLayout.new( w.view.bounds, 10@10, 20@5 );
Slider2D( w.view,100@160 ).background_( Color.rand );
d.used.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
d.used.postln;
Slider2D( w.view,150@150 ).background_( Color.rand );
d.used.postln;
w.front;
)
::

]
@section{method}
  reset
Resets the layout mechanism to 0,0.



