#lang scribble/manual
@(require (for-label racket))

@title{DrawGrid}
 Draws grid lines on a UserView for plotting@section{categories}
  GUI>Accessories
@section{related}
  Reference/plot, Classes/GridLines, Classes/Plotter, Classes/UserView

@section{description}

DrawGrid is used by Plotter to draw the grid lines on a graph.  It can however also be used to draw GridLines on any UserView and could even be used to add grid lines to UserViews behind sliders or in any GUI.

Note that DrawGrid does not hold any reference to the UserView but is meant to have its .draw method called inside of the UserView's drawFunc.  It only needs to know what bounds the grid lines should be drawn within and what the horizontal and vertical GridLines are.


@section{CLASSMETHODS}
 

@section{METHOD}
  new

@section{argument}
  bounds
the bounds to draw within.  Multiple DrawGrid may be used to draw grids on a single UserView.

@section{argument}
  horzGrid
a GridLines or BlankGridLines or GridLines subclass

@section{argument}
  vertGrid
a GridLines or BlankGridLines or GridLines subclass

@section{returns}
  a DrawGrid

@section{METHOD}
  test
For testing new GridLines objects.

@racketblock[
DrawGrid.test( \freq.asSpec.grid, \amp.asSpec.grid );
DrawGrid.test( nil, \degree.asSpec.grid );
::

]
@section{argument}
  horzGrid
a GridLines object or subclass

@section{argument}
  vertGrid
a GridLines object or subclass

@section{argument}
  bounds
default: 500 @ 400

@section{returns}
  a DrawGrid


@section{INSTANCEMETHODS}
 

@section{METHOD}
  draw
This draws to the currently active UserView. This method is meant to be called from inside the drawFunc of a UserView.

@section{returns}
  nil


@section{METHOD}
  horzGrid
set the x gridLines

@section{argument}
  g
a GridLines

@section{returns}
  self


@section{METHOD}
  vertGrid
set the y gridLines

@section{argument}
  g
a GridLines

@section{returns}
  self


@section{METHOD}
  bounds
get or set bounds

@section{argument}
  b
a Rect

@section{returns}
  a Rect


@section{METHOD}
  font
get or set Font

@section{argument}
  f
a Font

@section{returns}
  a Font

@section{METHOD}
  fontColor
get or set font color

@section{argument}
  c
a Color

@section{returns}
  a Color

@section{METHOD}
  gridColors
Set the colors of each of the axis.

@section{argument}
  colors
an array of two colors: x,y

@section{returns}
  self

@section{METHOD}
  opacity
get or set opacity

@section{returns}
  float

@section{METHOD}
  smoothing
see Pen smoothing

@section{returns}
  smoothing

@section{METHOD}
  linePattern
see Pen linePattern

@section{returns}
  (returnvalue)

@section{METHOD}
  init
private

@section{argument}
  bounds
@section{argument}
  h
@section{argument}
  v
@section{returns}
  (returnvalue)

@section{METHOD}
  x
private
A DrawGridX object that draws the x (horizontal) axis

@section{returns}
  a DrawGridX

@section{METHOD}
  y
private
A DrawGridY object that draws the y (vertical) axis

@section{returns}
  a DrawGridY




@section{METHOD}
  copy
safely make a copy of this object and its working members.

@section{returns}
  a new DrawGrid

@section{METHOD}
  clearCache
private

@section{returns}
  self


@section{EXAMPLES}
 


@racketblock[
(
w = Window.new.front;
u = UserView(w,Rect(20,20,300,300));
// the Spec can define its preferred grid system
x =  \freq.asSpec.grid;
y =  \amp.asSpec.grid;
d = DrawGrid(Rect(0,0,500,300), x,y);

u.drawFunc = {
	d.draw
};
)
::
]


