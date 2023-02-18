#lang scribble/manual
@(require (for-label racket))

@title{Layout}
 Superclass of all GUI layouts@section{categories}
  GUI>Layout
@section{related}
  Classes/HLayout, Classes/VLayout, Classes/GridLayout, Classes/StackLayout, Guides/GUI-Layout-Management

@section{description}

Layout is the abstract superclass of all layouts. Any layout can be installed on a view with the view's link::Classes/View#-layout#'layout':: setter method. See link::Guides/GUI-Layout-Management:: for details of operation common to all layouts.


@section{InstanceMethods}
 

@section{Method}
  spacing
The amount of empty pixels left between the managed views.

@section{argument}
  spacing
An integer representing the spacing in pixels.


@section{Method}
  margins
The amount of empty pixels left between the edges of the parent view and the managed children.

@section{argument}
  margins
An array of four integers defining margins in the following order: left margin, top margin, right margin, bottom margin; or an array of two integers applied to left/right margin and top/bottom margin respectively; or a single integer applied to all margins.



@section{Examples}
 


@racketblock[
w = Window.new;
w.layout = HLayout( TextView().string_("One"), TextView().string_("Two") );
w.layout.spacing = 20;
w.layout.margins = [10, 30, 10, 30];
w.front;
]


