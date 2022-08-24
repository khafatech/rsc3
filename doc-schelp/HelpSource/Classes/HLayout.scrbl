#lang scribble/manual
@(require (for-label racket))

@title{HLayout}
 A layout that distributes views in a horizontal line@section{categories}
  GUI>Layout
@section{related}
  Classes/VLayout, Classes/GridLayout, Classes/StackLayout, Guides/GUI-Layout-Management

@section{description}

See documentation of superclass link::Classes/LineLayout:: for details.


@section{EXAMPLES}
 

@racketblock[
(
w = Window().front;
a = { Button(w) } ! 10;
w.layout = HLayout(*a);
);

(
w = Window().front;
a = { { Button(w) } ! 10 } ! 10;
w.layout = HLayout(*a.collect { |x| VLayout(*x) });
)

(
w = Window().front;
a = { { PopUpMenu(w, Rect(0, 0, 50, 20)).items_({"abcdefghijklmno".scramble.keep(3)} ! 7) } ! 10 } ! 10;
w.layout = HLayout(*a.collect { |x| VLayout(*x) });
)

(
w = Window().front;
a = { HLayout(Slider(w, Rect(0, 0, 130, 20)), Button(w)) } ! 12;
b = { Slider2D(w) } ! 3;
w.layout = HLayout(VLayout(*a), VLayout(*b));
)

(
w = Window().front;
f = { SoundFileView(w).load(Platform.resourceDir +/+ "sounds/a11wlk01.wav", 1e5.rand, 1e4) };
a = { HLayout(VLayout(*{ Slider(w, Rect(0, 0, 230, 20)).minSize_(Size(130, 20)) } ! 3), f.value) } ! 4;
w.layout = HLayout(VLayout(*a));
)

::


]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  key
@section{PRIVATE}
  layoutClass

@section{INSTANCEMETHODS}
 


