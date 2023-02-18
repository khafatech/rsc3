#lang scribble/manual
@(require (for-label racket))

@title{HiliteGradient}
 A linear color fade between an outside and an inside color@section{categories}
  GUI>Accessories
@section{related}
  Classes/Color, Classes/Gradient

@section{description}

A linear color fade between an outside and an inside color.

@section{note}
  The use of HiliteGradient is strong::not supported yet in Qt GUI::. When HiliteGradient is used in place of Color, the average gradient color will be used instead. ::

@section{classmethods}
 

@section{method}
  new
@section{argument}
  color1
An instance of Color.
@section{argument}
  color2
An instance of Color.
@section{argument}
  direction

@racketblock[\h:: or ]

@racketblock[\v:: for horizontal and vertical respectively. Default value is ]

@racketblock[\v::.
]
@section{argument}
  steps
The resolution of the gradient. Default value is 64.
@section{argument}
  frac
The center of the gradient. Default value is 0.33, i.e. off center toward the top on a vertical gradient.

@section{examples}
 

@racketblock[
// basic usage
(
w = Window.new.front;
v = CompositeView(w, Rect(50, 50, 200, 50));
v.background = HiliteGradient(Color.gray, Color.white);
)

// change direction and resolution
(
w = Window.new.front;
w.view.background = HiliteGradient(Color.red, Color.white, \h, 12, 0.5);
)

// almost unnoticeable variations can be pleasant
(
w = Window.new.front;
v = CompositeView(w, Rect(50,50,300,300));
c = Color.rand;
d = c.vary(0.15);
v.background = HiliteGradient(c, d, \v);
[c, d].postln
)

(
var w, k, c, d, e, c1, c2, f, g;
w = Window.new.front;
k = Slider2D(w, Rect(50, 50, 300, 300));
f = {
	c = Color.rand;
	d = c.vary(0.5);
	e = d.vary(0.5);
};
g = {
	c1 = d.hueBlend(e, k.y).round(0.01);
	c2 = c.hueBlend(e, k.x).round(0.01);
	k.background = HiliteGradient(c1, c2, \v)
};
f.value; g.value;
k.action = g;
k.mouseUpAction = { [c1, c2].postln };
k.keyDownAction = f; // hit any key for new color
)
::
]


