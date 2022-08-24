#lang scribble/manual
@(require (for-label racket))

@title{UserView}
 A custom drawn view using Pen@section{categories}
  GUI>Views
@section{related}
  Classes/Pen

@section{description}

An empty view on which you can draw using the link::Classes/Pen:: class.

This view displays and does nothing by itself, but allows you to define how it will be drawn, and expects you to define its entire mode of interaction using mouse, key, and drag and drop actions.

To define how the view is drawn you set the link::#-drawFunc:: variable to a link::Classes/Function:: within which you can use the link::Classes/Pen:: class to draw graphical primitives, such as lines, rectangles, ellipses, curves, and also text.

This class offers convenient mechanisms for creating animations, that is, to automatically redraw itself in regular time intervals. See the link::#animation#Animation:: section of this document.

For a guide to using this view, see link::Guides/GUI-Introduction#Custom views::.

@section{note}
  Older tutorials might recommend subclassing UserView. Don't do that. Use composition, not inheritance. Make the UserView a property of your custom view class. ::

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key



@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Drawing

@section{METHOD}
  drawFunc

	Sets the Function to evaluate whenever the view is asked to redraw itself. This may be, for example, due to being hidden and shown again, being resized, another view moving on top of it, or after the link::Classes/View#-refresh:: method has been called.

	Within that Function you are allowed to use the link::Classes/Pen:: class to draw withing the bounds of the view. All the coordinates given to methods of Pen are relative to the top-left corner of the view. Usage of Pen is not allowed outside of that Function.

	The Function will be passed this view as the argument when evaluated.

@section{argument}
 
	A Function.


@section{METHOD}
  background
	Sets the color used to fill the whole area occupied by the view below the drawing done in link::#-drawFunc::. You can set the background at any moment, even within the 
@racketblock[drawFunc::, but any drawing done in that Function will always be displayed on top of the background.

	]
@section{argument}
 
		A Color.

@section{METHOD}
  drawingEnabled
	Whether the link::#-drawFunc:: will be called when the view is redrawing itself. Note that link::#-background:: will be painted regardless of this variable.

	The default value is 
@racketblock[true::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  clearOnRefresh
	Whether the view shall clear the last drawing done in link::#-drawFunc:: before being redrawn.

	If this is 
@racketblock[false::, the view will continuously draw on top of all the previous drawing whenever it is redrawn, until link::#-clearDrawing:: is called.

	The default value is ]

@racketblock[true::.

	]
@section{note}
  On macOS this functionality is only available for the version >= 10.4. ::

	@section{argument}
 
		A Boolean.

@section{METHOD}
  clearDrawing
	If link::#-clearOnRefresh:: is 
@racketblock[false::, you can call this method to manually clear any drawing done in link::#-drawFunc:: so far.



]
@section{SUBSECTION}
  Animation

@section{METHOD}
  animate
	Whether the view shall redraw itself internally at a regular time interval (frame rate). See link::#-frameRate:: for the way to adjust that interval.

	@section{note}
  This method is strong::not:: available in strong:: SwingOSC GUI ::. ::

	The default value is 
@racketblock[false::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  frameRate
	The interval at which the view regularly redraws itself, if link::#-animate:: is 
@racketblock[true::. You can change the desired frame rate by setting this variable.


	The default frame rate is 60fps.

	]
@section{note}
  Getting the value of this variable will return the average actual frame rate. If it is lower than the desired frame rate set as described above, that implies that the view tries but does not manage to redraw itself at that frame rate. The reason may typically be that the drawing defined in link::#-drawFunc:: is computationally too demanding for the system.
	::

@section{argument}
  fps
	A Float defining the interval between frames of animation, in frames per second.

@section{METHOD}
  frame

	The count of frames drawn while link::#-animate:: is 
@racketblock[true::; it will increase by 1 every time the view is redrawn.

	If animation is stopped and started again (by setting ]

@racketblock[animate:: to ]

@racketblock[false:: and then ]

@racketblock[true:: again), the frame count is restarted.

	]
@section{returns}
  An Integer.



@section{SUBSECTION}
  Actions

	The UserView by itself does not respond to any interaction by the user. You can define the modes of interaction entirely on your own using mouse and keyboard actions. See link::Guides/GUI-Introduction#Actions and hooks: make that button do something!#Actions and Hooks:: for detailed explanation.

	Note that there is no default mode of interaction with the UserView, so link::Classes/View#-action:: will never be triggered, if you don't implement that yourself.

@section{EXAMPLES}
 

'Introduction to GUI' contains an link::Guides/GUI-Introduction#Custom views#example:: with elaborate explanation on how to use UserView. Below are further examples.

@section{SUBSECTION}
  Basic Usage

Resize the window or click on the UserView to refresh the drawing:


@racketblock[
(
w=Window.new;
v=UserView(w, w.view.bounds.insetBy(50,50));
v.resize = 5;
v.background_(Color.rand);
v.drawFunc={|uview|
		Pen.moveTo(0@uview.bounds.height.rand);
		Pen.lineTo(uview.bounds.width@uview.bounds.height.rand);
		Pen.stroke;
	};
v.mouseDownAction={v.refresh};
w.front;
)
::

Coordinates are relative to the UserView. Try resizing the window:

]

@racketblock[
(
var func;

func = {|me|
	Pen.use{
		10.do{
			Color.red(rrand(0.0, 1), rrand(0.0, 0.5)).set;
			Pen.addArc((400.exprand(2))@(100.rand), rrand(10, 100), 2pi.rand, pi);
			Pen.perform([\stroke, \fill].choose);
		}
	}
};

w = Window.new("DrawFunc Examples").front;
w.view.background_(Color.white);

3.do{|i|
	v = UserView(w, Rect(20+(i*120), 100, 100, 100))
		.drawFunc_(func);
	v.resize=3; // the func coordinates ar valid even though the view move on resize
	v.background_(Color.rand);
};

w.refresh;
)
::

]
@section{SUBSECTION}
  Responding to mouse clicks and movement

Using link::Classes/View#Mouse actions#mouse actions:: you can make UserView change the way it is drawn in response to mouse interaction. The sequence of examples below will guide you through the various possibilities.

Clicking and moving the mouse on each of the painted squares in the following example will redraw them differently. See interpreter output for posted information that you can use in the mouse actions.


@racketblock[
// drag some circles
(
var w, r, u;
var clicked, relativeWhere;
r = { Rect(300.rand, 300.rand, 110, 110) } ! 4;

w = Window.new.front;
u = UserView(w, Rect(0, 0, 400, 400));
u.drawFunc = {
	r.do { |x, i|
		Pen.addOval(x); // wie addRect
		Pen.color = Color.hsv(0.9 * (1/(i+1)), 0.6, 1);
		Pen.draw;
	};
};
u.mouseDownAction = { |v, x, y|
	r.do { |rect, i|
		if(rect.contains(Point(x, y))) {
			clicked = i;
			relativeWhere = Point(x, y) - rect.origin;
		};
	};
};

u.mouseMoveAction = { |v, x, y|
	var rect;
	if(clicked.notNil) {
		rect = r.at(clicked);
		r.put(clicked, rect.origin = Point(x, y) - relativeWhere);
		w.refresh;
	}
};

u.mouseUpAction = {
	clicked = nil;
}
)


::

]

@racketblock[
// draw some random shapes
(
var drawFunc, mouseDownFunc, mouseUpFunc, mouseMoveFunc, sat = 0, startX;

drawFunc = {|me|
	Pen.use{
		10.do{
			Color.red(rrand(0.0, 1), rrand(0.3, 0.8)).set;
			Pen.addArc((400.exprand(2))@(100.rand), rrand(10, 50), 2pi.rand, pi);
			Pen.perform([\stroke, \fill].choose);
		}
	}
};

mouseDownFunc = {|me, x, y, mod|
	startX = x;
	postf("begin path: x=% realtive mouse coordinates:%\n",startX, x@y);
};

mouseUpFunc = {|me, x, y, mod|
	postf("end path: (startX-x)==% mouse coordinates:%\n",(startX-x), x@y);
};

mouseMoveFunc = {|me, x, y, mod|
	sat = ((startX-x)/100);
	(x@y).postln;
	me.refresh;
};

w = Window.new.front;
w.view.background_(Color.white);
3.do{|i|
	v = UserView(w, Rect(20+(i*120), 100, 100, 100));
	v.background_(Color.rand);
	v.drawFunc = drawFunc;
	v.mouseDownAction = mouseDownFunc;
	v.mouseUpAction = mouseUpFunc;
	v.mouseMoveAction = mouseMoveFunc;
};
w.refresh;
)
::

The following example uses the link::#-clearOnRefresh:: option to prevent the UserView from clearing its contents when redrawn. Clicking and moving the mouse within each square will draw ever more arcs on top of each other.

]

@racketblock[
(
var func, views;

func = {|me|
	Pen.use{
		1.do{
			Color(
				rrand(0.0, 1),
				rrand(0.0,0.2),
				rrand(0.0, 0.8),
				rrand(0.4, 0.8)
			).set;
			Pen.addArc((400.exprand(2))@(100.rand), rrand(10, 50), 2pi.rand, pi);
			Pen.perform([\stroke, \fill].choose);
		}
	}
};

w = Window.new("DrawFunc Examples").front;
w.view.background_(Color.white);
views = {|i|
	v = UserView(w, Rect(20+(i*120), 100, 100, 100));
	v.background = Color.rand;
	v.drawFunc = func;
	v.mouseMoveAction = {|me| me.refresh };
	v.clearOnRefresh_(false);
} ! 3;
w.refresh;
)
::

The following example uses the link::#-clearOnRefresh:: option to keep the old contents when redrawn, allowing you to draw a line which follows the mouse cursor by clicking and dragging on the view.

It also uses the mouse position to compute the color of the line.

]

@racketblock[
(
var w, txt, lines, points, drawLine;

drawLine = { |points, bounds|
	var p0;
	points.do { |p,i|
		if(i == 0){
			p0 = p;
		}{
			Pen.moveTo(p0);
			Pen.lineTo(p);
			Color(
				(p.x/bounds.width).clip(0,1),
				1.0-(p.x/bounds.width).clip(0,1),
				(p.y/bounds.height).clip(0,1)
			).set;
			Pen.stroke;
			p0 = p;
		}
	};
};

w = Window("draw on me", Rect(128, 64, 340, 360));

v = UserView(w,w.view.bounds)
	.clearOnRefresh_(false)
	.mouseDownAction_({|v,x,y|
		points = [x@y];
	})
	.mouseMoveAction_({|v,x,y|
		points = points.add(x@y);
		v.refresh;
	})
	.mouseUpAction_({|v,x,y|
		points = points.add(x@y);
		lines = lines.add(points);
		points = nil;
		v.refresh;
	})
	.background_(Color.white)
	.drawFunc_{|me|
		var r = me.bounds;

		Pen.use {
			Pen.width = 1;
			Color.black.set;

			lines.do { |linePoints|
				drawLine.value(linePoints, r);
			};
			lines = nil;

			drawLine.value(points, r);
			if( points.size > 0 ) { points = [points.last] };
		};
	};

w.front;
)
::

]
@section{SUBSECTION}
  Animation

The following is an animation with mouse interaction. Click and drag in the view to move the center of the rotating object.


@racketblock[
(
var width = 400, height = 400, mx = 0, my = 0, pt, r;

w = Window("animation and mouse interaction", Rect(100, 200, width, height), false);

u = UserView(w, Rect(0, 0, width, height));
u.background = Color.black;
u.animate = true; //animate this view

// allocate data in advance, for optimization:
pt = Point();
r = Rect();

u.drawFunc = {
	Pen.fillColor = Color.green;
	Pen.stringAtPoint(u.frameRate.asString, Point(10, 10)); // display frame rate
	Pen.stringAtPoint(u.frame.asString, Point(10, 30)); // display frame counter
	Pen.color = Color.white;
	pt.x=mx;
	pt.y=my;
	100.do{|i|
		Pen.moveTo(pt);
		pt.x = sin(u.frame*0.04.neg+i)*(5*i)+mx; //use .frame to drive animation
		pt.y = cos(u.frame*0.05+i)*(5*i)+my;
		r.left=pt.x;
		r.top=pt.y;
		r.width=i;
		r.height=i;
		Pen.lineTo(pt);
		Pen.fillStroke;
		Pen.addOval(r);
		Pen.fillStroke;
	};
};
u.mouseDownAction = {|v, x, y|
	mx = x;
	my = y;
};
u.mouseMoveAction = u.mouseDownAction;
w.front;
)

u.animate = false; //animation can be paused and resumed
u.animate = true;
w.close; //stops animation
::

A simple ball animation:

]

@racketblock[
(
var width = 400, height = 400, xspeed = 3, yspeed = 2, x = width*0.5, y = height*0.5;
w = Window("ball", Rect(100, 200, width, height));
u = UserView(w, Rect(0, 0, width, height));
u.background = Color.black;
u.animate = true;
u.drawFunc = {
	if(x<0 or:{x>width}, {xspeed = 0-xspeed});
	if(y<0 or:{y>height}, {yspeed = 0-yspeed});
	x = x+xspeed;
	y = y+yspeed;
	Pen.fillColor = Color.white;
	Pen.fillOval(Rect.aboutPoint(Point(x, y), 8, 8));
};
w.front;
)

( //replace the drawFunc above while running
u.drawFunc = {
	Pen.fillColor = Color.red;
	Pen.fillOval(Rect(200, 200, sin(u.frame*0.031)*200, sin(u.frame*0.044)*200));
	Pen.fillOval(Rect(200, 200, sin(u.frame*0.052)*200, sin(u.frame*0.065)*200));
	Pen.fillOval(Rect(200, 200, sin(u.frame*0.073)*200, sin(u.frame*0.086)*200));
}
)

//close the window to stop
w.close;
::

An animation that makes a good use of the link::#-clearOnRefresh:: option to keep the old contents when redrawing.

]
@section{note}
  In strong::Cocoa GUI:: this functionality is only available on macOS version >= 10.4. ::


@racketblock[
(
var width = 640, height = 480, w, theta = 0, drawFunc;
w = Window( "trails", Rect( 128, 64, width, height ), false );
drawFunc = { arg view;
	var x = 20 * sin( theta ), y = 42 * cos( theta );
	theta = theta + 0.01;
	//draw a semitransparent rect filling the screen:
	Pen.fillColor_( Color.red( 0.2, 0.1 ));
	Pen.fillRect( Rect( 0, 0, width, height ));
	Pen.strokeColor_( Color.white );
	Pen.translate( width * 0.5, height * 0.5 );
	6.do { arg i;
		Pen.rotate( theta * (1 - (i / 6)) );
		Pen.scale( 0.7 + (i * 0.4), 0.4 + (i * 0.5) );
		Pen.strokeOval( Rect.aboutPoint( Point( x, y ), 60, 40 ));
	};
};
x = UserView( w, Rect( 10, 10, width - 20, height - 20 ))
	.canFocus_( false )
	.drawFunc_( drawFunc )
	.clearOnRefresh_( false );

w.front;
x.animate = true
)
::

]
@section{SUBSECTION}
  Usage of refreshInRect(aRect)

@section{note}
  This functionality is only available in strong::Cocoa GUI:: ::

The 
@racketblock[refreshInRect:: method constrains the receiver's refresh area to the rectangle passed in aRect. You may use Quartz Debug's flash screen updates to see the refresh area of the view

]

@racketblock[
(
var userView, win, blob = Rect(0, 0, 50, 50), trackblob=false, pmouse;

a = SCImage.new("/Library/Desktop Pictures/Ripples Blue.jpg");

win = SCWindow.new("refreshInRect Test", Rect(400, 400, 600, 200), scroll:true).front;
win.onClose_({ a.free; });

userView = SCUserView(win, Rect(10,10,2000,800))
	.backgroundImage_(a, 5)
	.drawFunc_({|me|
		Color.blue.setFill;
		Pen.fillRect(blob);
	})
	.mouseDownAction_({|v, x, y, mod|
		pmouse = x@y;
		trackblob = blob.containsPoint(pmouse);
	})
	.mouseUpAction_({|v, x, y, mod|
		trackblob = false;
	})
	.mouseMoveAction_({|v, x, y, mod|
		var refresh, mouse, delta;
		mouse = x@y;

		if(trackblob, {
			refresh = blob.copy;
			delta = mouse-pmouse;
			blob = blob.moveBy(delta.x, delta.y);
			refresh = refresh.union(blob);
			v.refreshInRect(refresh);
		});
		pmouse = mouse;
	});

blob = blob.moveBy(userView.bounds.left, userView.bounds.top);
userView.bounds.postln;
)
::
]


