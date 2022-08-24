#lang scribble/manual
@(require (for-label racket))

@title{Window}
 Top-level container of views@section{categories}
  GUI>Views

@section{description}


The Window is the most fundamental element of the GUI. It occupies a rectangular space on the screen within which other GUI elements (Views) are displayed.

A child view is added into a window by passing the window to the view's constructor. See link::Classes/View#*new::.

@section{note}
 
There is no distinction between windows, views, and containers; a View can be displayed directly on screen, and can contain other views. Therefore, visual descriptions of Window and most of the methods that are specific to Window in other GUI kits, also apply to and make part of View in Qt, and are thus shared by all its subclasses.

The Window class is provided in Qt GUI for compatibility as well as convenience: e.g. unlike View, Window will be created by default in the center of the screen, and various aspects can be conveniently controlled using its constructor arguments.
::

The Window is usually drawn with a bar on its top edge that displays the window's title which you can set in the link::#*new#constructor::, or using link::#-name::.



@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  new

	Creates a new Window instance. You will need to call link::#-front:: on it to become visible.

	@section{argument}
  name
		A String for the text that will be displayed in the title bar. The default is 'panel'.
	@section{argument}
  bounds
		A Rect specifying position and size of the window. The size does not include the border and title bar. Position is measured from the bottom-left corner of the screen (this is different than link::Classes/View#-bounds::). The default is size 400x400 at position 128x64, but in Qt the window is centered on the screen by default.
	@section{argument}
  resizable
		A Boolean indicating whether this window is resizable by the user. The default is 
@racketblock[true::.
	]
@section{argument}
  border
		A Boolean indicating whether this window has a border. Borderless windows have no title bar and thus can only be closed in code. The default is 
@racketblock[true::.
	]
@section{argument}
  server
		This is a dummy argument which is here to provide compatibility with SwingOSC and has no effect.
	@section{argument}
  scroll
		A Boolean indicating whether this window will add scrollbars if its contents exceed its bounds. If this is set to 
@racketblock[true::, then link::Classes/View#-resize:: settings will be ignored for contained views. The default is false.

]
@section{METHOD}
  allWindows

	An array of all existing Window instances.

@section{METHOD}
  closeAll

	Calls link::#-close:: an all existing Window instances.

@section{METHOD}
  initAction

	The default action object to be evaluated whenever a new Window is instantiated.

@section{METHOD}
  screenBounds

	Returns a Rect with the size of the screen in pixels

@section{METHOD}
  availableBounds

	Returns a Rect describing the area of the screen that windows can actually occupy (i.e. excluding the Mac dock, the task bar, or similar).




@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  View hierarchy

@section{METHOD}
  view
	When a Window is created, it creates a container view, accessible using this method, that occupies the whole area of the window, and which will be used as the actual parent of the child widgets.

	@section{returns}
 
		A View.

@section{METHOD}
  asView
	Equivalent to link::#-view::

@section{METHOD}
  currentSheet
	@section{note}
  Only in Cocoa GUI ::
	returns:
The current modal sheet attached to this window, if it exists. See 
@racketblock[ "SCModalSheet".help ::.



]
@section{SUBSECTION}
  Visibility

@section{METHOD}
  front
	Displays the window on the screen (This has the same effect as setting link::#-visible:: to true).

@section{METHOD}
  minimize
	Hides the window, only keeping its representation in the dock, taskbar, etc..

@section{METHOD}
  unminimize
	Restores the window's previous state after being minimized.

@section{METHOD}
  fullScreen
	Displays the window full-screen.

@section{METHOD}
  endFullScreen
	Restores the window's previous state after being displayed full-screen.

@section{METHOD}
  alwaysOnTop
	Whether the window should always stay on top of other windows, even when it is not the active one.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  visible

	Whether the window is visible.

	Setting this to 
@racketblock[true:: has the same effect as link::#-front::, and setting it to false closes the window without destroying it.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  close

	Closes and destroys the window.

@section{METHOD}
  isClosed
	@section{returns}
  A Boolean stating whether the view has been closed.




@section{SUBSECTION}
  Geometry

@section{METHOD}
  bounds

	The position and size of the window. The position is relative to the bottom-left corner of the screen.

	@section{argument}
 
		A Rect or a Point interpreted link::Classes/Point#-asRect#as Rect::.
	@section{returns}
 
		A Rect.

@section{METHOD}
  setTopLeftBounds

	A convenience method that, unlike link::#-bounds::, sets the bounds by measuring position from the top-left corner of the screen, and vertically offset by 
@racketblock[menuSpacer::.

	]
@section{argument}
  rect
		A Rect.
	@section{argument}
  menuSpacer
		An Integer amount of pixels.

@section{METHOD}
  setInnerExtent

	Resizes the window, keeping its position intact.

	This is equivalent to link::Classes/View#-resizeTo:: called on the link::#-view::.

	@section{argument}
  w
		An Integer width in pixels.
	@section{argument}
  h
		An Integer height in pixels.

@section{METHOD}
  sizeHint
	Redirects to link::Classes/View#-sizeHint:: of the link::#-view::.

@section{METHOD}
  minSizeHint
	Redirects to link::Classes/View#-minSizeHint:: of the link::#-view::.

@section{METHOD}
  addFlowLayout

	A convenience method which sets 
@racketblock[decorator:: of the link::#-view:: to a new instance of FlowLayout. See link::Classes/FlowLayout:: for examples.

	]
@section{argument}
  margin
		A Point describing the link::Classes/FlowLayout#-margin#margin:: of the FlowLayout.

	@section{argument}
  gap
		A Point describing the link::Classes/FlowLayout#-gap#gap:: of the FlowLayout.

	@section{returns}
  The new FlowLayout instance.

@section{METHOD}
  layout
	Redirects to link::Classes/View#-layout:: of the link::#-view::.



@section{SUBSECTION}
  Appearance

@section{METHOD}
  name
	The title of the window.

	@section{argument}
 
		A String.

@section{METHOD}
  background
	The background color of the window.

	@section{argument}
 
		A Color.

@section{METHOD}
  alpha
	The transparency of the window.

	@section{argument}
 
		A Float between 0.0 (invisible) and 1.0 (opaque).

@section{METHOD}
  refresh
	Redraws the window and all its children.



@section{SUBSECTION}
  Interaction

@section{METHOD}
  userCanClose
	Whether the user can close the window. The default is 
@racketblock[true::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  acceptsClickThrough
	Whether the window receives clicks when it is not front-most. The default is 
@racketblock[true::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  acceptsMouseOver
	Whether the window and all its children receive mouse-over events. The default is 
@racketblock[false::.

	See also: link::Classes/View#-acceptsMouseOver:: and link::Classes/View#-mouseOverAction::.

	]
@section{argument}
 
		A Boolean.



@section{SUBSECTION}
  Actions and hooks

@section{METHOD}
  drawFunc
	Just like the link::Classes/UserView::, the window can be given a Function to evaluate whenever it is asked to redraw itself, so you can use the link::Classes/Pen:: class to draw on the window. See link::Classes/UserView#-drawFunc:: for explanation.

	@section{argument}
 
		A Function.

@section{METHOD}
  toFrontAction

	The action object to be evaluated whenever the window becomes the active one.

@section{METHOD}
  endFrontAction

	The action object to be evaluated whenever the window ceases to be the active one.

@section{METHOD}
  onClose

	The action object to be evaluated when the window is closed.

@section{METHOD}
  addToOnClose

	Adds an object to link::#-onClose::, wrapping the current value into an Array, if it is not yet.

@section{METHOD}
  removeFromOnClose

	Removes an object from link::#-onClose::, if the latter is an Array.



@section{EXAMPLES}
 

@section{subsection}
  Adding Views


@racketblock[
(
var w;
w = Window("my name is... panel", Rect(128, 64, 340, 360));

32.do({ arg i;
    b = Button(w, Rect(rrand(20,300),rrand(20,300), 75, 24));
    b.states = [["Start "++i, Color.black, Color.rand],
        ["Stop "++i, Color.white, Color.red]];
});
w.front;
)
::


]
@section{subsection}
  Using Decorator


@racketblock[
(
var w;
w = Window("my name is... panel", Rect(128, 64, 340, 360));

w.view.decorator = FlowLayout(w.view.bounds);
// w.addFlowLayout; // you can als write this instead of the line above

w.view.background = Color(0.6,0.8,0.8);
32.do({ arg i;
    b = Button(w, Rect(rrand(20,300),rrand(20,300), 75, 24));
    b.states = [["Start "++i, Color.black, Color.rand],
        ["Stop "++i, Color.white, Color.red]];
});

w.front;
)
::


]
@section{subsection}
  Setting Bounds


@racketblock[
// use screenbounds for precise placement from the top
(
x = Window.new("test", Rect(100,Window.screenBounds.height-180,300,100));x.front;
)

// bounds.top refers to the bottom edge of the window,
// measured from the bottom of the screen. Different than in View.
x.bounds_(Rect(100,400,300,300));
::


]
@section{subsection}
  Borderless Window


@racketblock[
w = Window.new(border:false).front; // can't be manually closed
w.close; // so close it in code
::


]
@section{subsection}
  Window with Scrollers


@racketblock[
(
w = Window(scroll: true); // you must set this when the window is created
c = Slider2D(w, Rect(0, 0, 1500, 300));
d = Slider(w, Rect(0, 310, 20, 300));
c.background = Color.grey.alpha = 0.6;
d.background = Color.grey.alpha = 0.6;
w.front;
)
::


]
@section{subsection}
 onClose


@racketblock[
(
x = Window.new.front;
x.alpha = 0.8;
x.onClose_({ y = Synth.new(\default) }); //close the window and the synth plays
)
x.close;
y.free;
::

]
@section{subsection}
  Using Layouts

Layouts are used to organize view sizes automatically. See: link::Guides/GUI-Layout-Management::.

@section{note}
  Only in Cocoa GUI ::


@racketblock[
// make a window and a layout
(
w = Window(bounds:Rect(700,200,200,200));
h = HLayout();
v = VLayout(h);
w.layout =  v;
w.front;
w.alwaysOnTop = true;
)

// add views step by step

v.add(k = Slider());
h.add(Slider());
h.add(Slider());
k.orientation = \horizontal;


h.add(g = VLayout(), 2, \left);
g.add(TextView());
g.add(TextView().string_("sand and hand"));

v.add(Button());
v.add(Button());
v.add(Button());
g.add(Button());

g.margins = [1, 1, 1, 1];
h.margins = [1, 1, 1, 1] * 23;
v.margins = [1, 1, 1, 1] * 5;

h.add(g = VLayout(), 1, \left);
g.add(g = HLayout(), 1, \left);
5.do { g.add(Slider().orientation_(\vertical)) };
::

]
@section{subsection}
  Drawing on Window with Pen


@racketblock[
(
var w, much = 0.02, string, synth;

w = Window.new("gui", Rect(100, 100, 300, 500)).front;
w.view.background_(Color.new255(153, 255, 102).vary);

string = "gui ".dup(24).join;

w.drawFunc = Routine {
    var i = 0;
    var size = 40;
    var func = { |i, j| sin(i * 0.07 + (j * 0.0023) + 1.5pi) * much + 1 };
    var scale;
    var font = Font("Helvetica", 40).boldVariant;
    loop {
        i = i + 1;
        Pen.font = font;
        string.do { |char, j|

            scale = func.value(i, j).dup(6);

            Pen.fillColor = Color.new255(0, 120, 120).vary;
            Pen.matrix = scale * #[1, 0, 0, 1, 1, 0];
            Pen.stringAtPoint(char.asString,
                ((size * (j % 9)) - 10) @ (size * (j div: 9))
            );
        };
        0.yield // stop here, return something unimportant
    }
};

{ while { w.isClosed.not } { w.refresh; 0.04.wait; } }.fork(AppClock);

w.front;

)
::
]


