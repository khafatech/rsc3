#lang scribble/manual
@(require (for-label racket))

@title{ScrollView}
 A container view that can scroll its contents@section{categories}
  GUI>Views
@section{related}
  Classes/CompositeView

@section{description}


A container view which allows the user to scroll across contents when they exceed the view's bounds.

@section{subsection}
  The canvas

The view places the children onto a emphasis::canvas::, which may then be scrolled. The child views' position is always relative to the canvas, and thus not affected by scrolling.

The size of the canvas is always equal to the collective bounds of the children, and automatically adjusts when they are added, moved, resized and removed. If you wish to set it to a particular size, you could do so by first placing e.g. a link::Classes/CompositeView:: (or another container) of desired size, and then placing all the other views into that container.

Exceptionally though, you can strong::replace the canvas:: with any other view (e.g. simply with link::Classes/View::), which allows you to install a link::Classes/Layout##layout:: on it. In that case, the canvas will fill the whole visible area of the ScrollView, if the layout contents allow so, or a larger area, if the layout contents demand so. Effectively, the strong::contents will resize:: together with the ScrollView, unless their size constraints prevent that, and if link::#-autohidesScrollers:: is 
@racketblock[true::, a scrollbar will only be shown if the contents can not be resized small enough in the scrollbar's direction. See link::#-canvas:: for further explanation.

]
@section{subsection}
  Restrictions

@section{note}
 
@section{list}
 
## The link::Classes/View#-resize:: mode of the children is ignored.
## One should not use a decorator such as FlowLayout directly on a ScrollView, only on a container view placed within it.
::
::


@section{CLASSMETHODS}
 
@section{PRIVATE}
 key


@section{INSTANCEMETHODS}
 


@section{SUBSECTION}
  Geometry

@section{METHOD}
  canvas

    Returns the current canvas that carries the child views, or replaces it with another.

    By default, the canvas is a subclass of QObject, and hence does not allow the type of manipulations that views do. However, it can only be replaced with a subclass of View, which greatly extends the possibilities, including the use of layouts on the canvas.

    The new canvas will always resize with the ScrollView, as far its size constraints allow. A plain link::Classes/View:: which completely disregards its children will be freely resizable, and hence the scrolling will never be possible, since the scrollbars are activated according to the size of the canvas. To prevent this, you can either place explicit size constraints on it using link::Classes/View#-minSize:: and similar, or you can install a layout on it, which will forward to it the size constraints of the children.

    See the link::#examples#example:: below.

    Once the canvas is replaced, new views constructed with the ScrollView as the parent will end up as children of the canvas view, equivalent to constructing them with the canvas as the parent, or inserting them into the layout installed on the canvas.

    @section{warning}
  Replacing the canvas will remove and destroy all the views placed on the previous one! ::

@section{METHOD}
  innerBounds

    Returns either the rectangle corresponding to the size of the canvas, or the visible area of the ScrollView's background, whichever is larger. The position of the rectangle is always 0@0.

    See the link::#description#@section{discussion}
  above regarding the size of the canvas.

    @section{Returns}
  A Rect.

@section{METHOD}
  visibleOrigin

    Gets the position on the canvas corresponding to its upper-left-most visible point, or moves the canvas so as to leave the given point on it at the top-left corner of the visible bounds, if possible.

    @section{Argument}
 
        A Point.


@section{SUBSECTION}
  Behavior

@section{METHOD}
  autohidesScrollers

    Sets or gets whether the view hides one or another scrollbar if the contents do not exceed the view's bounds in the scrollbar's direction.

    If link::#-hasHorizontalScroller:: or link::#-hasVerticalScroller:: is set to 
@racketblock[false::, the respective scrollbar will always be hidden, regardless of this policy.

    Defaults to strong::true::.

]
@section{METHOD}
  hasHorizontalScroller

    Sets or gets whether the view has the horizontal scrollbar. If this is 
@racketblock[true::, the scrollbar may still be hidden if link::#-autohidesScrollers:: allows so; however, if this is ]

@racketblock[false:: the scrollbar will never be shown.

    Defaults to strong::true::.

]
@section{METHOD}
  hasVerticalScroller

    Sets or gets whether the view has the vertical scrollbar. If this is 
@racketblock[true::, the scrollbar may still be hidden if link::#-autohidesScrollers:: allows so; however, if this it ]

@racketblock[false:: the scrollbar will never be shown.

    Defaults to strong::true::.

]
@section{METHOD}
  autoScrolls


    Sets or gets whether the view scrolls automatically when you drag on a child view past the edge of visible bounds.

    Defaults to strong::true::.


@section{SUBSECTION}
  Appearance

@section{METHOD}
  hasBorder

    Sets or gets whether the view draws its border.

    Defaults to strong::true::.

@section{SUBSECTION}
  Actions

@section{METHOD}
  action

    Sets or gets the object to be evaluated when the user moves the scrollbars, or when link::#-visibleOrigin:: is set.

@section{EXAMPLES}
 

@section{SUBSECTION}
  Layout management on the canvas


By replacing the canvas of the ScrollView with a View, and installing a layout on it, the contents will expand to the edge of the ScrollView, and only exceed the edge if necessary.


@racketblock[
(
var scroll = ScrollView(bounds:Rect(0,0,300,300).center_(Window.availableBounds.center));
var canvas = View();
var layout;
var i = 0;

var makeEntry = {
    var view = View().background_(Color.rand).layout_(
        HLayout(
            TextField().string_( ("This is entry number " + i.asString) ),
            Button().states_([["Delete"]]).action_({view.remove; i = i - 1;})
        )
    );
    i = i + 1;
    view;
};

layout = VLayout();
layout.add ( View().background_(Color.black).layout_(
    HLayout(
        Button().states_([["Add"]]).action_({ layout.insert(makeEntry.(), i) }),
        nil // stretch remaining empty space
    )
));

canvas.layout = layout;
10.do { canvas.layout.add( makeEntry.() ) };
canvas.layout.add(nil); // stretch remaining empty space

scroll.canvas = canvas;
scroll.front;
)
::

]
@section{SUBSECTION}
  Force a canvas size


@racketblock[
(
w = Window.new;

b = ScrollView(w, Rect(0, 0, 300, 300)).hasBorder_(true);
c = CompositeView(b, Rect(0, 0, 500, 500)); // 'canvas' is this big
c.decorator = FlowLayout(c.bounds); // now we can use a decorator

Slider2D(c, Rect(0, 0, 240, 240));
Slider2D(c, Rect(0, 0, 240, 240));
Slider2D(c, Rect(0, 0, 240, 240));

c.decorator.nextLine;
w.front;
)
::

]
@section{SUBSECTION}
  "Rulers", using an action function


@racketblock[
(
var drawFunc;
w = Window.new;

a = ScrollView(w, Rect(40, 40, 300, 300));
b = ScrollView(w, Rect(0, 40, 40, 300)).hasHorizontalScroller_(false).hasVerticalScroller_(false);
c = ScrollView(w, Rect(40, 0, 300, 40)).hasHorizontalScroller_(false).hasVerticalScroller_(false);
b.background = Color.grey;
c.background = Color.grey;

d = UserView(a, Rect(0, 0, 620, 620));
e = UserView(b, Rect(0, 0, 40, 630));
f = UserView(c, Rect(0, 0, 630, 40));

a.action = { var origin;
    origin = a.visibleOrigin;
    b.visibleOrigin = 0@(origin.y);
    c.visibleOrigin = (origin.x)@0;
};

drawFunc = {
    30.do({arg i;
        (i+1).asString.drawAtPoint((i+1 * 20)@0, Font("Courier", 9), Color.black);
    });
};

d.drawFunc = {
    Pen.use({
        Pen.translate(0, 5);
        drawFunc.value;
    });
    Pen.translate(15, 0).rotate(0.5pi);
    drawFunc.value;
};

e.drawFunc = {
    Pen.translate(40, 0).rotate(0.5pi);
    drawFunc.value;
};

f.drawFunc = {
    Pen.translate(0, 25);
    drawFunc.value;
};

w.front;
)
::
]


