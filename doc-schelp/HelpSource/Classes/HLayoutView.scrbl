#lang scribble/manual
@(require (for-label racket))

@title{HLayoutView}
 A container view that arranges its children horizontally@section{categories}
  GUI>Views
@section{related}
  Classes/VLayoutView, Classes/CompositeView

@section{description}


@section{note}
 
In Qt GUI, this class has been rendered strong::obsolete:: by a special set of layout classes; they are easier to use and more flexible. See link::Classes/HLayout:: for an equivalent to this class, and link::Guides/GUI-Layout-Management:: for a general description of the Qt layout system.
::

HLayoutView can be a parent to other views, and it automatically arranges its child views in horizontal order, expanding their height to its own bounds. Only the width of the children is relevant.

When arranging its children, HLayoutView takes the values of their 'minWidth' and 'maxWidth' properties into account. This is useful when a child's link::Classes/View#-resize#resize:: mode is set to 2, 5, or 8. See link::#@section{examples}
  below.

HLayoutView inherits some useful formatting methods from its superclasses.

@section{note}
 
HLayoutView is designed mainly for grouping and placing widgets. While you can set it to accept key presses, it does not accept mouse clicks or drags.
::

@section{CLASSMETHODS}
 
@section{PRIVATE}
  key

@section{EXAMPLES}
 

Child view height fills the HLayoutView automatically:


@racketblock[
(
q = 10;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));

Array.fill(q,{ arg i;
    Slider(h,Rect(0,0,20,75)).value_(i / q)
});
h.background_(Color.rand);

w.front
)
::

Stretching the layout view; Slider height fills the View automatically:

]

@racketblock[
(
q = 8;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.background = Color.rand;
h.resize = 5; // elastic

Array.fill(q,{ arg i;
    var s;
    s = Slider(h,Rect(0,0,20,75)).background_(Color.grey.alpha_(0.4));
    s.value = i / q;
    s
});
StaticText(h, Rect(0,0,105,20)).background_(Color.rand).string_(" Some Example\n Text");
w.front
)
::

Stretching the layout view and the contents; if all the contents are elastic, the widths of the contents are perfectly divided up. In this example, the StaticText is not elastic in order to preserve its width:

]

@racketblock[
(
q = 10;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.resize = 5; // elastic
h.background = Color.rand;

Array.fill(q,{ arg i;
    var s;
    s = Slider(h,Rect(0,0,20,75));
    s.resize = 5; // elastic
    s.value = i / q;
    s
});
StaticText(h, Rect(0,0,105,20)).background_(Color.rand).string_(" Some Example\n Text");

w.front
)
::

Setting minWidth on contents; beware that if the layout view width is smaller than the combined width of all the contents, things might disappear when you try to handle them with the mouse:

]

@racketblock[
(
q = 5;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.background = Color.rand;
h.resize = 5; // elastic

Array.fill(q,{ arg i;
    var s;
    s = Slider(h,Rect(0,0,20,75));
    s.value = i / 5;
    if(i < 2,{
        s.resize = 5; // some elastic
        s.setProperty(\minWidth,20);
    },{
        s.resize = 1; // some not elastic
    });
    s
});
StaticText(h, Rect(0,0,105,20)).background_(Color.rand).string_(" Some Example\n Text");

w.front
)
::

]

@racketblock[
(
q = 5;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.resize = 5; // elastic
h.background = Color.rand;
Array.fill(q,{ arg i;
    var s;
    s = Slider(h,Rect(0,0,20,75));
    s.value = i / 5;
    s.resize = 5;
    s.setProperty(\minWidth,20);
    s.setProperty(\maxWidth,40);
    s
});

w.front
)
::

Text flows:

]

@racketblock[
(
q = 5;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.resize = 5; // elastic

Array.fill(q,{ arg i;
    var s;
    s =     StaticText(h,120@20).string_("Some short text which wraps around");

    s.resize = 5;
    s.setProperty(\minWidth,10);
    s.setProperty(\maxWidth,120);

    // not working
    s.setProperty(\maxHeight,10);
    s.setProperty(\minHeight,10);

    s.background = Color.white;
    s
});

w.front
)
::

Spacing:

]

@racketblock[
(
q = 10;
w = Window.new;

h = HLayoutView(w,Rect(0,0,300,300));
h.setProperty(\spacing,0);

Array.fill(q,{
    Slider(h,Rect(0,0,20,75))
});

w.front
)
::
]


