#lang scribble/manual
@(require (for-label racket))

@title{Knob}
 A rotary controller view@section{categories}
  GUI>Views
@section{related}
  Classes/Slider, Classes/Slider2D

@section{description}


Knob displays a value from 0.0 to 1.0 in rotary fashion, and allows to control it with either circular or linear mouse motion.

It also displays the deviation of the value from either 0.0 or 0.5, which you can choose using link::#-centered::.

To switch between the mouse interaction modes, use link::#-mode::.

The amount by which the value changes at interaction can be fine-tuned using link::#-step::, link::#-keystep::, link::#-shift_scale::, link::#-ctrl_scale::, and link::#-alt_scale::

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  defaultMode

    The default link::#-mode:: for newly created Knobs.


@section{INSTANCEMETHODS}
 


@section{SUBSECTION}
  Data

@section{METHOD}
  value

    The displayed value.

    @section{argument}
 
        A Number in the range of 0.0 to 1.0.

@section{METHOD}
  valueAction

    Sets the value and triggers link::#-action::.

@section{METHOD}
  increment
    Increments the value by link::#-keystep:: multiplied by the argument.

    @section{argument}
 
        A Number.

@section{METHOD}
  decrement
    Decrements the value by link::#-keystep:: multiplied by the argument.

    @section{argument}
  zoom
        A Number.


@section{SUBSECTION}
  Interaction

@section{METHOD}
  mode

    The way value is controlled with respect to mouse movement after clicking on the view:
    @section{list}
 
    ## 
@racketblock[\round:: - value follows circular movement
    ## ]

@racketblock[\horiz:: - value follows linear movement in horizontal direction
    ## ]

@racketblock[\vert:: - value follows linear movement in vertical direction
    ::

    Defaults to ]

@racketblock[\round::.

    ]
@section{Argument}
 
        One of the symbols listed above.

@section{METHOD}
  keystep

    The amount by which the value is incremented/decremented when pressing a relevant key.

    Defaults to 0.01;

    @section{Argument}
 
        A Number.

@section{METHOD}
  step

    The amount by which the value is incremented/decremented using the mouse in 'horizontal' and 'vertical' link::#-mode#modes::.

    @section{Argument}
 
        A Number.

@section{METHOD}
  shift_scale
    The factor by which link::#-step:: or link::#-keystep:: is multiplied when used at mouse or keyboard interaction while the Shift key is pressed.

    @section{argument}
 
        A Float.

@section{METHOD}
  ctrl_scale
    The factor by which link::#-step:: or link::#-keystep:: is multiplied when used at mouse or keyboard interaction while the Ctrl key is pressed.

    @section{argument}
 
        A Float.

@section{METHOD}
  alt_scale
    The factor by which link::#-step:: or link::#-keystep:: is multiplied when used at mouse or keyboard interaction while the Alt key is pressed.

    @section{argument}
 
        A Float.



@section{SUBSECTION}
  Appearance

@section{METHOD}
  centered

    Whether the deviation of value will be displayed in relation to 0.0 or 0.5 (e.g. as in a panning controller);

    @section{Argument}
 
        A Boolean.

@section{METHOD}
  color

    The colors used by the Knob to draw the following elements:

    @section{list}
 
    ## the main Knob color
    ## the value indicator
    ## the deviation indicator
    ## the background of the deviation indicator
    ::

    @section{Argument}
 
        An Array of four Colors in the order listed above.


@section{SUBSECTION}
  Actions

@section{METHOD}
  action
    The action object evaluated whenever the user interacts with the Knob using the mouse or the keyboard.

@section{METHOD}
  defaultKeyDownAction

    Implements the default effects of key presses as follows:

    @section{table}
 
    ## strong::Key::   || strong::Effect::
    ## r               || valueAction_(1.0.rand)
    ## n               || valueAction_(0)
    ## x               || valueAction_(1)
    ## c               || valueAction_(0.5)
    ## ]               || increment
    ## [               || decrement
    ## up arrow        || increment
    ## down arrow      || decrement
    ## right arrow     || increment
    ## left arrow      || decrement
    ::

    See also: link::#-keystep::, link::#-shift_scale::, link::#-ctrl_scale::, link::#-alt_scale::.

@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
    @section{returns}
 
        The link::#-value::.

@section{METHOD}
  defaultCanReceiveDrag
    @section{returns}
 
        True if the current drag data is a Number.

@section{METHOD}
  defaultReceiveDrag
    Sets link::#-valueAction:: to the current drag data.


@section{EXAMPLES}
 

@section{subsection}
  Basic Example


@racketblock[
(
var window, size = 32; // try different sizes - from 15 to 200 or more!
window = Window.new("Knob", Rect(640,630,270,70)).front;
k = Knob.new(window, Rect(20, 10, size, size));
k.action_({|v,x,y,m| postf("action func called: %\n", v.value); });
//k.color[1] = Color.gray(alpha:0);
)

k.value
k.value = 0.25
k.valueAction = 0.125

// modes
k.mode = \vert;
k.mode = \horiz;
k.mode = \round; // default

k.visible
k.visible = false
k.visible = true
k.enabled = false
k.enabled_(true)
k.canFocus = false
k.canFocus = true
::


]
@section{subsection}
  Centered Mode

Center mode is useful for pan or eq gain control etc.


@racketblock[
(
var window;
window = Window.new("Pan Knob", Rect(640,630,270,70)).front;
k = Knob.new(window, Rect(20,10,36,36));
k.action_({|v,x,y,m| \pan.asSpec.map(v.value).postln; })
//  .mode_(\horiz)
    .centered_(false)
    .value_(\pan.asSpec.unmap(0)); // 0.5
//k.color[1] = Color.gray(alpha:0);
)
k.centered
k.centered = true
k.centered = false
::


]
@section{subsection}
  step

link::#-step:: only affects the 'horiz' and 'vert' modes:


@racketblock[
(
var window, midispec;
midispec = [0,127,'linear',1].asSpec;
window = Window.new("step Knob", Rect(640,630,270,70)).front;
k = Knob.new(window, Rect(20,10,32,32));
k.action_({|v,x,y,m| midispec.map(v.value).postln; })
       .value_(midispec.unmap(0));

k.mode = \vert;

)
k.step
k.step = 10/127 // step by 10

k.mode = \horiz;
k.mode = \round;
::


]
@section{subsection}
  mouseOverAction


@racketblock[
(
var size = 28;
w = Window.new("Knobs", Rect(250,500,270,70));
w.acceptsMouseOver=true; // must be true in parent window!
w.view.decorator = FlowLayout(w.view.bounds);
h = StaticText(w, 150 @ 10);
w.view.decorator.nextLine;
k = Array(8);
8.do({|item, i|
    var knob;
    knob = Knob.new(w, size @ size)
        .action_({|v,x,y,m| h.string = "val: " ++ v.value.asString; })
        .mouseOverAction_({|v,x,y| h.string = "val: " ++ v.value.asString; });

    k = k.add(knob);
});
w.front
)
k[4].value
::


]
@section{subsection}
  Drag and Drop


@racketblock[
(
var w, txt, size = 36;
w = Window.new("Knobs", Rect(400,400,250,100)).front;
w.acceptsMouseOver=true;
w.view.decorator = FlowLayout(w.view.bounds).gap_(10 @ 10).margin_(10 @10);
txt = StaticText(w, 200 @ 14);
w.view.decorator.nextLine;

k = Knob(w, size @ size);
k.action = {arg v,x,y;  v.value.postln; txt.string_("value: " ++ v.value); };
k.mouseOverAction = {|v| txt.string_("value: " ++ v.value); };

j = Knob(w, size @ size);
j.action = {arg v,x,y;  j.value.postln; txt.string_("value: " ++ v.value); };
j.mouseOverAction = { txt.string_("value: " ++ j.value); };

n = NumberBox(w, 100 @ 20);
//n.setProperty(\boxColor,Color.grey(alpha:0.0));
n.value = 0.0;
)

// customize drag and drop methods
k.canReceiveDragHandler
k.canReceiveDragHandler = false; // don't accept drops

k.canReceiveDragHandler = { View.currentDrag.isFloat }; // accept only if drag is float

k.receiveDragHandler = { ("value dropped in: " ++ View.currentDrag).postln }

k.receiveDragHandler = { k.valueAction = View.currentDrag.clip(0.0, 1.0); }

k.beginDragAction = { ("drag out -> " ++ k.value).postln; }

k.beginDragAction = { k.value.asFloat; }
::
]


