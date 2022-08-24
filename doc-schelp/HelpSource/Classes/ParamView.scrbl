#lang scribble/manual
@(require (for-label racket))

@title{ParamView}
 show a parameter of a JITLib process@section{categories}
  Libraries>JITLib
@section{related}
  Classes/JITGui, Overviews/JITLib

@section{description}

ParamView displays a parameter value, and switches representation as appropriate for value: A single number is shown by an EZSlider, a pair of numbers by an EZRanger, and anything else as code on an EZText.

First examples:

@racketblock[

w = Window("test", Rect(20, 820, 400, 100)).front;
w.addFlowLayout;
~pv = ParamView(w, Rect(20, 20, 360, 20));
~pv2 = ParamView(w, Rect(20, 20, 360, 40));

// not working properly yet
~pv.bounds_(Rect(4, 4, 300, 40));

~pv.dump
~pv.viewType_(0); // EZNumber
~pv.viewType_(1); // EZRanger
~pv.viewType_(2); // EZText

~pv.label_(\freq);
~pv.spec_(\freq);  // needs spec for EZSlider


~pv.value_(200);
~pv.value_(2000);
// switches to EZRanger
~pv.value_([200, 2000]);
// 3 numbers -> switches to EZText
~pv.value_([20, 200, 2000]);
// anything else -> EZText
~pv.value_(\blonk);
~pv.action = { |pv| pv.value.postcs };

::

]
@section{CLASSMETHODS}
 

@section{METHOD}
  new
create a new ParamView with
@section{ARGUMENT}
  parent
the parent window or view
@section{ARGUMENT}
  bounds
the bounds of the view
@section{ARGUMENT}
  label
a label to display
@section{ARGUMENT}
  spec
a controlspec for the value
@section{ARGUMENT}
  action
an action to do when the value changes
@section{ARGUMENT}
  initVal
an initial value
@section{ARGUMENT}
  initAction
a boolean whether to perform the action on init.


@section{INSTANCEMETHODS}
 

@section{METHOD}
  label
get and set the view's label

@section{METHOD}
  spec
get and set the view's control spec

@section{METHOD}
  action
get and set the paramview's action

@section{private}
  zone, zones

@section{METHOD}
  ezviews, slider, ranger, textview
the 3 ezviews between which the ParamView switches

@section{METHOD}
  currview
the currently shown view of these

@section{METHOD}
  value
get and set value

@section{METHOD}
  valueAction
get and set value and do action

@section{METHOD}
  doAction
do the view's action

@section{METHOD}
  viewType
get and set the view's type:
0 is single number -> EZSlider,
1 is pair of numbers -> EZRanger,
2 is anything else

@section{METHOD}
  valueType
determine viewType for a given value


@section{METHOD}
  background
get and set background color

@section{private}
  init


