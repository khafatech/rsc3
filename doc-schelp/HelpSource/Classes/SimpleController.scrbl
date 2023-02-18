#lang scribble/manual
@(require (for-label racket))

@title{SimpleController}
 Controller according to the model-view-controller (M-V-C) paradigm@section{categories}
  Core
@section{related}
  Classes/Object

@section{description}


SimpleController can be used as a controller according to the model-view-controller (M-V-C) paradigm. It provides an
link::Classes/IdentityDictionary:: of actions, which are called whenever the attached model sends a notification by
calling changed.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Creates a SimpleController instance with the model to be observed.

@section{argument}
  model
An object of any class


@section{INSTANCEMETHODS}
 

@section{private}
  init, update

@section{METHOD}
  put

@section{argument}
  what
Register an action, which is called when the model invokes changed(what, moreArgs).

@section{argument}
  action

Action to register.


@section{METHOD}
  remove

Remove a registered action.

@section{argument}
  action

Action to remove

@section{EXAMPLES}
 


@racketblock[
(
var controller, model, view;

model = Ref(0.5);
controller = SimpleController(model);
controller.put(\value,
	{|theChanger, what, moreArgs|
		view.value_(theChanger.value);
	});

view = Slider(Window("slider", Rect(100, 100, 330, 38)).front, Rect(5, 5, 320, 20));
view.onClose_{controller.remove};

// run a routine to change the model's value:
r{
	100.do{
		model.value_(1.0.rand.postln).changed(\value);
		0.5.wait;
	}
}.play(AppClock)
)
::
]


