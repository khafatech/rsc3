#lang scribble/manual
@(require (for-label racket))

@title{Slider}
 A view consisting of a sliding handle.@section{categories}
  GUI>Views

@section{description}


A view that allows setting a numerical value by means of moving a sliding handle. It can have horizontal or vertical orientation, meaning the direction in which the handle moves.




@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  new

	When a new Slider is created, its link::#-orientation:: is determined by the initial size: if it is wider than high, the orientation will be horizontal, otherwise it will be vertical.





@section{INSTANCEMETHODS}
 



@section{SUBSECTION}
  Data

@section{METHOD}
  value
	Numerical value between 0 and 1, represented by the handle position within the groove.

	@section{argument}
 
		A Float.

@section{METHOD}
  valueAction
	Sets link::#-value:: and triggers link::#-action::.

@section{METHOD}
  increment
	Increments the value by link::#-step:: multiplied by 'factor'.

	@section{argument}
  factor
		Any number.

@section{METHOD}
  decrement
	Decrements the value by link::#-step:: multiplied by 'factor'.

	@section{argument}
  factor
		Any number.




@section{SUBSECTION}
  Appearance

@section{METHOD}
  orientation
	The orientation of the Slider - the direction in which the handle moves. The default value depends on the size of the view when created.

	@section{argument}
 
		One of the two Symbols: \horizontal or \vertical.

@section{METHOD}
  thumbSize
	The size of the handle - its width or height, depending on link::#-orientation::.

	@section{argument}
 
		An Integer amount of pixels.

@section{METHOD}
  knobColor
	The color of the handle.

	@section{argument}
 
		A Color.



@section{SUBSECTION}
  Interaction

@section{METHOD}
  step
	The amount by which the value will changed when link::#-increment:: or link::#-decrement:: is called, or when related keys are pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  pixelStep
	The absolute amount by which the value would change if the handle moved by one pixel.

	@section{returns}
 
		A Float.

@section{METHOD}
  shift_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the value by keyboard while the Shift key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  ctrl_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the value by keyboard while the Ctrl key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  alt_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the value by keyboard while the Alt key is pressed.

	@section{argument}
 
		A Float.


@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user moves the handle.

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



@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
	@section{returns}
 
		The link::#-value::.

@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		True if the current drag data is a number.

@section{METHOD}
  defaultReceiveDrag
	Sets link::#-valueAction:: to the current drag data.



@section{EXAMPLES}
 
@section{subsection}
  Show the slider value in a NumberBox

@racketblock[
(
w = Window.new.front;
c = NumberBox(w, Rect(20, 20, 150, 20));
a = Slider(w, Rect(20, 60, 150, 20))
	.action_({
		c.value_(a.value)
		});
a.action.value;
)

( // change the bounds to become vertical
w = Window.new.front;
c = NumberBox(w, Rect(20, 20, 150, 20));
a = Slider(w, Rect(200, 60, 20, 150))
	.action_({
		c.value_(a.value)
		});
a.action.value;
)
::

]
@section{subsection}
  Use a Spec to round and map the output range

@racketblock[
(
w = Window.new.front;
b = ControlSpec(-50, 50, \linear, 0.01); // min, max, mapping, step
c = StaticText(w, Rect(20, 20, 150, 20)).align_(\center).background_(Color.rand);
a = Slider(w, Rect(20, 50, 150, 20))
	.focusColor_(Color.red(alpha:0.2))
	.background_(Color.rand)
	.value_(0.5)
	.action_({
		c.string_(b.map(a.value).asString)
		// round the float so it will fit in the NumberBox
		});
a.action.value;

)
::

]
@section{subsection}
  Change the stepsize of the slider, selected via a PopUpMenu

@racketblock[
(
w = Window.new.front;
a = ["0", "0.0625", "0.125", "0.25", "0.5", "1"];
b = Slider(w, Rect(20, 100, 100, 20))
	.action_({
		c.value_(b.value)
		}).background_(Color.rand);
d = PopUpMenu(w, Rect(20, 60, 100, 20))
	.items_(a)
	.action_({
		b.step_((a.at(d.value)).asFloat);
		});
StaticText(w, Rect(130, 60, 100, 20)).string_("change step");
c = NumberBox(w, Rect(20, 20, 100, 20));
)
::

]
@section{subsection}
  Use the slider view to accept key actions

@racketblock[
( // select the slider, type something and watch the post window
w = Window.new;
c = Slider(w,Rect(0,0,100,30));
c.keyDownAction = { arg view,char,modifiers,unicode,keycode;
[char,modifiers,unicode,keycode].postln;
};
w.front;
)
::

]
@section{subsection}
  Adding functionality to a view by the method addAction
This is useful for adding things to existing frameworks that have action functions already.

@racketblock[
(
w = Window.new("A Slider");
a = Slider.new(w, Rect(40, 10, 300, 30));
w.front
);

// now incrementally add some action to the slider
a.addAction({ |sl| sl.value.postln });
a.addAction({ |sl| w.view.background = Color.green(sl.value) });
a.addAction({ |sl| sl.background = Color.red(1 - sl.value) });

// adding and removing an action:
f = { |sl| "--------*******-------".postln; };
a.addAction(f);
a.removeAction(f);

// or remove all, of course
a.action = nil;
::

]
@section{subsection}
  Use Slider for triggering sounds

@racketblock[
(
s.waitForBoot({
	SynthDef(\pluck,{arg freq=55;
		Out.ar(0,
		Pluck.ar(WhiteNoise.ar(0.06),
			EnvGen.kr(Env.perc(0,4), 1.0, doneAction: Done.freeSelf),
			freq.reciprocal,
			freq.reciprocal,
			10,
		coef:0.1)
		);
	}).add;


	w = Window.new("Hold arrow keys to trigger sound",Rect(300,Window.screenBounds.height-300,400,100)).front;
	a = Slider(w, Rect(50, 20, 300, 40))
		.value_(0.5)
		.step_(0.05)
		.focus
		.action_({
			// trigger a synth with varying frequencies
			Synth(\pluck, [\freq,55+(1100*a.value)]);
			w.view.background_(Gradient(Color.rand,Color.rand));
		})
});

)
::

]
@section{subsection}
  Change background color of Window

@racketblock[
(
w = Window("RGB fader", Rect(100, 500, 400, 400))
	.front;
f = { w.view.background_(Color.new(r.value, g.value, b.value, 1)) };
r = Slider(w, Rect(100, 140, 200, 20))
	.value_(0.5)
	.action_({ f.value });
g = Slider(w, Rect(100, 170, 200, 20))
	.value_(0.5)
	.action_({ f.value });
b = Slider(w, Rect(100, 200, 200, 20))
	.value_(0.5)
	.action_({ f.value });
f.value;
);
::
]


