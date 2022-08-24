#lang scribble/manual
@(require (for-label racket))

@title{RangeSlider}
 A view consisting of a sliding extendable handle@section{categories}
  GUI>Views

@section{description}


A view that allows setting two numerical values between 0 and 1, represented by the two ends of a movable and extendable handle. It can have horizontal or vertical orientation, meaning the direction in which the handle moves and extends.

Dragging the mouse pointer on either end of the range moves the end by itself. Dragging in the middle of the range moves the whole range without changing its size.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  new

    When a new RangeSlider is created, its link::#-orientation:: is determined by the initial size: if it is wider than high, the orientation will be horizontal, otherwise it will be vertical.




@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Data

@section{METHOD}
  lo

	The low end of the range.

	If you attempt to set it higher then the current link::#-hi::, -hi will be set instead, and -lo will become the old -hi.

	When setting -lo the value will always be clipped to the range between 0 and 1.

	@section{argument}
 
		A Float between 0 and 1.

@section{METHOD}
  hi

	The high end of the range. If you attempt to set it lower then the current link::#-lo::, -lo will be set instead, and -hi will become the old -lo.

	When setting -hi the value will always be clipped to the range between 0 and 1.

	@section{argument}
 
		A Float between 0 and 1.

@section{METHOD}
  activeLo

	Sets link::#-lo:: to the argument and triggers link::#-action::.

@section{METHOD}
  activeHi

	Sets link::#-hi:: to the argument and triggers link::#-action::.

@section{METHOD}
  range

	The difference between link::#-hi:: and link::#-lo::. Setting -range will set -hi to -lo + -range.

@section{METHOD}
  activeRange

	Sets link::#-range:: to the argument and triggers link::#-action::.

@section{METHOD}
  setSpan

	Sets link::#-lo:: and link::#-hi:: to each of the arguments, respectively.

@section{METHOD}
  setSpanActive

	Calls link::#-setSpan::, forwarding the arguments, and triggers link::#-action::.

@section{METHOD}
  setDeviation

	Sets link::#-lo:: and link::#-hi:: according to their deviation and their average instead of their absolute values.

	@section{argument}
  deviation
		A Float determining the absolute deviation of -lo and -hi from their average.
	@section{argument}
  average
		A Float determining the average of -lo and -hi.

@section{METHOD}
  increment
	Increments both link::#-lo:: and link::#-hi:: by link::#-step:: multiplied by 'factor'.

	@section{argument}
  factor
		A Float.

@section{METHOD}
  decrement
	Decrements both link::#-lo:: and link::#-hi:: by link::#-step:: multiplied by 'factor'.

	@section{argument}
  factor
		A Float.





@section{SUBSECTION}
  Appearance

@section{METHOD}
  orientation
	The orientation of the RangeSlider - the direction in which the handle moves and is extendable. The default value depends on the size of the view when created.

	@section{argument}
 
		One of the two Symbols: \horizontal or \vertical.

@section{METHOD}
  knobColor
	The color of the handle.

	@section{argument}
 
		A Color.




@section{SUBSECTION}
  Interaction

@section{METHOD}
  step
	The amount by which the range will change when link::#-increment:: or link::#-decrement:: is called, or when related keys are pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  pixelStep
	The absolute amount by which the range would change if the handle moved by one pixel.

	@section{returns}
 
		A Float.

@section{METHOD}
  shift_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the range by keyboard while the Shift key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  ctrl_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the range by keyboard while the Ctrl key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  alt_scale
	The factor by which link::#-step:: is multiplied when incrementing or decrementing the range by keyboard while the Alt key is pressed.

	@section{argument}
 
		A Float.



@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user changes the position or size of the handle.

@section{METHOD}
  defaultKeyDownAction

	Implements the default effects of key presses as follows:

	@section{table}
 
	## strong::Key::   || strong::Effect::
	## a               || lo_(0), hi_(1), and triggers action
	## n               || lo_(0), hi_(0), and triggers action
	## x               || lo_(1), hi_(1), and triggers action
	## c               || lo_(0.5), hi_(0.5), and triggers action
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
 
		A Point of which the x and y coordinates are set to link::#-lo:: and link::#-hi::, respectively.

@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		True if the current drag data is a Point.

@section{METHOD}
  defaultReceiveDrag
	Sets  link::#-lo:: and link::#-hi:: to the two coordinates of the Point stored as the current drag data, respectively, and triggers the link::#-action::.



@section{EXAMPLES}
 

@section{subsection}
  Basic examples


@racketblock[
(
w = Window.new.front;
a = RangeSlider(w, Rect(20, 80, 120, 30))
    .lo_(0.2)
    .range_(0.4)
    .action_({ |slider|
        [\sliderLOW, slider.lo, \sliderHI, slider.hi].postln;
    });
)
::

]

@racketblock[
(
w = Window.new.front;
a = RangeSlider(w, Rect(20, 80, 120, 30))
    .lo_(0.2)
    .hi_(0.8)
    .action_({ |slider|
        b.activeLo_(slider.lo); // this will trigger the action of b (and set it's value)
        b.hi_(slider.hi);
    });
b = RangeSlider(w, Rect(220, 80, 20, 130))
    .lo_(0.2)
    .hi_(0.8)
    .knobColor_(HiliteGradient(Color.grey, Color.white,\h))
    .action_({ |slider|
        [\sliderLOW, slider.lo, \sliderHI, slider.hi].postln;
    });

)
::

]
@section{subsection}
  Use of setDeviation


@racketblock[
(

w = Window("setDeviation", Rect(300, 300, 300, 150));
a = RangeSlider(w, Rect(10, 10, 200, 30))
    .lo_(0)
    .hi_(1);
b = Slider(w, Rect(10, 50, 200, 30))
    .action_(
        {   arg me;
            a.setDeviation(c.value, b.value);
        });
c = Slider(w, Rect(10, 100, 200, 30))
    .action_(
        {   arg me;
            a.setDeviation(c.value, b.value);
        }
    );
c.valueAction = 0.2;
w.front;
)
::

]
@section{subsection}
  Sound example

Shape a bandpass filter.

In Cocoa GUI, hold down the Ctrl key to move the whole range; in other GUI kits you can simply click within the range and drag it.


@racketblock[
(
s.waitForBoot({
    a={arg freq=1800, bw=0.2;
            var r;
            BBandPass.ar(WhiteNoise.ar(0.3), freq, bw);

            }.play;

    w = Window("2DSlider", Rect(100,Window.screenBounds.height-400, 400 ,50));
    t = RangeSlider(w, Rect(10, 10, 380, 30))
            .lo_(0.4)
            .hi_(0.6)
            .action_({|sl|
                a.set(\freq,1800*(sl.lo+sl.lo)+10,\bw, (sl.hi-sl.lo).abs+0.01);
            });
    t.doAction;

    w.front;
    CmdPeriod.doOnce({w.close});
})
)
::
]


