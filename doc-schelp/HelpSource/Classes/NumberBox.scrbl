#lang scribble/manual
@(require (for-label racket))

@title{NumberBox}
 A view displaying a modifiable numerical value.@section{categories}
  GUI>Views

@section{description}


A view that displays a numerical value and allows to modify it by typing the value in, and incrementing or decrementing it using the keyboard, or mouse.

Using the keyboard, the value will change on each arrow key press by the amount defined by link::#-step::.

Mouse scrolling is performed by pressing a mouse button inside the view and dragging the mouse vertically. The value will change according to the mouse cursor movement, in steps defined by link::#-scroll_step::.

By default, holding down the Shift, Ctrl, or Alt key while incrementing or decrementing the value will multiply the steps by 100, 10, or 0.1 respectively, though you can customize this by setting link::#-shift_scale::, link::#-ctrl_scale::, or link::#-alt_scale::. Scrolling can be enabled or disabled by modifying the link::#-scroll:: variable.




@section{CLASSMETHODS}
 

@section{PRIVATE}
  key




@section{INSTANCEMETHODS}
 




@section{SUBSECTION}
  Data

@section{METHOD}
  value
	Numerical value between 0 and 1.

	@section{argument}
 
		A Float.

@section{METHOD}
  valueAction
	Sets link::#-value:: to the argument and triggers link::#-action::.

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

@section{METHOD}
  string
	Text to be displayed instead of the numerical value. Setting link::#-value:: after this will display the value again.

	@section{argument}
 
		A String.

@section{METHOD}
  object
	If link::#-setBoth:: is true, setting this variable also sets link::#-string:: to the argument interpreted link::Classes/Object#-asString#as String::.

	@section{argument}
 
		Any object, typically one which makes sense to display as a string, such as a Float.

@section{METHOD}
  setBoth
	A variable stating whether setting link::#-object:: will also set link::#-string::.

	@section{argument}
 
		A Boolean.




@section{SUBSECTION}
  Restrictions on data

@section{METHOD}
  clipLo
	The lowest numerical value allowed. Trying to set a lower value will clip it to this.

	@section{argument}
 
		A Float.

@section{METHOD}
  clipHi
	The highest numerical value allowed. Trying to set a higher value will clip it to this.

	@section{argument}
 
		A Float.

@section{METHOD}
  minDecimals
	The minimum amount of decimal places displayed. If the value can be completely described with less decimal places, zeros will be appended until reaching this.

	@section{argument}
 
		An Integer.

@section{METHOD}
  maxDecimals
	The maximum amount of decimal places displayed. The value will always be rounded to this amount of decimals.

	@section{argument}
 
		An Integer.

@section{METHOD}
  decimals
	Sets both link::#-minDecimals:: and link::#-maxDecimals:: to the argument.

	@section{argument}
 
		An Integer.




@section{SUBSECTION}
  Appearance

@section{METHOD}
  align
	The alignment of the displayed value. See link::Reference/gui_alignments:: for possible values.

@section{METHOD}
  stringColor
	The color used to display the value before it is ever changed by user interaction.

	@section{argument}
 
		A Color.

@section{METHOD}
  normalColor
	The color used to display the value after is has been typed in.

	@section{argument}
 
		A Color.

@section{METHOD}
  typingColor
	The color used to display the value while it is being typed in.

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
  scroll_step
	The amount by which the value will changed when scrolled using the mouse.

	@section{argument}
 
		A Float.

@section{METHOD}
  shift_scale
	The factor by which link::#-step:: or link::#-scroll_step:: is multiplied when incrementing or decrementing the value using keyboard or mouse while the Shift key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  ctrl_scale
	The factor by which link::#-step:: or link::#-scroll_step:: is multiplied when incrementing or decrementing the value using keyboard or mouse while the Ctrl key is pressed.

	@section{argument}
 
		A Float.

@section{METHOD}
  alt_scale
	The factor by which link::#-step:: or link::#-scroll_step:: is multiplied when incrementing or decrementing the value using keyboard or mouse while the Alt key is pressed.

	@section{argument}
 
		A Float.



@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user changes the value by interacting with the view.

@section{METHOD}
  defaultKeyDownAction

	Any key representing a character that can make part of a floating point number representation will initiated the editing of the value. Pressing Return (or Enter) will finish the editing and store the value typed in as a Float.

	Aside from that, this method implements the default effects of key presses as follows:

	@section{table}
 
	## strong::Key::   || strong::Effect::
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
  Basic Example


@racketblock[
(
    w = Window("NumberBox Example", Rect(100, 500, 400, 120));
    b =     NumberBox(w, Rect(150, 10, 100, 20));
    b.value = rrand(1,15);
    b.action = {arg numb; numb.value.postln; };
    w.front
)
// try these one at time
b.value = rrand(1,15) ;     // sets the value but does not perform the action
b.valueAction_(5);      // sets the value and performs the action
b.step_(0.1);           // change the increment/decrement size for the arrow keys
b.scroll_step=10;    // change the increment/decrement size for the mosueScrolling

b.background_(Color.grey);          // change the background color of the box
b.typingColor_(Color(0.3,1,0.3));   // change the typing color for the box
b.normalColor_(Color.white);        // change the normal color for the box. won't change until next value change

b.stringColor = Color.red;
b.align = \center;

b.increment; // increment or decrement by step
b.decrement;
::


]
@section{subsection}
  Sound Example

Change frequency of a playing synth by step using arrow keys:


@racketblock[
(
s.waitForBoot({

    n={arg freq=220;
        var out;
        out=SinOsc.ar(freq,0,0.2);
        8.do{out = AllpassN.ar(out, 0.2,0.02+0.20.rand,8)};
        out;
    }.play;

    w = Window("Use arrow keys to change the frequency by steps", Rect(100, 500, 500, 120));
    b = NumberBox(w, Rect(200, 10, 100, 20));
    b.value = 220;
    b.action = {arg numb; n.set(\freq,numb.value); };
    b.step=55; //make the step a fraction of the freq
    b.focus;
    w.front;

    CmdPeriod.doOnce({w.close});

});
)
::
]


