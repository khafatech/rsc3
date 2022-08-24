#lang scribble/manual
@(require (for-label racket))

@title{TextField}
 A view displaying editable text@section{categories}
  GUI>Views

@section{description}

A view displaying editable text.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  key



@section{INSTANCEMETHODS}
 


@section{SUBSECTION}
  Data

@section{METHOD}
  string
	The text displayed in the view.

	@section{argument}
 
		A String.

@section{METHOD}
  object
	If link::#-setBoth:: is true, setting this variable also sets link::#-string:: to the value interpreted link::Classes/Object#-asString#as String::.

	@section{argument}
 
		Any object, typically one which makes sense to display as a string, such as a Float.

@section{METHOD}
  setBoth
	A variable stating whether setting link::#-object:: will also set link::#-string::.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  value
	Gets the same as link::#-string::, but when setting also sets link::#-string:: to the value interpreted link::Classes/Object#-asString#as String:: regardless of the link::#-setBoth:: flag.

	@section{argument}
 
		Any object, typically one which makes sense to display as a string, such as a Float.

@section{METHOD}
  valueAction
	Sets link::#-value:: and triggers link::#-action::.


@section{SUBSECTION}
  Appearance

@section{METHOD}
  align
	The alignment of the displayed text. See link::Reference/gui_alignments:: for possible values.

@section{METHOD}
  font
	The font used to display the text.

	@section{argument}
 
		A Font.

@section{METHOD}
  stringColor
	The color used to display the text.

	@section{argument}
 
		A Color.

@section{METHOD}
  background
	Setting this variable colors the inside of the field under the text with the given color.

	@section{argument}
 
		A Color.


@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user changes the text.


@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
	@section{returns}
 
		The displayed link::#-string::.

@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		Always true.

@section{METHOD}
  defaultReceiveDrag
	Sets link::#-valueAction:: to the current drag data.


@section{EXAMPLES}
 

@racketblock[
(
w = Window.new.front;
a = TextField(w, Rect(10, 10, 150, 20));
a.string = "hi there";
a.action = {arg field; field.value.postln; };
)

// does not do the action
a.value = "yo";
a.string = "oy";

a.valueAction_("this is not a pipe"); //does the action, if the value has changed
a.doAction; //evaluates the action with the content of the text field as an argument

a.background_(Color.grey);
a.stringColor_(Color.white);
a.align_(\center);
::

]


