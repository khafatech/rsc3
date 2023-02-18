#lang scribble/manual
@(require (for-label racket))

@title{CheckBox}
 A view that toggles between two states.@section{categories}
  GUI>Views

@section{description}


A view that toggles between two states when clicked, displaying or hiding a check mark accordingly.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key



@section{INSTANCEMETHODS}
 



@section{SUBSECTION}
  Data



@section{METHOD}
  value
	Stating which of the two states the view is currently in, false meaning unchecked and true meaning checked. Default to false.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  valueAction
	Sets link::#-value:: and triggers link::#-action::.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  string
	The text displayed next to the check mark.



@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user toggles the state.



@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
	@section{returns}
 
		The link::#-value::.

@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		True if the current drag data is a Boolean.

@section{METHOD}
  defaultReceiveDrag
	Sets link::#-valueAction:: to the current drag data.


