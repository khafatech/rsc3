#lang scribble/manual
@(require (for-label racket))

@title{PopUpMenu}
 A view displaying a text item selectable from a drop-down menu.@section{categories}
  GUI>Views

@section{description}


When clicked, this view opens a menu containing several text items, then closes the menu and displays one of the items after it is selected.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key




@section{INSTANCEMETHODS}
 



@section{SUBSECTION}
  Data

@section{METHOD}
  items
	The list of items displayed in a menu when the view is clicked.

	@section{argument}
 
		An Array of Strings or Symbols.

@section{METHOD}
  clear
 Removes all items.

@section{METHOD}
  item
	The currently selected item.

	@section{returns}
 
		A String.

@section{METHOD}
  value
	The index of the currently selected item.

	@section{argument}
 
		An integer, or nil meaning no selected item.

@section{METHOD}
  valueAction
	Sets link::#-value:: and triggers link::#-action::.

	@section{argument}
 
		An integer, or nil meaning no selected item.



@section{SUBSECTION}
  Appearance

@section{METHOD}
  stringColor
	The color used to display text.

	@section{argument}
 
		A Color.

@section{METHOD}
  background
	Setting this variable colors the area of the view under the text with the given color.

	@section{argument}
 
		A Color.



@section{SUBSECTION}
  Interaction

@section{METHOD}
  allowsReselection
	Determines whether the action is triggered when selecting already selected item. Defaults to false.

	@section{argument}
 
		A Boolean.



@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user changes the selected item from the menu. See link::#-allowsReselection:: for customization.



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
w = Window.new("The Eightfold Path").front;
m = PopUpMenu(w, Rect(10, 10, 180, 20));

m.items = [
 "right view", "right thinking", "right mindfulness", "right speech",
 "right action", "right diligence", "right concentration", "right livelihood"
];

m.background_(Color.green(0.7));  // only changes the look of displayed item
m.stringColor_(Color.white);   // only changes the look of displayed item
m.font_(Font("Courier", 13));   // only changes the look of displayed item
m.action = { arg menu;
 [menu.value, menu.item].postln;
};
)

m.value;   // returns the index of the current item;
m.item;    // returns the String or Symbol of the current item

m.value_(2);  // changes the displayed item, but does not evaluate the action
m.valueAction_(3); // evaluates the action.
::

]
@section{subsection}
  Sound Example

Play different functions:


@racketblock[
(
s.waitForBoot({

 var w, menu, snd, funcs, b;

 w = Window.new.front;

 menu = PopUpMenu(w, Rect(10, 10, 90, 20))
  .items_(["Sine", "Saw" , "Noise" , "Pulse"]);

 funcs = [
  { SinOsc.ar(440, 0, 0.3) },
  { Saw.ar(440, 0.3) },
  { WhiteNoise.ar(0.3) },
  { Pulse.ar(440, 0.2, 0.3) }
 ];

 b = Button(w, Rect(110, 10, 180, 20))
  .states_([["play", Color.black, Color.green]])
  .mouseDownAction_({
    snd = funcs.at(menu.value).play;
   })
  .action_({ arg butt, mod;
    snd.release;
   });

 w.front;

 p = CmdPeriod.add({ b.value_(0) }); // set button to 0 on hitting Cmd-period
 w.onClose_{ snd.release; CmdPeriod.removeAll }; // clean up when window is closed

})
)
::
]

