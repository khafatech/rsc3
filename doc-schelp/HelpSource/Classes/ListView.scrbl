#lang scribble/manual
@(require (for-label racket))

@title{ListView}
 A view displaying a list of text items.@section{categories}
  GUI>Views

@section{description}


A view that displays a list of text items and allows one or more of them to be selected, depending on link::#-selectionMode::.

In default selection mode (single item selection), clicking on an item will select it, and pressing the up or down arrow keys will move selection to previous or next item, respectively. Other selection modes allow more complex interaction.

There is a difference between the concepts of link::#-value#current:: item, end link::#-selection#selected:: items. In default selection mode they will always be the same, but not so in other modes.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  key




@section{INSTANCEMETHODS}
 



@section{SUBSECTION}
  Data

@section{METHOD}
  items
	The list of items displayed by the view.

	@section{argument}
 
		An Array of Strings, each String defining the text to represent an item.

@section{METHOD}
  clear
    Removes all items.

@section{METHOD}
  value
	The index of the current item, or nil when there is no current item. Note that this may be different than link::#-selection:: when link::#-selectionMode:: allows multiple items to be selected.

	@section{argument}
 
		An Integer or nil.

@section{METHOD}
  valueAction
	Sets link::#-value:: and triggers the link::#-action::.

@section{METHOD}
  selection
	An array of all selected indexes. When setting selection, either an array or a single integer may be used.

	Note that this may be different than link::#-value:: when link::#-selectionMode:: allows multiple items to be selected. When setting selection in single-item selection mode, only the last index will remain selected.


@section{SUBSECTION}
  Appearance

@section{METHOD}
  colors
	The background colors of the items.

	@section{argument}
 
		An Array of Colors, one Color for each item.

@section{METHOD}
  stringColor
	The color used to display all the text of all unselected items.

	@section{argument}
 
		A Color.

@section{METHOD}
  selectedStringColor
	The color used to display the selected item's text.

	@section{argument}
 
		A Color.

@section{METHOD}
  hiliteColor
	The color used to indicate the selected item (aside from the color of its text).

	@section{argument}
 
		A Color.




@section{SUBSECTION}
  Interaction

@section{METHOD}
  selectionMode

	The allowed mode of item selection, according to the following table:

	@section{table}
 
	## strong::Value::  || strong::Meaning::
	## \none            || No item can be selected.
	## \single          || Only a single item can be selected at once.
	## \multi           || Multiple items can be selected. An item's selection is toggled when clicked.
	## \extended        || Multiple items can be selected, individually by holding the Ctrl key, and in a batch by holding the Shift key.
	## \contiguous      || Multiple neighbouring items can be selected by holding the Shift key.
	::

	@section{argument}
 
		One of the Symbols listed in the table above.



@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user changes the emphasis::current:: item, i.e. when link::#-value:: changes as a result of GUI interaction.

@section{METHOD}
  selectionAction

The action object evaluated whenever link::#-selection:: changes.

@section{METHOD}
  enterKeyAction
	The action object evaluated whenever the user presses the Enter (Return) key.

@section{METHOD}
  defaultKeyDownAction

	Implements the default effects of key presses as follows:

	@section{table}
 
	## strong::Key::   || strong::Effect::
	## space           || select next item and trigger action
	## r               || trigger enterKeyAction
	## n               || trigger enterKeyAction
	## a number        || trigger enterKeyAction
	## up arrow        || select previous item and trigger action
	## down arrow      || select next item and trigger action
	## left arrow      || select previous item and trigger action
	## right arrow     || select next item and trigger action
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
w = Window.new.front;

v = ListView(w,Rect(10,10,120,70))
    .items_([ "SinOsc", "Saw", "LFSaw", "WhiteNoise", "PinkNoise", "BrownNoise", "Osc" ])
    .background_(Color.clear)
    .hiliteColor_(Color.green(alpha:0.6))
    .action_({ arg sbs;
        [sbs.value, v.items[sbs.value]].postln; // .value returns the integer
    });
)
::


]
@section{subsection}
  Sound Example

Use ListView to switch filters:

@racketblock[
(
s.waitForBoot({
    var f, freq, ww;
    n={r=LFSaw.ar([220, 530],0,0.3)*LFPulse.ar(12,0,0.3,0.4); [r[0],Delay2.ar(r[1])]}.play;
    freq={SinOsc.kr(0.5,0,4000,4200)}.play;
    w=Window("Filters").front;
    v = ListView(w,Rect(10,10,180,120))
        .items_([ "No Filter","RLPF", "RHPF", "BPF", "Resonz", "MoogFF" ])
        .background_(Color.clear)
        .hiliteColor_(Color.green(alpha:0.6))
        .action_({arg v;
            v.value.switch(
            0,{try{f.free};"test".postln},
            1,{try{f.free};f={ReplaceOut.ar(0,RLPF.ar(In.ar(0,2),In.kr(0,1),0.2,0.3))}.play(addAction:\addToTail)},
            2,{try{f.free};f={ReplaceOut.ar(0,RHPF.ar(In.ar(0,2),In.kr(0,1),0.2,0.3))}.play(addAction:\addToTail)},
            3,{try{f.free};f={ReplaceOut.ar(0,BPF.ar(In.ar(0,2),In.kr(0,1),0.2,1.5))}.play(addAction:\addToTail)},
            4,{try{f.free};f={ReplaceOut.ar(0,Resonz.ar(In.ar(0,2),In.kr(0,1),0.2,2))}.play(addAction:\addToTail)},
            5,{try{f.free};f={ReplaceOut.ar(0,MoogFF.ar(In.ar(0,2),In.kr(0,1),1.5))}.play(addAction:\addToTail)}
            );
        });

    ww=FreqScope.new(400, 200, 0);
    w.bounds=Rect(50,Window.screenBounds.height-300,200,200);
    ww.window.bounds=ww.window.bounds.moveTo(255,Window.screenBounds.height-328);
    CmdPeriod.doOnce({{ww.window.close}.defer(0.5);w.close;});
            //defer or crash, because FreqScopeWindow Class contains its own routine for cleaning up on CmdPeriod
    w.onClose_({n.free;f.free;freq.free});
});
)
::
]


