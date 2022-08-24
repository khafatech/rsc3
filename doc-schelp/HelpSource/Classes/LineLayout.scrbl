#lang scribble/manual
@(require (for-label racket))

@title{LineLayout}
 Superclass of layouts that distribute views in a line@section{categories}
  GUI>Layout
@section{related}
  Classes/HLayout, Classes/VLayout, Classes/GridLayout, Classes/StackLayout, Guides/GUI-Layout-Management

@section{description}

This is an abstract superclass of link::Classes/HLayout:: and link::Classes/VLayout:: which distribute views in a horizontal or vertical line, respectively.

@section{subsection}
  Fine tuning

Each item can be assigned a strong::stretch factor:: and an strong::alignment:: flag to fine tune how its size and position are managed. This can be done at layout link::#*new#construction::, when an item is link::#-add#added:: or link::#-insert#inserted:: or for an already present item with link::#-setStretch:: and link::#-setAlignment:: methods.

The strong::stretch factor:: only affects distribution in the direction of the layout (vertical or horizontal). All items have a stretch factor of 0 by default, so only their own preferences will determine space distribution. As soon as an item is assigned a stretch factor higher than 0, the space will be redistributed according to proportions of stretch factors.

@section{subsection}
  Leaving empty space

An empty space with an arbitrary stretch factor may be inserted using nil in place of an item in combination with the stretch factor. Similarly, an empty space of fixed size may be inserted using an integer in place of an item. See link::#*new#constructor:: and link::#-add:: for details.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  layoutClass
@section{PRIVATE}
  parse

@section{METHOD}
  new

Create a link::Classes/HLayout:: or a link::Classes/VLayout:: and immediately fill it with items given as arguments. (Note that LineLayout is an abstract class and can not be instantiated, but HLayout and VLayout inherit this constructor).

@section{argument}
  ... items
Each item can be a strong::view::, a strong::layout::, strong::nil:: (for stretchable empty space) or an strong::Integer:: (for fixed-size empty space).

@section{discussion}
 

You can assign a strong::stretch factor:: and/or strong::alignment:: to an item by wrapping it into an array, followed by pairs of ('stretch', factor) and/or ('align', alignment). 'stretch' and 'align' may be abbreviated with 's' and 'a'. Simplified syntax for placing key-value pairs into an array comes handy (see link::Reference/Syntax-Shortcuts#Creating arrays with key-value pairs::, and the example below). For possible alignment values see link::Reference/gui_alignments::.

If the item is a stretchable empty space (nil) alignment will have no effect; if the item is a fixed-size empty space (an Integer), it is unaffected by both the stretch factor and alignment.

Example:

@racketblock[
w = Window.new;
w.layout = VLayout(
	[Button().states_([["Foo"]]), stretch:1, align:\bottomLeft],
	20,
	[TextView().string_("Bar\nBar\nBar\n"), s:3],
	[nil, s:1]
);
w.front;
::


]
@section{INSTANCEMETHODS}
 

@section{METHOD}
  add
Add an item to the right or the bottom end of the line, for HLayout and VLayout respectively.

@section{argument}
  item
The item can be a strong::view::, a strong::layout::, an strong::Integer:: (specifying amount in pixels of empty space) or strong::nil:: (for stretchable empty space).

@section{argument}
  stretch
An integer stretch factor.

@section{argument}
  align
A symbol expressing the alignment. See link::Reference/gui_alignments:: for possible values.

@section{discussion}
 
If the item is a stretchable empty space (nil) the align argument will have no effect; if the item is a fixed-size empty space (an Integer) both stretch and align arguments will have no effect.


@section{METHOD}
  insert
Insert an item at a specific position.

@section{argument}
  item
The item can be a strong::view::, a strong::layout::, an strong::Integer:: (specifying amount in pixels of empty space) or strong::nil:: (for stretchable empty space).

@section{argument}
  index
The integer position among current items at which to insert the new item. If index is smaller than 0 or larger than the current amount of items, the new item will always be added to the end of the line.

@section{argument}
  stretch
An integer stretch factor.

@section{argument}
  align
A symbol expressing the alignment. See link::Reference/gui_alignments:: for possible values.

@section{discussion}
 
If the item is a stretchable empty space (nil) the align argument will have no effect; if the item is a fixed-size empty space (an Integer) both stretch and align arguments will have no effect.



@section{METHOD}
  setStretch
Set stretch factor of an item contained in the layout.

@section{argument}
  item
A view or layout managed by this layout, or an Integer index of an item.

@section{argument}
  stretch
An integer stretch factor.



@section{METHOD}
  setAlignment
Set alignment of an item contained in the layout.

@section{argument}
  item
A view or a layout managed by this layout, or an Integer index of an item.

@section{argument}
  align
A symbol expressing the alignment. See link::Reference/gui_alignments:: for possible values.


@section{EXAMPLES}
 

Try resizing the window created by the following code.


@racketblock[
w = Window.new;
w.layout = VLayout(
	TextView().string_("Foo\nBar\nFoo\nBar\nFoo"),
	HLayout(
		Button().states_([["Foo"]]),
		[TextField().string_("Bar"), stretch:1],
		[TextField().string_("BarBarBar"), stretch:4]
	)
);
w.front;
::
]


