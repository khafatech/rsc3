#lang scribble/manual
@(require (for-label racket))

@title{TreeViewItem}
 An item in TreeView@section{categories}
  GUI>Views

@section{description}


An instance of TreeViewItem represents an item in TreeView. There may be multiple instances representing the same item, e.g. after calling link::Classes/TreeView#-currentItem:: multiple times.

@section{INSTANCEMETHODS}
 

@section{PRIVATE}
  prValidItem

@section{METHOD}
  index
	@section{RETURNS}
  An integer position of this item among its siblings.

@section{METHOD}
  parent
	@section{RETURNS}
  An new instance of TreeViewItem representing the parent item.

@section{METHOD}
  childAt
	@section{RETURNS}
  A new instance of TreeViewItem representing the child item at 
@racketblock[index::.

]
@section{METHOD}
  addChild
	Appends a new child to this item.

	@section{ARGUMENT}
  strings
		An array of Strings (or nil), each for the text of one data field.
	@section{RETURNS}
 
		An instance of TreeViewItem representing the new item.

@section{METHOD}
  insertChild
	Inserts a new child to this item at 
@racketblock[index::.

	]
@section{ARGUMENT}
  index
		The position at which to insert the child.
	@section{ARGUMENT}
  strings
		An array of Strings (or nil), each for the text of one data field.
	@section{RETURNS}
 
		An instance of TreeViewItem representing the new item.

@section{METHOD}
  strings
	The text in the data fields.

	@section{ARGUMENT}
  strings
		An array of Strings (or nil), each for the text of one data field.

@section{METHOD}
  setString
	Sets the text in the given data field.

	@section{ARGUMENT}
  column
		An integer index of a data field.
	@section{ARGUMENT}
  string
		A String or nil.

@section{METHOD}
  colors
	The background colors of the data fields.

	@section{ARGUMENT}
  colors
		An array of Colors, each for the color of one data field.

@section{METHOD}
  setColor
	Sets the background color of the given data field.

	@section{ARGUMENT}
  column
		An integer index of a data field.
	@section{ARGUMENT}
  color
		A Color.

@section{METHOD}
  textColors
	The text colors of the data fields.

	@section{ARGUMENT}
  textColors
		An array of Colors, each for the color of one data field.

@section{METHOD}
  setTextColor
	Sets the text color of the given data field.

	@section{ARGUMENT}
  column
		An integer index of a data field.
	@section{ARGUMENT}
  color
		A Color.

@section{METHOD}
  setView
	Places another view into the given data field. Only one view can be placed into a data field at once. If a view is already present, it will be removed and destroyed.

	If the number of data fields decreases due to a call to link::Classes/TreeView#-columns::, the views contained in removed data fields will also be removed and destroyed.

	@section{ARGUMENT}
  column
		An integer index of a data field.
	@section{ARGUMENT}
  view
		A View.

@section{METHOD}
  removeView
	Removes the view from the given data field, if any.

	@section{ARGUMENT}
  column
		An integer index of a data field.

@section{METHOD}
  view
	The view in the given data field.

	@section{ARGUMENT}
  column
		An integer index of a data field.

@section{METHOD}
  ==
	Implements equality comparison between two TreeViewItem instances. Two instances are equal if they represent the same item in TreeView.

	@section{RETURNS}
  A Boolean.

@section{METHOD}
  isNull
	Whether the item is invalid. After an item is removed, all related TreeViewItem instances become invalid.

	@section{RETURNS}
  A Boolean.


