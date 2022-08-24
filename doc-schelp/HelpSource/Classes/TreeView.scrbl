#lang scribble/manual
@(require (for-label racket))

@title{TreeView}
 A view displaying a tree of items with columns@section{categories}
  GUI>Views

@section{description}


A view that displays a hierarchy of items. It is divided into rows and column: each row represents an item, and each column represents a different data field of the items.

The items are represented in code by instances of link::Classes/TreeViewItem::, returned by the various TreeView methods. Top level items are added via the TreeView interface, while child items are added via the TreeViewItem interface, which also allows to manipulate items in more detail after their creation.

Items can be visually sorted with link::#-sort::, or by clicking on one of the column headers, if link::#-canSort:: is enabled.

Each item can hold other views in each of its data fields, which allows for rich graphical interaction. See link::Classes/TreeViewItem#-setView::.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{INSTANCEMETHODS}
 

@section{PRIVATE}
  prForEachColumnDataPair
@section{PRIVATE}
  prValidItem


@section{SUBSECTION}
  Data

@section{METHOD}
  columns
	Gets or sets the number of columns (data fields) and their names. When setting a smaller number of columns than the current the extra columns will be removed, and hence all the data stored stored in those columns.

	@section{ARGUMENT}
 
		An array of Strings for column names.

@section{METHOD}
  numColumns
	The total number of columns (data fields).

@section{METHOD}
  addItem
	Append a new top-level item.

	@section{ARGUMENT}
  strings
		An array of Strings (or nil), each for the text of one data field.
	@section{RETURNS}
 
		An instance of TreeViewItem representing the new item.

@section{METHOD}
  insertItem
	Insert a new top-level item at 
@racketblock[index::.

	]
@section{ARGUMENT}
  index
		The position at which to insert the item.
	@section{ARGUMENT}
  strings
		An array of Strings (or nil), each for the text of one data field.
	@section{RETURNS}
 
		An instance of TreeViewItem representing the new item.

@section{METHOD}
  removeItem
	Remove the given 
@racketblock[item::. After the item is removed, any usage of the related TreeViewItems will have no effect.

	]
@section{ARGUMENT}
 
		An instance of TreeViewItem.

@section{METHOD}
  numItems
	The total number of items.

@section{METHOD}
  clear
    Removes all items.

@section{METHOD}
  currentItem
	Gets or sets the currently selected item.

	@section{ARGUMENT}
 
		An instance of TreeViewItem.
	@section{RETURNS}
 
		An instance of TreeViewItem or nil, if no current item.

@section{METHOD}
  itemAt
	The item at 
@racketblock[index::.

]
@section{METHOD}
  childAt
	Alias for link::#-itemAt::, provided for compatibility with TreeViewItem.

@section{METHOD}
  addChild
	Alias for link::#-addItem::, provided for compatibility with TreeViewItem.

@section{METHOD}
  insertChild
	Alias for link::#-addChild::, provided for compatibility with TreeViewItem.

@section{SUBSECTION}
  Appearance

@section{METHOD}
  sort
	Sort items by data in 
@racketblock[column::. This works regardless of link::#-canSort::.

	]
@section{NOTE}
  Sorting has no effect on the logical order of the items, it only affects how they are displayed. ::

	@section{ARGUMENT}
  column
		The integer column index to sort by.
	@section{ARGUMENT}
  descending
		Whether to sort in descending or ascending fashion. The default is ascending.

@section{PRIVATE}
  background


@section{SUBSECTION}
  Interaction

@section{METHOD}
  canSort
	Whether the user can sort the items by clicking on a column header.

	When setting to 
@racketblock[true::, the items will be sorted immediately according to the current sorting column. While ]

@racketblock[true::, the view will also automatically sort new items.

	The default is ]

@racketblock[false::.

	See also: link::#-sort::.




]
@section{SUBSECTION}
  Actions

@section{METHOD}
  itemPressedAction
	The object to be evaluated when a mouse button is pressed on an item, passing this view as the argument.

@section{METHOD}
  onItemChanged
	The object to be evaluated whenever the current item changes, passing this view as the argument.


