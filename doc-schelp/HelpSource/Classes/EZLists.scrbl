#lang scribble/manual
@(require (for-label racket))

@title{EZLists}
 An abstract superclass for EZListView and EZPopUpMenu@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/EZListView, Classes/EZPopUpMenu

@section{description}

Users will not normally directly create instances of EZLists, but only use it through its subclasses. It provides the basic mechanisms for link::Classes/EZListView:: and link::Classes/EZPopUpMenu::.

@section{classmethods}
 

@section{method}
  new

@section{instancemethods}
 

@section{subsection}
  Building and Changing the List

@section{method}
  globalAction
Set/get the global function to be performed in addition to the item functions: 
@racketblock[ { arg listObj; value } ::.

]
@section{method}
  items
Set/get an link::Classes/Array:: of Associations including the labels and the item functions: 
@racketblock[ ['label' -> { arg listObj; value }, ] ::.
]
@section{discussion}
 
In menus, the macOS graphics system gives special meanings to some characters. See link::Classes/PopUpMenu:: ; Or and link::Classes/Array:: link::Classes/Symbol::s (if you are only using 
@racketblock[globalAction::). link::Classes/Array::s of link::Classes/Symbol::s will get converted into and array of link::Classes/Association::s with and empty link::Classes/Function:: ]

@racketblock[ ['label' -> {}, ] ::.

]
@section{method}
  item
@section{Returns}
  the item label of the current selection.

@section{method}
  itemFunc
@section{Returns}
  the item function of the current selection.

@section{method}
  addItem
Adds an item.
@section{argument}
  name
An instance of link::Classes/String:: or link::Classes/Symbol::. The name of the list/menu item.
@section{argument}
  action
An instance of link::Classes/Function::.

@section{method}
  insertItem
Inserts a list/menu item at index position.
@section{argument}
  index
An link::Classes/Integer::. The index where to insert an item.
@section{argument}
  name
An instance of link::Classes/String:: or link::Classes/Symbol::. The name of the list/menu item.
@section{argument}
  action
An instance of link::Classes/Function::.

@section{method}
  replaceItemAt
Replace a list/menu item at index position.
@section{argument}
  index
An link::Classes/Integer::. The index where to insert an item.
@section{argument}
  name
An instance of link::Classes/String:: or link::Classes/Symbol::. The name of the list/menu item. Default is the current item label.
@section{argument}
  action
An instance of link::Classes/Function::. Default is the current item action.

@section{method}
  removeItemAt
Removes a list/menu item at index position.
@section{argument}
  index
An link::Classes/Integer::. The index where to remove an item.

@section{method}
  remove
Removes both the view, label and the list/menu from the parent view.

@section{subsection}
  Accessing Values

@section{method}
  value
Gets/sets the list/menu to the index at value. Does not perform the action.
@section{argument}
  val
An link::Classes/Integer::.

@section{method}
  valueAction
Sets the value and performs the action at the index value and the global action.
@section{argument}
  val
An link::Classes/Integer::.

@section{method}
  doAction
Performs the action at the current index and the global action.

@section{method}
  initViews
Called by init and overridden by all subclasses. This is where the class specific views are built.


