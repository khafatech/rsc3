#lang scribble/manual
@(require (for-label racket))

@title{EZGui}
 An abstract superclass for EZ widget wrappers@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/EZListView, Classes/EZPopUpMenu, Classes/EZSlider, Classes/EZNumber, Classes/EZRanger, Classes/EZKnob

@section{description}

Users will not normally directly create instances of EZGui, but only use it through its subclasses. It provides the basic mechanisms for various EZ widget wrappers. It also provides a standard for EZ GUI Classes, and new EZ Classes should subclass EZGUI to help keep a consistent user interface.


@section{instancemethods}
 

@section{subsection}
  Accessing Instance Variables

@section{method}
  view
Returns the enclosing link::Classes/CompositeView::.

@section{method}
  bounds
Returns the bounds of the enclosing link::Classes/CompositeView::.

@section{method}
  label
Sets/gets it the label. Will add the label view if none was initially created.
@section{argument}
  string
An Instance of link::Classes/String::.

@section{method}
  window
Returns the window if you used the popUp window function.


@section{subsection}
  Accessing GUI options

@section{method}
  alwaysOnTop
Makes the popup window always on top, if there is one.
@section{argument}
  bool
An Instance of link::Classes/Boolean::. Default is false.

@section{method}
  visible
Sets/gets it the component views are visible.
@section{argument}
  bool
An Instance of link::Classes/Boolean::. Default is true.

@section{method}
  enabled
Sets/gets if the list is enabled.
@section{argument}
  bool
An Instance of link::Classes/Boolean::. Default is true.

@section{method}
  onClose
Sets/gets the onClose function of either 
@racketblock[view:: or ]

@racketblock[window::, depending on whether the EZ view used a popup window.
]
@section{argument}
  func
An Instance of link::Classes/Function:: or link::Classes/Function@section{List}
 .

@section{method}
  font
@section{argument}
  font
An instance of link::Classes/Font::.


@section{subsection}
  Subclassing

EZGui provides a standard and basic tools for most EZ classes. If you make a new EZ class, then subclass EZGui, and override the necessary methods. If your class only has a label and a widget, chances are, you need to override nothing, but only need to write the  new and init class methods. See existing subclasses of EZGui for examples of this. You may also want to override the following:

@section{method}
  widget
Returns the active widget. Subclasses will typically refer to it or ignore it, e.g.:

@racketblock[
MyEZClass{
	myOtherMethods{}
	....
	listView{ ^widget }
}
::

]
@section{method}
  action
Gets/sets the action of the EZ class instance.
@section{argument}
  func
An Instance of link::Classes/Function:: or link::Classes/Function@section{List}
 .

@section{method}
  value
Gets/sets the value of the 
@racketblock[widget::. Does not perform the action.
]
@section{argument}
  val
An integer.

@section{method}
  valueAction
Gets/sets the value of the widget. Performs do action.
@section{argument}
  val
An integer.

@section{method}
  doAction
Performs 
@racketblock[this.action.value(this)::.


]
@section{subsection}
  Internal Utilities

@section{method}
  prSubViewBounds
This calculates the bounds of the subviews and the gaps. It returns an array of Rects, which depends on how many subview there are. Subclasses override this if they have more than one widget.

@section{method}
  prMakeView
Called by init. Returns 
@racketblock[[view, bounds]::. The container is either the enclosing Container, or a pop up window with a container.

]
@section{method}
  prSetViewParams
Only defined by some subclasses. Sets the 
@racketblock[resize::  and ]

@racketblock[align:: of all the views, according to the state of ]

@racketblock[layout::.

]
@section{method}
  prMakeMarginGap
Called in the init method of all subclasses. Sets the margin and gap of 
@racketblock[view::. By default, it tries to get its parent's gap, otherwise it defaults to ]

@racketblock[2@2::. Setting ]

@racketblock[argGap:: overrides these.

]


