#lang scribble/manual
@(require (for-label racket))

@title{PageLayout}
 a Window with a FlowView on it for use in ObjectGui's MVC model@section{categories}
  GUI
@section{related}
  Classes/ObjectGui, Classes/FlowView, Classes/Window, Classes/NotificationCenter

@section{description}

This class encapsulates the common task of creating a Window, adding a FlowView (CompositeView with a FlowLayout on it).  It also supports the MVC model by registering controllers that are then removed (sent the .remove message) when the Window closes.  Additionally it can resize itself to fit the contents.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a Window with a FlowView on it.  The PageLayout object can be treated like a Window or like a View.

@section{argument}
  title
Window title

@section{argument}
  bounds
Bounds or nil.  Default of nil will size the window to the entire screen size.  Use .resizeToFit to shrink the window to the content size.

@section{argument}
  margin
FlowLayout margin.

@section{argument}
  background
Background color

@section{argument}
  scroll
boolean: add scroll bars or not.

@section{argument}
  front
boolean: whether to immediately display the window, bringing it to the front.  default is true.  You may choose to first add your views to the window and then front it which is useful for large slow GUIs

@section{returns}
  a PageLayout


@section{INSTANCEMETHODS}
 

@section{METHOD}
  window
the Window object

@section{returns}
  a Window

@section{METHOD}
  view
the top most view on the Window

@section{returns}
  a View

@section{METHOD}
  isClosed
boolean: has the window been closed ?

@section{returns}
  boolean

@section{METHOD}
  onClose
Just as for Window, this method is called when the PageLayout's window is closed.  The actual Window's onClose method is used to trigger clean up operations, releasing dependencies and will also call this onClose function.

@section{returns}
  get/set onClose handler

@section{METHOD}
  asView
returns the top view

@section{returns}
  a View

@section{METHOD}
  asFlowView

@section{argument}
  bounds
if bounds are nil then it returns self, as a PageLayout is compatible with FlowView.  If bounds are supplied then a child FlowView is placed and returned

@section{returns}
  self or a new FlowView

@section{METHOD}
  bounds
inner bounds of the top level view.

@section{returns}
  a Rect

@section{METHOD}
  asPageLayout
Similar to asFlowView, this message converts nil and various other objects to a PageLayout. This is already a PageLayout, so it returns self.

@section{argument}
  name
Ignored. If the receiver had been nil then the name would be the Window name. 

@section{argument}
  bounds
Ignored. Would have been used to size the PageLayout

@section{returns}
  self

@section{METHOD}
  startRow
compatible with FlowView

@section{returns}
  self

@section{METHOD}
  indentedRemaining
compatible with FlowView

@section{returns}
  self

@section{METHOD}
  checkNotClosed
isClosed.not

@section{returns}
  boolean

@section{METHOD}
  front
bring Window to the front

@section{returns}
  self

@section{METHOD}
  hide
Hide window

@section{returns}
  self

@section{METHOD}
  show
Show the window if it was previously hidden.

@section{returns}
  self

@section{METHOD}
  close
Close the window, releasing any dependencies and calling the onClose handler.

@section{returns}
  self

@section{METHOD}
  refresh
Refresh the top level view

@section{returns}
  self

@section{METHOD}
  background
set background color of top level view

@section{argument}
  c
color

@section{returns}
  self

@section{METHOD}
  removeOnClose
Register an object, usually a ObjectGui subclass or an Updater so that when the Window closes the .remove message will be sent to it. This will cause the object to release its dependencies on its Model.  This means the ObjectGui (or other controller object) will stop getting update messages and will stop trying to update the View which has just been closed along with the Window.  It also means that if there is no link to the Model and no longer any Views that held links to the controller object, that the controller is now unreferenced and will be garbage collected.

@section{argument}
  dependant
the object that wishes to be sent .remove on closing the window

@section{returns}
  self

@section{METHOD}
  resizeToFit
Resize the top FlowView to fit its contents and then resize the Window to fit that.=

@section{argument}
  reflow
boolean: FlowView can relay all of its child views in cases where the bounds have changed or views have been removed.  This puts them all back in place one by one for the updated bounds.  So this may result in smaller over all bounds, after which the window is shrunk.

@section{argument}
  center
boolean: after resizing, re-center the window in the screen.

@section{returns}
  self

@section{METHOD}
  reflowAll
see FlowView reflowAll

@section{returns}
  self

@section{METHOD}
  fullScreen
go Full screen

@section{returns}
  self

@section{METHOD}
  endFullScreen
end full screen

@section{returns}
  self

@section{subsection}
 FlowView extensions

@section{METHOD}
  flow
Place a new FlowView on the window

@section{argument}
  func
A handler that recieves the new FlowView as argument

@section{argument}
  bounds
Bounds fo the FlowView

@section{returns}
  (returnvalue)

@section{METHOD}
  vert
(describe method here)

@section{argument}
  func
(describe argument here)

@section{argument}
  bounds
(describe argument here)

@section{argument}
  spacing
(describe argument here)

@section{returns}
  (returnvalue)

@section{METHOD}
  horz
(describe method here)

@section{argument}
  func
(describe argument here)

@section{argument}
  bounds
(describe argument here)

@section{argument}
  spacing
(describe argument here)

@section{returns}
  (returnvalue)

@section{METHOD}
  comp
(describe method here)

@section{argument}
  func
(describe argument here)

@section{argument}
  bounds
(describe argument here)

@section{returns}
  (returnvalue)

@section{METHOD}
  scroll
(describe method here)

@section{argument}
   ... args
(describe argument here)

@section{returns}
  (returnvalue)


@section{EXAMPLES}
 


@racketblock[
PageLayout.new
::
]


