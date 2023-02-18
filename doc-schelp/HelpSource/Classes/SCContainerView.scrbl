#lang scribble/manual
@(require (for-label racket))

@title{SCContainerView}
 An abstract superclass for container views@section{categories}
  GUI>Kits>Cocoa
@section{related}
  Classes/CompositeView, Classes/HLayoutView, Classes/VLayoutView, Classes/ScrollView

@section{description}

Users will not normally directly create instances of ContainerView, but only use it through its subclasses. It provides the basic mechanisms for container views of various kinds, which are used for placing and grouping widgets in a window.



@section{subsection}
  Some Important Issues Regarding ContainerView

Container views are meant for placing and grouping child views and widgets. While they accept key actions, many do not accept mouse clicks or drags. The exception is SCTopView and its subclasses.

@section{classmethods}
 


@section{instancemethods}
 

@section{subsection}
  Accessing Instance and Class Variables

@section{method}
  decorator
An automatic layout management for a container. Currently the only one existing is link::Classes/FlowLayout::.
@section{note}
 Crucial Library also has a useful layout tool called GridLayout.::
@section{argument}
  arg1

@section{method}
  addFlowLayout
A convenience utility which sets decorator to link::Classes/FlowLayout:: and returns the decorator. See link::Classes/FlowLayout:: for examples.
@section{argument}
  margin
An instance of link::Classes/Point::.
@section{argument}
  gap
An instance of link::Classes/Point::.

@section{method}
  children
An array containing all the views (children) contained in the the container.



@section{subsection}
  Adding and Removing Subviews

@section{method}
  add
Adds a view to children. The placement of the child view will depend on the the decorator, and the child's bounds. Normally you don't need to call this directly, since subviews call it automatically when you create them.
@section{argument}
  child

@section{method}
  removeAll
Removes all children from the view.



@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
 init
@section{argument}
  argParent
@section{argument}
  argBounds

@section{method}
  prRemoveChild
Private method.
@section{argument}
  child

@section{method}
  prClose
Private method.


