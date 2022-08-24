#lang scribble/manual
@(require (for-label racket))

@title{SCViewHolder}
 Instead of subclassing a (Q/SC)View, this is a proxy/adapter object that holds the View@section{categories}
  GUI
@section{related}
  Classes/View

@section{description}

When writing gui widgets it is sometimes not desirable to subclass a view class. Its preferable to write a class that acts like a View and is used where Views normally are used but isn't a direct subclass of a View class.

In the View hierarchy it is not possible to subclass an View class because under the hood there is always a strict relationship between the View subclass and its paired C++ class.  The C++ class makes the actual view, the SC class is an interface to that C++ object.

In Qt there is a bit more flexibility.

But there are other reasons to not inherit from a specific view: your widget may not be a single view, in which case you would want to place a CompositeView and then place subviews inside of that.  Altogether these views are what your widget manages.  SCViewHolder can be used in this situation and it would set the top level CompositeView as its primary view.

Although it is still called "SC"ViewHolder it is in fact cross platform since it doesn't draw the view(s), it simply holds them.

Another possible name might be ViewAdapter or PseudoView.  It was originally called SCViewAdapter.

Most of the standard view methods here simply defer to the proxied view.  This makes the ViewHolder act and quack like a View.

Messages that are not understood by the view holder are forwarded to the proxied view.

Historical note: this class was originally in the cruciallib.  ObjectGui is a subclass of this.

@section{CLASSMETHODS}
 

@section{METHOD}
  consumeKeyDowns
global preference variable: if true then subclasses that do not set a keyDownAction do NOT bubble up keyDown events by default.

@section{returns}
  The result of calling the method on the proxied view


@section{INSTANCEMETHODS}
 


@section{METHOD}
  view
get or set the view for which this object is a proxy/adapter.

@section{argument}
  v
the view: a link::Classes/View::

@section{returns}
  The result of calling the method on the proxied view


@section{METHOD}
  doesNotUnderstand
Messages that are not understood by the view holder are forwarded to the proxied view.  So when the interpreter is told to send a message to a view holder object and that message is not understood, it calls doesNotUnderstand

@section{argument}
  selector
The message that was not understood

@section{argument}
   ... args
The arguments that were supplied

@section{returns}
  the result of calling the method on the proxied view


@section{METHOD}
  viewDidClose
Unsets the view variable. After the view is closed (removed from the window) then all calls to the view holder will fail, and should fail as there is no view anymore.  You can check viewHolder.isClosed if you are unsure

@section{returns}
  this

@section{METHOD}
  remove
Removes the view from the window if it has not already been removed.

@section{returns}
  this

@section{METHOD}
  action
This method is forwarded to the view

@section{argument}
  f


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  doAction
This method is forwarded to the view

@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  keyDownAction
This method is forwarded to the view

@section{argument}
  f


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  keyDownResponder
This method is forwarded to the view.  Note: this is a cruciallib convention and will be deprecated here.

@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  enableKeyDowns
This method is forwarded to the view. Note: this is a cruciallib convention and will be deprecated here.

@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  asView
returns the view

@section{returns}
  the view

@section{METHOD}
  bounds
This method is forwarded to the view

@section{argument}
  b


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  resize
This method is forwarded to the view

@section{argument}
  r


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  enabled
This method is forwarded to the view

@section{argument}
  b


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  refresh
This method is forwarded to the view

@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  background
This method is forwarded to the view

@section{argument}
  b


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  focus
This method is forwarded to the view

@section{argument}
  flag


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  visible
This method is forwarded to the view

@section{argument}
  boo


@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  isClosed
This method is forwarded to the view

@section{returns}
  The result of calling the method on the proxied view

@section{METHOD}
  font
This method is forwarded to the view

@section{argument}
  f


@section{returns}
  The result of calling the method on the proxied view






