#lang scribble/manual
@(require (for-label racket))

@title{NotificationCenter}
 let an object emit notifications@section{related}
  Classes/SimpleController, Classes/NodeWatcher
@section{categories}
  Control

@section{description}

One common OOP pattern is Model-View-Controller where one object (the controller) is a dependant of the model. Every time the model changes it notifies all of its dependants. In this case the model has a dictionary of dependants and iterates through those.

Another common pattern is NotificationCenter wherein an object emits a notification and clients can register functions that will be executed when that notification happens.

A link::Classes/Server:: emits a \newAllocators notification when it creates new node and bus allocators which it does when it quits or boots.


@racketblock[
NotificationCenter.notify(Server.default, \newAllocators);
::

You can listen for this:

]

@racketblock[
NotificationCenter.register(Server.default, \newAllocators, yourself, {
	// throw away all your node variables
	// or stop the music
});
::

The link::Classes/Buffer:: class register a function to clear its info cache whenever a server restarts. The server is emitting changed messages quite often (every 0.4 secs for the status updates), and the Buffer class is only interested in boot/quit events, so this is a more lightweight system for this purpose.

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 notify
The object emits a message and may also pass extra args.

@section{method}
 register
An interested client can register the action function for the object/message notification. A listener may only register one action per object/message notification.

@section{method}
 unregister
Remove the registrations.

@section{method}
 registerOneShot
After the notification has been emitted and handled, automatically unregister.

@section{method}
 registrationExists
Simply confirms if a registration is already in place.


