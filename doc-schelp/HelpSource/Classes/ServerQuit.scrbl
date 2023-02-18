#lang scribble/manual
@(require (for-label racket))

@title{ServerQuit}
 register actions to be taken when a server quits@section{related}
  Classes/Server, Classes/ServerBoot, Classes/ServerTree, Classes/ShutDown
@section{categories}
  Control

@section{description}

The singleton ServerQuit provides a place for registering functions and objects for events that should happen when a given server quits.

See link::Classes/AbstractServerAction:: for usage.

@section{ClassMethods}
 

@section{private}
 initClass


