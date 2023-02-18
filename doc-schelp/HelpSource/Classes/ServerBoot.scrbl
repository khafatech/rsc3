#lang scribble/manual
@(require (for-label racket))

@title{ServerBoot}
 register actions to be taken when a server has booted@section{related}
  Classes/Server, Classes/ServerQuit, Classes/ServerTree, Classes/StartUp
@section{categories}
  Control

@section{description}

The singleton ServerBoot provides a place for registering functions and objects for events that should happen when a given server has booted.

See link::Classes/AbstractServerAction:: for usage.

@section{ClassMethods}
 

@section{private}
 initClass


