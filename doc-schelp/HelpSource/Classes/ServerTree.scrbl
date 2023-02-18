#lang scribble/manual
@(require (for-label racket))

@title{ServerTree}
 register actions to be taken to initialise a basic tree of groups on the server@section{related}
  Classes/Server, Classes/ServerQuit, Classes/ServerBoot, Classes/CmdPeriod
@section{categories}
  Control

@section{description}

The singleton ServerTree provides a place for registering functions and objects for events that should happen when a given server has booted and when all synths are freed. This is to initialise a basic tree of groups on the server.

See link::Classes/AbstractServerAction:: for usage.

@section{ClassMethods}
 

@section{private}
 initClass


