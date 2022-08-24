#lang scribble/manual
@(require (for-label racket))

@title{ShutDown}
 register functions to be evaluated before system shuts down@section{related}
  Classes/StartUp, Classes/ServerQuit, Classes/ServerTree, Classes/CmdPeriod
@section{categories}
  Control

@section{description}

ShutDown registers functions to perform an action before system shut down.

@section{ClassMethods}
 

@section{method}
 run
Call the object in order.


