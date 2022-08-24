#lang scribble/manual
@(require (for-label racket))

@title{TdefEditor}
 will be replaced - please use TdefGui now!@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/TdefGui

@section{description}


@section{warning}
 
has been rewritten and renamed link::Classes/TdefGui::, which has largely the same functionality, but is more consistent and more flexible. There are some changes to the *new method:

@section{definitionList}
 
## instead of || *new(px, nVars, height, width, parent, makeWatcher)
## TdefGui uses || *new (object, numItems, parent, bounds, extras)
::

strong::px:: is now strong::object::, strong::nVars:: is now strong::numItems::, strong::width & height:: can be put as strong::bounds: height @ width ::, strong::parent:: is still strong::parent::, strong::makeWatcher:: is now strong::makeSkip:: .

Please see link::Classes/TdefGui:: for more details.


