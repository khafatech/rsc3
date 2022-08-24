#lang scribble/manual
@(require (for-label racket))

@title{PdefEditor}
 will be replaced - please use PdefGui now!@section{categories}
  Libraries>JITLib>GUI, Live Coding
@section{related}
  Classes/PdefGui

@section{description}


@section{warning}
 
has been rewritten and renamed link::Classes/PdefGui::, which has largely the same functionality, but is more consistent and more flexible. There are some changes to the strong::*new:: method:

@section{definitionList}
 
## instead of || *new(px, nVars, height, width, parent, makeWatcher)
## PdefGui uses || *new (object, numItems, parent, bounds, extras)
::

strong::px:: is now strong::object::, strong::nVars:: is now strong::numItems::, strong::width & height:: can be put as strong::bounds: height @ width ::, strong::parent:: is still strong::parent::, strong::makeWatcher:: is now strong::makeSkip:: .

Please see link::Classes/PdefGui:: for more details.
::


