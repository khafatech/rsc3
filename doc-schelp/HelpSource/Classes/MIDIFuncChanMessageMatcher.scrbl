#lang scribble/manual
@(require (for-label racket))

@title{MIDIFuncChanMessageMatcher}
 Matches incoming MIDI messages to responder funcs based on message channel@section{categories}
  External Control>MIDI>Matchers
@section{related}
  Classes/AbstractMessageMatcher, Classes/MIDIFuncSrcMessageMatcher, Classes/MIDIFuncChanArrayMessageMatcher, Classes/MIDIFuncSrcMessageMatcherNV, Classes/MIDIFuncBothMessageMatcher, Classes/MIDIFuncBothCAMessageMatcher

@section{description}

This is used by link::Classes/MIDIMessageDispatcher:: to match incoming MIDI messages to instances of link::Classes/MIDIFunc:: or link::Classes/MIDIdef:: using MIDI channel. This class is private, and generally users should not need to address instances directly.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Make a new instance.

@section{argument}
  chan
(describe argument here)

@section{argument}
  func
The link::Classes/Function:: to evaluate if a match is found.

@section{returns}
  An MIDIFuncChanMessageMatcher.


@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{METHOD}
  value
Check to see if a message matches, and evaluate func if it does.

@section{argument}
  value
The message value (e.g. velocity, etc.) of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-127.

@section{argument}
  num
The message number (e.g. note number, etc.) of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-127.

@section{argument}
  testChan
The channel number of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-15.

@section{argument}
  srcID
The UID of the source of the MIDI message as an link::Classes/Integer::.


