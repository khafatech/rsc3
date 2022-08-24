#lang scribble/manual
@(require (for-label racket))

@title{Key Value Pairs}
 An interface for translating between three common data structures: Dictionaries, Arrays of Associations and of Pairs@section{categories}
  Collections, Interfaces
@section{related}
 Classes/IdentityDictionary, Classes/Array, Classes/Association

@section{SECTION}
 Motivation

There are three very similar ways to represent maps between keys and values, each of which have a specific purpose:

@section{TABLE}
 
## key value pairs || common representation of arguments || 
@racketblock[[\freq, 452, \amp, 0.2]::
## collections of associations || ordering, array and collection methods || ]

@racketblock[[0 -> [1, 2, 3], 1 -> [2, 1]]::
## dictionaries: fast lookup || event compatibility || ]

@racketblock[(instrument: \sine, freq: 561)::
::

To make it easy to translate between these purposes and representations, there is a uniform set of methods:

]
@section{TABLE}
 
##
@racketblock[asPairs:: || returns an array of key value pairs
##]

@racketblock[asAssociations:: || returns an array of associations
##]

@racketblock[asDict:: || returns an IdentityDictionary
::

]
@section{EXAMPLES}
 


@racketblock[

// the following all return [\freq, 452, \amp, 0.2]

[\freq, 452, \amp, 0.2].asPairs
[\freq -> 452, \amp -> 0.2].asPairs
(freq: 452, amp: 0.2).asPairs


// the following all return [\freq -> 452, \amp -> 0.2]

[\freq, 452, \amp, 0.2].asAssociations
[\freq -> 452, \amp -> 0.2].asAssociations
(freq: 452, amp: 0.2).asAssociations

// the following all return (freq: 452, amp: 0.2) or the equivalent IdentityDictionary

[\freq, 452, \amp, 0.2].asDict
[\freq -> 452, \amp -> 0.2].asDict
(freq: 452, amp: 0.2).asDict


::


]


