#lang scribble/manual
@(require (for-label racket))

@title{04_Help}
 Mark Polishook tutorial@section{categories}
  Tutorials>Mark_Polishook_tutorial
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 Help

SuperCollider has a built-in help system. To see the main help page, press cmd-d (without first selecting anything). From that page, click on underlined topics. Another useful document is link::Guides/More-On-Getting-Help::.

In general, there are help files for classes (capitalized words, such as SinOsc, Array, Nil, etc.). Select the name of a class and press Cmd-d. A help file, if one exists, will open.

@section{section}
 Class definitions, message implementations, and the Find command

To see source code for class definitions, select the name of a class and type cmd-j

To see how a class or classes implement a particular message, select the message name and press cmd-y.

Use the Find and Find Next commands, available through the Edit menu, to search for text in the frontmost document

@section{section}
 grep

Use grep in the Terminal (in the Applications->Utilities folder) to search for all occurrences of a given word or phrase. For example, to see all documents that use the LFSaw class, evaluate (in the Terminal application)


@racketblock[
grep -r LFSaw /Applications/SuperCollider
::

Because lines in the terminal application break according to the size of the window and not through schemes that enhance readability, it may be easier to write grep results to a file, as in

]

@racketblock[
// change the name of the path (the argument after the '>' sign, as appropriate
grep -r LFSaw /Applications/SuperCollider/ > /Users/yourHomeDirectory/Desktop/grep_results
::

]
@section{section}
 Additional sources

The SuperCollider wiki:
@section{list}
 
## http://swiki.hfbk-hamburg.de:8888/MusicTechnology/6
::

The SuperCollider users mailing list archive:
@section{list}
 
## http://www.listarc.bham.ac.uk/marchives/sc-users/
::

The SuperCollider user or developer lists (or both).

@section{list}
 
## http://www.beast.bham.ac.uk/research/sc_mailing_lists.shtml
::

David Cottle has a large course on sound synthesis based around SC3.

A course by Nick Collins:

@section{list}
 
## http://www.informatics.sussex.ac.uk/users/nc81/courses/cm1/workshop.html
::

The pseudonym tutorial:

@section{list}
 
## http://www.psi-o.net/pseudonym/
::

The MAT tutorial (UC-Santa Barbara) tutorial:

@section{list}
 
## http://www.mat.ucsb.edu/~sc/
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/05_The_network::


