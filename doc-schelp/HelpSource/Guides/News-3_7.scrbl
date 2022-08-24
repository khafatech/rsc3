#lang scribble/manual
@(require (for-label racket))

@title{News in 3.7}
 A summary of news in SC 3.7@section{categories}
  News
@section{related}
 Guides/News-3_6, Guides/News-3_5, Guides/Debugging-tips


In addition to the new features and changes described here, there are STRONG::many bugfixes and interesting improvements::, a full list of which can be found in CHANGELOG.md.


@section{SECTION}
 SuperCollider IDE

@section{LIST}
 
##Menu entries for Recording, scope and server inspection
##Modify and query IDE documents from sclang
##Support for the Atom text editor
##Integrated help with auto-completion
##Autosave feature
::

@section{SECTION}
 SuperCollider Language




@section{LIST}
 
##Improved link::Classes/Quark:: system and many new interesting Quarks.
##LINK::Classes/TempoClock#-beats:: can be set.
##An interface for key-value-pairs, see link::Reference/Key-Value-Pairs::
##Refactored JITLib, see link::Other/JITLibChanges3.7:: (in particular dynamic channel expansion).
##LINK::Classes/QuartzComposerView::
::


@section{SUBSECTION}
 External Interfacing

There is an entirely new HID (Human Interface Device) implementation: see link::Guides/Working_with_HID:: that works cross platform (Linux and macOS thus far). This deprecates the GeneralHID interface. Also the link::Classes/LID:: interface has been updated to match the API of the new HID implementation.



@section{SUBSECTION}
 New methods and classes

@section{LIST}
 
##link::Classes/Collection#-collectCopy::, link::Classes/Collection#-collectInPlace::
##link::Classes/SimpleNumber#-lcm:: and link::Classes/SimpleNumber#-gcd:: have consistent interpretations of negative values and zero.
##link::Classes/Dictionary#-embedInStream:: can be customized from within the dictionary.
##link::Classes/Server#*remote:: Create a new Server instance corresponding to a server app running on a separate machine.
::

@section{SUBSECTION}
 Deprecated classes and methods
@section{LIST}
 
##TuningInfo
##ScaleInfo
##Proutine (use Prout instead)
##Document style api - set postColor background etc.
##Date-bootTime
##Platform-getMouseCoords (use GUI.cursorPosition instead)
::



@section{SECTION}
 SuperCollider Server

Apart from UDP, the TCP-protocol is now supported.

When mapping controls of synths to busses, their number of channels is limited to the number of control channels, avoiding a "spill-over" of mappings.

@section{SUBSECTION}
 List of new UGens

@section{LIST}
 
##NodeID (UGen that returns the current node id)
##LINK::Classes/Dconst::
::

@section{SUBSECTION}
 Improved or corrected behavior
@section{LIST}
 
##LINK::Classes/LinXFade2:: (correct fading direction)
##LINK::Classes/LFPulse:: (when width = 0.5, return exactly as many 0 as 1)
##LINK::Classes/TrigControl:: is now independent of synth order, like LINK::Classes/Control::.
##LINK::Classes/TRand::, LINK::Classes/TExpRand::, LINK::Classes/TIRand:: (can operate at audio rate)
##LINK::Classes/Env#*new:: accepts a new TELETYPE::step2:: shape, which steps to a value at the end of a shape
##LINK::Classes/UGen#-curvelin:: is now inverse of LINK::Classes/UGen#-lincurve::
##LINK::Classes/Server#-record:: correctly closes short files
##In combinations of demand-ugens: Instances of PV_Copy are added automatically where necessary for parallel processing
::


@section{SUBSECTION}
 More operators work uniformly across sclang and scserver

The following operators have been added as UGens and work the same as in sclang:

@section{TABLE}
 
##unary operators || 
@racketblock[rand, rand2, linrand, bilinrand, sum3rand, coin::

##binary operators || ]

@racketblock[lcm, gcd, rrand, exprand::
::

See link::Overviews/Operators::


]
@section{SECTION}
  Known Issues

While much has improved and many bugs from 3.6 have been fixed, there are still many known issues. For a complete list see: link::https://github.com/supercollider/supercollider/issues/::

Please do not hesitate to add new issues you find to the LINK::https://github.com/supercollider/supercollider/issues##issue tracker:: or mention them on the  LINK::http://www.birmingham.ac.uk/facilities/ea-studios/research/supercollider/mailinglist.aspx##mailing @section{list}
 


