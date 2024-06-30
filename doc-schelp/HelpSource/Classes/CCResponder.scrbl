#lang scribble/manual
@(require (for-label racket))

@title{CCResponder}
 allow functions to be registered to respond to MIDI control change events@section{related}
  Classes/MIDIFunc, Classes/MIDIdef, Classes/MIDIResponder, Classes/NoteOnResponder
@section{categories}
  External Control>MIDI

@section{description}

@section{note}
  SC 3.5 added the link::Classes/MIDIFunc:: and link::Classes/MIDIdef:: classes. These are faster, and aim to have a more convenient, logical and consistent interface, which shares a common design with link::Classes/OSCFunc:: and link::Classes/OSCdef::. They also provide support for all MIDI message types.::
@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 function
A link::Classes/Function:: to be evaluated. Arguments passed to the function are: src, chan, num, value.

@section{argument}
 src
The src number may be the system UID (obtained from 
@racketblock[ MIDIClient.sources[index].uid ::) or the index of the source in the ]

@racketblock[ MIDIClient.sources :: array. nil matches all.

]
@section{argument}
 chan
An link::Classes/Integer:: between 0 and 15 that selects which MIDI channel to match. nil matches all. May also be a link::Classes/Function:: which will be evaluated to determine the match. eg: { |val| val < 2 }

@section{argument}
 num
An link::Classes/Integer:: between 0 and 127 that selects which controller number to match. nil matches all. May also be a link::Classes/Function:: which will be evaluated to determine the match. eg: { |val| val >= 4 }

@section{argument}
 value
An link::Classes/Integer:: between 0 and 127 to filter values. nil matches all. May also be a link::Classes/Function:: which will be evaluated to determine the match. eg: { |val| val < 50 }

@section{argument}
 install
If true, install the responder automatically so it is active and ready to respond. If false, then it will be inactive.

@section{argument}
 swallowEvent
If true, then if the midi event is matched, cease testing any further responders. Note that doing this will prevent any other responders of this type from responding, including ones added behind the scenes in classes. Note also that this functionality is sensitive to the order in which responders are added. 

@section{InstanceMethods}
 

@section{method}
 learn
Wait for the next CC message, reset to match src, chan, cc num.

@racketblock[
(

c = CCResponder({ |src,chan,num,value|
		[src,chan,num,value].postln;
	});
	c.learn; // wait for the first controller
)
CCResponder.removeAll
::


]
@section{Examples}
 


@racketblock[
(
	c = CCResponder({ |src,chan,num,value|
		[src,chan,num,value].postln;
		},
		nil, // any source
		nil, // any channel
		nil, // any CC number
		nil // any value
	)
)

c.remove
::

]

@racketblock[
(
	c = CCResponder({ |src,chan,num,value|
		[src,chan,num,value].postln;
		},
		nil, // any source
		nil, // any channel
		80, // CC number 80
		{ |val| val < 50 } // any value less than 50
	)
)

c.remove
::
]

