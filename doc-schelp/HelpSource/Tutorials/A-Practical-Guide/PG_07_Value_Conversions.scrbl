#lang scribble/manual
@(require (for-label racket))

@title{Pattern Guide 07: Value Conversions}
 Describes the default event's conversions for pitch, rhythm and amplitude@section{related}
  Tutorials/A-Practical-Guide/PG_06g_Data_Sharing, Tutorials/A-Practical-Guide/PG_08_Event_Types_and_Parameters
@section{categories}
  Streams-Patterns-Events>A-Practical-Guide

@section{section}
 Pitch and rhythm conversions in the default event

Using the default event prototype, pitch and rhythm can be specified in Pbind at different levels depending on the musical requirement. The default event prototype includes logic to convert higher-level abstractions into the physical parameters that are useful for synthesis.

The descriptions below start with the ending value that will actually be used, following up with the other values that are used in the calculations: e.g., \delta is based on \dur and \stretch. The calculations may be bypassed by providing another value for the calculated item. If your pattern specifies 
@racketblock[\delta:: directly, ]

@racketblock[\dur:: and ]

@racketblock[\stretch:: are ignored.

Note also that there is no obligation to use these constructs. The default event prototype is not meant to enforce one model of pitch or rhythm over any other; it simply provides these options, which you may use if they suit the task, or ignore or override if your task calls for something else entirely.

]
@section{subsection}
 Timing conversions

Rhythm is based on 
@racketblock[\delta:: and ]

@racketblock[\sustain:: event keys. Both of these can be calculated from higher-level abstractions: ]

@racketblock[\dur::, ]

@racketblock[\stretch:: and ]

@racketblock[\legato::.

]
@section{definitionList}
 
## delta || The number of beats until the next event. You can give the delta pattern directly, or the default event prototype can calculate it for you based on other values:
@section{definitionList}
 
## dur || Duration of this event.
## stretch || A multiplier for duration: 
@racketblock[delta = dur * stretch::.
::
::

]
@section{definitionList}
 
## sustain || How many beats to hold this note. After 
@racketblock[\sustain:: beats, a release message will be sent to the synth node setting its ]

@racketblock[gate:: control to ]

@racketblock[0::. Your SynthDef should use ]

@racketblock[gate:: in an link::Classes/EnvGen:: based on a sustaining envelope (see link::Classes/Env::), and the EnvGen should have a ]

@racketblock[doneAction:: ( link::Classes/Done:: ) that releases the synth at the end. You can give the sustain pattern directly, or the default event prototype can calculate it for you based on:
]
@section{definitionList}
 
## legato || A fraction of the event's duration for which the synth should sustain. 
@racketblock[1.0:: means this synth will release exactly at the onset of the next; ]

@racketblock[0.5:: means the last half of the duration will be a rest. Values greater than ]

@racketblock[1.0:: produce overlapping notes. ]

@racketblock[sustain = dur * legato * stretch::.
::
::

]
@section{subsection}
 Pitch conversions

Pitch handling in the default event is rich, with a large number of options. To use events, it is not necessary to understand all of those options. As the examples have shown, a note-playing pattern produces sensible results even specifying only 
@racketblock[\degree::. The other parameters allow you to control how the event gets from ]

@racketblock[\degree:: to the frequency that is finally passed to the new synth. The default event prototype includes reasonable defaults for all of these.

To go from the highest level of abstraction down:

]
@section{definitionList}
 
## \degree || represents a scale degree. Fractional scale degrees support accidentals: adding 
@racketblock[0.1:: to an integer scale degree raises the corresponding chromatic note number by a semitone, and subtracting ]

@racketblock[0.1:: lowers the chromatic note number. ]

@racketblock[0.2:: raises or lowers by two semitones, and so on.
## \note || is a chromatic note index, calculated from ]

@racketblock[\degree:: based on a ]

@racketblock[\scale:: and modal transposition (]

@racketblock[\mtranspose::, scale degrees to raise or lower the note). ]

@racketblock[\]
@section{note}
  is in equal-tempered units of any number of steps to the octave ( 
@racketblock[\stepsPerOctave:: ).
## \midinote || is a 12ET conversion of ]

@racketblock[\]
@section{note}
 , transposed into the right 
@racketblock[\octave:: and applying gamut transposition (]

@racketblock[\gtranspose::, given in stepsPerOctave units). If ]

@racketblock[\stepsPerOctave:: is anything other than ]

@racketblock[12::, the non-12ET units are scaled into 12 ]

@racketblock[\midi]
@section{note}
  units per octave.
## \freq || is calculated from 
@racketblock[\midi]
@section{note}
  by 
@racketblock[midicps::. A chromatic transposition in 12ET units ( ]

@racketblock[\ctranspose:: ) is added.
::

Most note-playing SynthDefs use ]

@racketblock[freq:: as an argument. If desired, they may use ]

@racketblock[midi]
@section{note}
 , 
@racketblock[]
@section{note}
  or even 
@racketblock[degree::.

To simplify into rules of thumb:

]
@section{list}
 
## If your material is organized around scales or modes, use 
@racketblock[\degree::.
]
@section{list}
 
## If the scale has different ascending and descending patterns, use 
@racketblock[\]
@section{note}
  in your Pbind, with the filter pattern link::Classes/Pavaroh::.
::
## If your material is organized around equal divisions of the octave (not necessarily 12 divisions), use 
@racketblock[\]
@section{note}
  (and 
@racketblock[\stepsPerOctave:: for equal temperament other than 12 notes).
## If your material is organized around MIDI note numbers (or 12-tone equal temperament), ]

@racketblock[\midi]
@section{note}
  will also work.
## If you prefer to give frequencies directly in Hz, use 
@racketblock[\freq::.
::

Following is a complete description of all elements of the pitch system. Feel free to use the ones that are of interest, and ignore the rest.

]
@section{definitionList}
 
## freq || Frequency in Hz. May be given directly, or calculated based on the following. Pitch may be expressed at any one of several levels. Only one need be used at a time. For instance, if you write pitch in terms of scale degrees, the note, MIDI note and frequency values are calculated automatically for you.
@section{definitionList}
 
## ctranspose || Chromatic transposition, in 12ET units. Added to midinote.
## midinote || MIDI note number; 12 MIDI notes = one octave. This may be fractional if needed. Calculated based on:
@section{definitionList}
 
## root || The scale root, given in 12ET MIDI note increments.
## octave || The octave number for 
@racketblock[\note = 0 ::. The default is ]

@racketblock[5::, mapping note ]

@racketblock[0:: onto MIDI note ]

@racketblock[60::.
## stepsPerOctave || How many ]

@racketblock[\]
@section{note}
  units map onto the octave. Supports non-12ET temperaments.
## gtranspose || Non-12ET transposition, in 
@racketblock[\]
@section{note}
  units. Added to note.
## note || The note number, in any division of the octave. 
@racketblock[0:: is the scale root. Calculated based on:
]
@section{definitionList}
 
## degree || Scale degree.
## scale || Mapping of scale degrees onto semitones. Major, for instance, is 
@racketblock[[0, 2, 4, 5, 7, 9, 11]::.
## stepsPerOctave || (Same as above.)
## mtranspose || Modal transposition; added to degree.
::
::
::
::

See also the link::Classes/Scale:: class for a repository of scale configurations, and the possibility of non-ET tuning.

]

@racketblock[
(
// approximate a major scale with a 19TET chromatic scale
p = Pbind(
	\scale, #[0, 3, 6, 8, 11, 14, 17],
	\stepsPerOctave, 19,
	\degree, Pwhite(0, 7, inf),
	\dur, 0.125,
	\legato, Pexprand(0.2, 6.0, inf)
).play;
)

p.stop;
::

]
@section{subsection}
 Amplitude conversion

Finally, you can specify amplitude as 
@racketblock[\db:: or ]

@racketblock[\amp::. If it's given as ]

@racketblock[\db::, the amplitude will be calculated automatically using ]

@racketblock[.dbamp::.


Previous:	link::Tutorials/A-Practical-Guide/PG_06g_Data_Sharing::

Next:		link::Tutorials/A-Practical-Guide/PG_08_Event_Types_and_Parameters::
]


