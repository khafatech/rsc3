# rsc3 for Schemers: An Introduction to rsc3

Rohan Drape\
August 2003

> These are notes for a talk addressed to Schemers about rsc3, a scheme
> client to the SuperCollider (Sc3) synthesis server.  This talk
> provides a brief history of computer music in order to place Sc3 in
> context and to define the problem domain, and then a description of
> and rationale for rsc3.

Max Mathews, working at AT&T, wrote the Music system in 1958
(Mathews1960a) and successive variants of this system through
Music-V.  Musics I through III were experimental and not used outside
AT&T, Musics IV and V were written in Fortran and were the first
widely distributed computer music synthesis systems, used for many
years in studios including those at Stanford, Princeton, Columbia,
MIT, IRCAM and Marseille.  In 1969 Matthews published the important
text _The Technology of Computer Music_ (Mathews1969a) which is
in two parts, the first discusses basic digital signal processing
theory, the second is a manual for Music-V.  Barry Vercoe at MIT wrote
Music-11 (Vercoe1979a) and variants through CSound
(Vercoe1985a) which is highly portable and very widely used.  Eric
Scheirer and others at MIT wrote the MPEG4 structured audio
specification (Scheirer1998a), a variant of CSound.  These systems
are all considered to be part of a _Music-N_ family.

## The Music-N Paradigm

Systems in the Music-N family are acoustical compilers, reading a set
of instruction files to generate a signal file.  Users define a set of
signal processing graphs called _instruments_ that together form
an _orchestra_.  The nodes of the signal flow graph are called
_unit generators_ or _Ugens_.  Ugens read and write continuous
signals from unidirectional _ports_.  For efficiency many Music-N
systems provide three rates of signal flow, initialization rate
_i-rate_, control rate _k-rate_ and audio rate _a-rate_.

~~~~
instr 1
k1    linen   p5,p6,p3,p7
a2    oscil   k1,p4,2
out   a2
endin
~~~~

A piece is written by specifying a sequence of _notes_ in a
_score_.  A note is a set of parameters, the first five parameters are
traditionally instrument number, start time, duration, frequency and
amplitude.

~~~~
i1 0   1 440 0.1 0.5  0.25
i1 0.5 1 442 0.1 0.25 0.5
~~~~

## Other Computer Music Systems

A different family of systems follow the Patcher (Puckette1988a)
paradigm due to Puckette working at IRCAM.  Systems in use include Max
(Puckette1991b, Zicarelli1998a) and Pd (Puckette1997a).  A
patch is a graph that combines both continuous signal processing
elements and asynchronous messaging elements.  This is at once the
most interesting and problematic aspect of patcher systems.  Patches
are ordinarily created and edited using a drawing editor.
Ideomatically the graph drawing represents the state of the system,
however in practice graphs often become too complicated to be written
in this manner and sub-graphs and references to stored data files are
required.

Another family of systems follow the Editor (Moore1985a) paradigm,
due to Moore working at Lucasfilm.  These systems have direct
precedents in analog studios and are very widely used in digital
studios.  Two current implementations are ProTools from Digidesign
and Logic from Apple.

## SuperCollider

SuperCollider (Sc) is a family of real-time audio signal processing
systems written by James McCartney.  SuperCollider is a descendant of
Pyrite, a system for describing and generating Max patches.

The first SuperCollider (McCartney1996a) is a dialect of Scheme
highly optimized for musical signal processing.  Sc2
(McCartney1998a) and Sc3d5 (McCartney2000a) are dialects of
SmallTalk with the same optimizations.  The interpreters for these
languages generate real-time audio signals as a side effect of
evaluating certain expressions.

~~~~
f = LFSaw.kr(0.4, 0, 24, LFSaw.kr([8,7.23], 0, 3, 80)).midicps;
CombN.ar(SinOsc.ar(f, 0, 0.04), 0.2, 0.2, 4);
~~~~

Sc3 is a variant of Sc2 that cleanly separates the language intepreter
and synthesis engine into two processes.  These processes communicate
over network sockets using a subset of the Open Sound Control (Osc)
protocol (Wright1997a).  The Sc3 synthesis engine, _scsynth_,
manages a graph of instruments.  Instruments are specified as byte
strings.  All operations on the graph are initiated by sending an Osc
message over a network socket.  Osc messages are timestamped using the
Network Time Protocol (Ntp).  Operations that are not atomic reply to
the client when the operation completes.  Ugens are loaded dynamically
when the system boots and can be written by users.  The Sc3 language
interpreter _sclang_ implements the same SmallTalk dialect
provided by Sc2.  Sc3 is efficient, well designed and well
implemented.

## Music-N, Sc3 and Moore's Law

Earlier systems had provided high level languages for music signal
processing by targeting Music-N systems.  Common Lisp Music (Clm)
(Schottstaedt1994a) is one instance of this.

The most significant contribution of SC is to real-time musical signal
processing.  Music-N systems were designed as accoustical compilers at
a time when works were submitted to computer administrators on punch
card and the output tapes were sent to a digital to analgue converter
that rendered analogue tapes offline.  Although traditional Music-N
systems have been progressively adapted for real time environments the
basic architectures are not properly dynamic.  SuperCollider was
initially designed as a high level language interpreter for real-time
music signal processing.

Correct dynamic behavior of the signal processing system requires:

1. graphs of instruments

2. dynamic insertion and deletion of instruments at these graphs (this
requires real-time constraints on Ugen instatiation as well as runtime
operation)

3. dynamic audio and control signal routing and rerouting (global
audio and control signal paths).

Real time systems adapt to offline compilation use well.

## Scheme

As this paper is addressed to Schemers this section will be terser
still.  Scheme is a good working environment for music composition.
Scheme is simple, dynamic, fast and well supported.

## rsc3

rsc3 is an R6RS (Sperber2009a) library that facilitates using
scheme as a client to the Sc3 synthesis server.

rsc3 is a client of the Sc3 synthesis server in the same sense that
sclang is.  Where appropriate rsc3 provides a similar interface layer
and uses the same or similar names and is therefore a derivative work
of Sc3.  The rsc3 core implements:

1. The Osc protocol.  A bytecode generator and parser for the
   subset of the Osc protocol used by Sc3.

2. Sc3 Synth Definition management. A bytecode generator and
   parser.  Implementations for all Ugens distributed with Sc3.  Sc3 type
   input replication (multiple channel expansion).

3. An _Emacs_ (Stallman1981a) mode, with rsc3 and Sc3 session
   management, expression evaluation, textual rewriting for evaluation,
   graph drawing and symbol lookup of rsc3 source and help files.

The expressions below show the equivalent Sc3 language and rsc3
declarations of a trivial Synth definition.

~~~~
Synthdef("sin", {
  arg f=440, a=0.1;
  Out.ar(0, SinOsc.ar(f, 0) * a)})
~~~~

~~~~
(synthdef "sin"
  (letc ((f 440) (a 0.1))
    (Out 0 (Mul (SinOsc f 0) a))))
~~~~

rsc3 provides a moderate set of procedures related to audio signal
processing and musical composition.  Using modern scheme systems
thread latency is adequate for most musical work and Gc stop times are
reasonable though not ideal.

The rsc3 source repository is available from: <http://rohandrape.net/?t=rsc3>.

## Examples

A series of examples demonstrate: the Emacs mode, partial Ugen graphs,
graph drawing, the dissasembler, the Utc and tempo schedulers, the
widget set and control data integration.

## References

M. V. Mathews. Computer Program to Generate Acoustic Signals. _Journal of the Acoustical Society of America_, 32:1493, 1960.

M. V. Mathews. _The Technology of Computer Music_. MIT Press, Cambridge, MA, 1969.

James McCartney. SuperCollider: a new real time synthesis language. In _Proceedings of the International Computer Music Conference_. International Computer Music Association, 1996.

James McCartney. Continued evolution of the SuperCollider real time synthesis environment. In _Proceedings of the International Computer Music Conference_, pages 133--136. International Computer Music Association, 1998.

James McCartney. A New, Flexible Framework for Audio and Image Synthesis. In _Proceedings of the International Computer Music Conference_, pages 258--261. International Computer Music Association, 2000.

F. R. Moore. _The Lucasfilm digital audio facility_. W. Kaufmann, Los Altos, CA, 1985.

Miller Puckette. The Patcher. In _Proceedings of the International Computer Music Conference_, pages 420--429, San Francisco, 1988. Proceedings of the International Computer Music Conference.

Miller Puckette. Combining Event and Signal Processing in the Max Graphical Programming Environment. _Computer Music Journal_, 15(3):68--77, 1991.

Miller Puckette. Pure Data. In _Proceedings of the International Computer Music Conference_, pages 224--227. International Computer Music Association, 1997.

Eric Scheirer. SAOL: The MPEG-4 structured audio orchestra language. In _Proceedings of the International Computer Music Conference_, pages 432--438, 1998.

William Schottstaedt. Machine Tongues XVII: CLM - Music V meets Common Lisp. _Computer Music Journal_, 18(2), 1994.

Michael Sperber, R. Kent Dybvig, Matthew Flatt, Anton Van Straaten, Robby Findler, and Jacob Matthews. Revised6 report on the algorithmic language scheme. _J. Funct. Program._, 19(S1):1--301, 2009.

Richard Stallman. EMACS: The Extensible, Customizable, Self Documenting Display Editor. _Symposium on Text Manipulation_, pages 147--156, 1981.

Barry Vercoe. _Reference Manual for the Music 11 Sound Synthesis Language_. MIT Electronic Music Studio, Cambridge, MA, 1979.

Barry Vercoe. _Csound: A manual for the audio processing system_. MIT Media Lab, Cambridge, MA, 1985. Revised 1996.

Matthew Wright and Adrian Freed. Open Sound Control: A New Protocol for Communicating with Sound Synthesizers. In _Proceedings of the International Computer Music Conference_, pages 101--104. International Computer Music Association, 1997.

David Zicarelli. An extensible real-time signal processing environment for Max. In _Proceedings of the International Computer Music Conference_, pages 463--466. International Computer Music Association, 1998.

* * *

Minor revisions November 2006, May 2010, June 2014, June 2019, May 2021
