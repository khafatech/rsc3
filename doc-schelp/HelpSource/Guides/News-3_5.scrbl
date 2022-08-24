#lang scribble/manual
@(require (for-label racket))

@title{News in 3.5}
 A summary of news in SC 3.5@section{categories}
  News

@section{SECTION}
  Language-side news

@section{subsection}
  Qt GUI
A new cross-platform GUI kit intended to replace Cocoa and an alternative to SwingOSC.
See link::Guides/News-Qt-GUI:: for more.

@section{subsection}
  SCDoc
A new help-system provides consistent documentation with good introspection and easy link::Search##searching:: and link::Browse##browsing::.

The help-files are written in a markup language which is then parsed and used to generate HTML files, which are displayed with the new link::Classes/WebView:: widget inside the link::Classes/HelpBrowser::.

See link::Classes/SCDoc::, link::Reference/SCDocSyntax::, link::Guides/WritingHelp::.

Also a new method link::Classes/Help#*methodArgs:: returns a human-readable string of arguments and default values for a method. Example: 
@racketblock[Help.methodArgs("SinOsc.ar")::

In SuperCollider version 3.5.2, SCDoc has been rewritten and the parser is now implemented in C++ for speed and stability.
The syntax has gotten stricter, and it will throw errors or warnings if there are faults in the documentation. See link::Guides/WritingHelp#News in SC 3.5.2:: for some important changes to keep in mind.

]
@section{subsection}
  OSC and MIDI responders
The new link::Classes/OSCFunc:: and link::Classes/MIDIFunc:: provides better alternatives to the old link::Classes/OSCresponderNode:: and link::Classes/NoteOnResponder::, etc.

OSCFunc can receive on any port, not only the main 
@racketblock[NetAddr.langPort::.

]
@section{subsection}
  New Location of Startup file

The link::Reference/StartupFile##sclang startup file:: has moved to 
@racketblock[Platform.userConfigDir +/+ "startup.scd"::.
Old platform-specific startup file locations have been deprecated. The new startup file is plain-text, rtf is not supported.

]
@section{subsection}
  Language configuration files

The Linux-only library configuration file has been deprecated. It is replaced by a cross-platform language configuration
file, which is located at 
@racketblock[Platform.userConfigDir +/+ "sclang_conf.yaml" :: . It can be configured via the
link::Classes/LanguageConfig:: class.

]
@section{subsection}
  Sced3
The GEdit plugin sced has been updated to support GEdit version 3.

@section{subsection}
  WiiMote

@racketblock[WiiMote.discover:: now returns the device object, or nil if it failed.

]
@section{subsection}
  Bus-asMap in patterns
Bus-asMap symbols are now allowed in 
@racketblock[\freq:: and friends in patterns.

]
@section{subsection}
  Warn on classlib overwrites
Warnings are posted when extensions overwrites methods in main class lib, unless the extensions are put in a subfolder named "SystemOverwrites".

@section{subsection}
  Filesystem utils
New cross-platform filesystem utilities: link::Classes/File#*copy::, link::Classes/File#*mtime::, link::Classes/File#*mkdir::, link::Classes/File#*realpath::, link::Classes/File#*type::, link::Classes/File#*fileSize::

@section{subsection}
  String-openTextFile
link::Classes/String#-openTextFile:: now works also on frontends without link::Classes/Document:: support. It falls back to link::Classes/String#-openOS:: to open the file with the default application for that file type.

@section{subsection}
  Optional trailing comma
It was already allowed to have a trailing comma in arrays: 
@racketblock[[1,2,3,]::,
but now it's also allowed in Event construction and message arguments:
]

@racketblock[
(a:1, b:2, c:3,);

Pbind(
    \foo, 1,
    \bar, 2,
);

myFunc.value(1, 2, 3,);
::

]
@section{subsection}
  Various bugfixes
A lot of bugs has been fixed, for example: String regexp primitives, multichannel wrappers of SequenceableCollection, CoinGate.ar, T2K, WiiMote, SynthDesc.

@section{subsection}
  Interpreter Performance Improvements

The sclang now uses token threading @section{footnote}
 http://www.complang.tuwien.ac.at/forth/threaded-code.html:: instead of one huge switch statement for bytecode dispatching.

@section{subsection}
  PriorityQueue stable order

The link::Classes/PriorityQueue:: now provides a stable heap order: items of the same time value will have a FIFO order.

@section{subsection}
  plot improvements

link::Reference/plot:: has been changed to use the link::Classes/Plotter:: class, which was formerly used by the 
@racketblock[plot2:: methods. ]

@racketblock[plot2:: has been deprecated, the old behavior is still available via the ]

@racketblock[plotOld:: methods, which have also been deprecated.

]
@section{subsection}
  UI deprecated

The UI class has been deprecated. Its functionality is now provided by the link::Classes/ShutDown:: and
link::Classes/OnError:: classes.

@section{subsection}
  Panner and XFade classes deprecated

The Panner and XFade classes that been used internally are now deprecated. UGens are better off, implementing the

@racketblock[checkInputs:: explicitly.

]
@section{subsection}
  Quark files outside DIRECTORY

teletype::*.quark:: files are now allowed to be within the quark itself rather than in the DIRECTORY.
This allows manually installed quarks to be easily managed by link::Classes/Quarks#*gui::

@section{subsection}
  New error marker
Instead of the non-cross-platform bullet-character (•), the error token in error messages are now underlined with teletype::^^^:: characters instead.

For example, the code 
@racketblock[[a, %%&&**, b]:: results in:
teletype::
  line 1 char 10:

  [a, %%&&**, b]
      ^^^^^^
::

]
@section{SECTION}
  Server-side news

@section{subsection}
  Bitwise ops
The bitwise operators 
@racketblock[&:: (and), ]

@racketblock[|:: (or), ]

@racketblock[ xor: :: (xor), ]

@racketblock[<<:: (left shift) and ]

@racketblock[>>:: (right shift) are now supported server-side on audio and control signals. Example:
]

@racketblock[
// 8-bit magic
(
play {
    var t = PulseCount.ar(Impulse.ar(8e3));
    HPF.ar(
        (
            ((t * 15) & (t >> 5)) |
            ((t * 5)  & (t >> [3, 4])) |
            ((t * 2)  & (t >> 9)) |
            ((t * 8)  & (t >> 11))
            - 3 % 256
        ) / 127-1 * 3
        , 20
    ).tanh
}
)
::

]
@section{subsection}
  VarLag UGen
The new link::Classes/VarLag:: UGen provides the same functionality as Lag but with linear and other curves.

@section{subsection}
  DelTapWr/DelTapRd UGens
The new link::Classes/DelTapRd:: and link::Classes/DelTapWr:: UGen can be used to easily implement multitap delays.

@section{subsection}
  Node-onFree
A new method link::Classes/Node#-onFree:: runs a function when node finished playing.

@section{subsection}
  LocalIn initial value
link::Classes/LocalIn:: now has an input for initial value.

@section{subsection}
  Close buffers on free
teletype::/b_free:: also free's soundfile if open (like teletype::/b_close::)

@section{subsection}
  More done flags
link::Classes/Demand::, link::Classes/VDiskIn:: and link::Classes/DiskIn:: now sets done flag (to be used by link::Classes/Done:: or link::Classes/FreeSelfWhenDone::)


@section{subsection}
  Shared Memory Server Interface

A shared-memory interface to the server has been introduced. This allows scoping and synchronous control bus access
for local clients.

@section{subsection}
  HPF/RHPF internal precision

The recursive filter loop for link::Classes/HPF:: and link::Classes/RHPF:: has been changed to double-precision in
order to avoid quantization noise. Pieces that depend on the quantization noise can make use of the GlitchUGens provided in sc3-plugins, which implement the old behavior.

@section{subsection}
  Plugin entry point

Plugins should use the teletype::PluginLoad:: macro as entry point instead of implementing a teletype::load:: funcion.
This allows ABI version checks and ensures that the entry point function is correctly exported from the shared library.
See link::Guides/WritingUGens:: for details.

@section{subsection}
  New FFT Plugin API

A new FFT API for server plugins has been introduced: this simplifies writing FFT ugens in a cross-platform manner,
since no external FFT libraries are required.

@section{subsection}
  Supernova

A new multi-processor implementation of scsynth. Parallelism of the synthesis graph is exposed to the user via the 
link::Classes/ParGroup:: class. Supernova is currently Linux-only. It is not provided in the macOS/Windows binaries.
In order to play patterns inside a ParGroup, the link::Classes/PparGroup:: can be used. Scsynth emulates parallel groups
with groups.

Plugins have to be adapted by acquiring spinlocks when accessing busses or buffers. See link::Guides/WritingUGens:: for
details.


