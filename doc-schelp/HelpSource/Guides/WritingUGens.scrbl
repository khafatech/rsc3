#lang scribble/manual
@(require (for-label racket))

@title{Writing Unit Generators}
 Get started with writing unit generators@section{categories}
  Internals
@section{related}
  Reference/ServerPluginAPI

SuperCollider has a vast library of unit generators that can be assembled in unlimited ways, but sometimes even those
aren't sufficient. You may have a need for an unusual signal processing algorithm, or you're running into efficiency
problems that can be solved by condensing parts of your SynthDef into a single UGen.

UGens are defined in server plugins written in C++. Server plugins are not to be confused with quarks, which extend the
SuperCollider language. UGens exist more or less independently of the interpreter and you don't need much familiarity
with SC to write them.

Writing UGens is not too difficult, but it's arguably far less convenient and intuitive than the high-level tools that
SuperCollider provides. You'll need a build system and a good amount of boilerplate code -- even fairly basic signal
processing operations can require a lot of code. You don't have an instant live coding environment, and mistakes can
easily crash the server. SuperCollider's UGens are stable and well-tested, and custom UGens are best viewed as a last
resort for when the limitations of SC are impassable.

@section{section}
  Pseudo-UGens

Before we proceed to the real UGens, we'll take a quick detour for the sake of completeness. A pseudo-UGen is a bit of
SuperCollider code that abbreviates a certain configuration of UGens that gets used repeatedly. A pseudo-UGen is a class
that superficially resembles a UGen class, but it only returns a composition of existing UGens. It has no efficiency
savings, but it does save typing.

The below example has only a 
@racketblock[.ar:: method, but you can just as easily have both ]

@racketblock[.ar:: and ]

@racketblock[.kr::
methods.

]

@racketblock[
BoringMixer {
    *ar { arg left, right;
        ^(left + right) * 0.5;
    }
}
::

Examples of pseudo-UGens found in SC include link::Classes/BLowPass4:: and link::Classes/BHiPass4::, which break down
into link::Classes/SOS:: UGens.

There are very few restrictions on what these classes can contain, but you should keep the following in mind:

]
@section{list}
 
## It is courteous to leave a note in your class's help file that identifies it as a pseudo-UGen.
## To avoid confusion, pseudo-UGens should not inherit from the base class link::Classes/UGen::.
## A SynthDef can only have two link::Classes/LocalIn::/link::Classes/LocalOut:: pairings — one for control rate and one
for audio rate. Putting LocalIn and LocalOut in a pseudo-UGen is a bad idea, as it could interfere with other feedback
loops in the SynthDef.
::

@section{section}
  Basics of UGens

A (real) UGen needs two components: a plugin for the server, and a class for the language. The class goes in an ordinary

@racketblock[*.sc:: file, and defines the interface for your UGen in the language. This class is generally just a few lines of
code that ultimately call the class method link::Classes/UGen#-multiNew::.

The server plugin is where the actual UGen behavior is defined. It is given by a dynamically loaded library written in
C++, whose format is platform-dependent:

]
@section{list}
 
## 
@racketblock[*.so:: for *nix
## ]

@racketblock[*.dll:: for Windows
## ]

@racketblock[*.scx:: for macOS
::

A plugin file can contain more than one UGen, and it can also define things other than UGens such as buffer fill
("/b_gen") commands.

When the server boots, it will look for plugin files in ]

@racketblock[Platform.userExtensionDir::. Since sclang also looks for
class files in the same location, the class file and the library file can go in the same place.

Plug-ins are loaded during the startup of the server, so it will have to be restarted after (re-)compiling a
plugin. If you modify the plugin file but not the class file, you don't need to reboot the interpreter.

]
@section{section}
  FAUST

FAUST @section{footnote}
 http://faust.grame.fr/:: is an open source DSP language that describes real-time audio units. It can
compile to SuperCollider plugins, providing an easy way to create UGens in SuperCollider.

FAUST provides a shell script useful for SuperCollider users called 
@racketblock[faust2supercollider::. This compiles a .dsp
file into a class file and server plugin, which you can then drop into your extensions directory.

FAUST plugins are often quick to develop and can be painlessly ported to other environments. Unfortunately, they can't
take advantage of all of the server's features, such as accessing Buffers or random number generators, and some UGens
featuring very complex logic are difficult or impossible to write in FAUST. Furthermore, the FAUST compiler is quite
intelligent but it might not always offer the best efficiency in its results. If a UGen you are developing hits these
limitations, it is time to move on to handwritten C++.

]
@section{section}
  Example Plugins

To get an idea of the necessary ingredients for writing UGens, it's often best to poke around at complete examples.
We've set up a GitHub repository at https://github.com/supercollider/example-plugins, which contains some example
plugins numbered roughly by complexity. Each directory in that repository is self-contained with its own build system,
so you can copy out a directory to form a starting point for your own UGens. The source codes of these plugins are
heavily commented.

The first example, BoringMixer, is very minimal. The UGen is stateless and has only one calculation function, which is
audio rate.

MySaw introduces states and multiple calculation functions. AnalogEcho introduces real-time memory management through
internal buffers, and demonstrates how to do cubic interpolation from an array of samples.

@section{section}
  Anatomy of a UGen

The SC source code has a header file, 
@racketblock[include/plugin_interface/SC_PlugIn.h::, that gives you your interface to the server
architecture as well as a bunch of helper functions. These are documented at link::Reference/ServerPluginAPI::.

]
@section{subsection}
  The Entry Point

When the library is loaded the server calls a function in the library, which is defined by the 
@racketblock[PluginLoad()::
macro.  This entry point has two responsibilities:

]
@section{list}
 
## It needs to store the passed in pointer to the InterfaceTable in a global variable.
## It registers the unit generators.
::

Unit Generators are defined by calling a function in the InterfaceTable and passing it the name of the unit generator,
the size of its C data struct, and pointers to functions for constructing and destructing it. There are 4 macros, which
can be used to simplify the process.

@section{definitionList}
 
## DefineSimpleUnit || Define a `simple' unit generator
## DefineDtorUnit || Define a unit generator with a destructor
## DefineSimpleCantAliasUnit || Define a `simple' unit generator, whose input and output buffers cannot alias
## DefineDtorCantAliasUnit || Define a unit generator with a destructor, whose input and output buffers cannot alias
::

These macros depend on a specific naming convention:
@section{list}
 
## The unit generator struct is named like the plug-in.
## The unit generator constructor is named 
@racketblock[PluginName_Ctor::
## The unit generator destructor is named ]

@racketblock[PluginName_Dtor::
::

]
@section{subsection}
  The Calculation Function

The meat of the UGen is its calculation function, which gets called every control period with the UGen object as an
argument. (This is for control-rate and audio-rate UGens -- demand-rate is different.) In this function, the UGen reads
from its inputs and writes to its outputs.

The calculation function is selected in the 
@racketblock[PluginName_Ctor:: function with the ]

@racketblock[SETCALC:: macro. You can
name the calculation function whatever you want, but the convention is ]

@racketblock[PluginName_next::.

UGens often have multiple calculation functions, depending on the rate of the UGen itself and the rate of its inputs.
For example, Phasor can be .ar or .kr, and its argument can be either .ar or .kr. So it has four calculation functions:
Phasor_next_aa, Phasor_next_ak, Phasor_next_ka, and Phasor_next_kk. You don't need to be this thorough for your own
UGens, however. For example, link::Classes/FreeVerb:: has only one calculation function. Who would want a control-rate
reverb?

]
@section{subsection}
  Building Unit Generator Plugins

The most portable way to build plugins is using cmake @section{footnote}
 http://www.cmake.org::, a cross-platform build
system.

The examples in the example repository contain 
@racketblock[CMakeLists.txt:: files.

]
@section{section}
  Coding Guidelines

Unit generator plugins are called from the real-time context, which means that special care needs to be taken in order
to avoid audio dropouts.

@section{definitionList}
 
## Memory Allocation || Do not allocate memory from the OS via 
@racketblock[malloc:: / ]

@racketblock[free:: or ]

@racketblock[new::/ ]

@racketblock[delete::.
                        Instead you should use the real-time memory allocator via ]

@racketblock[RTAlloc:: / ]

@racketblock[RTFree::.
## STL Containers || It is generally not recommended to use STL containers, since they internally allocate memory. The only
                     way the STL containers can be used is by providing an Allocator, which maps to the allocating functions of
                     the server.
## Blocking API Calls || Unit generators should not call any code, which could block the execution of the current thread. In
                         particular, system calls should be avoided. If synchronization with other threads is required, this has to be
                         done in a lock-free manner.
::


]
@section{subsection}
  Thread Safety

There are two different implementations of the SuperCollider server. strong::scsynth:: is the traditional server and
strong::supernova:: is a new implementation with support for multi-processor audio synthesis. Since the plugins in
strong::supernova:: can be called at the same time from multiple threads, write access to global data structures needs
to be synchronized.

@section{definitionList}
 
## Shared Global Data Structures || Unit generators should not share data structures, which are written to. While it it safe to use
    global data structures for read-only purposes (e.g. different unit generators could use the same constant wavetable),
    the data structures that are modified by the unit generators should not be shared among different instances.

## Resource Locking || SuperCollider's buffers and busses are global data structures, and access needs to be synchronized.
    This is done internally by using reader-writer spinlocks. This is done by using the 
@racketblock[ACQUIRE_::, ]

@racketblock[RELEASE_::, and
    ]

@racketblock[LOCK_:: macros, which are defined in SC_Unit.h. As exception, buffers in the wavetable format are not required to be
    locked.
::

]
@section{subsection}
  Deadlock Prevention

In order to prevent deadlocks, a simple deadlock prevention scheme is implemented, based on the following constraints.

@section{list}
 
## Lock resources only when required: few unit generators actually require the access to more than one resource at the same time.
   The main exception of this rule are the FFT Chain UGens, which access multiple buffers at the same time. There is no known unit
   generator, which accesses both buffers and busses at the same time.
## Acquire reader locks if possible. Since multiple UGens can acquire a reader lock to the same resource at the same time, their
   use reduces contention.
## Resources have to be acquired in a well-defined order: busses should be acquired before buffers and resources with a high index
   should be acquired before resources with a low index.
::


