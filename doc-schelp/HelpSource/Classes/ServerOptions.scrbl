#lang scribble/manual
@(require (for-label racket))

@title{ServerOptions}
 Encapsulates commandline and other options for a Server@section{categories}
  Server
@section{related}
  Classes/Server, Reference/Server-Architecture, Reference/Server-Command-Reference

@section{description}


ServerOptions encapsulates various options for a server app within an object. This makes it convenient to launch multiple servers with the same options, or to archive different sets of options, etc.
Every link::Classes/Server:: has an instance of ServerOptions created for it if one is not passed as the options argument when the Server object is created. (This is the case for example with the local and internal Servers which are created at startup.)

A Server's instance of ServerOptions is stored in its options instance variable, which can be accessed through corresponding getter and setter methods.

N.B. A ServerOptions' instance variables are translated into commandline arguments when a server app is booted. Thus a running Server must be rebooted before changes will take effect. There are also a few commandline options which are not currently encapsulated in ServerOptions. See link::Reference/Server-Architecture:: for more details.

@section{ClassMethods}
 
@section{private}
  prListDevices

@section{method}
  new
Create and return a new instance of ServerOptions.

@section{method}
  devices
macOS only. Return an Array of Strings listing the audio devices currently available on the system.

@section{method}
  inDevices
macOS only. Return an Array of Strings listing the audio devices currently available on the system which have input channels.

@section{method}
  outDevices
macOS only. Return an Array of Strings listing the audio devices currently available on the system which have output channels.

@section{instancemethods}
 
@section{subsection}
  The Options

@section{method}
  blockSize
The number of samples in one control period. The default is 64.

@section{method}
  device
A String that allows you to choose a sound device to use as input and output. The default, nil will use the system's default input and output device(s) (more below in the examples).

@section{note}
 When the server is compiled for jack natively, the teletype::device:: can be used to connect to a named server and
use a specific client name. The argument specifies either a server name or a server name and the requested client
name. Passing 
@racketblock[nil:: is equivalent to to teletype::default:SuperCollider::.

The jack connections can be configured via the environment variables teletype::SC_JACK_DEFAULT_INPUTS:: and
teletype::SC_JACK_DEFAULT_OUTPUTS::. The format is either a string that specifies the another jack client or a
comma-separate list of jack ports.

]

@racketblock[
// connect first to input channels with system
"SC_JACK_DEFAULT_INPUTS".setenv("system:capture_1,system:capture_2");

// connect all output channels with system
"SC_JACK_DEFAULT_OUTPUTS".setenv("system");
::

::

]
@section{method}
  inDevice
A String that allows you to choose an input sound device. The default, nil will use the system's default input device (more below in the examples).

@section{method}
  outDevice
A String that allows you to choose an output sound device. The default, nil will use the system's default output device (more below in the examples).

@section{method}
  hardwareBufferSize
The preferred hardware buffer size. If non-nil the server app will attempt to set the hardware buffer frame size. Not all sizes are valid. See the documentation of your audio hardware for details. Default value is nil.

@section{method}
  initialNodeID
By default, the Server object in the client begins allocating node IDs at 1000, reserving 0-999 for "permanent" nodes. You may change this default here.

@section{method}
  inputStreamsEnabled
A String which allows turning off input streams that you are not interested in on the audio device. If the string is "01100", for example, then only the second and third input streams on the device will be enabled. Turning off streams can reduce CPU load. The default value is nil.

@section{method}
  loadDefs
A Boolean indicating whether or not to load the synth definitions in synthdefs/ (or anywhere set in the environment variable SC_SYNTHDEF_PATH) at startup. The default is true.

@section{method}
  maxNodes
The maximum number of nodes. The default is 1024.

@section{method}
  maxSynthDefs
The maximum number of synthdefs. The default is 1024.

@section{method}
  memSize
The number of kilobytes of real time memory allocated to the server. This memory is used to allocate synths and any memory that unit generators themselves allocate (for instance in the case of delay ugens which do not use buffers, such as CombN), and is separate from the memory used for buffers. Setting this too low is a common cause of 'exception in real time: alloc failed' errors. The default is 8192.

@section{method}
  numAudioBusChannels
The number of audio rate busses, which includes input and output busses. The default is 1024.

@section{method}
  numBuffers
The number of global sample buffers available. (See Buffer.) The default is 1024.

@section{method}
  numControlBusChannels
The number of internal control rate busses. The default is 16384.

@section{method}
  numInputBusChannels
The number of audio input bus channels. This need not correspond to the number of hardware inputs. The default is 2.

@section{method}
  numOutputBusChannels
The number of audio output bus channels. This need not correspond to the number of hardware outputs (this can be useful for instance in the case of recording). The default is 2.

@section{method}
  numRGens
The number of seedable random number generators. The default is 64.

@section{method}
  numWireBufs
The maximum number of buffers that are allocated to interconnect unit generators. (Not to be confused with the global sample buffers represented by Buffer.) This sets the limit of complexity of SynthDefs that can be loaded at runtime. This value will be automatically increased if a more complex def is loaded at startup, but it cannot be increased thereafter without rebooting. The default is 64.

@section{method}
  outputStreamsEnabled
A String which allows turning off output streams that you are not interested in on the  audio device. If the string is "11000", for example, then only the first two output streams on the device will be enabled. Turning off streams can reduce CPU load.

@section{method}
  protocol
A Symbol representing the communications protocol. Either 
@racketblock[\udp:: or ]

@racketblock[\tcp::. The default is ]

@racketblock[\udp::.

]
@section{method}
  remoteControlVolume
A Boolean indicating whether this server should allow its volume to be set remotely. The default value is 
@racketblock[false::.

]
@section{method}
  sampleRate
The preferred sample rate. If non-nil the server app will attempt to set the sample rate of the hardware. The hardware has to support the sample rate that you choose.

@section{method}
  verbosity
Controls the verbosity of server messages. A value of 0 is normal behaviour. -1 suppresses informational messages. -2 suppresses informational and many error messages, as well as messages from Poll. The default is 0.

@section{method}
  zeroConf
A Boolean indication whether or not the server should publish its port using zero configuration networking, to facilitate network interaction. This is true by default; if you find unacceptable delays (beachballing) upon server boot, you can try setting this to false.

@section{method}
  ugenPluginsPath
A path or an Array of paths. If non-nil, the standard paths are NOT searched for plugins. This corresponds with the option "-U".

@section{method}
  restrictedPath
Allows you to restrict the system paths in which the server is allowed to read/write files during running. A nil value (the default) means no restriction. Otherwise, set it as a string representing a single path.

@section{method}
  threads
Number of audio threads that are spawned by supernova. For scsynth this value is ignored. If it is 
@racketblock[nil::or 0, it
uses the one thread per CPU. Default is ]

@racketblock[nil::.

]
@section{method}
  useSystemClock
Tells supernova whether to sync to the driver's sample clock, or to the system clock.
For scsynth this value is ignored.
@section{list}
 
## 
@racketblock[false:: (default) -- Use the sample clock. This helps to
support sample-accurate scheduling; however, messaging latency from
the SuperCollider language will drift over long periods of time.
## ]

@racketblock[true:: -- Use the system clock. Timestamped messages will maintain consistent
latency over long sessions, but may not be perfectly sample-accurate.
::

]
@section{method}
  memoryLocking
A Boolean indicating whether the server should try to lock its memory into physical RAM. Default is 
@racketblock[false::.

]
@section{method}
  maxLogins
An Integer indicating the maximum number of clients which can simultaneously receive notifications from the server. When using TCP this is also the maximum number of simultaneous connections. This is also used by the language to split ranges of link::Classes/Node##Nodes::, link::Classes/Buffer##Buffers::, or link::Classes/Bus##Busses::. In multi-client situations you will need to set this to at least the number of clients you wish to allow. This must be the same in the Server instances on every client. The default is 1.


@section{subsection}
  Other Instance Methods
@section{method}
  asOptionsString
@section{argument}
  port
The port number for the resulting server app. Default value is 57110.
@section{Returns}
  a String specifying the options in the format required by the command-line server app (scsynth or supernova).

@section{method}
  firstPrivateBus
@section{Returns}
  the index of the first audio bus on this server which is not used by the input and output hardware.

@section{method}
  pingsBeforeConsideredDead
Number of failed pings (attempts to contact server process) before server is considered dead.
Default value is 5.

@section{Examples}
 

@racketblock[
// Get the local server's options

o = Server.local.options;

// Post the number of output channels

o.numOutputBusChannels.postln;

// Set them to a new number

o.numOutputBusChannels = 6; // The next time it boots, this will take effect

// specify a device

o.device ="MOTU Traveler"; 	// use a specific soundcard
o.device = nil;			// use the system default soundcard

// Create a new instance of ServerOptions

o = ServerOptions.new;

// Set the memory size to twice the default

o.memSize = 4096;

// Create a new Server on the local machine using o for its options

t = Server(\Local2, NetAddr("127.0.0.1", 57111), o);
t.makeWindow;
t.boot;
t.quit;
::

]


