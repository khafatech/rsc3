#lang scribble/manual
@(require (for-label racket))

@title{jitlib_networking}
 networked programming@section{categories}
  Libraries>JITLib>Tutorials, Tutorials>JITLib
@section{related}
  Overviews/JITLib

emphasis::please note any problems, I'll try to add solutions here.::

@section{section}
 1) using ProxySpace with more than one client, with separate bus spaces

@section{note}
 
if only one client is using a remote server, only step (a) and step (d) are relevant. The clientID argument can be left out then.
::

@section{subsection}
 before you start

remember to synchronize your system clocks. This can be done by:
@section{definitionList}
 
## in macOS || SystemPreferences>Date&Time: set emphasis::"Set Date & Time automatically":: to true.
## in Linux || set the ntp clock
::
a local time server is better than the apple time server. if you cannot sync the time, you can set the server latency to 
@racketblock[nil::. This will break the pattern's functionality though.


]
@section{subsection}
 a) boot the (remote) server and create a local model


@racketblock[
s = Server("serverName", NetAddr(hostname, port));
s.options.maxLogins = 16; // or the maximum number of participants in the network
s.boot; // you cannot directly boot a remote server instance, but this initializes everything that is needed
::
]
@section{definitionList}
 
## serverName || can be any name
## hostname || is an ip address, or if you have a name resolution, a network name
## port || the port on which the server is listening. default is 57110
::
see link::Classes/Server::

@section{subsection}
 b) from each client, initialize the default node and set notify to true:


@racketblock[
s.boot; // this will initialize the tree and start notification

// if needed, a server window can be created:
s.makeWindow;
::


]
@section{subsection}
 c) now create a ProxySpace from the server:


@racketblock[
p = ProxySpace.push(s);
::

]


