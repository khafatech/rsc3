#lang scribble/manual
@(require (for-label racket))

@title{Download}
 Fetch a file from a remote URL@section{categories}
  Files
@section{related}
  Classes/File

@section{description}

Download allows you to download a file from a specified URL


@section{CLASSMETHODS}
 
@section{private}
  qtClass

@section{METHOD}
  new
Create and start a new Download.

@section{ARGUMENT}
  requestedURL
A link::Classes/String:: containing the URL of the file to download.

@section{ARGUMENT}
  localPath
A link::Classes/String:: containing the local path at which to save the downloaded file.

@section{ARGUMENT}
  finishedFunc
A link::Classes/Function:: to evaluate when the download is complete.

@section{ARGUMENT}
  errorFunc
A link::Classes/Function:: to evaluate if the download fails due to an error.

@section{ARGUMENT}
  progressFunc
A link::Classes/Function:: to process the download's progress. This Function will be passed two arguments, the bytes received, and the total bytes.

@section{returns}
  A new Download.

@section{METHOD}
  cancelAll
Cancel all active Downloads.


@section{INSTANCEMETHODS}
 
@section{private}
  doProgress, doError, doFinished, doOnShutDown, cleanup, startDownload, init

@section{METHOD}
  cancel
Cancel the download.

@section{METHOD}
  errorFunc
Get or set the error link::Classes/Function::.

@section{METHOD}
  finishedFunc
Get or set the download finished link::Classes/Function::.

@section{METHOD}
  progressFunc
Get or set the download progress link::Classes/Function::.



@section{EXAMPLES}
 


@racketblock[
Download("http://art-on-wires.org/wp-content/uploads/2011/03/nick_collins.png", "/tmp/nick.png", {\huzzah.postln;}, {\error.postln}, {|rec, tot| [rec, tot].postln}); // beautify your tmp directory

d = Download("http://scottwilson.ca/files/flame.mp3", "/tmp/flame.mp3", {\huzzah.postln;}, {\error.postln}, {|rec, tot| [rec, tot].postln});
d.cancel; // cancel this

]


