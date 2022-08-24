#lang scribble/manual
@(require (for-label racket))

@title{HelpBrowser}
@section{categories}
  HelpSystem, GUI>Interfaces
@section{related}
  Classes/SCDoc
 Browse the SuperCollider help documentation
@section{description}


HelpBrowser is the GUI help browser that lets you browse the documentation of SuperCollider. It is coupled with SCDoc to allow on-the-fly rendering of HTML help files.

@section{classmethods}
 
@section{private}
  getOldWrapUrl, initClass

@section{method}
  instance
The singleton HelpBrowser instance.

@section{method}
  new
Create a new HelpBrowser instance with given home URL.

@section{method}
  defaultHomeUrl
Get or set the default home URL.

@section{method}
  openNewWindows
Get or set the default for "open in new windows" toggle.

@section{method}
  goTo
Go to url with singleton instance or a new window, depending on the 
@racketblock[openNewWindows:: setting.

]
@section{method}
  openHelpFor
Open the relevant help page for given text in the singleton HelpBrowser instance.

@section{method}
  openSearchPage
Open the help search page with given text in the singleton HelpBrowser instance.

@section{method}
  openBrowsePage
Open the category browser page in the singleton HelpBrowser instance.

@section{argument}
  category
An optional String to start at specified category, like "UGens>Filters"

@section{method}
  openHelpForMethod
Open help for specified method.
@section{argument}
  method
a link::Classes/@section{Method}
 

@section{instancemethods}
 
@section{private}
  init, openTextFile, startAnim, stopAnim

@section{method}
  homeUrl
Get or set the home URL.

@section{method}
  window
The GUI window for this HelpBrowser.
@section{discussion}
 
Mainly useful for when you need to show the browser:

@racketblock[
HelpBrowser.instance.window.front;
::

]
@section{method}
  goTo
Go to specific URL. If the URL points to a file under link::Classes/SCDoc#*helpTargetDir:: it will be rendered on demand if needed.

@section{method}
  goHome
Go to the home URL.

@section{method}
  goBack
Go back.

@section{method}
  goForward
Go forward.



