#lang scribble/manual
@(require (for-label racket))

@title{SCDocHTMLRenderer}
 Render SCDoc markup text to HTML@section{categories}
  HelpSystem
@section{related}
  Classes/SCDoc, Guides/WritingHelp, Reference/SCDocSyntax

@section{description}

This class is part of the SCDoc help system, and handles the rendering of the parsed document tree into HTML output.

In normal cases you won't need to use this class directly, link::Classes/SCDoc:: uses this class by default to render help files.

@section{classmethods}
 

@section{method}
  renderOnStream
Renders a parsed document as HTML onto given stream.
@section{argument}
  stream
A stream, for example a link::Classes/File:: instance.
@section{argument}
  doc
An instance of link::Classes/SCDocEntry::
@section{argument}
  root
An instance of link::Classes/SCDocNode::

@section{method}
  renderToFile
Opens a file and passes it to link::#*renderOnStream::

@section{method}
  htmlForLink
Create a html string for the given scdoc link.
@section{argument}
  link
An scdoc link, such as a document key like "Classes/SinOsc", or an URL, or link to other file installed with the help.
@section{argument}
  escape
a boolean to set whether to escape special characters.
@section{returns}
 
A String

@section{method}
  makeArgString
Used internally.
@section{Returns}
  A link::Classes/String:: representing the arguments (with defaults) for a link::Classes/@section{Method}
 .


@section{section}
  CSS styling
The rendered HTML reads the global style from teletype::scdoc.css::, but also reads teletype::frontend.css:: and teletype::custom.css:: (in that order) if available, to enable specific frontends and users to override the CSS.

So to customise the CSS, the user can create a teletype::custom.css:: in their link::Classes/SCDoc#*helpTargetDir:: or at the root of any HelpSource directory (for example in teletype::YourExtension/HelpSource/custom.css:: ).



