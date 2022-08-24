#lang scribble/manual
@(require (for-label racket))

@title{WebView}
 Web page display and browser@section{categories}
  GUI>Views

@section{description}


WebView displays web pages and provides all the standard browsing functionality.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  clearCache

  Clears all the memory caches, so that link::#-reload#reloading:: a page is ensured to reload all the resources.


@section{INSTANCEMETHODS}
 


@section{SUBSECTION}
  Navigation

@section{METHOD}
  url

    Gets the current URL, or navigates to a new one.

@section{METHOD}
  reload

    Reloads the current page.

@section{METHOD}
  back

    Navigates to the previous page in history.

@section{METHOD}
  forward

    Navigates to the next page in history.

@section{METHOD}
  findText

    Finds and selects the next instance of given text on the current page. When the given text changes, the search starts anew.

    @section{Argument}
  string
        The text to find; a String.

    @section{Argument}
  reverse
        Whether to search in reverse direction; a Boolean.


@section{SUBSECTION}
  Data

@section{METHOD}
  html

    Gets or sets the displayed html content.

    @section{Argument}
 
        A String.

@section{METHOD}
  plainText

    Tries to extract plain text from the displayed content, and returns it.
    @section{Returns}
  A String.

@section{METHOD}
  title

    The title of the current page.
    @section{Returns}
  A String.

@section{METHOD}
  selectedText

    The currently selected text.
    @section{Returns}
  A String.


@section{SUBSECTION}
  Behavior and appearance

@section{METHOD}
  enterInterpretsSelection
    Whether pressing Ctrl+Return or Ctrl+Enter while some text is selected should evaluate the selection as SuperCollider code.
    @section{Argument}
 
        A Boolean.

@section{METHOD}
  setFontFamily
    Sets a specific font family to be used in place of a CSS-specified generic font family.
    @section{Argument}
  generic
        The CSS generic font family to assign a font to; one of the following symbols: 
@racketblock[\standard, \fixed, \serif, \sansSerif, \cursive, \fantasy::.
    ]
@section{Argument}
  specific
        A font family name to be assigned to the generic family; a String.

@section{METHOD}
  editable
    Get or set whether the entire web page is editable.
    @section{Argument}
 
        A Boolean.

@section{SUBSECTION}
  Actions

@section{METHOD}
  onLoadFinished
    Sets the object to be evaluated when a page has loaded successfully, passing the view as the argument.

@section{METHOD}
  onLoadFailed
    Sets the object to be evaluated when a page has failed to load, passing the view as the argument.

@section{METHOD}
  onLinkActivated
    Sets the object to be evaluated when the user triggers a link, passing the view and the URL of the link (as String) as the arguments.

    When this is set to other than nil, WebView will stop handling links altogether. Setting this to nil will restore WebView link handling again.

@section{METHOD}
  onReload
    Sets the object to be evaluated whenever a page reload is requested, passing the view and the URL to be reloaded (as String) as the arguments.

    When this is set to other than nil, WebView will do nothing on reload requests. Setting this to nil will restore WebView reload handling again.


@section{SUBSECTION}
  JavaScript

@section{METHOD}
  evaluateJavaScript
    Evaluates the given JavaScript code in the context of the current page.
    @section{Argument}
 
        A String.


