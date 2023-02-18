#lang scribble/manual
@(require (for-label racket))

@title{TextView}
 A view displaying editable formatted text@section{categories}
  GUI>Views

@section{description}


TextView consists of an area where strong::multi-line text:: can be typed in and edited.

Using the view's methods, the text can be formatted: different strong::font:: and strong::text color:: can be applied to parts of the text. Text can also be inserted, removed, and selected programmatically.

The view can strong::open text documents:: and load from them both strong::plain text::, as well as formatted text in strong::HTML::, although it can not save the text back to files. However, you can get the contents of the view using the link::#-string:: method and then implement saving on your own, but the -string method will only return plain text, regardless of how the contents of the view are formatted.


@section{CLASSMETHODS}
 

@section{PRIVATE}
  key



@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Text and Formatting

@section{METHOD}
  open
	Opens a file at 
@racketblock[path:: and loads text from it.

	The file can be in plain text or HTML (or RTF, in Cocoa GUI) format. Note however that saving formatted text in the view is not supported.

	If loading the text from the file succeeds, it will replace any current contents of the view.

	]
@section{argument}
  path
		A String.

@section{METHOD}
  string
	The entire displayed contents of the view, as plain text.

	Setting this variable will replace any current contents of the view.

	@section{argument}
 
		A String.

@section{METHOD}
  setString
	Inserts the 
@racketblock[string:: at ]

@racketblock[rangeStart:: position, replacing ]

@racketblock[rangeSize:: amount of following characters. If ]

@racketblock[rangeSize:: is 0, the text will be inserted without any characters being removed.

	]
@section{argument}
  aString
		A String - the text to insert.
	@section{argument}
  rangeStart
		An Integer position within the text, in characters.
	@section{argument}
  rangeSize
		An Integer amount of characters.

@section{METHOD}
  currentLine
	The plain text of the line at text cursor.


@section{SUBSECTION}
  Formatting

TextView supports text font and color, and can syntax colorize sclang code.

@section{note}
 The formatting is reset when the string changes.::



@racketblock[
(
var text = "Tous ces nombres paraissent bien concrets";
t = TextView(bounds: Rect(300, 400));
t.string = text;
t.front;
fork {
	loop {
		2.0.rand.wait;
		defer {
			t.setFont(Font("Times", rrand(12, 48)), rand(text.size - 1), rrand(3, 17));
			t.setStringColor(Color.rand, rand(text.size - 1), rrand(3, 17));

		}
	}
};
)
::


]
@section{METHOD}
  setFont
	Applies the 
@racketblock[font:: to ]

@racketblock[rangeSize:: amount of characters following the ]

@racketblock[rangeStart:: position.

	]
@section{argument}
  font
		A Font to apply to the desired range of text.
	@section{argument}
  rangeStart
		An Integer position within the text, in characters.
	@section{argument}
  rangeSize
		An Integer amount of characters.

@section{METHOD}
  setStringColor
	Applies the 
@racketblock[color:: to ]

@racketblock[rangeSize:: amount of characters following the ]

@racketblock[rangeStart:: position.

	]
@section{argument}
  color
		A Color to apply to the desired range of text.
	@section{argument}
  rangeStart
		An Integer position within the text, in characters.
	@section{argument}
  rangeSize
		An Integer amount of characters.



@section{METHOD}
  syntaxColorize
	Applies colors to text throughout the entire contents of the view, according to the SuperCollider language syntax highlighting scheme.


@racketblock[
(
t = TextView(bounds: Rect(300, 400));
t.string = this.cmdLine;
t.syntaxColorize;
t.front;
)
::



]
@section{SUBSECTION}
  Text Selection

@section{METHOD}
  selectedString
	The plain text contained in the current selection.

	When getting this variable and there is no selection, the entire line at text cursor is returned (equivalent to link::#-currentLine::).

	Setting this variable will replace text in the selection with the argument, or do nothing if there is no selection.

	@section{argument}
 
		A String.
	@section{returns}
 
		A String.

@section{METHOD}
  selectionStart
	The starting position of the selection. If no text is selected this variable represents the cursor position.

	@section{returns}
 
		An Integer position within the text, in characters.

@section{METHOD}
  selectionSize
	The size of the current selection.

	@section{returns}
 
		An Integer amount of characters - 0 if no text is selected.

@section{METHOD}
  select

	@section{note}
  Not available in strong::Cocoa GUI::. ::

	Selects 
@racketblock[size:: amount of characters following the ]

@racketblock[start:: position. The cursor will remain at the end of the new selection.

	]
@section{argument}
  start
		An Integer position within the text, in characters.
	@section{argument}
  size
		An Integer amount of characters.




@section{SUBSECTION}
  Appearance

@section{METHOD}
  font
	The default font of the entire text. This font applies to any text to which a font has not been applied using link::#-setFont::.

	@section{argument}
 
		A Font.

@section{METHOD}
  stringColor
	The default color of the entire text. This color applies to any text to which a color has not been applied using link::#-setStringColor::.

@section{note}
 
Calling 
@racketblock[stringColor_:: does emphasis::not:: affect the cursor's color. Setting a dark background, using ]

@racketblock[background_::, and a light text color will leave the cursor as a dark color. It is recommended to set the background and string colors by setting the TextView's palette to an instance of link::Classes/QPalette::.

]

@racketblock[
(
t = TextView(nil, Rect(800, 50, 500, 400))
.string_("Some text")
.palette_(QPalette.dark)  // set all colors here
.front;
)
::
::


]
@section{METHOD}
  tabWidth
	The width of tab characters as they are displayed.




@section{SUBSECTION}
  Interaction

@section{METHOD}
  editable
	Whether the contents of the view are editable, i.e. the text can be typed in and deleted by the user.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  enterInterpretsSelection
	Whether the selection will be interpreted and invoked as SuperCollider code when Ctrl/Cmd/Shift + Enter key combination is pressed.

	Defaults to 
@racketblock[false::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  usesTabToFocusNextView
	Whether the tab key will - instead of inserting a tab character into the text - switch focus to the next view (as usual for other views).

	Defaults to 
@racketblock[false::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  hasHorizontalScroller
	Whether the horizontal scroller is shown.

	Note that if link::#-autohidesScrollers:: is 
@racketblock[true:: the scroller may be hidden despite this variable being set to ]

@racketblock[true::. Since the TextView typically wraps text into the next line when a line reaches the edge of the view, the horizontal scroller may never be shown, unless link::#-autohidesScrollers:: is ]

@racketblock[false::.

	Defaults to ]

@racketblock[true::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  hasVerticalScroller
	Whether the vertical scroller is shown.

	Note that if link::#-autohidesScrollers:: is 
@racketblock[true:: the scroller may be hidden despite this variable being set to ]

@racketblock[true::.

	Defaults to ]

@racketblock[true::.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  autohidesScrollers
	Whether each of the scrollers will be automatically hidden if there is no use for it, i.e. the content is not scrollable in the direction of the scroller.

	If link::#-hasHorizontalScroller:: or link::#-hasVerticalScroller:: is 
@racketblock[false::, the respective scroller will always be hidden, regardless of this variable.

	Defaults to ]

@racketblock[true::.

	]
@section{argument}
 
		A Boolean.



@section{SUBSECTION}
  Drag and Drop

@section{note}
  Default drag-and-drop behavior of TextView is not defined in standard SC methods, but in the view implementation instead (except for link::#-defaultGetDrag::). It may or may not be overridable by adding your own handlers (see link::Classes/View#Drag and drop::), depending on the GUI kit in use.
::

Dragging from TextView will give the selected text in a String as drag data, while dropping will accept any object and insert it link::Classes/Object#-asString#as String:: at the drop location.

You can also drag files from outside SuperCollider onto a TextView, and it will insert their URLs at the drop location.

@section{METHOD}
  defaultGetDrag

	@section{returns}
 
		The link::#-selectedString::.



@section{EXAMPLES}
 


@racketblock[
(
w = Window.new("Text View Example",Rect(100,Window.screenBounds.height-400, 520,300)).front;
t = TextView(w.asView,Rect(10,10, 500,200))
    .focus(true);
)

// Using the Window you just created, try these in succession, and test how the text view responds
t.mouseUpAction_{|it, x, y, modifiers, buttonNumber, clickCount, pos| [pos].postln};
t.autohidesScrollers_(false);
t.hasVerticalScroller_(false);
t.hasVerticalScroller_(true);
t.hasHorizontalScroller_(false);
t.hasHorizontalScroller_(true);
t.autohidesScrollers_(true);

t.open("Help/GUI/Main-GUI/Button.html"); // load an html file

// selective editing and formatting
t.setStringColor (Color.red, 5, 5);
t.setFont (Font("Courier",12), 5, 10);
t.setString ("\nA replacement String\n", 12, 6);

// compare with these methods, which change everything
t.font_(Font("Courier",14));
t.stringColor_(Color.blue);
::
]


