#lang scribble/manual
@(require (for-label racket))

@title{EZListView}
 A wrapper class for a label plus a listView with per item actions@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/ListView

@section{description}

EZListView is wrapper class which creates an (optional) label and a listView. It includes per item actions as well as a global action which are both evaluated upon selection of an item. Convenience methods for inserting and deleting list items are also included . If the parent is nil, then EZListView will create its own window. See link::Classes/EZGui:: and link::Classes/EZLists:: for all of the options.

@section{subsection}
  Some Important Issues Regarding EZListView

The convenience methods for EZListView require that the items array is an array of associations of labels and functions, not like in ListView, where items is simply an array of strings. If 
@racketblock[label:: is nil, then no staticText is created.

]
@section{classmethods}
 

@section{subsection}
  Creation / Class Methods

@section{method}
  new

@section{argument}
  parentView
The parent view or window. If the parent is nil, then EZListView will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

@section{argument}
  bounds
An instance of link::Classes/Rect:: or link::Classes/Point::. Default value is 
@racketblock[160@200::.

]
@section{argument}
  label
The label. Default value is 
@racketblock[nil::. If ]

@racketblock[nil::, then no link::Classes/StaticText:: is created.

]
@section{argument}
  items
Default value is 
@racketblock[nil::. An link::Classes/Array:: of link::Classes/Association::s ]

@racketblock[ ['label' -> { arg listObj; value }, ] ::. Or and link::Classes/Array:: link::Classes/Symbol::s (if you are only using ]

@racketblock[globalAction::).

]
@section{argument}
  globalAction
A global function to be performed in addition to the item functions 
@racketblock[ { arg listObj; value } ::.

]
@section{argument}
  initVal
Initial value of the List, i.e. the index selected. Default value is 0.

@section{argument}
  initAction
An instance of link::Classes/Boolean::. Performs the action at 
@racketblock[initVal:: on creation of the list, plus the ]

@racketblock[globalAction::. Default value is ]

@racketblock[false::.

]
@section{argument}
  labelWidth
Default value is 80. Not used if layout is 
@racketblock[\vert::.

]
@section{argument}
  labelHeight
Default value is 20. Not used if layout is 
@racketblock[\horz::.

]
@section{argument}
  layout

@racketblock[\vert:: or ]

@racketblock[\horz::. default is ]

@racketblock[\vert::.

]
@section{argument}
  gap
A link::Classes/Point::. By default, the view tries to get its parent's gap, otherwise it defaults to 
@racketblock[2@2::. Setting it overrides these.

]
@section{argument}
  margin
A link::Classes/Point::. This will inset the bounds occupied  by the subviews of view.

@section{discussion}
 
Example:

@racketblock[
(
// default with vertical layout
w = Window.new.front;
w.view.decorator = FlowLayout(w.view.bounds);
g = EZListView.new(w,
	230@230,
	"An ListView:",
	[
		\item0 ->{ |a| ("this is item 0 of " ++ a).postln },
		\item1 ->{ |a| ("this is item 1 of " ++ a).postln },
		\item2 ->{ |a| ("this is item 2 of " ++ a).postln },
	],
	globalAction: { |a| ("this is a global action of "++a.asString ).postln },
	initVal: 2,
	initAction: true,
	labelWidth: 120,
	labelHeight: 16,
	layout: \vert,
	gap: 2@2
	);

)

// or a more simple syntax (uses decorator gap settings):
(
w = Window.new.front;
w.view.decorator = FlowLayout(w.view.bounds);
g = EZListView.new(w,200@230, " List:");
g.addItem(\item0, { |a| ("this is item 0 of " ++ a).postln });
g.addItem(\item1, { |a| ("this is item 1 of " ++ a).postln });
g.addItem(\item2, { |a| ("this is item 2 of " ++ a).postln });
g.setColors(Color.grey, Color.white);
)
::

]
@section{instancemethods}
 

@section{subsection}
  Changing Appearance

@section{method}
  setColors
@section{argument}
  stringBackground
An instance of link::Classes/Color::. The 
@racketblock[background:: of the label and unit views.
]
@section{argument}
  stringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the label and unit views.
]
@section{argument}
  listBackground
An instance of link::Classes/Color::. The 
@racketblock[background:: of the list view.
]
@section{argument}
  listStringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the list view.
]
@section{argument}
  selectedStringColor
An instance of link::Classes/Color::. The 
@racketblock[selectedStringColor:: of the listView.
]
@section{argument}
  hiliteColor
An instance of link::Classes/Color::. The 
@racketblock[hiliteColor:: of the list view.
]
@section{argument}
  background
An instance of link::Classes/Color::. The 
@racketblock[background:: of the list view.

]
@section{method}
  font
Set the link::Classes/Font:: used by all the views.
@section{argument}
  font
An instance of link::Classes/Font::.

@section{examples}
 
Creates its own window if parent is nil:

@racketblock[
(
g = EZListView.new(label: " My PopUp List: ");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.setColors(Color.grey,Color.white);
)
::
Layout horizontal:
]

@racketblock[
(
g = EZListView.new(nil,205@180, "Choose One: ", layout:\horz);
10.do{|i| g.addItem("item"++i.asString, {("this is item" ++i.asString). postln})};
g.setColors(Color.grey,Color.white);
)
::
No labelView created, so set the window title:
]

@racketblock[
(
g = EZListView.new(bounds:200@230); // no label
12.do{|i| g.addItem("item"++i.asString, {("this is item" ++i.asString). postln})};
g.view.parent.findWindow.name=" choose item";
)
::
insert item:
]

@racketblock[
(
g = EZListView.new(nil,200@200, "List:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item4, {"this is item 4". postln});
)

g.insertItem(3, \item3, {"this is item 3". postln});
::
remove item:
]

@racketblock[
(
g = EZListView.new(nil,200@200, "List:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item4, {"this is item 4". postln});
g.insertItem(3, \item3, {"this is item 3". postln});
)

g.removeItemAt(1);
::
replace item:
]

@racketblock[
(
g = EZListView.new(nil,200@200, "List:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item3, {"this is item 3". postln});
)

g.replaceItemAt(2, \item2_replaced, {"this is item 2 replaced". postln});
::

]


