#lang scribble/manual
@(require (for-label racket))

@title{EZPopUpMenu}
 A wrapper class for a label plus a popUpMenu with per item actions@section{categories}
  GUI>EZ-GUI
@section{related}
  Classes/PopUpMenu

@section{description}

EZPopUpMenu is wrapper class which creates an (optional) label and a popUpMenu. It includes per item actions as well as a global action which are both evaluated upon selection of an item. Convenience methods for inserting and deleting menu items are also included . If the parent is nil, then EZPopUpMenu will create its own window.See link::Classes/EZGui:: and link::Classes/EZLists:: for all of the options.

@section{subsection}
  Some Important Issues Regarding EZPopUpMenu
The convenience methods for EZPopUpMenu require that the items array is an array of associations of labels and functions, not like in link::Classes/PopUpMenu::, where items is simply an array of strings. If 
@racketblock[label:: is ]

@racketblock[nil::, then no staticText is created.

]
@section{classmethods}
 
@section{method}
  new

@section{argument}
  parentView
The parent view or window. If the parent is nil, then EZPopUpMenu will create its own link::Classes/Window::, and place it conveniently on the screen if the bounds are a link::Classes/Point::. If the bounds are a link::Classes/Rect::, then the link::Classes/Rect:: determines the window bounds.

@section{argument}
  bounds
An instance of link::Classes/Rect:: or link::Classes/Point::. Default value is 
@racketblock[160@22::.

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

@racketblock[ ['label' -> { arg menuObj; value }, ] ::. Or and link::Classes/Array:: link::Classes/Symbol::s (if you are only using ]

@racketblock[globalAction::).

]
@section{argument}
  globalAction
A global function to be performed in addition to the item functions 
@racketblock[ { arg menuObj; value } ::.

]
@section{argument}
  initVal
Initial value of the menu, i.e. the index selected. Default value is 0.

@section{argument}
  initAction
An instance of link::Classes/Boolean::. Performs the action at 
@racketblock[initVal:: on creation of the menu, plus the ]

@racketblock[globalAction::. Default value is ]

@racketblock[false::.

]
@section{argument}
  labelWidth
Default value is 80.

@section{argument}
  labelHeight
Default value is 20. Not used if layout is 
@racketblock[\horz::.

]
@section{argument}
  layout

@racketblock[\vert:: or ]

@racketblock[\horz::. default is ]

@racketblock[\horz::.

]
@section{argument}
  gap
A link::Classes/Point::. By default, the view tries to get its parent's 
@racketblock[gap::, otherwise it defaults to ]

@racketblock[2@2::. Setting it overrides these.

]
@section{argument}
  margin
A link::Classes/Point::. This will inset the bounds occupied  by the subviews of view.

@section{discussion}
 

@racketblock[
(
w = Window.new.front;
w.view.decorator = FlowLayout(w.view.bounds);
g = EZPopUpMenu.new(
	w,
	230@22,
	"A PopUpMenu: ",
	[
		\item0 ->{|a| ("this is item 0 of " ++ a).postln},
		\item1 ->{|a| ("this is item 1 of " ++ a).postln},
		\item2 ->{|a| ("this is item 2 of " ++ a).postln},
	],
	globalAction: {|a| ("this is a global action of "++a.asString ).postln},
	initVal: 1,
	initAction: true,
	labelWidth: 120,
	labelHeight: 20,
	layout: \horz,
	gap: 2@2
);
)

// or a more simple syntax:
(
w = Window.new.front;
w.view.decorator = FlowLayout(w.view.bounds);
g = EZPopUpMenu.new(w, 200@22, "Menu: ");
g.addItem(\item0, { |a| ("this is item 0 of " ++ a).postln });
g.addItem(\item1, { |a| ("this is item 1 of " ++ a).postln });
g.addItem(\item2, { |a| ("this is item 2 of " ++ a).postln });
g.value = 0;
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
  menuBackground
An instance of link::Classes/Color::. The 
@racketblock[background:: of the menu.
]
@section{argument}
  menuStringColor
An instance of link::Classes/Color::. The 
@racketblock[stringColor:: of the menu.
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
 

@racketblock[
// try several examples together
(

// many menus
// inherits the parent's decorator gap

(
w=Window.new("oscillators", Rect(200,500,200,160)).front;
w.view.decorator = FlowLayout(w.view.bounds).gap_(2@2);
5.do{|i|
	g = EZPopUpMenu.new(w,190@22, "Oscillator % : ".format(i+1));
	g.addItem(\off, {"off". postln});
	g.addItem(\sine, {"sine". postln});
	g.addItem(\saw, {"saw". postln});
	g.addItem(\pulse, {"pulse". postln});
	g.setColors(Color.grey,Color.white);
	g.value=0;
};
w.bounds=w.bounds.moveBy(300,60);
);


// Creates its own window if parentView is nil:
(
g = EZPopUpMenu.new(nil,250@22 ," Select : ");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.setColors(Color.grey,Color.white);
g.value=0;

);

// layout vertical:
(
g = EZPopUpMenu.new(nil,200@42, " Choose",layout:\vert);
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.setColors(Color.grey,Color.white);
g.window.bounds=g.window.bounds.moveBy(300,-200);
g.value=0;
);

// No labelView created, so set the window title;
(
g = EZPopUpMenu.new(bounds:180@22); // no label
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.value=0;
g.window.name=" choose item";
g.window.bounds=g.window.bounds.moveBy(0,-200);
);
)
// insertItem;

(
g = EZPopUpMenu.new(nil,200@22, "Menu:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item4, {"this is item 4". postln});
g.value=0;
);

g.insertItem(3, \item3, {"this is item 3". postln});


// remove Item ;

(
w=Window.new.front;
w.view.decorator = FlowLayout(w.view.bounds);
g = EZPopUpMenu.new(w,200@22, "Menu:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item4, {"this is item 4". postln});
g.insertItem(3, \item3, {"this is item 3". postln});
g.value=0;
)

g. removeItemAt(0);



// replace item;
(
g = EZPopUpMenu.new(nil,200@22, "List:");
g.addItem(\item0, {"this is item 0". postln});
g.addItem(\item1, {"this is item 1". postln});
g.addItem(\item2, {"this is item 2". postln});
g.addItem(\item3, {"this is item 3". postln});
)

g.replaceItemAt(2, \item2_replaced, {"this is item 2 replaced". postln});
::
]


