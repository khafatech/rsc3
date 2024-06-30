#lang scribble/manual
@(require (for-label racket))

@title{EnvelopeView}
 A configurable view with nodes and connections@section{categories}
  GUI>Views
@section{related}
  Classes/MultiSliderView, Classes/SCEnvelopeEdit

@section{description}


A view which can graphically display nodes at x/y coordinates, connection lines, cross-connections, node markers, and labels. All of the values for these are stored in arrays. While this view is typically used to make editable envelopes interfaces, it can be used to draw very complex interconnection graphs as well.

You can make the view display an link::Classes/Env:: using link::#-setEnv::. Note however that the view will work on a copy of the data of the Env object, therefore moving the nodes through the view will have no effect on the Env.

You can also define nodes with arrays of x and y values using link::#-value::, and the connections using link::#-connect::.

@section{SUBSECTION}
  Appearance

The view supports two strong::display styles::: the default one draws nodes as small dots, with labels next to them, while another style draws nodes as rounded rectangles with labels drawn inside. See link::#-style::.

A strong::label:: for each of the nodes can be set using link::#-strings:: and link::#-setString::.

@section{SUBSECTION}
  Interaction

Nodes can be selected and moved using mouse. Shift-clicking a node will add it to the selection.

You can also move selected nodes and change selection using keyboard. Pressing the arrow keys will move selected nodes (as long as link::#-step:: is larger than 0). Pressing the left or right arrow keys while holding down Alt will select previous or next node, and holding down Shift will extend selection to the left or to the right. Other GUI kits may differ.

link::#-keepHorizontalOrder:: allows you to enforce the order of nodes in horizontal direction to match their index order. In that case, node movement to the left and to the right will be restricted by the positions of their immediate neighbours. This is especially useful when EnvelopeView is used to display an link::Classes/Env::.

link::#-elasticSelection:: determines whether moving multiple nodes will be blocked altogether if any of the nodes meet an obstacle (the view bounds or a neighbour node), or only those individual nodes will be blocked.

Node selection can also be changed programmatically using link::#-index::, link::#-selectIndex::, and link::#-deselectIndex::. The link::#-index#current:: node can be moved programmatically using link::#-x:: and link::#-y::.

@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Data

@section{METHOD}
  setEnv
	Sets an link::Classes/Env:: to be displayed by the view. The view will extract data from the Env object to display (times, values and curve types).

	Any nodes existent prior to calling this method will be removed.

	@section{argument}
 
		An Env.


@section{METHOD}
  value
	
@racketblock[value:: retrieves the node positions, returning an array in the format of the Argument below. ]

@racketblock[value_(anArray):: Sets the positions of the nodes, creating them if not existent.

	If there were already existent nodes and their amount is different than the amount of x/y pairs in the argument, nodes will be added or removed (in order of creation reversed) until the amounts match, and then the new values will be applied.

	]
@section{argument}
 
		An Array containing two other Arrays, of which one contains the horizontal and the other the vertical position of the new nodes. The values must be between 0 and 1. For example: 
@racketblock[ [[ x1, x2, x3, ... ], [ y1, y2, y3, ... ]] ::

]
@section{METHOD}
  valueAction
	Sets link::#-value:: to the argument and triggers the link::#-action::.

@section{METHOD}
  x
	The horizontal position of the emphasis::current:: node.

	@section{argument}
 
		A Float between 0 and 1.

@section{METHOD}
  y
	The vertical position of the emphasis::current:: node.

	@section{argument}
 
		A Float between 0 and 1.

@section{METHOD}
  currentvalue
	Synonym for link::#-y::.

@section{METHOD}
  curves
	The shapes of connections between nodes. See below for the valid objects that describe a shape.

	If a single shape is given, it will be applied to all the existing nodes. If an Array of shapes is given, each of its elements will be applied to an existing node, in order of index.

	A connection curve shape applied to a node will determine the shape of the connections originating at that node. If no connections have been created using link::#-connect::, the origin node of a connection is the one with lower index. If there are such connections however, their origin is the node that was passed as the first argument to link::#-connect::.

	@section{argument}
 
		The valid objects to describe a shape are listed in link::Classes/Env#*new::. The argument can be either a single, or an Array of those values.

@section{METHOD}
  strings
	The labels of the nodes.

	@section{note}
  In order for the labels to be visible, you might need to ensure that the link::#-strokeColor:: contrasts the link::#-fillColor:: (depending on how the view draws the nodes and the labels).
	::

	@section{argument}
 
		An Array of Strings.

@section{METHOD}
  setString
	Sets the label of the node at the given index.

	@section{note}
  In order for the label to be visible, you might need to ensure that the link::#-strokeColor:: contrasts the link::#-fillColor:: (depending on how the view draws the nodes and the labels).::

	@section{argument}
 index
	the index of the node.

	@section{argument}
 string
		A String.

@section{METHOD}
  setFillColor
	Sets the color used to draw the inside of the node at the given index.

	@section{argument}
  index
		An Integer.
	@section{argument}
  color
		A Color.

@section{METHOD}
  setThumbWidth
	Sets the width of the node at the given index.

    @section{NOTE}
  For compatibility with existing code, this will set the link::#-style:: to strong::'rects'::. ::

	@section{argument}
  index
		An Integer.
	@section{argument}
  width
		An Integer.

@section{METHOD}
  setThumbHeight
	Sets the height of the node at the given index.

    @section{NOTE}
  For compatibility with existing code, this will set the link::#-style:: to strong::'rects'::. ::

	@section{argument}
  index
		An Integer.
	@section{argument}
  height
		An Integer.

@section{METHOD}
  setThumbSize
	Sets both width and height of the node at the given index to 
@racketblock[size::.

	]
@section{argument}
  index
		An Integer.
	@section{argument}
  size
		An Integer.


@section{METHOD}
  connect
	Removes any connections created when the link::#-value:: was set, and then creates new ones from the node at index given in the first argument to each of the nodes at indexes given in the second argument.

	@section{argument}
  source
		An Integer - the index of the node to become one end of all the new connections.
	@section{argument}
  targets
		An Array of Integers - indexes of nodes, each to become the second end to a new connection created.

@section{METHOD}
  selection
	Returns an array of indexes of all selected nodes.


@section{SUBSECTION}
  Appearance

@section{METHOD}
  style


    One of the following drawing styles:

    @section{list}
 

    ## strong::'dots':: - nodes are drawn as small dots within a larger circle indicating the area of mouse sensitivity. Labels are drawn next to the dots (see link::#-setString::). This style always draws nodes with emphasis::equal width and height::, and will use the smaller of the node's sizes, if different (it never draws ellipses).

    ## strong::'rects':: - nodes are drawn as rounded rectangles. Labels are drawn within the bounds of the rectangles.
    ::

    @section{NOTE}
  For compatibility with existing code, calling any of link::#-thumbWidth::, link::#-thumbHeight::, link::#-setThumbWidth::, or link::#-setThumbHeight:: will automatically switch style to strong::'rects'::. You can still set a different style afterwards.
    ::

    @section{argument}
 
        One of the symbols: 
@racketblock[\dots:: or ]

@racketblock[\rects::. Alternatively, an integer 0 or 1, for each style respectively.

    ]
@section{returns}
  An integer 0 or 1.

@section{METHOD}
  drawLines
	Whether to draw the connections between the nodes.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  drawRects
	Whether to draw the nodes

	@section{argument}
 
		A Boolean.


@section{METHOD}
  gridOn
	Whether to draw the grid.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  grid
	The resolution of the grid.

	@section{argument}
 
		A Point of which x and y correspond to grid spacing on the horizontal and the vertical axis, respectively. If one of the two is 0, the grid on that axis will not be drawn.

@section{METHOD}
  thumbWidth
    Sets the width of all nodes.

    @section{NOTE}
  For compatibility with existing code, this will set the link::#-style:: to strong::'rects'::. ::

	@section{argument}
 
		An Integer.

@section{METHOD}
  thumbHeight
    Sets the height of all nodes.

    @section{NOTE}
  For compatibility with existing code, this will set the link::#-style:: to strong::'rects'::. ::

	@section{argument}
 
		An Integer.

@section{METHOD}
  thumbSize
	Sets both link::#-thumbWidth:: and link::#-thumbHeight:: to the argument.

@section{METHOD}
  strokeColor
	The color used to draw the connections and the node labels.

	@section{argument}
 
		A Color.

@section{METHOD}
  fillColor
	The default color used to draw the nodes. If the color of a specific node has been set using link::#-setFillColor::, it will take precedence.

	@section{argument}
 
		A Color.

@section{METHOD}
  selectionColor
	The color of a node when it is selected.

	@section{argument}
 
		A Color.


@section{METHOD}
  gridColor
	The color of the grid.

	@section{argument}
 
		A Color.

@section{METHOD}
  colors
	Sets the link::#-strokeColor:: and the link::#-fillColor:: to the arguments, respectively.



@section{SUBSECTION}
  Interaction

@section{METHOD}
  index
	The index of the emphasis::current:: node, i.e. the node affected by link::#-x:: and link::#-y:: methods.

    This is the selected node with lowest index, or -1 if no selection.

	@section{argument}
 
		An Integer.

@section{METHOD}
  lastIndex
    The last node selected, regardless of the current state of selection.


@section{METHOD}
  selectIndex
	Selects the node at given index and makes it the current one, i.e. link::#-currentvalue:: will relate to that node. As a special case, if the argument is -1 all nodes will be deselected.

	@section{argument}
 
		An Integer.

@section{METHOD}
  deselectIndex
	Deselects the node at given index.

	@section{note}
  Not available in strong:: Cocoa GUI ::. ::

	@section{argument}
 
		An Integer.

@section{METHOD}
  editable
	Whether any node is editable.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  setEditable
	Sets whether the node at given index is editable. Regardless of this, no node will be editable unless link::#-edi@section{table}
  is 
@racketblock[true::.

	]
@section{argument}
  index
		An Integer.

	@section{argument}
  flag
		A Boolean.

@section{METHOD}
  step
	Makes the nodes snap (i.e. quantized) to the nearest multiple of the argument. Unless this is larger than 0, nodes will not be movable using keyboard.

	@section{argument}
 
		A Float.

@section{METHOD}
  keepHorizontalOrder

	Whether the position of nodes on the horizontal axis shall be restricted by their immediate neighbours (in order of their index).

	Setting this to 
@racketblock[true:: will immediately modify the positions of existing nodes to match the order.

	]
@section{argument}
 
		A Boolean.

@section{METHOD}
  elasticSelection

	Whether the relative positions of nodes within the selection can change when the selection is moved using mouse or keyboard, in order to adapt to obstacles (the view bounds or, in case link::#-keepHorizontalOrder:: is 
@racketblock[true::, a neighbour node).

	If this is ]

@racketblock[false::, movement of multiple nodes will be blocked altogether when an obstacles is met, otherwise only the individual nodes will be blocked at their obstacles.

	]
@section{argument}
 
		A Boolean.


@section{SUBSECTION}
  Actions

@section{METHOD}
  action
	The action object evaluated whenever the user moves a node.

@section{METHOD}
  metaAction
	The action object evaluated whenever the user moves a node while the Ctrl key is pressed.

@section{METHOD}
  defaultKeyDownAction

    Implements the default behavior on key presses.

    The default behavior is defined in the C++ implementation of the view instead of this method. See link::Classes/View#Key and mouse event processing:: for explanation of how to override the behavior.




@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
	@section{returns}
 
		The link::#-value::.

@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		True for any drag data, but the data should be in the same format as link::#-value::.

@section{METHOD}
  defaultReceiveDrag
	If the drag data is of the acceptable form (see link::#-defaultCanReceiveDrag:: above), sets link::#-value:: using that data.



@section{EXAMPLES}
 

Use as envelope view

@racketblock[
(
// use shift-click to keep a node selected
w = Window("envelope", Rect(150 , Window.screenBounds.height - 250, 250, 100)).front;
w.view.decorator = FlowLayout(w.view.bounds);

b = EnvelopeView(w, Rect(0, 0, 230, 80))
	.drawLines_(true)
	.selectionColor_(Color.red)
	.drawRects_(true)
	.resize_(5)
	.step_(0.05)
	.action_({arg b; [b.index, b.value].postln})
	.thumbSize_(5)
	.value_([[0.0, 0.1, 0.5, 1.0],[0.1,1.0,0.8,0.0]]);
w.front;
)

// show grid
b.grid = Point(0.2, 0.2);
b.gridOn_(true);

// show Env
b.setEnv(Env.asr(0.5,1, 0.2));

// make the first point unmoveable
(
b.setEditable(0,false);
)
::

Use shift click to select/unselect the points
]

@racketblock[
(
w = Window("envelope", Rect(150 , Window.screenBounds.height - 250, 400, 150)).front;
w.view.decorator = FlowLayout(w.view.bounds);

b = EnvelopeView(w, Rect(0, 0, 350, 100))
	.thumbSize_(5)
	.drawLines_(true)
	.fillColor_(Color.green)
	.selectionColor_(Color.red)
	.drawRects_(true)
	.value_([(0.0, 0.1 .. 1.0), (0.0, 0.1 .. 1.0)])
	.setEditable(0,false);
)

(
r = Routine({
	var j = 0;
	20.do({ arg i;
		b.selectIndex((b.size - 1).rand.abs);
		0.1.wait;
		b.x_(1.0.rand.abs);
		b.y_(1.0.rand.abs);
	});
	b.selectIndex(-1);
});
AppClock.play(r);
)
::

Show boxes with a string in it:
]

@racketblock[
(
a = Window("text-boxes", Rect(200 , 450, 450, 450));
a.view.decorator = FlowLayout(a.view.bounds);

b = EnvelopeView(a, Rect(0, 0, 440, 440))
	.thumbWidth_(60.0)
	.thumbHeight_(15.0)
	.drawLines_(true)
	.drawRects_(true)
	.selectionColor_(Color.red)
	.value_([[0.1, 0.4, 0.5, 0.3], [0.1, 0.2, 0.9, 0.7]]);
4.do({arg i;
	b.setString(i, ["this", "is", "so much", "fun"].at(i));
	b.setFillColor(i,[Color.yellow, Color.white, Color.green].choose);
});
a.front;
)

(
b.connect(3, [2.0,0.0,1.0]); // the text objects can be connected
b.connect(0,[2.0,3.0,1.0]);
)
::
]

