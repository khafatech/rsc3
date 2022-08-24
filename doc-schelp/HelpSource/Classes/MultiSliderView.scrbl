#lang scribble/manual
@(require (for-label racket))

@title{MultiSliderView}
 A view displaying an array of sliders@section{categories}
  GUI>Views

@section{description}


MultiSliderView displays a collection of values, each represented by the position of one of the sliders placed side by side.

When clicking into the view, the value of the slider under the mouse pointer will be set. Whenever the mouse is moved with a mouse button pressed, the slider currently under the mouse pointer will be adjusted.

The last slider modified is considered to be the strong::current:: one, i.e. the link::#-index:: method will return its index, and link::#-currentValue:: relates to its value.

The current slider is also considered to be the strong::selected:: one. Selection can be extended to more than one slider by modifying link::#-selectionSize::. Whenever a different slider becomes the current one, the selection size shrinks back to 1. Note that the selection will only be visually indicated if link::#-showIndex:: is 
@racketblock[true::.


]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  new


	A new MultiSliderView is created empty, without any columns. link::#-size:: or link::#-value:: has to be set in order to create some columns.

	So if you want a specific number of sliders, then it is best to specify the link::#-size:: and set link::#-elasticMode:: to 1. Then you will get a MultiSliderView which distributes link::#-size:: amount of sliders over 
@racketblock[bounds.width::, where the slider widths are at maximum link::#-indexThumbSize:: (default 12) and the link::#-gap:: is adjusted accordingly.



]
@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Data

@section{METHOD}
  size
	The amount of sliders.

	When setting -size, if the new amount is larger then the current, new sliders will be added with the value of 0. In the opposite case, the value of sliders up to the new amount will be preserved, and the rest of the sliders will be removed.

	@section{note}
  strong:: In Cocoa and SwingOSC GUIs: ::

	Changing -size after the view has been drawn or after the link::#-value:: array has been set will lead to unexpected results. Instead, you should change the link::#-value::, if you need to change the contents of the view.

	::

	@section{argument}
 
		An Integer.

@section{METHOD}
  value

	Sets the values of the sliders to those of the elements of the argument.

	@section{note}
 
	If the amount of elements in the argument does not match link::#-size::, then makes link::#-size:: match before applying the new values.
	::

	@section{argument}
 
		An array of Floats.

@section{METHOD}
  valueAction

	Sets link::#-value:: and triggers link::#-action::.

@section{METHOD}
  reference

	The reference values in relation to which the values will be visually represented. The default for each slider is 0.

	@section{argument}
 
		An array of Floats.

@section{METHOD}
  index

	The index of the current slider, i.e. the first one in the selection.

	@section{argument}
 
		An Integer.

@section{METHOD}
  selectionSize

	The amount of sliders in the selection (starting at link::#-index::).

@section{METHOD}
  currentvalue

	The value of the slider at link::#-index::

	@section{argument}
 
		A Float.



@section{SUBSECTION}
  Display

@section{METHOD}
  indexIsHorizontal

	The orientation of the view: if true, the sliders are displayed in a horizontal order, otherwise in a vertical order.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  elasticMode

	If enabled (set to 1), the sliders from link::#-startIndex:: to the last one will be distributed so as to occupy the whole area of the view. The link::#-gap:: variable will be ignored. The size of each slider in the direction of index will be maximally link::#-indexThumbSize::, or smaller in order for all the sliders to fit into the view.

	@section{argument}
 
		0 (disabled) or 1 (enabled).

@section{METHOD}
  gap

	The gap between the sliders in pixels when link::#-elasticMode:: is disabled.

	@section{argument}
 
		An Integer.

@section{METHOD}
  indexThumbSize

	The size of the sliders in the direction of index in pixels . If link::#-elasticMode:: is enabled, this will be the maximum size, but the actual size might be smaller in order for all the sliders to fit into the view.

	@section{argument}
 
		An Integer.

@section{METHOD}
  valueThumbSize

	The size of the slider handles in the direction of value in pixels (if drawn).

	@section{argument}
 
		An Integer.

@section{METHOD}
  thumbSize

	Sets both link::#-indexThumbSize:: and link::#-valueThumbSize:: to the argument.

@section{METHOD}
  startIndex

	The index of the slider displayed at the left or top edge of the view (depending on whether link::#-indexIsHorizontal:: is true or false, respectively). Sliders with lower index than this will not be visible.

	@section{argument}
 
		An Integer.


@section{SUBSECTION}
  Appearance

@section{METHOD}
  showIndex

	Whether the slider selection is visually indicated.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  drawRects

	Whether to draw the sliders.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  drawLines

	Whether to draw a line connecting the points that represent the link::#-value#values:: of the sliders, and a line connecting the points that represent the link::#-reference#references::.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  isFilled

	If true, the sliders will have their area between the link::#-reference:: and the link::#-value:: colored, and the area bounded by the lines connecting the reference and the value points will be colored as well.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  strokeColor

	The color used to draw the lines described in link::#-drawLines::.

	@section{argument}
 
		A Color.

@section{METHOD}
  fillColor

	The color used to visualize the areas described in link::#-isFilled::.

	@section{argument}
 
		A Color.

@section{METHOD}
  colors

	Sets link::#-strokeColor:: and link::#-fillColor:: to the two arguments, respectively.



@section{SUBSECTION}
  Interaction

@section{METHOD}
  editable

	Whether the values can be edited using mouse or keyboard.

	@section{argument}
 
		A Boolean.

@section{METHOD}
  readOnly

	The opposite if link::#-edi@section{table}
 .

	@section{argument}
 
		A Boolean.

@section{METHOD}
  step

	If the argument is larger than 0, makes the MultiSliderView keep the values quantized to the nearest multiple of the argument.



@section{SUBSECTION}
  Actions


@section{METHOD}
  action
	The action object evaluated whenever the user changes the value of a slider.

@section{METHOD}
  metaAction
	The action object evaluated whenever the user changes the value of a slider while the Ctrl key is pressed.

@section{METHOD}
  defaultKeyDownAction

	Implements the default effects of key presses as follows:

	@section{table}
 
	## strong::Key::   || strong::Effect::
	## up arrow        || increment -currentValue by -step
	## down arrow      || decrement -currentValue by -step
	## right arrow     || increment -index by 1
	## left arrow      || decrement -index by 1
	::



@section{SUBSECTION}
  Drag and drop

@section{METHOD}
  defaultGetDrag
	@section{returns}
 
		a) If link::#-selectionSize:: is 0, returns link::#-value::.

		b) If link::#-selectionSize:: > 1, returns an Array with the values at the indexes in the selection.

		If link::#-reference:: is not nil, returns an Array containing (a) or (b), and an Array of the corresponding reference values: 
@racketblock[ [[values], [references]] ::.

]
@section{METHOD}
  defaultCanReceiveDrag
	@section{returns}
 
		True for any drag data, but the data should either be an Array of values ( 
@racketblock[ [values] :: ), or an Array containg an Array of values and an Array of corresponding reference values ( ]

@racketblock[ [[values], [references]] :: ).

]
@section{METHOD}
  defaultReceiveDrag
	If the drag data is in one of the acceptable forms (see link::#-defaultCanReceiveDrag:: above), sets link::#-value:: (and link::#-reference::) using that data.



@section{EXAMPLES}
 

@section{subsection}
  Basic Examples


@racketblock[
(
n=20;
w = Window.new.front;
m = MultiSliderView(w,Rect(10,10,n*13+2,100)); //default thumbWidth is 13
m.value=Array.fill(n, {|v| v*0.05}); // size is set automatically when you set the value
m.action = { arg q;
    q.value.postln;
};
)
::

Looks like a candlestick graph:

]

@racketblock[
(
var size;
size = 350 / 6;
w = Window.new;
w.view.decorator = FlowLayout(w.view.bounds);
m = MultiSliderView(w, Rect(0, 0, 350, 100));
m.value_(Array.fill(size, {0.01}));
m.isFilled_(true); // width in pixels of each stick
m.indexThumbSize_(2.0); // spacing on the value axis
m.gap_(4);
w.front;
)

// rotate the above graph
(
m.bounds_(Rect(0, 0, 100, 350));
m.indexIsHorizontal_(false);
)
::


]
@section{subsection}
  Interactive Example

A walk through all the graphic options:


@racketblock[
(
n=40;

w = Window("MultiSlider Options", Rect(200, Window.screenBounds.height-550, 600, 450));
f={
    w.view.decorator = FlowLayout( w.view.bounds, 10@10, 10@2 );
    m = MultiSliderView(w,Rect(0,0,580,200)); // default thumbWidth is 13
    m.value=Array.fill(n, {|v| 0.5+((0.3*v).sin*0.25)});
    m.action = { arg q;q.value.postln; };

    StaticText(w,380@18).string_("indexThumbSize or thumbSize");
    Slider(w,580@10).action_({arg sl; m.indexThumbSize=sl.value*24}).value_(0.5);
    StaticText(w,380@18).string_("valueThumbSize");
    Slider(w,580@10).action_({arg sl; m.valueThumbSize=sl.value*24}).value_(0.5);
    StaticText(w,580@18).string_("xOffset or gap");
    Slider(w,580@10).action_({arg sl; m.xOffset=sl.value*50});
    StaticText(w,580@18).string_("startIndex");
    Slider(w,580@10).action_({arg sl; m.startIndex = sl.value *m.size};);

    CompositeView(w,580@10);//spacer
    Button(w,100@20).states_([["RESET",Color.red]])
        .action_({ w.view.removeAll; f.value; });
    h=StaticText(w,450@18).string_("").stringColor_(Color.yellow);
    Button(w,100@20).states_([["elasticMode = 0"],["elasticMode = 1",Color.white]])
        .action_({|b| m.elasticMode = b.value});
    Button(w,160@20).states_([["indexIsHorizontal = false"],["indexIsHorizontal = true",Color.white]])
        .action_({|b| m.indexIsHorizontal = b.value.booleanValue}).value_(1);
    Button(w,120@20).states_([["isFilled = false"],["isFilled = true",Color.white]])
        .action_({|b| m.isFilled = b.value.booleanValue});
    Button(w,120@20).states_([["drawRects = false"],["drawRects = true",Color.white]])
        .action_({|b| m.drawRects = b.value.booleanValue}).valueAction_(1);
    Button(w,100@20).states_([["drawLines = false"],["drawLines = true",Color.white]])
        .action_({|b| m.drawLines = b.value.booleanValue});
    Button(w,160@20).states_([["readOnly = false"],["readOnly = true",Color.white]])
        .action_({|b| m.readOnly = b.value.booleanValue});
    Button(w,120@20).states_([["showIndex = false"],["showIndex = true",Color.white]])
        .action_({|b| m.showIndex = b.value.booleanValue});
    Button(w,120@20).states_([["reference = nil"],["reference filled",Color.white],["reference random",Color.yellow]])
        .action_({|b| b.value.booleanValue.if({
            (b.value>1).if(
                {m.reference=Array.fill(n, {1.0.rand})},
                {m.reference=Array.fill(m.size, {0.5})});
                },{ q=m.value;m.reference=[]; h.string="reference can't be returned to nil presently. please hit RESET."}
            )
        });
    Button(w,180@20).states_([["fillColor = Color.rand"]]).action_({m.fillColor=Color.rand});
    Button(w,180@20).states_([["strokeColor = Color.rand"]]).action_({m.strokeColor=Color.rand});
    Button(w,180@20).states_([["background = Color.rand"]]).action_({m.background=Color.rand});

};
f.value;
w.front;

)
::


]
@section{subsection}
  Display a Sound File


@racketblock[
(
// press shift to extend the selection
// use as waveView: scrubbing over the view returns index
// if showIndex(false) the view is not refreshed (faster);
// otherwise you can make a selection with shift - drag.
var size, file, maxval, minval;
size = 640;
a = Window("test", Rect(200 , 140, 650, 150));
a.view.decorator = FlowLayout(a.view.bounds);
b = MultiSliderView(a, Rect(0, 0, size, 50));
b.readOnly_(true);
a.view.decorator.nextLine;

d = Array.new;
c = FloatArray.newClear(65493);

r = Slider( a, Rect(0, 0, size, 12));
r.action = {arg ex; b.gap = (ex.value * 4) + 1};

file = SoundFile.new;
file.openRead(Platform.resourceDir +/+ "sounds/a11wlk01.wav");
file.numFrames.postln;
file.readData(c);
// file.inspect;
file.close;
minval = 0;
maxval = 0;
f = Array.new;
d = Array.new;
c.do({arg fi, i;
    if(fi < minval, {minval = fi});
    if(fi > maxval, {maxval = fi});

    //f.postln;
    if(i % 256 == 0,{
        d = d.add((1 + maxval ) * 0.5 );
        f = f.add((1 + minval ) * 0.5 );

        minval = 0;
        maxval = 0;
    });
});

b.reference_(d); // this is used to draw the upper part of the table
b.value_(f);

r = Slider( a, Rect(0, 0, size, 12));
r.action = {arg ex; b.startIndex = ex.value *f.size};

// b.enabled_(false);
b.action = {arg xb; ("index: " ++ xb.index).postln};
b.drawLines_(true);
b.drawRects_(false);
b.isFilled_(true);
b.selectionSize_(10);
b.index_(10);
b.thumbSize_(1);
b.gap_(0);
b.colors_(Color.black, Color.blue(1.0,1.0));
b.showIndex_(true);
a.front;

)
::


]
@section{subsection}
  Use as a Sequencer


@racketblock[
(
var size;
size = 12;
s.waitForBoot({
    n={arg freq=330; SinOsc.ar(freq,0,0.2)}.play;

    w = Window("test", Rect(200 , 450, 10 + (size * 17), 10 + (size * 17)));
    w.view.decorator = FlowLayout(w.view.bounds);
    b = MultiSliderView(w, Rect(0, 0, size * 17, size * 17));
    b.value_( Array.fill(size,{|i| i/size}) );
    b.background_(Color.rand);
    b.action = {arg xb;
        n.set(\freq, 330+(1100*xb.value.at(xb.index)));
        ("index: " ++ xb.index ++" value: " ++ xb.value.at(xb.index)).postln};
    b.elasticMode_(1); // makes the squares fit evenly
    b.showIndex = true; // cursor mode
    b.readOnly=true;
    w.front;

    r = Routine({
        0.1.wait;
        30.do({ arg i;
            b.index_(i%size);

            b.doAction;
            0.1.wait;
        });

        20.do({ arg i;
            b.index_(b.size.rand);
            b.doAction;
            [0.1,0.2].choose.wait;
        });
        1.wait;
        n.free;
        {w.close}.defer;
    });
    AppClock.play(r);
});
)
::
]


