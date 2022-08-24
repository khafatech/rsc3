#lang scribble/manual
@(require (for-label racket))

@title{SoundFileView}
 Sound file display@section{categories}
  GUI>Views
@section{related}
  Classes/SoundFile


@section{description}


A sound file viewer with numerous options.

Zoom in and out using Shift + right-click + mouse-up/down;

Scroll using right-click + mouse-left/right.

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key


@section{INSTANCEMETHODS}
 

@section{SUBSECTION}
  Data

@section{METHOD}
  soundfile

    @section{argument}
 
        An Instance of SoundFile to display.


@section{METHOD}
  read

    Reads a section of the link::#-soundfile:: and displays it in the view. For large files, you may want to use readWithTask instead.

    The 'block' argument has no effect; the display resolution is infinite.

    @section{argument}
  startFrame
        The beginning of the section, in frames.

    @section{argument}
  frames
        The size of the section, in frames.

    @section{argument}
  block
        The block size - visual resolution of the display. An Integer of the form 2**n.

    @section{argument}
  closeFile
        If true, closes the SoundFile after reading.

    @section{argument}
  doneAction
        A Function called when the file reading has completed.


@section{METHOD}
  readFile

    Reads a section of an open instance of SoundFile, and displays it in the view. For large files, you may want to use the method readWithTask instead.

   The 'block' argument has no effect; the display resolution is infinite.

    @section{argument}
  aSoundFile
        An open instance of SoundFile.

    @section{argument}
  startFrame
        The beginning of the section, in frames.

    @section{argument}
  frames
        The size of the section, in frames.

    @section{argument}
  block
        The block size - visual resolution of the display. An Integer of the form 2**n.

    @section{argument}
  closeFile
        If true, closes the SoundFile after reading.
    @section{argument}
  doneAction
        A Function called when the file reading has completed.

@section{METHOD}
  readWithTask

    Reads a section of the link::#-soundfile:: asynchronously (in the background), updating the link::#-readProgress:: along the way. If the 
@racketblock[showProgress:: argument is ]

@racketblock[true::, a SoundFileViewProgressWindow opens to show the progress.

   The 'block' argument has no effect; the display resolution is infinite. The 'showProgress' argument has no effect; the view always displays reading progress within itself.

    ]
@section{argument}
  startFrame
        The beginning of the section, in frames.

    @section{argument}
  frames
        The size of the section, in frames.

    @section{argument}
  block
        The block size - visual resolution of the display. An Integer of the form 2**n.

    @section{argument}
  doneAction
        An optional function to be evaluated on completion.

    @section{argument}
  showProgress
        Whether to open a progress window. Defaults to 
@racketblock[true::.


]
@section{METHOD}
  readFileWithTask

    Reads a section of an open instance of SoundFile asynchronously (in the background), updating the link::#-readProgress:: along the way. If the 
@racketblock[showProgress:: argument is ]

@racketblock[true::, a SoundFileViewProgressWindow opens to show the progress.

        The 'block' argument has no effect; the display resolution is infinite.
        The 'showProgress' argument has no effect; the view always displays reading progress within itself.

    ]
@section{argument}
  soundFile
        An open instance of SoundFile.

    @section{argument}
  startFrame
        The beginning of the section, in frames.

    @section{argument}
  frames
        The size of the section, in frames.

    @section{argument}
  block
        The block size - visual resolution of the display. An Integer of the form 2**n.

    @section{argument}
  doneAction
        An optional function to be evaluated on completion.

    @section{argument}
  showProgress
        Whether to open a progress window. Defaults to 
@racketblock[true::.

]
@section{METHOD}
  data

    Gets the display data, or sets custom data instead of a sound file.

    It is not possible to get the data.

   Setting this property assumes 1 channel and sample rate of 44100 Hz. Use link::#-setData:: instead if you want more control.

    @section{argument}
 
        An Array of Floats; multiple channel data should be interleaved.

@section{METHOD}
  setData

    Sets custom display data instead of a sound file, interpreting it using specified number of channels and sample rate.


    @section{argument}
  data
        An Array of Floats; multiple channel data should be interleaved.

    @section{argument}
  block
The block size - visual resolution of the display. An Integer of the form 2**n. (since the port to QT, the 'block' argument has no effect; the display resolution is infinite).

    @section{argument}
  startFrame
        An integer.

    @section{argument}
  channels
        An integer.

    @section{argument}
  samplerate
        An integer.

@section{METHOD}
  alloc

Allocates a desired amount of display channels and frames; all frames have initial value of 0.

    @section{argument}
  frames
        An Integer.
    @section{argument}
  channels
        An Integer.
    @section{argument}
  samplerate
        An Integer.

@section{METHOD}
  set

Overwrites a range of display data with another data. This method can be used after link::#-alloc:: or link::#-setData:: has been called, but not while the view is displaying a sound file.

    @section{argument}
  offset
        The frame at which to start overwriting; an Integer.
    @section{argument}
  data
        The new data; an Array of Floats; multiple channel data should be interleaved.

@section{METHOD}
  startFrame

    The beginning of the read section of the soundfile, or 0 if link::#-alloc:: or link::#-setData:: has been used.

@section{METHOD}
  numFrames

    The total amount of frames in the view; this is unrelated to link::#-zoom#zooming:: and link::#-scroll#scrolling::.

@section{METHOD}
  readProgress

     The reading progress, updated periodically when reading a soundfile using link::#-readWithTask:: or link::#-readFileWithTask::.

@section{SUBSECTION}
  Navigation

@section{METHOD}
  viewFrames

    The amount of currently visible frames in the view.

@section{METHOD}
  zoom

    Zooms by a factor relative to current zoom.

    @section{argument}
 
        A Float.

@section{METHOD}
  zoomToFrac

    Zooms to a specific scale.

    @section{argument}
 
        A Float.

@section{METHOD}
  zoomAllOut

    Zooms to the link::#-currentSelection#current selection::.

@section{METHOD}
  zoomSelection

    Zooms to a specific selection.

    @section{argument}
 
        The index of the selection; an Integer between 0 an 63.

@section{METHOD}
  scrollPos

    The scrolling position of the view, as a fraction of the total scrolling range.

    @section{returns}
 
        A Float in the range of 0.0 to 1.0.

@section{METHOD}
  scrollTo

    Scrolls to a fraction of the total scrolling range.

    @section{argument}
 
        A Float in the range of 0.0 to 1.0.

@section{METHOD}
  scroll

    Scrolls by a fraction of the visible range.

    @section{argument}
 
        A Float.

@section{METHOD}
  scrollToStart

    Scrolls to the beginning.

@section{METHOD}
  scrollToEnd

    Scrolls to the end.


@section{SUBSECTION}
  Selection

@section{METHOD}
  selections

    All the selections.

    @section{returns}
  An array of 64 arrays of start frames and sizes: [ [ start0, size0 ] , [ start1, size1 ], ... ].

@section{METHOD}
  selection

    The selection at index.

    @section{returns}
 
        An Array of the form 
@racketblock[[start, size]::, where start and size denote frames.

]
@section{METHOD}
  setSelection

    Sets the selection at index.

    @section{argument}
  index
        An Integer between 0 an 63.

    @section{argument}
  selection
        An Array of the form 
@racketblock[[start, size]::, where start and size are Integers and denote frames.

]
@section{METHOD}
  currentSelection

    The index of the current selection

    @section{argument}
 
        An integer between 0 an 63.

@section{METHOD}
  selectionStart

    The start frame of a selection.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{returns}
 
        An Integer.

@section{METHOD}
  setSelectionStart

    Sets the start frame of a selection.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{argument}
  frame
        The starting frame of the selection, an Integer.

@section{METHOD}
  selectionSize

    The size of a selection.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{returns}
 
        An Integer.

@section{METHOD}
  setSelectionSize

    Sets the size of a selection.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{argument}
  frames
        The size in frames of the selection, an Integer.

@section{METHOD}
  selectionStartTime

    The start of a selection in seconds.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{returns}
 
        A Float.

@section{METHOD}
  selectionDuration

    The size of a selection in seconds.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{returns}
 
        A Float.

@section{METHOD}
  selectAll

    Sets a selection to span the whole data range.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.


@section{METHOD}
  selectNone

    Sets the size of a selection to 0.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

@section{METHOD}
  setSelectionColor

    Sets the color of a selection.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{argument}
  color
        A Color.

@section{METHOD}
  setEditableSelectionStart

    Sets whether the start point of a selection can be edited.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{argument}
  editable
        A Boolean.

@section{METHOD}
  setEditableSelectionSize

    Whether the end point of a selection can be edited.

    @section{argument}
  index
        The index of the selection; an Integer between 0 an 63.

    @section{argument}
  editable
        A Boolean.

@section{METHOD}
  readSelection

    @section{note}
  Not implemented ::

    Read the data within a selection.

    @section{argument}
  block
        The block size - visual resolution of the display. An Integer of the form 2**n.

    @section{argument}
  closeFile
        If true, closes the SoundFile after reading.

@section{METHOD}
  readSelectionWithTask

    @section{note}
  Not implemented ::

    Read the data within the current selection asynchronously (in the background), showing the progress in a separate window.


@section{SUBSECTION}
  Display

@section{METHOD}
  gridOn

    Whether the grid is displayed. Defaults to 
@racketblock[true::.

    ]
@section{argument}
 
        A Boolean.

@section{METHOD}
  gridResolution

    The resolution of the grid.

    @section{argument}
 
        An instance of Float.

@section{METHOD}
  gridOffset

    Sets the grid offset.

    @section{argument}
 
        An integer.

@section{METHOD}
  gridColor

    The color of the grid.

    @section{argument}
 
        A Color.

@section{METHOD}
  drawsWaveForm

    Whether the data is displayed. Defaults to 
@racketblock[true::.

    ]
@section{argument}
 
        A Boolean.

@section{METHOD}
  timeCursorOn

    Whether the time cursor is displayed. Defaults to 
@racketblock[false::.

    ]
@section{argument}
 
        A Boolean.

@section{METHOD}
  timeCursorPosition

    The position of the time cursor in frames.

    @section{argument}
 
        An Integer.

@section{METHOD}
  timeCursorColor

    The color of the time cursor.

    @section{argument}
 
        A Color.

@section{METHOD}
  elasticMode

    ???

@section{SUBSECTION}
  Actions

@section{METHOD}
  action

    The object to be evaluated whenever the user interacts with the view.

@section{METHOD}
  metaAction

    The object to be evaluated on Ctrl + click.


@section{EXAMPLES}
 

@section{SUBSECTION}
  Basic example


@racketblock[

// To zoom in/out: Shift + right-click + mouse-up/down
// To scroll: right-click + mouse-left/right
(
w = Window.new("soundfile test", Rect(200, 300, 740, 100));
a = SoundFileView.new(w, Rect(20,20, 700, 60));

f = SoundFile.new;
f.openRead(Platform.resourceDir +/+ "sounds/a11wlk01.wav");
f.inspect;

a.soundfile = f;
a.read(0, f.numFrames);
a.elasticMode = true;

a.timeCursorOn = true;
a.timeCursorColor = Color.red;
a.timeCursorPosition = 2050;
a.drawsWaveForm = true;
a.gridOn = true;
a.gridResolution = 0.2;

w.front;
)
::

]
@section{SUBSECTION}
  Step by step examples


@racketblock[
( // make a simple SoundFileView
y = Window.screenBounds.height - 120;
w = Window.new("soundfile test", Rect(200, y, 740, 100)).alwaysOnTop_(true);
w.front;
a = SoundFileView.new(w, Rect(20,20, 700, 60));

f = SoundFile.new;
f.openRead(Platform.resourceDir +/+ "sounds/a11wlk01.wav");
// f.inspect;

a.soundfile = f;            // set soundfile
a.read(0, f.numFrames);     // read in the entire file.
a.refresh;                  // refresh to display the file.
)

// To zoom in/out: Shift + right-click + mouse-up/down
// To scroll: right-click + mouse-left/right

// reading file
a.read(0, f.numFrames / 2).refresh; // read first half
a.read.refresh;                     // read entire file by default
a.read(f.numFrames / 2).refresh;    // read second half
a.read(0, -1).refresh;              // -1 also reads entire file, like buffer.

// the resolution of the view is always infinite;
// you can always zoom in until you see a single sample.

a.read(0, -1).refresh;

// for longer files, you can use:
a.readWithTask;

// zoom is relative
a.zoom(0.2).refresh;
a.zoom(2).refresh;
a.zoom(2).refresh;
a.zoomToFrac(0.5); // zoom to half file size
a.zoomAllOut;

a.gridOn = true;            // time grid, 1 second by default,
a.gridResolution = 0.2;     // or set resolution in seconds
a.gridColor = Color.cyan;   // color is changeable.
a.gridOffset_(0.1);         // not sure if this is working?

a.timeCursorOn = true;          // a settable cursor
a.timeCursorPosition = 2050;    // position is in frames.
a.timeCursorColor = Color.white;

// toggle drawing on/off
a.drawsWaveForm = false;
a.drawsWaveForm = true;

// these methods should return view properties:
a.gridOn
a.gridResolution
a.gridColor
a.timeCursorOn
a.timeCursorPosition
a.timeCursorColor

// Selections: multiple selections are supported.
// e.g. use selection 0:
a.setSelectionColor(0, Color.red);  // set...( index, value )
a.selectionStart(0);                // at index
a.setSelectionStart(0, 12345);
a.setSelectionSize(0, 12345);

a.setSelectionStart(0, 1234);
a.selectionStart(0);

// now selection 1
a.setSelectionColor(1, Color.white);
a.setSelectionStart(1, 1234).setSelectionSize(1, 1234 * 2);
a.selectionStart(1);
a.setSelectionStart(0, 12345);

// the current selection gets changed when click/dragging in view.
a.currentSelection;     // index of current selection;
a.currentSelection_(1); // switch current selection - try click/drag white now.
a.currentSelection;

a.selections.size;      // 64 selections
a.selections[0];
a.selections[1];
a.selections;

// setSelection (index, selection);
a.setSelection(0, [234, 2345]);
a.selection(1); // returns [start, size].


a.elasticMode = true;   // not sure if this is working yet?

(       // mouseUpAction
a.mouseUpAction = {
    ("mouseUp, current selection is now:"
        + a.selections[a.currentSelection]).postln;
};
)
// lock selection 0:
a.currentSelection_(0);
a.setEditableSelectionStart(0, false);
a.setEditableSelectionSize(0, false);


// unlock selection 0:
a.setEditableSelectionStart(0, true);
a.setEditableSelectionSize(0, true);

a.selectionStartTime(0);
a.selectionDuration(0);


a.setSelectionStart(0, 12345);
a.setSelectionSize(0, 12345);
a.readSelection.refresh;
a.readSelection(16).refresh;    // in higher resolution
a.read.refresh;                 // go back to entire file.


a.dataNumSamples;   // visual data have this many points
a.data.plot;
a.setData(a.data.reverse);


a.zoom(0.25);       // scrolling is normalized
a.scrollTo(0.5);    //
a.scrollTo(0.6);    //
a.scroll(12);       // scroll is in viewFrames.

a.zoom(4);

w.close;

::
]


