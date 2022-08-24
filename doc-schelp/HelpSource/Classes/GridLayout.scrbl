#lang scribble/manual
@(require (for-label racket))

@title{GridLayout}
 A layout that organizes views in a grid@section{categories}
  GUI>Layout
@section{related}
  Classes/HLayout, Classes/VLayout, Classes/StackLayout, Guides/GUI-Layout-Management

@section{description}


GridLayout distributes its space into a strong::grid of rows and columns::, where each item can occupy strong::one or more cells::.

You can construct the layout in two ways using link::#*rows:: and link::#*columns::. In the former constructor you pass arrays of items by rows, and in the latter by columns. Items can also be added later using link::#-add:: and link::#-addSpanning::. To remove an item, simply use link::Classes/View#-remove:: for views, or link::Classes/QObject#-destroy:: for views or layouts.

It is possible to add more than one view into the same cell. The last added view will be the top-most. However, it is most probably more convenient to use a link::Classes/StackLayout:: for that purpose.

The layout manages the grid size automatically: you can add an item at any row and cell number. When items are added or removed, the grid will re-adjust according to the last occupied row and column.

@section{subsection}
  Fine tuning

Each item can be assigned an strong::alignment:: either at layout link::#*rows#construction:: or later using link::#-setAlignment::. An item will then get at most its default size, if available (see: link::Classes/View#-sizeHint::), and will be aligned within its cell according to the specified alignment.

Each row or column can be assigned a strong::stretch factor:: using link::#-setRowStretch:: and link::#-setColumnStretch::. Rows or columns that would otherwise get equal space are then distributed according to the relative proportions of their stretch factors.

Each row or column can also be assigned a strong::minimum:: size using link::#-setMinRowHeight:: and link::#-setMinColumnWidth::, to override the size constraints imposed by the contained views.

In addition to adjusting the spacing between cells using link::Classes/Layout#-spacing:: you can control the spacing between rows and between columns separately using link::#-hSpacing:: and link::#-vSpacing::.

@section{subsection}
  Leaving empty space

You can leave any cell empty by not placing any item into it, or at link::#*rows#construction:: using 
@racketblock[nil:: instead of a view or another layout. Note though that the empty cells will always be regarded as freely stretchable and will not impose any constraints on space distribution.



]
@section{CLASSMETHODS}
 

@section{PRIVATE}
  key
@section{PRIVATE}
  qtClass

@section{METHOD}
  rows

    Creates a GridLayout and fills each row with an array of items given as arguments.

    @section{argument}
  ... rows

    Each argument is an Array of items to form a consecutive row. An item can be a strong::view::, another strong::layout::, or strong::nil:: for an empty cell.

    @section{discussion}
 

    You can make an item span more than one cell by wrapping it into an Array, followed by pairs of (\rows, number) and/or (\columns, number). You can also assign an alignment to an item by following it with a pair of (\align, alignment). \rows, \columns, and \align can be abbreviated with \r, \c, and \a, respectively. For possible alignment values see link::Reference/gui_alignments::.

    The simplified syntax for placing key-value pairs into an array comes handy (see link::Reference/Syntax-Shortcuts#Creating Arrays with key-value pairs::, and the example below).

    Example:

@racketblock[
(
w=Window().layout_( GridLayout.rows(
    [Slider2D(), Slider2D(), [Slider(), rows:2]],
    [Slider2D(), Slider2D()],
    [[Slider().orientation_(\horizontal), columns:2]]
)).front;
)
::

]
@section{METHOD}
  columns

    Creates a GridLayout and fills each column with an array of items given as arguments.

    @section{argument}
  ... cols

    Each argument is an Array of items to form a consecutive column. An item can be a strong::view::, another strong::layout::, or strong::nil:: for an empty cell.

    @section{discussion}
 

    To make an item span several cells, or assign an alignment to it, the same instructions as for link::#*rows:: apply.



@section{INSTANCEMETHODS}
 

@section{METHOD}
  add

    Adds an item into the cell at specified row and column.

    @section{argument}
  item
    The item can be a strong::view:: or another strong::layout::.

    @section{argument}
  row
    The row index.

    @section{argument}
  column
    The column index.

    @section{argument}
  align
    A symbol denoting the alignment, or nil. See link::Reference/gui_alignments:: for possible values.

@section{METHOD}
  addSpanning

    Adds an item into the grid so as to occupy several cells.

    @section{argument}
  item
    The item can be a strong::view:: or another strong::layout::.

    @section{argument}
  row
    The row index.

    @section{argument}
  column
    The column index.

    @section{argument}
  rowSpan
    The amount of cells to occupy in vertical direction.

    @section{argument}
  columnSpan
    The amount of cells to occupy in horizontal direction.

    @section{argument}
  align
    A symbol denoting the alignment, or nil. See link::Reference/gui_alignments:: for possible values.

@section{METHOD}
  hSpacing

    The spacing between columns, in Integer amount of pixels.

@section{METHOD}
  vSpacing

    The spacing between rows, in Integer amount of pixels.

@section{METHOD}
  setRowStretch

    Sets the stretch factor of a row. By default rows have a stretch factor of 0. If a larger factor is assigned to a row, rows will get their space redistributed according to the relative proportions of their factors.

    @section{argument}
  row
    The index of a row.

    @section{argument}
  factor
    An Integer.

@section{METHOD}
  setColumnStretch

    Sets the stretch factor of a column. By default columns have a stretch factor of 0. If a larger factor is assigned to a column, columns will get their space redistributed according to the relative proportions of their factors.

    @section{argument}
  column
    The index of a column.

    @section{argument}
  factor
    An Integer.

@section{METHOD}
  setAlignment

    Sets the alignment of an item managed by the layout.

    @section{argument}
  item
    A view or a layout managed by this layout, or a Point of which x denotes the column index and y the row index of an item.

    @section{argument}
  align
    A symbol denoting the alignment. See link::Reference/gui_alignments:: for possible values.

@section{METHOD}
  minRowHeight

    Gets the minimum height assigned to a row.

    @section{argument}
  row
    The index of a row.

@section{METHOD}
  setMinRowHeight

    Sets the minimum height of row.

    @section{argument}
  row
    The index of a row.

    @section{argument}
  height
    An Integer amount of pixels.

@section{METHOD}
  minColumnWidth

    Gets the minimum width assigned to a column.

    @section{argument}
  column
    The index of a column.

@section{METHOD}
  setMinColumnWidth

    Sets the minimum width of a column.

    @section{argument}
  column
    The index of a column.

    @section{argument}
  width
    An Integer amount of pixels.


