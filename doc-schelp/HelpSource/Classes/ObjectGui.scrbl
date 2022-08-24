#lang scribble/manual
@(require (for-label racket))

@title{ObjectGui}
 Controller class for MVC architecture, superclass for all XYZGui classes@section{related}
  Reference/gui
@section{categories}
  GUI

@section{description}

In the MVC architecture this is the Controller, which creates Views for manipulating the properties of your Model, and receives messages from the View and enacts the changes on the Model.

Each class specifies its Gui class via the guiClass method.

The default guiClass for an Object is ObjectGui.  So if a class does not implement guiClass at all, then at least there is a default ObjectGui that will display the name.

Many subclasses override the guiClass method to specify a different class, one that subclasses ObjectGui.

It is the simplest display, it is just the object asString.

The .gui method is called on your model:


@racketblock[
// standard usage
theModel.gui( parent, bounds )

// this results in these steps:
guiClass = theModel.guiClass;
gui = guiClass.new( theModel );
gui.gui( parent, bounds );
::

In addition to those steps the model/gui dependencies are managed, defaults (nil parent or nil bounds) are managed and when the window or parent view is closed then dependencies are safely managed.


]
@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a gui controller object but does not yet create the views / window.  Call .gui to create the views.

@section{argument}
  model
The model is the object that the GUI is a graphical interface for.

@section{returns}
  the ObjectGui or subclass object


@section{INSTANCEMETHODS}
 

@section{METHOD}
  guiBody
When implementing subclasses this is the primary and often the only method that needs to be implemented.  The ObjectGui parent class takes care of setting up all windows and dependencies and then the guiBody method adds views to the layout.  It is normal to declare instance variables in the ObjectGui subclass that are used to store the widgets so they can be updated later.

@section{argument}
  layout
Usually a FlowView : a parent view with a FlowLayout to add views to.

@section{argument}
  bounds
nil or a Rect.

@section{argument}
  ... args
More args may be passed here.

@section{returns}
  this

@section{METHOD}
  update
When the model is changed and the .changed method is called then .update is called on all dependants including this gui object.  Update the views you have placed in the guiBody.

@section{argument}
  theChanged
The model.  Within your gui class the model is already in the instance variable 'model'.

@section{argument}
  theChanger
Depends on the conventions of how .changed was called.  If an object called someModel.changed(this) then it is supplying itself as the changer and will be passed through here.  Sometimes a flag is used: someModel.changed('points') and the gui may know of and participate in that convention.  Sometimes no changer is passed in.


@section{METHOD}
  gui
The standard method to create a view / window.  Usually you call yourModel.gui(parent,bounds) and this creates the gui (of the related ObjectGui subclass) and then theObjectGui.gui(parent,bounds) is called, forwarding the arguments.  So this method is what receives the forwarded (parent,bounds) from the initial call to theModel.gui(parent,bounds). Usually you do not call this manually and would avoid reimplementing it.

@section{argument}
  parent
parent view : nil, a Window, a FlowView or any other usable container view.

@section{argument}
  bounds
nil or a Rect.  The default of nil will offer the entire bounds to the guiBody method and then shrink the view size afterwards to the exact size of the contents that were actually added.

@section{argument}
   ... args
More args may be passed into theModel.gui(parent,bounds,anArg,moreArg) and will be forwarded to guiBody.

@section{returns}
  this

@section{METHOD}
  guify
This converts a supplied parent and bounds into a usable parent container view on a window.  It creates a window if needed.

@section{argument}
  parent
parent view or nil

@section{argument}
  bounds
desired bounds or nil

@section{argument}
  title
window title IF a new window is being created.  if there is a parent view then title is ignored.

@section{returns}
  converted parent


@section{METHOD}
  model
set a new model. This allows to use a single gui and dynamically swap in a different model of the same class.  The old model releases the gui as a dependant and the new model adds the gui as a dependent.  Then the views are updated.

@section{argument}
  newModel
The new object

@section{returns}
  (returnvalue)

@section{METHOD}
  dragSource
The default implementation of writeName places a nameplate on the gui that is draggable.  This method is an accessor for that dragSource object.

@section{returns}
  a GUi.dragSource

@section{METHOD}
  viewDidClose
This is called when the parent view closes. It releases dependants.

@section{returns}
  this

@section{METHOD}
  background
Each ObjectGui subclass may implement a default background color.

@section{returns}
  a color

@section{METHOD}
  writeName
ObjectGui by default makes a nameplate with the name of the model.  Implement this in subclasses if a different name style or no nameplate is desired.  Note: this may change in the near future.  So many classes override this to shut off the name.

@section{argument}
  layout
The layout to place the nameplate on.  Probably the same as is being passed to guiBody

@section{METHOD}
  prWriteName
The default write name implementation.  You could call this from a subclass if you are primarily implementing writeName to customize what name is shown or to add other items to that area.

@section{argument}
  layout
the layout

@section{argument}
  name
the string to display


@section{EXAMPLES}
 


@racketblock[

YourSimpleGuiClass : ObjectGui {

	guiBody { arg layout;
	
		// we refer to the model and
		// access its variable howFast.
		// if its a simple number, it will display
		// using the default ObjectGui class, which
		// will simply show its value as a string.
		model.howFast.gui(layout);
	}
}


// more complex
YourGuiClass : ObjectGui {
	
	var numberEditor;
	
	//for example
	guiBody { arg layout;
		var r;
		// the object you are making a gui for is referred to as the model
		
		// display some param on screen.
		// here we assume that someParam is something that
		//  has a suitable gui class
		// implemented, or that the default ObjectGui is sufficient.
		model.someParam.gui(layout);
		
		// using non 'gui' objects
		r = layout.layRight(300,300); // allocate yourself some space
		Button(layout.win,r)
			.action_({ arg butt;
				model.goApeShit;
			});
		
		// note: NumberEditor is a cruciallib class
		// which is itself a model (its an editor of a value)
		// and has its own gui class that creates and manages the NumberBox view
		numberEditor = NumberEditor(model.howFast,[0,100])
			.action_({ arg val; 
				model.howFast = val; 
				model.changed(this); 
				// tell the model that this gui changed it
			});
		numberEditor.gui(layout);
	}
	
	// your gui object will have update called any time the .changed message
	// is sent to your model
	update { arg changed,changer;
	
		if(changer !== this,{ 
			/* if it is this gui object that changed the value
				using the numberEditor, then we already have a correct
				display and don't need to waste cpu to update it.
				if anyone else changed anything about the model,
				we will update ourselves here.
			*/
			numberEditor.value = model.howFast;
			/*
				note that 
					numberEditor.value = model.howFast;
				is passive, and does not fire the numberEditor's action.	

				numberEditor.activeValue = model.howFast
				would fire the action as well, resulting in a loop that would
				probably crash your machine.
			*/
		}
	}

}
::
]


