reflex-sumtype-utilities

1. Utilities for dealing efficienty with Dynamics of a sum-type.  As long as your type, A,  is an instance of Generics.SOP.Generic and Generics.SOP.HasdatatypeInfo (both derivable from GHC.Generic), this library allows you to get events which fire only for changes to a particular constructor of A.  This is useful for building anything that will need to do rebuilding or show/hide of a widget when the updated value is from a different constructor than the old value.

2. Some extra functionality to automatically handle the widget building case.  Takes a function which can build a widget for each constructor (by building widgets for each field and then sequencing the results) and a Dynamic t (Maybe a) and returns a list of (name,event, widget) where name is the name of the constructor (as a String), an event that fires when that constructor has a new value and whatever widget your input function built for that constructor.

For example:  You can use this to build a widget for any generic sum-type which switches to whatever input it is set to, allows user input and user choosing among constructors, but only rebuilds/switches the widget if the constructor changes.  This can be a significant reduction in rebuilding.
