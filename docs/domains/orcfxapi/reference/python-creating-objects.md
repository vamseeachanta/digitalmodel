# Python interface: Creating objects

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

To create an empty OrcaFlex model, create an instance of the class `Model` with no parameters:

import OrcFxAPI

model = OrcFxAPI.Model()

This empty model already contains some objects created for you. Conveniently this includes the environment and the general model objects, these are available as properties of the model object:

general = model.general

environment = model.environment

If an existing model is opened then the model objects within can be accessed by name:

line = model["MyLineName"]

linetype = model["LineType1"]

A list of all the objects in a model is returned by calling:

modelobjects = model.objects

These objects are instances of `OrcaFlexObject` or one of its [subclasses](Pythonreference,OrcaFlexObject.htm), according to the object type requested.

To add more model objects to your model you need to call the `CreateObject` method which itself calls the C API `C_CreateObject` function. The Python interface method also allows you to set an object name in the same method call. For example, Python code to create a line type, line and a vessel is shown below:

linetype = model.CreateObject(OrcFxAPI.ObjectType.LineType, "Line type1")

line = model.CreateObject(OrcFxAPI.ObjectType.Line, "Line1")

vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel, "FPSO1")
