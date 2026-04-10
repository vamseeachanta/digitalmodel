# Python interface: Introduction

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

|  |  |
| --- | --- |
| Note: | A detailed introduction to the Python interface to OrcaFlex, including source code, is available for [download](https://www.orcina.com/wp-content/uploads/training/An%20introduction%20to%20the%20Python%20interface%20to%20OrcaFlex.zip). |

Python is an object-orientated, dynamically typed scripting language which offers many advantages over other languages for accessing the OrcaFlex API.
Python is interpreted so it does not require a compile or link step before you run your script. The Python interpreter handles many of the programming housekeeping tasks itself, such as memory management and type casting, and variables need not be declared before they can be used.

The Python interface to OrcaFlex is a wrapper to the OrcFxAPI DLL and passes functions calls onto the C API. However, where the C API may require several separate calls, with error checks, to carry out a task the Python interface wraps these steps into a single function call. Setting object data is simplified since the Python interface handles the differences in data types, and OrcaFlex object data names themselves can be used as attribute names to Python interface objects. This further simplifies using the OrcaFlex API and makes the Python interface a good choice for productive development of pre and post-processing applications.

For example the following script adds a line to an empty model, sets its length, runs and saves the simulation, and then displays the maximum effective tension values occurring during the simulation:

import OrcFxAPI

model = OrcFxAPI.Model() # Create a new model

line = model.CreateObject(OrcFxAPI.ObjectType.Line, "MyLine") # Create a line and give it a name

line.Length[0] = 125.0 # Set the length of the first section

model.RunSimulation()

model.SaveSimulation("Test.sim")

# Extract range graph results

rangeResults = line.RangeGraph("Effective Tension")

print rangeResults.Max # display the range graph maximum values
