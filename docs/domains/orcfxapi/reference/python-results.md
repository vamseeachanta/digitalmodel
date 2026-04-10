# Python interface: Results

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Time Histories

To obtain a time history for a variable, call:

timehistory = myobject.TimeHistory(variableName, period, objectExtra)

`variableName` is the name of the variable from the list in the OrcaFlex select results form. Available time history result variable names for an object can be obtained using the following function:

resultVars = myobject.vars(ResultType.TimeHistory, objectExtra)

See `OrcaFlexObject.vars` for more information on this function.

The `period` parameter is a Python interface object representing the C API structure `TPeriod`. For the whole simulation, use `Period(PeriodNum.WholeSimulation)` or for part of a simulation use `SpecifiedPeriod(fromtime, totime)`, see [Python interface: Period](Pythonreference,Period.htm). If period is omitted then the default period is used. This default will depend on the model state. If the model state is `ModelState.InStaticState` then the period will be `Period(PeriodNum.StaticState)`, for `ModelState.SimulationStopped` the period will be `Period(PeriodNum.WholeSimulation)`.

`objectExtra` is a Python interface object representing the C API structure `TObjectExtra2`. The Python interface provides some helper functions for constructing these objects, see Python interface: `ObjectExtra`.

## Time history examples

Line end moment time history for `EndA` for the whole simulation:

import OrcFxAPI

model = OrcFxAPI.Model("MyFile.sim")

line = model["Line1"]

EndMoment\_TH = line.TimeHistory("End Moment", None, OrcFxAPI.oeEndA)

Bend moment at a specified arc length for a specified period:

BendMoment\_TH = line.TimeHistory("Bend Moment", OrcFxAPI.SpecifiedPeriod(-1.5, 3.4), OrcFxAPI.oeArcLength(34.5))

## Range graphs

Range graphs are obtained from line objects with the `RangeGraph` function. The call is of the form:

line = model["MyLine"]

RangeGraphResults = line.RangeGraph(variableName, period, objectExtra, arclengthRange)

The parameters `variableName`, `period` and `objectExtra` are as described for time histories above. `arclengthRange` is a Python interface object representing the C API `TArclengthRange`, this parameter is optional. The Python interface provides three helper functions for creating an `ArclengthRange` object, see Python interface: `ArclengthRange`. The `RangeGraph` function returns an object whose attributes are the desired range graph arrays. For instance to get an array of the maximum values:

max\_values = RangeGraphResults.Max

## Range graph examples

Range graph of curvature over for the whole simulation:

RangeGraphResults = line.RangeGraph("Curvature")

Bend moment range graph for a specified period:

RangeGraphResults = line.RangeGraph("Bend Moment", OrcFxAPI.SpecifiedPeriod(5.0, 7.0))
