# Python reference: OrcaFlexObject

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

The class `OrcaFlexObject` is the Python interface representation of an OrcaFlex model object such as a vessel, line type or buoy.

Some objects are represented by subclasses:

* Lines are represented by `OrcaFlexLineObject`.
* Line types and bend stiffness variable data items are represented by `OrcaFlexWizardObject`.
* Vessels are represented by `OrcaFlexVesselObject`.
* Turbines are represented by `OrcaFlexTurbineObject`.

## Construction

A new `OrcaFlexObject` is normally created from a `Model` object like this:

buoy = model["Buoy1"] # Existing 6D Buoy object

linetype = model.CreateObject(ObjectType.LineType, "linetype2") # Create a new line type object called "linetype2".

See the `CreateObject` method of `Model`.

## Attributes and methods

### modelHandle

Returns the handle of the parent model, see `Model`.

### handle

Returns the handle of this object instance as returned by the C API, see `C_CreateObject`.

### type

Returns a numeric value which identifies the OrcaFlex model object type, see `TObjectInfo`.

### typeName

This is a property which returns the name of the object type of this model object instance. Calls `C_GetObjectTypeName`.

### groupFirstChild

This property returns the first child of the object in the model's group structure.

### groupNextSibling

This property returns the next sibling of the object in the model's group structure. If there is no next sibling object because this is the last object at this level, `None` is returned.

### groupPrevSibling

This property returns the previous sibling of the object in the model's group structure. If there is no previous sibling object because this is the first object at this level, `None` is returned.

### groupParent

This property gets and sets the parent of the object in the model's group structure.

### GroupChildren

GroupChildren(recurse=True)

A generator that iterates over the object's children. The **recurse** parameter determines whether or not the generator recurses into the children of children.

### GroupMoveAfter

GroupMoveAfter(target)

Moves an object in the model's group structure to be at the same level, and immediately after, the specified `target` object.

### GroupMoveBefore

GroupMoveBefore(target)

Moves an object in the model's group structure to be at the same level, and immediately before, the specified `target` object.

### DataNameValid

DataNameValid(dataName)

`dataName` is the name of a data item in OrcaFlex, the method returns `True` if this is valid for this model object. The method calls `C_GetDataType` to determine validity.

|  |  |
| --- | --- |
| Note: | The name of a data item is obtained from the OrcaFlex data form by right clicking on the data item and clicking **data names**. |

### DataNameRequiresIndex

DataNameRequiresIndex(dataName)

`dataName` is the name of a data item for this model object. The method returns `True` if this data item is indexed data.

### DataType

DataType(dataName)

`dataName` is a valid data item name for this model object. Determines what type the data item is. The return value is one of the following: `DataType.Double`, `DataType.Integer`, `DataType.IntegerIndex`, `DataType.Boolean`, `DataType.String` or `DataType.Variable`. The method is implemented by calling `C_GetDataType`.

### VariableDataType

VariableDataType(dataName, index)

`dataName` is a valid data item name for this model object, `index` is a zero-based index value for the data item (non-indexed data items ignore this value). Determines whether a variable data item is currently a constant value or is currently variable. This method can only be called for variable data items. The return value is `DataType.Double` to indicate a constant value or `DataType.String` to indicate a variable value. The method is implemented by calling `C_GetVariableDataType`.

### GetData

GetData(dataNames, index)

Returns the values of the data item(s) specified by the data item name(s). `dataNames` is a valid data item name or list of data item names for this model object, `index` is a zero-based index value for the data item (non-indexed data items ignore this value). If `dataNames` contains a string, then a single value is returned; if `dataNames` contains a tuple or list of strings, then a list of values is returned.

The method first calls `C_GetDataType` and depending on the data type value returned then calls one of `C_GetVariableDataType`, `C_GetDataDouble`, `C_GetDataInteger`, or `C_GetDataString`.

For a single data item, the Python interface enables an alternative, more convenient, form which uses a `dataNames` parameter as a property name and calls `GetData` in the backgound. The examples below get the End A Azimuth and the Length of the second section of a line:

azimuth = line.EndAAzimuth # non-indexed data

sectionLength = line.Length[1] # indexed data, zero-based

### SetData

SetData(dataNames, index, values)

Sets the values of the data item(s) specified by the data item name(s). `dataNames` is a valid data item name or list of data item names for this model object, `index` is a zero-based index value for the data item (non-indexed data items ignore this value) and `values` is the data to be assigned to the data item(s) (a single value of the appropriate data type when assigning to a single data item; a tuple or list of the appropriate length when assigning to multiple data items). The method first calls `C_GetDataType` to determine the expected data type and based on the data type value returned then calls one of `C_SetDataDouble`, `C_SetDataInteger`, or `C_SetDataString`.

For a single data item, the Python interface enables an alternative and more convenient way to set data, using a `dataNames` parameter as a property name. The following examples set the End B Connection of a line and the duration of simulation stage 3:

line.EndBConnection = "Anchored" # non-indexed data

model.general.StageDuration[3] = 23.6 # indexed data (note Stage 0 is the Build-up)

### dataChange

The `dataChange` method on an OrcaFlex object provides a Python context manager whose usage is to suppress model updates during setting of object data. This can sometimes provide improved speed of data entry into an OrcaFlex model. Our example here demonstrates the use of the context manager with a relatively small set of uniform depths entered into a seabed profile.

import numpy

import OrcFxAPI



model = OrcFxAPI.Model()

env = model.environment

with env.dataChange():

env.SeabedType = "Profile"

env.SeabedProfileDistanceFromSeabedOrigin = numpy.linspace(0, 10000, 10001)

env.SeabedProfileDepth = numpy.fill(10001, 100)

### IsDefault, HasChanged, IsMarkedAsChanged, SetToDefault, SetToOriginalValue, MarkAsChanged

These functions are used to query and modify the state of data specified by `dataName` and `index`.

`IsDefault` and `SetToDefault` are available for all model types. The other functions are only available for variation model and restart analysis model types, for objects whose data changes are being tracked. In particular, newly added objects in a child model do not have data changes tracked.

IsDefault(dataName, index)

Returns a boolean indicating whether the current value of the data is equal to the default value.

HasChanged(dataName, index)

Returns a boolean indicating whether the current value has changed from the value in the parent model or has been explicitly marked as changed.

IsMarkedAsChanged(dataName, index)

Returns a boolean indicating whether the current value has been explicitly marked as changed.

SetToDefault(dataName, index)

Set the data to its default value.

SetToOriginalValue(dataName, index)

Set the data to the value in the parent model.

MarkAsChanged(dataName, index, value)

Determines whether or not the data is explicitly marked as changed, as specified by **value**, which should be a `bool`.

### GetDataRowCount

GetDataRowCount(indexedDataName)

This function returns the number of rows in the table of the indexed data item specified by `indexedDataName`. See the C API documentation for `C_GetDataRowCount`. The Python interface enables an alternative form of this function call by treating the indexed data item `indexedDataName` as a Python object and calling the `rowCount` property of this object, for example:

numOfRows = line.Length.rowCount

### SetDataRowCount

SetDataRowCount(indexedDataName, rowCount)

This function sets the number of rows in the table of the indexed data item specified by `indexedDataName`. See the C API documentation `C_SetDataRowCount`. The Python interface enables an alternative form of this function call by treating the indexed data item `indexedDataName` as a Python object and calling the `rowCount` property of this object, for example:

line.Length.rowCount = 10

### InsertDataRow

InsertDataRow(indexedDataName, index)

This function inserts a new data row into the table of the indexed data item specified by `indexedDataName`. The row is inserted before the row `index`. If `index` is equal to the row count for that data item then the new row is added at the end of the table. See the C API documentation for `C_InsertDataRow`.

The Python interface enables an alternative form of this function call by treating the indexed data item `indexedDataName` as a Python object and calling the `InsertRow` function on this object, for example inserting a new line section before section 4:

line.Length.InsertRow(3) # zero-based indexing

In the following example the `rowCount` property of an indexed data item is used to add a row to the end of a curved plate profile table:

numOfRows = curvedPlate.ProfileDiameter.rowCount

curvedPlate.ProfileDiameter.InsertRow(numOfRows)

### DeleteDataRow

DeleteDataRow(indexedDataName, index)

This function deletes the row, `index`, from the table of an indexed data item, `indexedDataName`. See the C API documentation `C_DeleteDataRow`. The Python interface enables an alternative form of this function call by treating the indexed data item `indexedDataName` as a Python object and calling the `DeleteRow` function on this object, for example deleting the third section from a line:

line.Length.DeleteRow(2) # zero-based indexing

### tags

A mapping object (i.e. supporting the same interface as a Python `dict` object) representing the object's tags. If the object does not support tags then `None` is returned.

As well as implementing the mapping interface, tags can be accessed using attribute syntax as an extra convenience.

lineType = model["Line type1"]

# mapping syntax

lineType.tags["foo"] = "bar"

assert lineType.tags["foo"] == "bar"

del lineType.tags["foo"]

assert not "foo" in lineType.tags

# attribute syntax

lineType.tags.foo = "bar"

assert lineType.tags.foo == "bar"

del lineType.tags.foo

assert not "foo" in lineType.tags

### AssignWireFrameFromPanelMesh

AssignWireFrameFromPanelMesh(filename, format, symmetry, importOrigin=None, wireFrameType=WireFrameType.Panels, scale=1, bodyNumber=1, importDryPanels=True)

Imports the panel mesh file named `filename`, and then set the object's wire frame drawing data based on the imported panel mesh.

The **format** parameter specifies the format of the panel mesh file, and can be one of the following:

* `PanelMeshFileFormat.WamitGdf`
* `PanelMeshFileFormat.WamitFdf`
* `PanelMeshFileFormat.WamitCsf`
* `PanelMeshFileFormat.NemohDat`
* `PanelMeshFileFormat.HydrostarHst`
* `PanelMeshFileFormat.AqwaDat`
* `PanelMeshFileFormat.SesamFem`
* `PanelMeshFileFormat.GmshMsh`
* `PanelMeshFileFormat.WavefrontObj`

The `symmetry` parameter must be one of `PanelMeshSymmetry.none`, `PanelMeshSymmetry.XZ`, `PanelMeshSymmetry.YZ` or `PanelMeshSymmetry.XZYZ`. When symmetry is specified, the panel mesh should only contain panels on one side of the symmetry plane (`PanelMeshSymmetry.XZ`, `PanelMeshSymmetry.YZ`) or in one quadrant (`PanelMeshSymmetry.XZYZ`).

The `importOrigin` defines the location of the mesh file body origin, specified relative to the object body origin. If `None` is passed then the import origin is assumed to be zero.

The `wireFrameType` determines whether the mesh is specified by edges or panels. It must be one of `WireFrameType.Edges` or `WireFrameType.Panels`.

The `scale` parameter is used to scale the imported vertices. This is useful when you wish to convert between different length units. Pass 1 to import the mesh without scaling.

The `bodyNumber` parameter is used to specify the body to import, in the case where the mesh file contains multiple bodies.

The `importDryPanels` parameter determines whether or not all panels will be imported (that is both wet diffracting panels and dry panels), or just the panels that are specified as being wet.

### CreateClone

clone = source.CreateClone(name=None, model=None)

This function creates a new object that is a clone of another source object. The newly created cloned object will have identical data to the source object.

The `name` parameter is an optional text value required for a non-default object name. If the parameter is omitted then OrcaFlex creates a unique default name. The optional `model` parameter allows you to specify which model the newly created object is created in. If this parameter is omitted then the new object is created in the same model as the source object.

This function is implemented using `C_CreateClone2`.

If you need to clone multiple objects, or browser groups, use the model function `CreateClones` which also clones any dependencies such as type objects.

### SampleTimes

SampleTimes(period=None)

Returns an array of the sample times falling within the specified period, where `period` is a [Period](Pythonreference,Period.htm) object. If `period` is omitted then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm).

If the model uses either the implicit or explicit time domain dynamics solution methods, this function returns the sample times that fall within the simulation period defined by `period`. It is implemented with calls to the C API functions `C_GetNumOfSamples` and `C_GetSampleTimes`.

If the model uses the frequency domain dynamics solution method, this function returns the sample times for a synthesised time history, over the specified period, with a sample interval specified by the model data `FrequencyDomainSampleInterval`. It is implemented with calls to the C API functions `C_GetFrequencyDomainTimeHistorySampleCount` and `C_GetFrequencyDomainTimeHistorySampleTimes`.

### SampleCount

SampleCount(period=None)

Returns the number of samples in the specified period. The `period` argument is interpreted in the same way as by the `SampleTimes` method.

### SampleTimesCollated

SampleTimesCollated(period=None, restartModels=None)

Returns an array of the sample times falling within the specified period, collated for the specified restart models. If a `progressHandler` has been set, it will be called during this operation.

If `period` is omitted then all sample times are returned. For a [specified period](Pythonreference,Period.htm), a value of `OrcinaDefaultReal()` for `FromTime` or `ToTime` means the first sample of the first model or the last sample of the last model, respectively.

The `restartModels` argument is an iterable of integer indices specifying which models are to be included. You can use `restartFileNames` to obtain the file names of the models in the restart chain, and also the length of the restart chain. If `restartModels` is omitted, then all models in the restart chain are included.

### vars, varDetails

vars(resultType=ResultType.TimeHistory, objectExtra=None)

varDetails(resultType=ResultType.TimeHistory, objectExtra=None)

`vars` returns a list of result variable names. The `resultType` parameter is the result type required from the select results form in OrcaFlex: `ResultType.TimeHistory`, `ResultType.RangeGraph`, `ResultType.LinkedStatistics` or `ResultType.FrequencyDomain`. If omitted the default is `ResultType.TimeHistory`. `objectExtra` is an `ObjectExtra` object that is only required in specific cases, and is omitted or set to `None` otherwise. See `C_EnumerateVars2`.

`varDetails` is very similar but instead the list contains more details for each variable. Each item in the list is an object with the following attributes:

* `VarName`: the name of the variable, the same as returned by vars.
* `VarUnits`: the units of the variable, e.g. m, ft, `kN`, deg, etc.
* `FullName`: a string containing the object name, the name of the variable, the object extra and the units, e.g. `"Line1 ZZ Stress (kPa) at End A, Inner, 0.00 deg"`.

### ObjectExtraFieldRequired

ObjectExtraFieldRequired(varName, field)

Returns a boolean indicating whether the specified results variable requires a particular object extra field.

`varName` is the name of the results variable. `field` specifies the object extra field and can be one of the following:

* `ObjectExtraField.EnvironmentPos`
* `ObjectExtraField.LinePoint`
* `ObjectExtraField.RadialPos`
* `ObjectExtraField.Theta`
* `ObjectExtraField.WingName`
* `ObjectExtraField.ClearanceLineName`
* `ObjectExtraField.WinchConnectionPoint`
* `ObjectExtraField.RigidBodyPos`
* `ObjectExtraField.ExternalResultText`
* `ObjectExtraField.DisturbanceVesselName`
* `ObjectExtraField.SupportIndex`
* `ObjectExtraField.SupportedLineName`
* `ObjectExtraField.BladeIndex`
* `ObjectExtraField.ElementIndex`
* `ObjectExtraField.SeaSurfaceScalingFactor`

### RequiredObjectExtraFields

RequiredObjectExtraFields(varName)

Returns a list of the object extra fields required for the results variable specified by `varName`.

### LineResultPoints

LineResultPoints(varName)

Returns a value identifying at which points results are reported along a line, a turbine blade or a Morison element.

`varName` is the name of the results variable. The return value can be one of the following:

* `LineResultPoints.None`
* `LineResultPoints.WholeLine`
* `LineResultPoints.Nodes`
* `LineResultPoints.MidSegments`
* `LineResultPoints.MidSegmentsAndEnds`
* `LineResultPoints.Ends`

### UnitsConversionFactor

UnitsConversionFactor(units)

Returns the scaling factor required to convert between the values in the model unit system, and the SI unit system. Multiply by this conversion factor to convert a value from SI units into model units. Divide by this conversion factor to convert a value from model units into SI units.

**units** is a string which defines the units of the value to be converted. The string is made up of zero or more fundamental components, separated by `"."`. The fundamental components are of the form `"<base_unit>^N"`, where `<base_unit>` denotes the physical quantity, e.g. length, mass, etc., and $N$ is the exponent. If the exponent is omitted then its value is taken to be 1.

The possible values for the base unit are:

* `"LL"`: length
* `"MM"`: mass
* `"TT"`: time
* `"FF"`: force
* `"$S"`: stress or pressure
* `"$E"`: energy
* `"$P"`: power

The following examples demonstrate how to specify some commonly used units:

* `"LL^2"`: area
* `"FF.LL"`: moment
* `"MM.LL^-3"`: density
* `"FF.LL^-2"`: stress or pressure, equivalent to `"$S"`

### TimeHistory

TimeHistory(varNames, period=None, objectExtra=None)

Returns time histories for the specified variable name(s). `period` is a Period object, if omitted or set to `None` then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is only required for specific results, and is otherwise omitted.

If `varNames` contains a string, then a single time history is returned. If `varNames` contains a tuple or list of strings, then a 2D numpy array is returned. The rows of the array correspond to the time axis, and each specified variable is a column in the array.

If the model uses either the implicit or explicit time domain dynamics solution methods, this function is implemented with calls to the C API functions `C_GetVarID`, `C_GetNumOfSamples` and `C_GetTimeHistory2`. If a `progressHandler` has been set, it will be called during this operation.

If the model uses the frequency domain dynamics solution method, this function returns a time history synthesised from the frequency domain process, over the specified period, with a sample interval specified by the model data `FrequencyDomainSampleInterval`. The static value is added to the time history before it is returned. The function is implemented with calls to the C API functions `C_GetFrequencyDomainResultsProcess`, `C_GetFrequencyDomainTimeHistorySampleCount` and `C_GetFrequencyDomainTimeHistoryFromProcess`.

### StaticResult

StaticResult(varNames, objectExtra=None)

This function is equivalent to the following:

TimeHistory(varNames, Period(PeriodNum.StaticState), objectExtra)[0]

### TimeHistoryCollated

TimeHistoryCollated(varNames, period=None, objectExtra=None, restartModels=None)

Returns time histories for the specified variable name(s), that fall within the specified period, collated for the specified restart models. If a `progressHandler` has been set, it will be called during this operation.

If `period` is omitted then all sample times are returned. For a [specified period](Pythonreference,Period.htm), a value of `OrcinaDefaultReal()` for `FromTime` or `ToTime` means the first sample of the first model or the last sample of the last model, respectively.

`objectExtra` is only required for specific results, and is otherwise omitted.

The `restartModels` argument is an iterable of integer indices specifying which models are to be included. You can use `restartFileNames` to obtain the file names of the models in the restart chain, and also the length of the restart chain. If `restartModels` is omitted, then all models in the restart chain are included.

### SpectralDensity

SpectralDensity(varName, period=None, objectExtra=None, fundamentalFrequency=None)

Returns the spectral density in an object with attributes `X` and `Y` containing the X axis values and Y axis values. `varName` is the result name for which the spectral density is required. `objectExtra` is an `ObjectExtra` object that is required for some `varNames`.

For time domain dynamics this is calculated by FFT from time history, as described in `C_CreateTimeHistorySummary2`. `fundamentalFrequency` specifies the minimum, or fundamental, frequency of the spectral density calculation. If this value is omitted, the value specified in the model (on the General data form) is used.

For frequency domain dynamics the spectral density is available as a direct output from the frequency domain solution, by calling the C API function `C_GetFrequencyDomainSpectralDensityGraph`. The `period` and `fundamentalFrequency` parameters are not required for frequency domain spectral density derivation and so can be omitted.

### EmpiricalDistribution, RainflowHalfCycles, UnorderedRainflowHalfCycles, RainflowAssociatedMean

These methods return time history summary results as described in `C_CreateTimeHistorySummary2`.

EmpiricalDistribution(varName, period=None, objectExtra=None)

RainflowHalfCycles(varName, period=None, objectExtra=None)

UnorderedRainflowHalfCycles(varName, period=None, objectExtra=None)

RainflowAssociatedMean(varName, period=None, objectExtra=None)

`varName` is the result name for which the time history summary is required. `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is an `ObjectExtra` object that is required for some object types and results variables.

The return value of `EmpiricalDistribution` is an object containing attributes named `X` and `Y`. These contain the result value and the associated distribution, respectively.

The value returned by `RainflowHalfCycles` and `UnorderedRainflowHalfCycles` is an array containing the rainflow half-cycle ranges. The array returned by `RainflowHalfCycles` is ordered.

The value returned by `RainflowAssociatedMean` is an object containing attributes named `HalfCycleRange`, `AssociatedMean` and `Rratio` containing the half-cycle ranges, their associated mean values and the corresponding R ratio values.

### CycleHistogramBins

Returns a tuple containing the binned rainflow cycles for the specified time history result variable.

CycleHistogramBins(varName, period=None, objectExtra=None, binSize=None)

`varName` is the result name for which the time history summary is required. `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is an `ObjectExtra` object that is required for some object types and results variables. `binSize` determines the width of each bin. The first bin covers the range 0 to `binSize`, the second bin covers the range `binSize` to `2*binSize`, etc. If `None` is passed then OrcaFlex chooses a default bin size based on the range of the data and the total number of cycles.

The return value is a tuple containing the cycle bins. Each element of the tuple is an object containing attributes named `Value` and `Count`, corresponding to the fields of the C API type `TCycleBin`.

### SpectralResponseRAO

SpectralResponseRAO(varName, objectExtra=None)

`varName` is the result name for which the spectral response is required. `objectExtra` is an `ObjectExtra` object that is required for some `varNames`, and is otherwise omitted. This functions calls the C API function `C_GetSpectralResponseGraph` and returns an object with attributes `X` and `Y` containing the X axis values and Y axis values.

### AnalyseExtrema

AnalyseExtrema(varName, period=None, objectExtra=None)

This function calls the C API function `C_AnalyseExtrema`. It returns an object with the following attributes: Max, Min, `IndexOfMax`, `IndexOfMin`.
`varName` is the result name for which extrema are required. `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is an `ObjectExtra` object that is required for some specific `varNames`, and is omitted otherwise.

### ExtremeStatistics

ExtremeStatistics(self, varName, period=None, objectExtra=None)

The `varName` parameter is the result name, `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is an `ObjectExtra` object that is required for some `varNames`, and is omitted otherwise. This function returns an `ExtremeStatistics` object.

### LinkedStatistics

LinkedStatistics(self, varNames, period=None, objectExtra=None)

The `varNames` parameter is a list of result names of interest, `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). `objectExtra` is an `ObjectExtra` object that is required for some `varNames`, and is omitted otherwise. This function returns a `LinkedStatistics` object.

### TimeSeriesStatistics

TimeSeriesStatistics(varName, period=None, objectExtra=None)

This function calls the C API function `C_CalculateTimeSeriesStatistics` and returns a `TimeSeriesStats` object. The `TimeSeriesStats` object represents the C API structure `TTimeSeriesStatistics` and has the same attributes. The parameter `period` is a [Period](Pythonreference,Period.htm) object, if omitted or set to `None` the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). The parameter `objectExtra` is an `ObjectExtra` object that is required for some `varNames`, and is omitted otherwise.

### FrequencyDomainResults

FrequencyDomainResults(varName, objectExtra=None)

Extracts frequency domain results. The model must use the frequency domain dynamics solution method and be in the simulation complete state. The function returns an object with the same attributes as the C API `TFrequencyDomainResults` structure. `varName` is the result name for which the frequency domain results are required. `objectExtra` is an `ObjectExtra` object that is required for some `varNames`. This function is implemented with a call to the C API function `C_GetFrequencyDomainResults`.

### FrequencyDomainMPM

FrequencyDomainMPM(varName, stormDurationHours, objectExtra=None)

Calculates the most probable maximum for a frequency domain result. `stormDurationHours` specifies the storm duration in hours. The other parameters are the same as passed to `FrequencyDomainResults`.

The value returned is the Rayleigh extremes MPM given by σ[2ln(T/Tz)]½ where σ is the standard deviation, T is the storm duration and Tz is the mean up-crossing period.

### FrequencyDomainResultsProcess

FrequencyDomainResultsProcess(varName, objectExtra=None)

Extracts frequency domain results expressed as a process. The model must use the frequency domain dynamics solution method and be in the simulation complete state. The function returns a numpy array of complex values. This function is implemented with a call to the C API function `C_GetFrequencyDomainResultsProcess`.

In typical usage the process will be linearly combined with other processes and then passed to the `FrequencyDomainResultsFromProcess`, `FrequencyDomainSpectralDensityFromProcess` or `FrequencyDomainSpectralResponseRAOFromProcess` methods of `Model`.

### SaveSummaryResults

SaveSummaryResults(filename, abbreviated=True)

Saves summary results tables for an OrcaFlex object to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

If **abbreviated** is `True` then the abbreviated summary results will be output. Note that abbreviated results tables are only available for certain objects, e.g. lines and turbines.

### SaveSummaryResultsMem

SaveSummaryResultsMem(spreadsheetFileType=SpreadsheetFileType.Xlsx, abbreviated=True)

Saves summary results tables for an OrcaFlex object, returning them as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

If **abbreviated** is `True` then the abbreviated summary results will be output. Note that abbreviated results tables are only available for certain objects, e.g. lines and turbines.

### SaveAddedMassDampingData

SaveAddedMassDampingData(filename)

Saves added mass and damping data for a vessel type or a multibody group to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveAddedMassDampingDataMem

SaveAddedMassDampingDataMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves added mass and damping data for a vessel type or a multibody group, returning them as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveConnectionsReport

SaveConnectionsReport(filename)

Saves the connections report spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveConnectionsReportMem

SaveConnectionsReportMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the connections report spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveDetailedPropertiesReport

SaveDetailedPropertiesReport(filename)

Saves the detailed properties of an OrcaFlex object to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveDetailedPropertiesReportMem

SaveDetailedPropertiesReportMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the detailed properties of an OrcaFlex object, returning them as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.
