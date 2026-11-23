# Python reference: Model

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

[dummy link for benefit of anchor linked from external source](#threadCount)[dummy link for benefit of anchor linked from external source](#correctExternalFileReferencesHandler)

The class Model is the Python interface representation of an OrcaFlex model.

## Construction

Model(filename=None, threadCount=None, handle=None)

The constructor creates a new OrcaFlex model object. If the optional `filename` parameter is specified then the constructor attempts to open this as a simulation file first and as a data file second. The optional `threadCount` parameter specifies the number of processing threads used for simulations, if omitted then the default is the number of processing cores of the computer. If the optional `handle` parameter is specified (from another Model instance) the new model instance will refer to the same underlying OrcaFlex model. The `handle` is used internally, and by external functions, and is not intended for general use. If both `filename` and `handle` are omitted, or are `None`, then an empty model is created. See `C_CreateModel` and `C_CreateModel2`.

When the Python garbage collector destroys this object, and it has ownership of the Python objects it contains, then `C_DestroyModel` is called.

## Attributes and methods

### defaultViewParameters

This property returns a `ViewParameters` object with the attributes set to default values. See `C_GetDefaultViewParameters`.

### objects

This property returns a tuple of all the objects in the OrcaFlex model.

Individual objects can be accessed directly by treating the model itself as the dictionary. For example, `obj = model["MyObjectName"]` will assign an instance of the object called `"MyObjectName"`. The standard Python [mapping](https://docs.python.org/3/library/collections.abc.html#collections.abc.Mapping) interface can be used, including the `in` operator, the `get`, `keys`, `values` and `items` methods, etc.

### simulationStartTime

This property returns the simulation start time.

### simulationStopTime

This property returns the simulation stop time.

### simulationComplete

This property returns `true` if the simulation has completed. See `C_GetSimulationComplete`.

### state

Identifies the current state of the OrcaFlex model returned by `C_GetModelState`.

### dongleName, dongleAccessMode, donglePort, dongleServer, licenceFileLocation

Properties that give details of the dongle used to provide the OrcaFlex licence. These properties are not available for **FlexNet** licences.

### licenceStatus

A string detailing how the OrcaFlex licence is provided. This property is available for both dongle and FlexNet licences.

### latestFileName

The name of file that was most recently loaded or saved.

This property can also be assigned to. This is sometimes necessary when loading memory files in order to provide a root for relative paths.

### type

The model type, one of the following: `ModelType.Standard`, `ModelType.Variation` or `ModelType.Restart`.

It is possible to set this property to `ModelType.Standard`, but attempts to assign a value of `ModelType.Variation` or `ModelType.Restart` are not allowed. In order to create new variation models or restart analysis models, use `NewVariationModel()` or `NewRestartAnalysis()`.

### outputBrowserGroupStructureWhenTrackingChanges

A boolean property that determines whether or not the group structure is output when saving text data files for `ModelType.Variation` or `ModelType.Restart` model types.

### restartParentFileNames

A property returning the full chain of parent file names for a restart analysis model.

### restartFileNames

A property returning the full chain of file names for a restart analysis model, including this model, and the parent models.

### isFrequencyDomainDynamics

A boolean property that indicates whether the model uses frequency domain dynamics.

### isTimeDomainDynamics

A boolean property that indicates whether the model uses time domain dynamics.

[suppress check for dead internal links, OrcaFlex help file links to this target](#canResumeSimulation)

### canResumeSimulation

A boolean property that indicates whether the simulation can be resumed after having been saved and then loaded.

### targetRestartStateRecordingTimes

The target simulation times where mid-simulation restart state will be recorded.

### actualRestartStateRecordingTimes

The actual simulation times where mid-simulation restart state have been recorded. These can differ from the target times if the simulation times do not happen to fall exactly at target times.

### EulerBucklingLimitExceeded

Indicates whether a line in the model has exceeded its Euler buckling limit during the simulation.

### simulationDrawFrequencyDomainSolveType

|  |  |
| --- | --- |
| Note: | This property is only relevant when working with frequency domain simulations that use the [low frequency, combined linearisation solution frequencies](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Generaldata,Frequencydomain.htm). In this particular scenario both wave frequency and low frequency solves have been performed, and it is necessary to specify which solve is used for drawing. |

This property is used to get or set the frequency domain solve type used by drawing functions such as `SaveModelView`, `SaveModelViewMetafile`, etc. The value is one of the `FrequencyDomainSolveType` enum values: `FrequencyDomainSolveType.Latest`, `FrequencyDomainSolveType.WaveFrequency` or `FrequencyDomainSolveType.LowFrequency`.

See `C_SetSimulationDrawFrequencyDomainSolveType`.

### simulationDrawTime

This property is used to get or set the simulation time used by drawing functions such as `SaveModelView`, `SaveModelViewMetafile`, etc. See `C_SetSimulationDrawTime`.

### simulationTimeStatus

This property returns an object with attributes `StartTime`, `StopTime` and `CurrentTime`. See `C_GetSimulationTimeStatus`.

### simulationTimeToGo

This property returns the estimated remaining runtime for a long simulation, see `C_GetSimulationTimeToGo`.

### recommendedTimeSteps

This property returns a structure with attributes `InnerTimeStep` and `OuterTimeStep`. See `C_GetRecommendedTimeSteps`.

### ModuleEnabled

ModuleEnabled(module)

This method returns `True` if the optional module identified the `module` parameter is enabled. See the C API function `C_ModuleEnabled`.

### threadCount

The `threadCount` property gets and sets the number of processing threads the model can use. See `C_GetModelThreadCount` and `C_SetModelThreadCount`.

### groupFirstChild

This property returns the first child in the model's group structure.

### staticsProgressHandler, dynamicsProgressHandler, progressHandler, batchProgressHandler

Progress handler callback functions can be assigned with these attributes. The `staticsProgressHandler` is called during `CalculateStatics()` and `InvokeLineSetupWizard()` (see `StaticsProgressHandlerProc`), the `dynamicsProgressHandler` is called while the simulation is running (see `DynamicsProgressHandlerProc`) and `progressHandler` is called during `TOrcFxAPIHandle` running operations (such as Load and Save operations, see `C_SetProgressHandler`). The `batchProgressHandler` is called when running a batch script (see `BatchScriptProgressHandlerProc` and `ProcessBatchScript`). The function signatures for these progress handlers are:

def StaticsProgress(model, progress)

def DynamicsProgress(model, time, start, stop)

def PercentProgress(model, percent)

def BatchProgress(model, progress)

* The `model` parameter is a reference to the OrcaFlex model.
* The `progress` parameter is a string indicating the current state of progress.
* The `time, stop and start` parameters are the current simulation time, simulation start time and simulation stop time respectively.
* The `percent` parameter is an integer indicating percentage complete.

The progress handler functions should return `False` to continue processing or `True` to cancel the operation. `PauseSimulation()` can be called from within the dynamics progress handler.

|  |  |
| --- | --- |
| Warning: | There is a [bug](https://github.com/python/cpython/issues/82929) in 32 bit Python versions 3.8-3.11 inclusive, which results in Python crashing when using `dynamicsProgressHandler`. |

### warnings

This property returns a list of any warning texts generated by a model. This property combines calls to `C_GetNumOfWarnings` and `C_GetWarningText` in the API.

### waveComponents

This property returns a list of `WaveComponent` objects. See `C_GetWaveComponents2`.

### windComponents

This property returns a list of `WindComponent` objects. See `C_GetWindComponents`.

### frequencyDomainProcessComponents

frequencyDomainProcessComponents(frequencyDomainSolveType=FrequencyDomainSolveType.Latest)

Returns a tuple of `FrequencyDomainProcessComponent` objects for the specified frequency domain solve type. `frequencyDomainSolveType` can be one of the following:

* `FrequencyDomainSolveType.Latest`: returns components from the most recent solve, e.g., low frequency components for a low frequency, combined linearisation solution.
* `FrequencyDomainSolveType.WaveFrequency`: returns the wave frequency components, if available.
* `FrequencyDomainSolveType.LowFrequency`: returns the low frequency components, if available.

See `C_GetFrequencyDomainProcessComponents3`.

### AttachToThread, DetachFromThread

AttachToThread()

DetachFromThread()

Models have [affinity to a particular thread](Multithreading.htm). That is, all calls to functions which operate on a particular model, or objects within that model, must be made from the same thread. When a model is created it has affinity to the thread in which it is created. A model can be assigned affinity to a different thread by taking the following steps:

1. Call `DetachFromThread` from the thread to which the model currently has affinity.
2. Call `AttachToThread` from a different thread, after which the model will have affinity to this thread.

### CreateObject

The `CreateObject` method creates a new model object and returns either an `OrcaFlexObject` or one of its [subclasses](Pythonreference,OrcaFlexObject.htm), according to the object type requested. This method calls `C_CreateObject`.

CreateObject(type, name=None)

The `type` parameter corresponds to the `ObjectType` value required by `C_CreateObject`. The `name` parameter is an optional text value required for a non-default object name. If the parameter is omitted then OrcaFlex creates a unique default name.

### DestroyObject

DestroyObject(obj)

This method removes an object from the model, where `obj` is the Python object reference to an OrcaFlex object, or a string containing the name of an object in the model. See `C_DestroyObject`.

### CreateClones

CreateClones(objects, model=None)

This function creates new objects that are clones of an existing collections of `objects`. The newly created cloned objects will have identical data to the source objects. Any dependencies, such as type objects, which are not present in the destination model, will also be cloned.

The new objects will be created in the model specified by the `model` parameter. If this parameter is omitted then the new objects will be created in this model.

### DeleteUnusedTypes

DeleteUnusedTypes()

Delete any types (e.g. line types, clump types etc.) that are not in use.

### DeleteUnusedVariableDataSources

DeleteUnusedVariableDataSources()

Delete any variable data sources that are not in use.

### Reset

Reset()

Discards any active calculation and returns the model to the reset state.

### Clear

Clear()

Removes all objects from the model, returns the general and environment data to their default values and sets the model to be a standard model.

### NewVariationModel

NewVariationModel(parentFileName)

Sets the model to be a variation model based on the specified `parentFileName`.

### NewRestartAnalysis

NewRestartAnalysis(parentFileName)

Sets the model to be a restart analysis based on the specified `parentFileName`.

### DefaultInMemoryLogging, DisableInMemoryLogging, ForceInMemoryLogging

DefaultInMemoryLogging()

DisableInMemoryLogging()

ForceInMemoryLogging()

These functions allow you to control the logging storage policy for the model. For more details, see the documentation for the C API function `C_DisableInMemoryLogging`.

### UseVirtualLogging

UseVirtualLogging()

Enables virtual logging for this model. When virtual logging is enabled, log files are not created, and post-processing is performed by reading directly from the simulation file. For more details, see the documentation for the C API function `C_UseVirtualLogging`.

### LoadData

LoadData(filename)

Load model data from `filename`. If a `progressHandler` has been set, it will be called during this operation.

The following file types are supported:

* [OrcaFlex data file](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Datafiles.htm) in binary or text format
* [OrcaFlex simulation file](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Simulationfiles.htm) (only the data part of the file is loaded)
* [OrcaWave results file](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata,OrcaWave.htm)

### LoadDataMem

LoadDataMem(buffer, dataFileType=DataFileType.Binary)

Load model data from `buffer` which should be a `bytes` or `bytearray` instance. The `dataFileType` parameter can be either `DataFileType.Binary` or `DataFileType.Text` to specify the data format.

If a `progressHandler` has been set, it will be called during this operation.

The same file types are supported as by `LoadData`.

### SaveData

SaveData(filename)

Save the model data to `filename`. If a `progressHandler` has been set, it will be called during this operation.

If the file extension is '.yml' then a text data file will be saved; otherwise a binary data file will be saved.

### SaveDataMem

SaveDataMem(dataFileType=DataFileType.Binary)

Saves the model data returning it as a `bytearray` object. The `dataFileType` parameter can be either `DataFileType.Binary` or `DataFileType.Text` to specify the data format.

If a `progressHandler` has been set, it will be called during this operation.

### LoadSimulation

LoadSimulation(filename)

Load a simulation file from `filename`. If a `progressHandler` has been set, it will be called during this operation.

### LoadSimulationMem

LoadSimulationMem(buffer)

Load a simulation from `buffer` which should be a `bytes` or `bytearray` instance. If a `progressHandler` has been set, it will be called during this operation.

### SaveSimulation

SaveSimulation(filename)

Save a simulation to `filename`. If a `progressHandler` has been set, it will be called during this operation.

### SaveSimulationMem

SaveSimulationMem()

Saves the simulation returning it as a `bytearray` object. If a `progressHandler` has been set, it will be called during this operation.

### correctExternalFileReferencesHandler

Loading of an OrcaFlex simulation file can fail if the simulation contains out-of-date external file references. This attribute of the model object can receive assignment of a function which can update those external file references to valid locations. The function receives the model object, so that external file references can be set to new values. No other model data can be changed. See `C_SetCorrectExternalFileReferencesHandler` for more details.

def correctWaveHistory(model):

model.environment.WaveTimeHistoryFileName = "waveTimeHistory.txt"



model = OrcFxAPI.Model()

model.correctExternalFileReferencesHandler = correctWaveHistory

model.LoadSimulation("simulationWithInvalidWaveHistoryFileReference.sim")

### CalculateStatics

CalculateStatics()

Run a statics calulation on the model. See `C_CalculateStatics`. If a `staticsProgressHandler` has been set this will be repeatedly called during the statics calculation.

### RunSimulation

RunSimulation(enableAutoSave=False, autoSaveIntervalMinutes=DefaultAutoSaveIntervalMinutes, autoSaveFileName=None)

RunSimulation() # Autosave disabled

RunSimulation(True, 10, "AutoSaved.sim") # Autosave every 10 minutes to AutoSaved.sim

RunSimulation(True, autoSaveFileName="AutoSaved.sim") # Autosave using the default interval

Run a simulation, including calculating statics if the model is in reset state. See `C_RunSimulation2`. The parameters are only required if autosave is needed, if no parameters are specified then autosave is disabled by default. If the `autoSaveIntervalMinutes` parameter is omitted and `enableAutoSave` is true then the default autosave interval, `DefaultAutoSaveIntervalMinutes`, is used (60 minutes). If a `dynamicsProgressHandler` has been set this will be repeatedly called while the simulation is in progress.

### ExtendSimulation

ExtendSimulation(time)

This function extends a simulation by adding a new stage with a duration of `time`. This function can only be called when the model is either paused or completed. See the C API function `C_ExtendSimulation`.

### PauseSimulation

PauseSimulation()

Pauses a running simulation, this method should be called from within a [dynamics progress handler](#ProgressHandlers). See `C_PauseSimulation`.

### ProcessBatchScript

ProcessBatchScript(filename, enableAutoSave=False, autoSaveIntervalMinutes=DefaultAutoSaveIntervalMinutes, autoSaveFileName=None)

ProcessBatchScript(filename) # Run the batch script in filename with no auto save.

ProcessBatchScript(filename, True, autoSaveFileName="AutoSaved.sim") # Run batch script with auto saving at the default interval.

ProcessBatchScript(filename, True, 10, "AutoSaved.sim") # Run batch script with auto saving at 10 minute intervals.

This function runs an OrcaFlex batch script file, `filename`. The autosave parameters are optional, see `RunSimulation()`. Callbacks will be made to the following [progess handlers](#ProgressHandlers) if they are defined: `batchProgressHandler`, `staticsProgressHandler` and `dynamicsProgressHandler`. See the documentation for the C API function `C_ProcessBatchScript`.

### InvokeLineSetupWizard

InvokeLineSetupWizard()

Invokes the line setup wizard calculation. The input data for the wizard should first be set using data assignment commands. These data are owned by a variety of different objects. The model wide data (e.g. calculation mode and convergence parameters) are owned by the general object. The line specific data are owned by each individual line. The following script illustrates this:

model = OrcFxAPI.Model("inputfile.dat")

model.general.LineSetupCalculationMode = "Calculate line lengths"

model.general.LineSetupMaxDamping = 20

model["Line1"].LineSetupTargetVariable = "Effective tension"

model["Line1"].LineSetupLineEnd = "End A"

model["Line1"].LineSetupTargetValue = 830.0

model["Line2"].LineSetupIncluded = "No"

model.InvokeLineSetupWizard()

### UseCalculatedPositions

UseCalculatedPositions(setLinesToUserSpecifiedStartingShape=False, simulationTime=None)

When called with the `setLinesToUserSpecifiedStartingShape` set to its default value of `False` this is equivalent to the **Model | Use calculated positions** menu item in OrcaFlex. When `setLinesToUserSpecifiedStartingShape` is set to `True` this is equivalent to the **Model | Use specified starting shape for lines** menu item in OrcaFlex.

If `simulationTime` is set to `None` or `OrcinaDefaultReal()`, then the calculated positions will be based on the static state, if the model is in statics completed state, or the latest calculated simulation time if a time domain dynamic simulation has been performed. Otherwise, the calculated positions will be based on the closest log sample to the specified `simulationTime`.

### UseStaticLineEndOrientations

UseStaticLineEndOrientations()

This is equivalent to the **Model | Use static line end orientations** menu item in OrcaFlex.

### SaveModelView

SaveModelView(filename, viewParameters=None)

Saves a bitmap of the view defined in `viewParameters` to `filename`. If `viewParameters` is omitted then a default `ViewParameters` object is used. You can obtain a default `ViewParameters` object by calling `defaultViewParameters`. See `C_SaveModel3DViewBitmapToFile`.

### SaveModelViewMem

SaveModelViewMem(viewParameters=None)

Saves a bitmap of the view defined in `viewParameters` returning it as a `bytearray` object. If `viewParameters` is omitted then a default `ViewParameters` object is used. You can obtain a default `ViewParameters` object by calling `defaultViewParameters`. See `C_SaveModel3DViewBitmapMem`.

### SaveModelViewMetafile

SaveModelViewMetafile(filename, viewParameters=None)

Saves an [enhanced metafile](https://en.wikipedia.org/wiki/Windows_Metafile) of the view defined in `viewParameters` to `filename`. If `viewParameters` is omitted then a default `ViewParameters` object is used. You can obtain a default `ViewParameters` object by calling `defaultViewParameters`. See `C_SaveModel3DViewMetafileToFile`.

This function requires that the calling thread is DPI aware. This can be achieved by calling one of a number of Win32 functions which affect DPI awareness state. For example:

import ctypes

ctypes.windll.user32.SetProcessDpiAwarenessContext(ctypes.c\_void\_p(-4))

DPI awareness must be set before the `OrcFxAPI` module is first imported.

### SaveModelViewMetafileMem

SaveModelViewMetafileMem(viewParameters=None)

Saves an [enhanced metafile](https://en.wikipedia.org/wiki/Windows_Metafile) of the view defined in `viewParameters` returning it as a `bytearray` object. If `viewParameters` is omitted then a default `ViewParameters` object is used. You can obtain a default `ViewParameters` object by calling `defaultViewParameters`. See `C_SaveModel3DViewMetafileMem`.

This function requires that the calling thread is DPI aware – see above for details.

### SaveWaveSearchSpreadsheet

SaveWaveSearchSpreadsheet(filename)

Saves an OrcaFlex wave search spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveWaveSearchSpreadsheetMem

SaveWaveSearchSpreadsheetMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves an OrcaFlex wave search spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveLineTypesPropertiesSpreadsheet

SaveLineTypesPropertiesSpreadsheet(filename)

Saves the OrcaFlex line types properties spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveLineTypesPropertiesSpreadsheetMem

SaveLineTypesPropertiesSpreadsheetMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the OrcaFlex line types properties spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveCodeChecksProperties

SaveCodeChecksProperties(filename)

Saves the OrcaFlex code checks properties spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveCodeChecksPropertiesMem

SaveCodeChecksPropertiesMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the OrcaFlex code checks properties spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveConnectionsReport

SaveConnectionsReport(filename)

Saves the connections report spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveConnectionsReportMem

SaveConnectionsReportMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the connections report spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SaveReferencesReport

SaveReferencesReport(filename)

Saves the references report spreadsheet to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

### SaveReferencesReportMem

SaveReferencesReportMem(spreadsheetFileType=SpreadsheetFileType.Xlsx)

Saves the references report spreadsheet, returning it as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

### SampleTimes

SampleTimes(period=None)

Returns an array of the sample times falling within the specified period, where `period` is a [Period](Pythonreference,Period.htm) object. If `period` is omitted then the default value depends on the model state, see [Python interface: Results](Pythoninterface,Results.htm). If a `progressHandler` has been set, it will be called during this operation.

If the model uses either the implicit or explicit time domain dynamics solution methods, this function returns the sample times that fall within the simulation period defined by `period`. It is implemented with calls to the C API functions `C_GetNumOfSamples` and `C_GetSampleTimes`.

If the model uses the frequency domain dynamics solution method, this function returns the sample times for a synthesised time history, over the specified period, with a sample interval specified by the model data `FrequencyDomainSampleInterval`. It is implemented with calls to the C API functions `C_GetFrequencyDomainTimeHistorySampleCount` and `C_GetFrequencyDomainTimeHistorySampleTimes`.

### SampleCount

SampleCount(period=None)

Returns the number of samples in the specified period. The `period` argument is interpreted in the same way as by the `SampleTimes` method. This method is typically used only when implementing OrcaFlex user defined results.

### SampleTimesCollated

SampleTimesCollated(period=None, restartModels=None)

Returns an array of the sample times falling within the specified period, collated for the specified restart models. If a `progressHandler` has been set, it will be called during this operation.

If `period` is omitted then all sample times are returned. For a [specified period](Pythonreference,Period.htm), a value of `OrcinaDefaultReal()` for `FromTime` or `ToTime` means the first sample of the first model or the last sample of the last model, respectively.

The `restartModels` argument is an iterable of integer indices specifying which models are to be included. You can use `restartFileNames` to obtain the file names of the models in the restart chain, and also the length of the restart chain. If `restartModels` is omitted, then all models in the restart chain are included.

### SaveSummaryResults

SaveSummaryResults(filename, abbreviated=True)

Saves summary results tables for all objects to `filename`. The file can be an Excel spreadsheet (`.xlsx` or `.xls`), a tab delimited file (`.txt`) or a comma separated file (`.csv`). The decision is taken based on the file extension that you specify. This method is implemented by calling `C_SaveSpreadsheet`.

If `abbreviated` is `True` then the abbreviated summary results will be output. Note that abbreviated results tables are only available for certain objects, e.g. lines and turbines.

### SaveSummaryResultsMem

SaveSummaryResultsMem(spreadsheetFileType=SpreadsheetFileType.Xlsx, abbreviated=True)

Saves summary results tables for all objects, returning them as a `bytearray` object. The `spreadsheetFileType` parameter can be either `SpreadsheetFileType.Csv`, `SpreadsheetFileType.Tab` or `SpreadsheetFileType.Xlsx` to specify the spreadsheet format. This method is implemented by calling `C_SaveSpreadsheetMem`.

If `abbreviated` is `True` then the abbreviated summary results will be output. Note that abbreviated results tables are only available for certain objects, e.g. lines and turbines.

### FrequencyDomainResultsFromProcess, FrequencyDomainSpectralDensityFromProcess, FrequencyDomainSpectralResponseRAOFromProcess

FrequencyDomainResultsFromProcess(process)

FrequencyDomainSpectralDensityFromProcess(process)

FrequencyDomainSpectralResponseRAOFromProcess(process)

Analogous to the `FrequencyDomainResults`, `SpectralDensity` and `SpectralResponseRAO` methods of `OrcaFlexObject`, respectively. Instead of returning results for a specified variable name, these functions instead return results for a specified process. Typically the process is obtained by linearly combining processes returned from calls to `FrequencyDomainResultsProcess`.

These methods are implemented by calling `C_GetFrequencyDomainResultsFromProcess`, `C_GetFrequencyDomainSpectralDensityGraphFromProcess`, and `C_GetFrequencyDomainSpectralResponseGraphFromProcess`.

### FrequencyDomainTimeHistorySampleTimes

FrequencyDomainTimeHistorySampleTimes(period, sampleInterval=None)

Returns the sample times for a synthesised time history, over the specified period and sample interval. If `sampleInterval` is not specified, the model data `FrequencyDomainSampleInterval` is used. It is implemented with calls to the C API function `C_GetFrequencyDomainTimeHistorySampleCount`.

### FrequencyDomainTimeHistorySampleCount

FrequencyDomainTimeHistorySampleCount(period, sampleInterval=None)

Returns the number of samples in the specified period. The arguments are interpreted in the same way as by the `FrequencyDomainTimeHistorySampleTimes` method.

### ExecutePostCalculationActions

ExecutePostCalculationActions(filename, actionType, treatExecutionErrorsAsWarnings=False)

Executes the post calculation actions for the specified `actionType` which can be either `PostCalculationActionType.InProcPython` or `PostCalculationActionType.CmdScript`.

The `filename` is the name of the simulation file associated with the model. This name will be passed to the post calculation action.

Pass true to `treatExecutionErrorsAsWarnings` if you wish errors that occur during action execution to be treated as OrcaFlex warnings. Pass false if you want such errors to be treated as errors and so raise Python exceptions.

### FrequencyDomainTimeHistoryFromProcess

FrequencyDomainTimeHistoryFromProcess(process, period, sampleInterval=None)

Analogous to the `TimeHistory` method of `OrcaFlexObject`. Instead of returning results for a specified variable name, this functions instead returns a synthesised time history for a *specified process* with a sample interval specified by the `sampleInterval` parameter. If the `sampleInterval` parameter is omitted then the model data `FrequencyDomainSampleInterval` is used. Typically the process is obtained by linearly combining processes returned from calls to `FrequencyDomainResultsProcess`.

Unlike a call to the `TimeHistory` method, the static value associated with the process in unknown and so can not be added to the time history before it is returned, i.e. the synthesised time history contains just the dynamic component of the value. The function is implemented with calls to the C API functions `C_GetFrequencyDomainTimeHistorySampleCount` and `C_GetFrequencyDomainTimeHistoryFromProcess`.
