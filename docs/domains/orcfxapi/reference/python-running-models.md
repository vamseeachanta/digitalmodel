# Python interface: Running models

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

To open an existing OrcaFlex file with the Python interface you can create a new `Model` object with a file name as a constructor parameter:

import OrcFxAPI

model = OrcFxAPI.Model("MyFile.dat")

or similarly for a simulation file:

model = OrcFxAPI.Model("MyFile.sim")

Alternatively you can use an existing `Model` instance and open a data file like this:

model = OrcFxAPI.Model()

model.LoadData("MyFile.dat")

Similarly for a simulation file:

model = OrcFxAPI.Model()

model.LoadSimulation("MyFile.sim")

To run a simulation (which also calculates statics if the model is in reset state), use:

model.RunSimulation()

To calculate statics only use the call:

model.CalculateStatics()

You can determine what state the model is in by reading the `state` property of the model:

modelstate = model.state

This will return a value such as `Reset` or `SimulationStopped`.

To save a simulation or data file use one the following calls:

model.SaveSimulation("MyFile.sim")

model.SaveData("MyFile.dat")

Results can now be extracted from the model object, see [Python interface: Results](Pythoninterface,Results.htm)

### OrcaFlex Batch Scripts

Normally using the OrcaFlex spreadsheet is the most effective way of creating and running batch scripts and post-processing the results, but this can also be done through the Python API if neccessary. Use the `ProcessBatchScript` command of the model object, for example:

model.ProcessBatchScript("MyScript.txt")

model.LoadSimulation("GeneratedModel.sim")

### Progress Reporting and Cancelling

If progress updates are required then progress handlers can be set for statics, dynamics, processing batch scripts and other `TOrcFxAPIHandle` operations, see [Progress handlers](Pythonreference,Model.htm#ProgressHandlers). The progress handler also gives an opportunity to cancel or pause a long running operation, the following example sets a dynamics progress handler:

def DynamicsProgressHandler(model, time, start, stop):

Cancel = False

DoSomethingWithProgressUpdate(start, time, stop) # Your function to report progress.

if TestForPause(): # Your function to check if pause required.

model.PauseSimulation()

else:

Cancel = TestForCancel() # Your function to test if the simulation should be cancelled.

return Cancel



model.dynamicsProgressHandler = DynamicsProgressHandler

model.LoadData("LongRunningModel.dat")

model.RunSimulation()
