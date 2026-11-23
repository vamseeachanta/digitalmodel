# Python interface: Object data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

The class `OrcaFlexObject` has functions `GetData` and `SetData` which correspond to the C API functions `C_GetDataDouble`, `C_GetDataInteger`, `C_GetDataString` and `C_SetDataDouble`, `C_SetDataInteger` and `C_SetDataString` respectively. The Python interface determines which of the C API functions to call according to the data type. So for example to get and set the `EndAX` position of a line the Python code is:

import OrcFxAPI

model = OrcFxAPI.Model("MyFile.dat")

line = model["MyLine"]

lineEndAX = line.GetData("EndAX", -1)

line.SetData("EndAX", -1, lineEndAX + 5.2)

However, the Python interface offers an even more convenient way to get and set an object's data – the data name itself can be used as an object attribute name. For example the above code can be written as:

lineEndAX = line.EndAX

line.EndAX = lineEndAX + 5.2

And for integer and string values:

line.NumberOfSections = 4

vessel.VesselType = "Vessel type2"

The data name is obtained from the object's data form in OrcaFlex by pressing the F7 key in the data field of interest.

### Boolean data items

Historically, boolean data was always treated as string data with possible values `"Yes"` and `"No"`. It is possible for this data to be treated as true Python `bool` data, but for reasons of backwards compatibility this functionality is not enabled by default. To enable this functionality you must activate the `EnableBooleanDataType` policy.

Without activating this policy, code that works with boolean data looks like this:

if vessel.IncludeCurrentLoad == "Yes":

vessel.IncludeAppliedLoads = "No"

This is somewhat inconvenient and so we would recommend activating the policy in new code.

OrcFxAPI.SetLibraryPolicy("EnableBooleanDataType")

if vessel.IncludeCurrentLoad:

vessel.IncludeAppliedLoads = False

All the examples below assume that this policy is enabled.

### Indexed data items

Indexed data can be handled just as conveniently, for example to set the lengths of a line's sections:

line.NumberOfSections = 3

line.Length[0] = 12.5

line.Length[1] = 32.0

line.Length[2] = 257.0

print line.Length # line.Length returns a list of section length values...

[12.5, 32.0, 257.0]

Alternatively, an indexed data item can be assigned to in one operation using an array. This sets the row count of the data item to the length of the array:

line.Length = [12.5, 32.0, 257.0]

Indexed data items also offer two functions, `InsertRow` and `DeleteRow`, and a property `rowCount` that are convenient ways of manipulating indexed data. `InsertRow` and `DeleteRow` insert or delete a row in the table for an indexed data item, see the documentation for the `OrcaFlexObject` functions `InsertDataRow` and `DeleteDataRow`. `rowCount` is a property of an indexed data item that gets or sets the number of rows in its table. The following example sets the number of sections for a line then inserts and deletes specific rows, (different indexed data names are used but they are all in the same line section table):

line.Length.rowCount = 5 # Set line to have 5 sections

line.LineType.InsertRow(3) # Insert new section before section 4

line.Length.DeleteRow(0) # Delete the first section

|  |  |
| --- | --- |
| Note: | The Python language uses 0-based indices. OrcaFlex, on the other hand, uses 1-based indices. For consistency with other Python code we have opted to use 0-based indices in the Python interface. The Python interface handles the conversion automatically by incrementing any indices by 1 before calls are passed onto the C API. |

### Data found on the general and environment data forms

These data are owned by the objects named `"General"` and `"Environment"`. So they can be accessed using `model["General"]` and `model["Environment"]`. However, for convenience they can also be accessed using dedicated attributes `model.general` and `model.environment`.

Data can therefore be accessed like this:

model.general.ImplicitConstantTimeStep = 0.1



model.environment.SeaBedStiffness = 3000

model.environment.WaterDepth = 80

### Show/hide objects

Objects can be [shown and hidden](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Modelbrowser.htm#ModelBrowserShowHide) using a boolean data item named `Hidden`.

line1.Hidden = True

line2.Hidden = False

For the environment, to show and hide the sea, seabed, sky and wind, use boolean data items named `SeaHidden`, `SeabedHidden`, `SkyHidden` and `WindHidden`, respectively.

environment = model.environment



environment.SeaHidden = True

environment.SeabedHidden = True

environment.SkyHidden = True

environment.WindHidden = True

### Lock/unlock objects

Objects can be [locked and unlocked](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Modelbrowser.htm#LockUnlock) using a boolean data item named `Locked`.

line1.Locked = True

line2.Locked = False

### Expand/collapse browser groups

[Browser groups](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Modelbrowserviews.htm#ModelBrowserGroupsView) can be expanded and collapsed using a boolean data item named `Expanded`.

group1.Expanded = True

group2.Expanded = False

### Wave train data

Wave train data belongs to the environment object. However, for data specific to a particular wave train, and because there may be multiple wave trains, you must first select the wave train:

environment = model.environment



environment.SelectedWave = "Swell from SW"

environment.WaveDirection = 135

environment.WaveType = "Dean stream"

environment.WaveHeight = 2.5

environment.WavePeriod = 18



environment.SelectedWave = "Local wind sea"

environment.WaveDirection = 40

environment.WaveType = "JONSWAP"

environment.WaveHs = 5.7

environment.WaveTz = 9

The above code selects by name, but as an alternative you may select by index. For instance, if the wave train named `"Swell from SW"` is the first in the list and `"Local wind sea"` is the second, then the following would have exactly the same effect as the previous example:

environment.SelectedWaveIndex = 0

environment.WaveDirection = 135

environment.WaveType = "Dean stream"

environment.WaveHeight = 2.5

environment.WavePeriod = 18



environment.SelectedWaveIndex = 1

environment.WaveDirection = 40

environment.WaveType = "JONSWAP"

environment.WaveHs = 5.7

environment.WaveTz = 9

Note that this index, as other indices in Python is 0-based.

### Data for post calculation actions

Post calculation actions are owned by the general object. To modify existing [post calculation actions](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Generaldata,Postcalculationactions.htm) they must first be selected, by name. The example below shows how to modify the script file name for the post calculation action named *Action3*:

general = model.general

general.SelectedPostCalculationAction = "Action3"

general.PostCalculationActionScriptFileName = "PostCalculationAction.py"

### Data for user defined results

[User defined results](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Generaldata,Userdefinedresults.htm) are accessed in the same way as post calculation actions. The example below shows how to modify the script file name for the user defined result named *Result3*:

general = model.general

general.SelectedUserDefinedResult = "Result3"

general.UserDefinedResultScriptFileName = "UserDefinedResult.py"

### Data for current data sets

[Current data sets](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,Currentdata.htm#MultipleCurrentDataSets) are owned by the environment object. Because there can be multiple current data sets, selection is again required. Selection can be by name or index, as shown below:

environment = model.environment

environment.MultipleCurrentDataCanBeDefined = True

environment.CurrentName = ["120deg", "150deg"]

environment.SelectedCurrent = "120deg"

environment.RefCurrentDirection = 120

environment.SelectedCurrentIndex = 1

environment.RefCurrentDirection = 150

environment.ActiveCurrent = "150deg"

### Vessel type data

Some vessel type data are set in a straightforward manner as follows:

vesselType = model["Vessel type1"]

vesselType.Length = 120

vesselType.PenWidth = 3

vesselType.Symmetry = "xz plane"

However, the majority of vessel type data require that you also specify to which draught the data apply:

vesselType.SelectedDraught = "Transit draught"

vesselType.CurrentCoeffSurgeArea = 1200

vesselType.CurrentCoeffSwayArea = 1100

vesselType.CurrentCoeffYawAreaMoment = 120e3

To set data for displacement RAOs, wave load RAOs, wave drift QTFs and sum frequency QTFs you must also specify to which type of RAO the data apply. For example:

vesselType.SelectedDraught = "Survival draught"



vesselType.SelectedRAOs = "Displacement"

vesselType.RAOOriginX = 10

vesselType.RAOOriginY = 0

vesselType.RAOOriginZ = 2



vesselType.SelectedRAOs = "WaveLoad"

vesselType.RAOOriginX = 0

vesselType.RAOOriginY = 0

vesselType.RAOOriginZ = 0



vesselType.SelectedRAOs = "WaveDrift"

vesselType.RAOOriginX = -3

vesselType.RAOOriginY = 0

vesselType.RAOOriginZ = 4



vesselType.SelectedRAOs = "SumFrequencyQTF"

vesselType.RAOOriginX = -3

vesselType.RAOOriginY = 0

vesselType.RAOOriginZ = 4

Note that the data names RAOOriginX etc are the same in each case, but different data are set depending on which type of RAOs has been selected.

In addition, when setting RAO table data (for displacement RAOs, wave load RAOs and wave drift QTFs) you must specify to which direction the data apply. You may specify either the index of the direction in the list of all directions for that type, or the value of the direction. For example:

vesselType.SelectedDraught = "Survival draught"

vesselType.SelectedRAOs = "Displacement"

vesselType.SelectedRAODirectionIndex = 2

vesselType.RAOYawAmplitude[2] = 0.13

vesselType.SelectedRAODirectionValue = 135

vesselType.RAOYawAmplitude[2] = 0.18

However, it is worth pointing out that situations where you would wish to specify RAO table data in Python code are rare. It is much more likely that you would import these data into OrcaFlex from some external source and then save it as part of the base case data file.

### Multibody group data

Some multibody group data are set in a straightforward manner as follows:

group = model["Multibody group1"]

group.BodyName = ["Body1", "Body2"]

group.VesselType = ["Vessel type1", "Vessel type2"]

To set the stiffness data you must first select the body:

group.SelectedStiffnessBody = "Body2"

group.DisplacedVolume = 85e3

To set the added mass and damping data you must select the period or frequency (by either index or value) and the row/column body:

group.NumberOfAMDPeriodsOrFrequencies = 2

group.AMDPeriodOrFrequency[0] = 11.0

group.AMDPeriodOrFrequency[1] = 13.0

group.SelectedAMDPeriodOrFrequencyValue = 11.0

group.SelectedAMDRowBody = "Body1"

group.SelectedAMDColumnBody = "Body2"

group.AddedMassX[0] = 42.0

group.DampingX[0] = 35.0

The fact that it is possible to set these data in Python code does not necessarily make it a good idea to do so! These data are exceedingly unwieldy to manipulate in Python code. Should you find yourself doing so, it might be prudent to look for a more elegant solution to your problem.

### P-y model data

Setting [P-y model](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?P-ymodels,Data.htm) data is complicated because each depth must be selected before the model data can be accessed. As with other data selection, you can select depth by value, using `SelectedDepthBelowSeabedFrom` or select the depth by index in the list of depths using `SelectedDepthBelowSeabedIndex`.

pyModel = model["P-y model1"]

pyModel.DepthBelowSeabedFrom = [0.0, 10.0, 20.0]



pyModel.SelectedDepthBelowSeabedIndex = 0

pyModel.ModelType = "API RP 2A soft clay"

pyModel.EffectiveUnitSoilDensity = 1.6

pyModel.UndrainedShearStrength = 6.0

pyModel.J = 0.5

pyModel.Epsilonc = 4.0



pyModel.SelectedDepthBelowSeabedFrom = 10.0

pyModel.ModelType = "API RP 2A sand"

pyModel.EffectiveUnitSoilDensity = 1.3

pyModel.C1 = 1.1

pyModel.C2 = 2.0

pyModel.C3 = 15.0

pyModel.k = 42.0



pyModel.SelectedDepthBelowSeabedFrom = 20.0

pyModel.ModelType = "P-y table"

pyModel.NumberOfEntries = 2

pyModel.Deflection = [0.0, 0.2]

pyModel.Resistance = [0.0, 11.0]

### SHEAR7 data

SHEAR7 data ownership is divided between the SHEAR7 object and line objects. The SHEAR7 version, output file options and S-N curve data are owned by the SHEAR7 object:

shear7 = model["SHEAR7 data"]

shear7.SHEAR7Version = "4.6"

shear7.SHEAR7OutputDmg = True

To access the S-N curve table requires that the curve is selected first, either by name or by index:

shear7.SelectedSHEAR7SNCurve = "Curve2"

shear7.SHEAR7SNCurveNumberOfPoints = 3

shear7.SHEAR7SNCurveS[2] = 10e5

shear7.SHEAR7SNCurveN[2] = 10e4

shear7.SHEAR7SNCurveEnduranceLimit = 750.7



shear7.SelectedSHEAR7SNCurveIndex = 0

shear7.SHEAR7SNCurveEnduranceLimit = 531.0

SHEAR7 whole line data and stress concentration factors are accessed through a line object:

line = model["Line1"]

line.SHEAR7CurrentProfileDiscretisation = "Regular spacing"

line.SHEAR7CurrentProfileTargetSpacing = 10

line.SHEAR7LocalSCFArcLength[0] = 32.0

line.SHEAR7LocalSCF[0] = 1.15

The SHEAR7 hydrodynamic and structural section data apply to a line section, so the index of the section is required:

line.SHEAR7StrouhalType[0] = "Rough cylinder"

line.SHEAR7LiftFactor[0] = 0.9

line.SHEAR7SectionSNCurve[1] = "Curve1"

### Line contact data

To edit penetrator data, the penetrator locations data set must first be selected either by name or by index:

lineContact = model["Line contact data"]



lineContact.SelectedPenetratorLocationsDataSetIndex = 0

lineContact.Penetratorx[1] = 13.6

lineContact.Penetratory[1] = -2.5

lineContact.PenetratorContactArea[1] = 0.85



lineContact.SelectedPenetratorLocationsDataSet = "Locations2"

lineContact.Penetratorx[1] = 10.9

lineContact.Penetratory[1] = 3.0

lineContact.PenetratorContactArea[1] = 0.25

### Code checks data

Code checks data ownership is divided between the code checks object and line type objects. Model-wide data items are owned by the code checks object:

codeChecks = model["Code checks"]

codeChecks.APIRP2RDDesignCaseFactor = 1

The per line type code check data are just like any other line type data:

lineType = model["Line type1"]

lineType.APIRP2RDTcorr = 0.001

lineType.APIRP2RDSMYS = 380e3

### Variable data sources

The procedure for accessing data for [variable data sources](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Variabledata.htm) is illustrated below:

dataSource = model["Stiffness1"]

dataSource.IndependentValue = [0.0, 0.2, 0.4]

dataSource.DependentValue = [0.0, 1000.0, 5000.0]

Note that `IndependentValue` and `DependentValue` are the data names for the X and Y columns of the variable data source. If, for example, you are setting data for a bending stiffness data source, then `IndependentValue` denotes curvature and `DependentValue` denotes bend moment.

### Line type wizard

The [line type wizard](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Linetypewizard.htm) can be used from Python. You must first set the data for the line type. Once this is complete the wizard is invoked by calling the `InvokeWizard` method:

lineType = model["Line type1"]

lineType.WizardCalculation = "Rope/wire"

lineType.RopeNominalDiameter = 0.03

lineType.RopeConstruction = "6x19 wire with fibre core"

lineType.InvokeWizard()

### Plasticity wizard

The [plasticity wizard](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Linetypes,Plasticitywizard.htm) can be used from Python. You must first set the data for the bend stiffness variable data source. Once this is complete the wizard is invoked by calling the `InvokeWizard` method:

stiffness = model["Stiffness1"]

stiffness.StressOD = 0.30

stiffness.StressID = 0.27

stiffness.CurveType = "Stress-strain table"

stiffness.Strain = [0, 1, 5]

stiffness.Stress = [0, 380000, 400000]

stiffness.InvokeWizard()

stiffness.Hysteretic = False

### Line setup wizard

The [line setup wizard](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Linesetupwizard.htm) can be used from Python. The input data for the wizard should first be set using data assignment commands. These data are owned by a variety of different objects. The model wide data (e.g. calculation mode and convergence parameters) are owned by the general object. The line specific data are owned by each individual line. The following code illustrates this:

model.general.LineSetupCalculationMode = "Calculate line lengths"

model.general.LineSetupMaxDamping = 20

model["Line1"].LineSetupTargetVariable = "Effective tension"

model["Line1"].LineSetupLineEnd = "End A"

model["Line1"].LineSetupTargetValue = 830.0

model["Line2"].LineSetupIncluded = False

model.InvokeLineSetupWizard()

### Polar coordinates data on the all objects form

The [all objects](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Allobjectsdataform.htm) data form allows connection data to be specified as [polar coordinates](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Allobjectsdataform.htm#AllObjectsPolarCoordinates). The data appear in a table containing all connections in the model. However, the data still belong to each individual object and the appearance of a table of data is purely presentational. This means that to set the data you must first select the individual object and then set the data, as illustrated below:

obj = model["6D buoy1"]

obj.PolarR = 10.0

obj.PolarTheta = 30.0



obj = model["Line1"]

obj.PolarR[0] = 20.0

obj.PolarTheta[0] = 45.0

obj.PolarR[1] = 340.0

obj.PolarTheta[1] = 45.0



obj = model["Line2"]

obj.PolarR[0] = 20.0

obj.PolarTheta[0] = 90.0

obj.PolarR[1] = 340.0

obj.PolarTheta[1] = 90.0



obj = model["Winch1"]

obj.PolarR[2] = 10.0

obj.PolarTheta[2] = 90.0

obj.PolarR[3] = 10.0

obj.PolarTheta[3] = 90.0

For objects with multiple connections, an index must be specified to identify the connection. For lines and links an index of 0 means end A and an index of 1 means end B. For winches the index identifies the winch connection point. For turbines, index 0 means the turbine connection, index 1 means the end A tower connection, and index 2 means the end B tower connection.

### Object tags

For any object that supports [object tags](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Objecttags.htm), the `tags` attribute provides access.

### Colour data

Drawing colour data items are defined using RGB values:

model["Line type1"].PenColour = 255

model["Line type2"].PenColour = 65280

Instead of using decimal values for RGB it is more practical to specify colour using hexadecimal:

model["Line type1"].PenColour = 0x0000ff # red

model["Line type2"].PenColour = 0x00ff00 # green

model["Line type3"].PenColour = 0xff0000 # blue

|  |  |
| --- | --- |
| Note: | The integer for red is the final pair of these hexadecimal values, and the blue contribution is first, which could be denoted `0xBBGGRR`. It is very common elsewhere to see hexadecimal colours in the opposite order, red-first and blue-last, as `0xRRGGBB`. Your expected input might therefore need to be reversed to match the OrcaFlex convention. |

### Fatigue analysis data

Fatigue analysis data are set in a very similar way to OrcaFlex data. The most complex data are S-N and T-N curves which must be selected before assigning their data. As usual, you may select them either by their name or their index.

fatigue = OrcFxAPI.FatigueAnalysis()

fatigue.DamageCalculation = "Homogeneous pipe stress"

fatigue.AnalysisType = "Rainflow"

fatigue.ArclengthIntervalsCount = 1

fatigue.FromArclength[0] = 0.0

fatigue.ToArclength[0] = 30.0

fatigue.SCF[0] = 1.5

fatigue.SNcurveCount = 2

fatigue.SelectedSNcurveIndex = 0

fatigue.SNcurveEnduranceLimit = 15.0

fatigue.SNcurveName[1] = "ProjectSteel"

fatigue.SelectedSNcurve = "ProjectSteel"

fatigue.SNcurveEnduranceLimit = 0.0
