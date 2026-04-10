# Data: Model

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Model type

Can be one of the following: **standard**, [variation model](#VariationModels) or [restart analysis](#RestartAnalyses). The model type is fixed when the model is created (e.g. via the [file menu](Userinterface,Filemenu.htm)) and cannot be directly modified subsequently.

### Variation models

A variation model is specified entirely in terms of the **differences** between itself and another OrcaWave model – the **parent** model. This differs from the usual concept of an OrcaWave model, in which all the data specifying an analysis is held within a single model.

The differences defining a variation model are most concisely expressed in YAML [text data file](Userinterface,Textdatafiles.htm) format. The main benefit of this is that it permits collections of minor variations to be set up from a single parent model. This is an efficient aid to automation tasks because only those data items which differ from the parent model are present in the text data. This means that you can make modifications to the parent model, and then those changes are automatically inherited by the variation models.

Variation models form a distinct class of model type compared to **standard** OrcaWave models and come with additional functionality, [change tracking](#VariationModelsChangeTracking), to identify those data items that differ versus the parent model. The difference between a variation model and a standard model is in how the model data is stored and presented; fundamentally, the model could be expressed in either format and the results of the analysis would be the same. Both of these model types differ fundamentally from the third class of OrcaWave models, [restart analyses](#RestartAnalyses), which can be understood as an advancement on a variation model which also inherits intermediate results from the parent analysis in order to accelerate the calculation.

In terms of terminology, we find it easiest to refer to the base file, against which changes are tracked, as the *parent* model; whereas, the variation (or restart) model is known as the *child* model.

As already indicated, variation models are most naturally expressed as text data. The following YAML example illustrates the concept:

# variation.yml

BaseFile: parent.owd
WaterDepth: 140

When this text data file is loaded in OrcaWave, the program does the following:

1. Opens the OrcaWave binary data file named `parent.owd`, located in the same directory as the text data file.
2. Sets the water depth to 140.

Variation models can be created from the [file menu](Userinterface,Filemenu.htm#MenuFileNewVariationModel) in OrcaWave, and also via the [OrcFxAPI](https://www.orcina.com/webhelp/OrcFxAPI/) programming interface; they can also be created manually in a text editor. However, once a variation model has been created, the choice of parent model becomes fixed and cannot be directly changed in the user interface. It can, however, be changed by editing the text data file representation or using the [edit data as text](Userinterface,Modelmenu.htm#MenuModelEditDataAsText) capability that is built into the user interface. Editing a variation text data file by hand in a text editor is a good way to create a base file for automation, or to discover data names and data structure for an object. Variation models can be also be [converted into standard OrcaWave models](Userinterface,Modelmenu.htm#MenuModelConvertToStandardModelType), if required, although this means that all the data from the parent is copied into the child model and any subsequent changes to the parent model will have no effect on the child.

|  |  |
| --- | --- |
| Warning: | Variation files can also be saved using binary data file format. This is not recommended because it leads to a copy of the parent model data being stored in the file, which means that any changes made in the parent model will not be inherited when the child model is opened. The binary data format is only intended to be used to save variation data when dealing with text file incompatibilities between different OrcaWave versions. The text file format is recommended in all other situations. |

### Change tracking

The data in a new variation model is effectively the same as in the parent model. Changing a data item, e.g. WaterDepth on the environment page, will result in that data item being highlighted. By default, this takes the form of a red line (this colour is a user [preference](Userinterface,Toolsmenu.htm#PreferencesModifiedDataColour)) around the boundary of the relevant data control. We say such data items are *marked as changed*.

|  |  |
| --- | --- |
| Figure: | A data item marked as changed |

When the variation file is saved, only those data items that are marked as changed are written to the text data file.

For purposes of change tracking, all data items that were not available in the parent model, but become available in the child, are treated as having changed. Similarly, all data items on newly added objects (those that were not in the parent model) are also treated as changed. Data items that are read-only also get marked as changed if their read-only value differs (compared to the equivalent parent model value) because of changes to other data items. Mostly, such read-only data items are not output to the YAML text file anyway, but it can be helpful to see that the value of such items is different from that in the parent model.

### Restart analyses

In a restart analysis, OrcaWave will use intermediate results from a previous analysis to accelerate the calculation. Typical applications for this option are to repeat an analysis using different [inertia](Data,Inertia.htm) or [constraints](Data,Constraints.htm) data for one or more bodies, or using different [QTF calculation](Data,QTFs.htm) data.

The **parent model file** must be an [OrcaWave data file](Userinterface,OrcaWavemodelfiles.htm) (either binary or text) in which the [output option](Data,Calculationandoutput.htm#OutputOptions) for intermediate results is selected. In the restart model some fundamental input data is determined by the parent model, while other data can be edited as normal (see [below](#InputDataForRestartAnalyses) for details). The parent model cannot itself be a restart analysis.

When the restart calculation begins, OrcaWave will look for a results file with the same name as the parent model data file, and use intermediate results from that results file. For example, if the parent model file is named `foo.owd`, then OrcaWave will look for a results file named `foo.owr`. It is therefore important to ensure that both `foo.owd` and `foo.owr` are maintained in sync.

|  |  |
| --- | --- |
| Note: | The [batch processing](Automation,Batchprocessing.htm) facility performs restart analysis tasks last. Therefore you can include both the parent model and restart model(s) in the same batch. |

### Input data for restart analyses

When performing restart analyses, the time-consuming step of populating and solving the [first-order boundary integral matrix equations](Theory,First-orderequations.htm) is avoided. For this approach to be valid, the fundamental input data that affects those matrix equations must be identical in the parent model and the restart model. These data items are:

* The [units system](#Units)
* The calculation options for [damping lid](Data,Calculationandoutput.htm#ResonanceDampingLid), [divide non-planar panels](Data,Calculationandoutput.htm#DivideNonPlanarPanels) and the values of the [calculation tolerances](Data,Calculationandoutput.htm#CalculationTolerances)
* The environment data for [water depth](Data,Environment.htm#Water), [wave periods or frequencies](Data,Environment.htm#WavePeriodsOrFrequencies) and [wave headings](Data,Environment.htm#WaveHeadings)
* The number of [bodies](Data,Bodies.htm) and, for each body: the [mesh position and orientation](Data,Bodies.htm#MeshPositionAndOrientation), the [body origin](Data,Bodies.htm#BodyOrigin), all [body mesh data](Data,Bodies.htm#BodyMesh) and the options for [adding interior surface panels](Data,Bodies.htm#InteriorSurfacePanels)

All the data items listed above are populated in the restart model with values loaded from the parent model file. In the restart model these items are shown in grey and cannot be edited.

All other data items can be edited as normal in the restart model, with a small number of restrictions:

* The [output option](Data,Calculationandoutput.htm#OutputOptions) for intermediate results is not available in the restart model.
* The [solve type](Data,Calculationandoutput.htm#SolveType) of the restart model must be compatible with the parent: if the parent model is **potential formulation only** then the restart must be the same; if the parent model has a [damping lid](Data,Calculationandoutput.htm#ResonanceDampingLid) then the restart cannot be **full QTF calculation**.

### Program version compatibility for restart analyses

The restrictions on the [input data for restart analyses](#InputDataForRestartAnalyses) given above are strictly necessary for a valid analysis. Some restrictions are enforced by data items that cannot be edited in the child model, others are enforced by [validation errors](Data,Validation.htm#RestartParentModelValidation) in the child model.

In addition, we generally advise that the child and parent models of a restart analysis should be run in the *same version* of OrcaWave. Newer versions of the software may behave differently to older versions, e.g. due to bug fixes or other code changes (as documented on the [what's new page](What'snewinthisversion.htm)). In some cases, it may be clear that a difference between versions is relevant to your analysis, but in other cases it can be more subtle, especially if the changes from several versions are combined together. The best practice is to run both parent and child models in the latest available version of OrcaWave.

## Units

The **units system** may be chosen to be **SI**, **US** or **user**. Units are defined for length, mass, force and time, and the value of gravitational acceleration $g$ is also given.

Selecting the **SI** or **US** system will fix the units of length, mass, force and, time and the value of $g$. SI gives length in metres, mass in tonnes, force in kN and time in seconds; US gives length in feet, mass in kips, force in kips and time in seconds.

If neither the SI nor the US systems meets your requirements, then the **user**-defined option gives complete flexibility: you may select individually from the length, mass, force and time units on offer and also specify a value for $g$.

If the units are changed, then OrcaWave converts all the existing data in the model from the old units into the new ones.

|  |  |
| --- | --- |
| Note: | For any given set of length, force and time units, there is a corresponding **inertial** mass unit, which is the mass that would be accelerated at 1 unit of acceleration when one unit of force was applied. For example in the US units system the unit of acceleration is $\textit{1 ft/s}^2$ and the unit of force is 1 kip force, so the inertial mass unit is 1 kiloslug (= 1000 slugs = approximately 32000 pounds mass), since a 1 kip force applied to a 1000 slug mass would give an acceleration of $\textit{1 ft/s}^2$. |
|  | US units, and many of the possible user-defined units systems, are **non-inertial systems**: they use mass units that are **not** the inertial mass unit for their length, force and time units. You should be aware that this help document assumes, as do most text books, that any terms involving mass units (so mass, moment of inertia, density etc.) are expressed in the inertial mass unit, **not** the non-inertial mass unit used by the unit system. OrcaWave automatically allows for this when you use US units or any user defined system which is non-inertial. |

## Comments

A free form multi-line text field that can be used to store notes about the model. OrcaWave does not use this text.
