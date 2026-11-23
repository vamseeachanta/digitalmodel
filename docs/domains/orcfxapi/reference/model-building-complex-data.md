# Model building: More complex data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

|  |  |
| --- | --- |
| Note: | The [Batch processing: Examples of setting data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Batchprocessing,Examplesofsettingdata.htm) in the OrcaFlex documentation is another useful reference for data manipulation. That topic considers the issues from the perspective of OrcaFlex batch script but many of the issues covered there also apply to the current setting. |

### Variable Data

Certain data items in OrcaFlex are variable. For example line type bending stiffness can either be a constant value or can specify a moment / curvature relationship. A call to `C_GetDataType` for a line type object using the data name `xBendingStiffness` will return `dtVariable`.

To set a variable data item you call either `C_SetDataDouble` or `C_SetDataString` to set either a constant value or a variable value respectively.

For examples the code fragment below demonstrates both possibilities:

C\_SetDataDouble(LineTypeHandle, "xBendingStiffness", 0, 50.0, &Status);

C\_SetDataString(LineTypeHandle, "xBendingStiffness", 0, "Stiffness1", &Status);

The first line sets the bending stiffness to the constant value 50kN.m2 (assuming the model uses SI units). The second line sets the bending stiffness to the variable data set called `"Stiffness1"`.

### Wave Train Data

The environment data in OrcaFlex allows for multiple wave trains. Before setting data for a wave train you need to select it by calling either `C_SetDataString` with the data name `SelectedWaveTrain` or `C_SetDataInteger` with the data item `SelectedWaveTrainIndex`. For example the code fragment below sets up a wave with 2 wave trains and illustrates both forms of selection:

C\_SetDataInteger(EnvironmentHandle, "NumberOfWaveTrains", 0, 2, &Status);



/\* select 1st wave train by index \*/

C\_SetDataInteger(EnvironmentHandle, "SelectedWaveTrainIndex", 0, 1, &Status);

C\_SetDataString(EnvironmentHandle, "WaveTrainType", 0, "JONSWAP", &Status);

C\_SetDataDouble(EnvironmentHandle, "WaveTrainDirection", 0, 45.0, &Status);



/\* select 2nd wave train by name \*/

C\_SetDataString(EnvironmentHandle, "SelectedWaveTrain", 0, "Wave2", &Status);

C\_SetDataString(EnvironmentHandle, "WaveTrainType", 0, "ISSC", &Status);

C\_SetDataDouble(EnvironmentHandle, "WaveTrainDirection", 0, 90.0, &Status);

### Current data sets

Manipulation of data when multiple current data sets are defined is similar to that for wave trains and makes use of the data names `SelectedCurrent` and `SelectedCurrentIndex`:

C\_SetDataString(EnvironmentHandle, "MultipleCurrentDataCanBeDefined", 0, "Yes", &Status);

C\_SetDataInteger(EnvironmentHandle, "NumberOfCurrentDataSets", 0, 2, &Status);



/\* select 1st data set by name \*/

C\_SetDataString(EnvironmentHandle, "SelectedCurrent", 0, "Current1", &Status);

C\_SetDataDouble(EnvironmentHandle, "RefCurrentSpeed", 0, 0.7, &Status);



/\* select 2nd data set by index \*/

C\_SetDataInteger(EnvironmentHandle, "SelectedCurrentIndex", 0, 2, &Status);

C\_SetDataDouble(EnvironmentHandle, "RefCurrentSpeed", 0, 0.6, &Status);



/\* set which data set is active \*/

C\_SetDataString(EnvironmentHandle, "ActiveCurrent", 0, "Current2", &Status);

### Vessel type data

Most vessel type data are draught-specific, in which case a draught must be selected before accessing the data in the usual way. For example:

C\_SetDataString(VesselTypeHandle, "SelectedDraught", 0, "Draught1", &Status);

In the case of RAOs there is at least one further selection to be made: the RAO type must be specified as one of Displacement, Wave Load or QTF. For example:

C\_SetDataString(VesselTypeHandle, "SelectedRAOs", 0, "Displacement", &Status);

To set the data in an RAO table you also need to select the direction associated with that table. The number of directions (and hence number of tables) may be set to N as follows:

C\_SetDataInteger(VesselTypeHandle, "NumberOfRAODirections", 0, N, &Status);

and the ith direction, 1 ≤ i ≤ N, is then selected by

C\_SetDataInteger(VesselTypeHandle, "SelectedRAODirectionIndex", 0, i, &Status);

Alternatively you may, if you know it, select by the direction value:

C\_SetDataDouble(VesselTypeHandle, "SelectedRAODirectionValue", 0, 45.0, &Status);

Following these selection commands you can, for instance, set the value of the direction itself (in degrees) by:

C\_SetDataDouble(VesselTypeHandle, "RAODirection", 0, 40.0, &Status);

or, say, the surge amplitude in the first row of the table by:

C\_SetDataDouble(VesselTypeHandle, "RAOSurgeAmplitude", 1, 0.01, &Status);

Frequency-dependent added mass and damping data are set in a similar way, with in this case the required selection being that of the individual period or frequency. Again, you must first select the draught; having done so, you can then access the period or frequency data through either the index or the value, analogously to RAO directions:

C\_SetDataInteger(VesselTypeHandle, "NumberOfAddedMassAndDampingPeriodsOrFrequencies", 0, M, &Status);

C\_SetDataInteger(VesselTypeHandle, "SelectedAddedMassAndDampingPeriodOrFrequencyIndex", 0, i, &Status);

C\_SetDataDouble(VesselTypeHandle, "SelectedAddedMassAndDampingPeriodOrFrequencyValue", 0, 12.5, &Status);

If you use select-by-value, you must ensure that the value you pass is consistent with the `WavesReferredToBy` convention in use for this vessel type: it must be one of "period (s)", "frequency (rad/s)" or "frequency (Hz)". You can get and set this value in the usual way:

C\_GetDataString(VesselTypeHandle, "WavesReferredToBy", 0, &Status);

C\_SetDataString(VesselTypeHandle, "WavesReferredToBy", 0, "frequency (Hz)", &Status);

Having selected the required period or frequency you can then, for instance, set the value of the period or frequency itself (in the prevailing convention) by:

C\_SetDataDouble(VesselTypeHandle, "AddedMassAndDampingPeriodOrFrequency", 0, 8.0, &Status);

or, say, the surge-surge component of added mass by:

C\_SetDataDouble(VesselTypeHandle, "AddedMassMatrixX", 1, 500.0, &Status);

or the heave-pitch component of damping by:

C\_SetDataDouble(VesselTypeHandle, "DampingRy", 3, 1.0E4, &Status);

There are two tricks to remember here. Firstly, to consider the matrix, either added mass or damping, as a set of six named columns each with six rows. So `"DampingRy"` here refers to the pitch column, the fifth column of the selected damping matrix, and the `3` to the third row, the heave row. And secondly, to recall that only the top-right part of these symmetric matrices is editable in OrcaFlex, so to be able to set the data you must interpret "heave-pitch component" in this way, rather than heave column and pitch row which would be in the bottom left.
