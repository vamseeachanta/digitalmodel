# Model building: An example

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

The example C program below illustrates the techniques described in the previous topics.

[Download this example as a text file.](c_example.txt)

```
#include <stdio.h>
#include <stdlib.h>
#include "OrcFxAPI.h"

#define VesselName "The vessel"
#define DraughtName "The draught"

void CheckStatus(int Status)
{
    if (Status!=stOK) {
        int Length = C_GetLastErrorString(NULL);
        LPSTR ErrorString = malloc((size_t) Length);
        C_GetLastErrorString(ErrorString);
        printf("Error: %s\n", ErrorString);
        free(ErrorString);
        exit(0);
    }
}

void SetWaveTrainData(TOrcFxAPIHandle EnvironmentHandle, LPCSTR WaveTrainName, double Hs, double Direction)
{
    int Status;

    /* select this wavetrain */
    C_SetDataString(EnvironmentHandle, "SelectedWaveTrain", 0, WaveTrainName, &Status);
    CheckStatus(Status);

    /* set the data */
    C_SetDataString(EnvironmentHandle, "WaveTrainType", 0, "JONSWAP", &Status);
    CheckStatus(Status);

    C_SetDataDouble(EnvironmentHandle, "WaveTrainHs", 0, Hs, &Status);
    CheckStatus(Status);

    C_SetDataDouble(EnvironmentHandle, "WaveTrainDirection", 0, Direction, &Status);
    CheckStatus(Status);
}

int main(int argc, char **argv)
{
    if (argc!=2) {
        printf("Error: must pass the data file name as the single command line parameter\n");
    } else {
        int Status;
        TOrcFxAPIHandle ModelHandle, EnvironmentHandle, VesselTypeHandle, VesselHandle, LineHandle;
        TObjectInfo ObjectInfo;

        /* create the model */
        C_CreateModel(&ModelHandle, 0, &Status);
        CheckStatus(Status);

        /* this is not strictly necessary since we have just created the model */
        C_ClearModel(ModelHandle, &Status);
        CheckStatus(Status);

        /* create a Vessel Type */
        C_CreateObject(ModelHandle, otVesselType, &VesselTypeHandle, &Status);
        CheckStatus(Status);

        /* create a Vessel (it will use the Vessel Type just created) */
        C_CreateObject(ModelHandle, otVessel, &VesselHandle, &Status);
        CheckStatus(Status);

        C_SetDataString(VesselHandle, "Name", 0, VesselName, &Status);
        CheckStatus(Status);

        /* create a Line (a Line Type will be created automatically since none yet exist) */
        C_CreateObject(ModelHandle, otLine, &LineHandle, &Status);
        CheckStatus(Status);

        /* connect the Line top end to the Vessel */
        C_SetDataString(LineHandle, "EndAConnection", 0, VesselName, &Status);
        CheckStatus(Status);

        /* anchor the Line bottom end */
        C_SetDataString(LineHandle, "EndBConnection", 0, "Anchored", &Status);
        CheckStatus(Status);

        /* set the Line end positions */
        C_SetDataDouble(LineHandle, "EndAX", 0, 50.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "EndAY", 0, 0.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "EndAZ", 0, 10.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "EndBX", 0, 220.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "EndBY", 0, 0.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "EndBZ", 0, 0.0, &Status);
        CheckStatus(Status);

        /* give the line 2 sections */
        C_SetDataInteger(LineHandle, "NumberOfSections", 0, 2, &Status);
        CheckStatus(Status);

        /* set the section lengths and segmentation */
        /* first of all section 1 ... */
        C_SetDataDouble(LineHandle, "Length", 1, 90.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "TargetSegmentLength", 1, 5.0 , &Status);
        CheckStatus(Status);

        /* ... and then section 2 */
        C_SetDataDouble(LineHandle, "Length", 2, 130.0, &Status);
        CheckStatus(Status);

        C_SetDataDouble(LineHandle, "TargetSegmentLength", 2, 9.0, &Status);
        CheckStatus(Status);

        /* setup wave train data */
        C_ObjectCalled(ModelHandle, "Environment", &ObjectInfo, &Status);
        CheckStatus(Status);

        EnvironmentHandle = ObjectInfo.ObjectHandle;
        C_SetDataInteger(EnvironmentHandle, "NumberOfWaveTrains", 0, 2, &Status);
        CheckStatus(Status);

        SetWaveTrainData(EnvironmentHandle, "Wave1", 4.0, 120.0);
        SetWaveTrainData(EnvironmentHandle, "Wave2", 3.7, 67.0);

        /* save the model */
        C_SaveData(ModelHandle, argv[1], &Status);
        CheckStatus(Status);

        /* destroy the model */
        C_DestroyModel(ModelHandle, &Status);
        CheckStatus(Status);
    }

    return 0;
}
```
