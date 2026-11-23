# Model building: Setting the data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

There a number of different but closely related functions for setting data. They all receive a parameter called `lpDataName` which is a string identifying the data item to be set.

These names are the same names used to assign values in an OrcaFlex batch script. To find out the name of a data item, open the appropriate data form in OrcaFlex, select the data item, and then open (e.g. by right click) the pop-up menu and select the 'data names' option. This displays the data item name which you can then copy+paste into your program.

Once you know the data name you can call one of the functions `C_SetDataDouble`, `C_SetDataInteger` or `C_SetDataString`. Usually you will know which of these functions to call. For example to set the position of an object you would use `C_SetDataDouble`; to set the number of segments in a line section you would use `C_SetDataInteger`; to set the name of an object you would use `C_SetDataString`. Alternatively you can call `C_GetDataType` to find out the type of a particular data item.

## Examples

### Setting the name of an object:

C\_SetDataString(ObjectHandle, "Name", 0, "The new name", &Status);

### Setting the name of an object in a more general way:

void SetName(TOrcFxAPIHandle ObjectHandle, LPCSTR NewName, **int** \*lpStatus)

{

C\_SetDataString(ObjectHandle, "Name", 0, NewName, lpStatus);

}

### Setting the position of a line:

C\_SetDataDouble(LineHandle, "EndAX", 0, 100.0, &Status);

C\_SetDataDouble(LineHandle, "EndAY", 0, 0.0, &Status);

C\_SetDataDouble(LineHandle, "EndAZ", 0, 10.0, &Status);

C\_SetDataDouble(LineHandle, "EndBX", 0, 160.0, &Status);

C\_SetDataDouble(LineHandle, "EndBY", 0, 30.0, &Status);

C\_SetDataDouble(LineHandle, "EndBZ", 0, -15.0, &Status);

### Setting the section data of a line:

C\_SetDataInteger(LineHandle, "NumberOfSections", 0, 3, &Status);

/\* section 1 \*/

C\_SetDataString(LineHandle, "LineType", 1, "LType1", &Status);

C\_SetDataDouble(LineHandle, "Length", 1, 30.0, &Status);

C\_SetDataInteger(LineHandle, "NumberOfSegments", 1, 20, &Status);

/\* section 2 \*/

C\_SetDataString(LineHandle, "LineType", 2, "LType2", &Status);

C\_SetDataDouble(LineHandle, "Length", 2, 50.0, &Status);

C\_SetDataInteger(LineHandle, "NumberOfSegments", 2, 10, &Status);

/\* section 3 \*/

C\_SetDataString(LineHandle, "LineType", 3, "LType3", &Status);

C\_SetDataDouble(LineHandle, "Length", 3, 150.0, &Status);

C\_SetDataInteger(LineHandle, "NumberOfSegments", 3, 35, &Status);
