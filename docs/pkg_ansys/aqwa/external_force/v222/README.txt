This is an example of how to run Ansys Aqwa connected to a python server providing dynamically calculated forces to be applied on an Ansys Aqwa Time History analysis.

The file AqwaServerMgr.py contains a Python module providing the user with all the necessary tools to define their own functions easily.

The file AqwaSocketUserForceExample.py contains an example of a user created python script that uses AqwaServerMgr.py and defines a set of three different functions for defining the forces.

The file AD_PYTHONUSERFORCE.DAT is the Aqwa input file to be used in this example

The file StartAqwaPythonUserForceServer.bat is a simple script that the user can use to start the python program.

--------------------------------------------------------------------------------

Instructions :


1). Start the python server by dragging-dropping AqwaSocketUserForceExample.py onto StartAqwaPythonUserForceServer.bat

2). Start Aqwa by dragging-dropping AD_PYTHONUSERFORCE.DAT onto your Aqwa.exe program/icon in your Aqwa installation directory

3). Repeat step 2 three times and see what happens.
