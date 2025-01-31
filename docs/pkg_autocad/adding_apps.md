To run a Lisp routine in AutoCAD, follow these steps:

Load the Lisp file:

- Use the APPLOAD command to load the Lisp file. You can do this by typing APPLOAD at the AutoCAD command 
prompt and pressing ENTER.
- In the Load/Unload Applications dialog box, browse to the location of your Lisp file (.lsp), select it,
     and click Load.

Run the Lisp routine:

After loading the Lisp file, you can run the Lisp routine by typing the command defined in the Lisp
 file at the AutoCAD command prompt and pressing ENTER.
Here is an example of how to load and run a Lisp routine:

Make sure your Lisp file is correctly written and contains the necessary AutoCAD commands.



- Using APPLOAD command load the file app file code eg: "make3dpoly.txt"
- Type MAKE3DPOLYFROMCSV at the autocad command prompt and press ENTER
- This should create the polyline using the coordinates created by .csv file.
