import os
import os.path
import subprocess
resultSets = []

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Environment variables from AQWAWorkbench
AqwaInputFile = os.environ["AQWA_INPUT_FILE"]
AqwaInputFileNoExt = os.environ["AQWA_INPUT_FILE_NOEXT"]
AqwaWorkDir = os.environ["AQWA_WORK_DIR"]
AqwaUserFilesDir = os.environ["AQWA_USER_DIR"]
AqwaInstallDir = os.environ["AQWA_INSTALL_DIR"]
AqwaPythonExe = os.environ["AQWA_PYTHON_EXE"]
AqwaRunCompleted = os.environ["AQWA_RUN_COMPLETED"]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Common parameters (do not change per results set)
#ANSYSInstallDir = "C:\\Program Files\\ANSYS Inc\\v<version>"
ANSYSInstallDir = AqwaInstallDir

#inputFile = "Analysis.PLT"
inputFile = AqwaInputFileNoExt+".PLT"

# Variable parameters (one entry per results set)
#				   Output file		PLT1	PLT2	PLT3	PLT4
resultSets.append(["MyResults1",	   1,	   1,	   1,	   1])
resultSets.append(["MyResults2",	   1,	   1,	   1,	   2])
resultSets.append(["MyResults3",	   1,	   1,	   1,	   6])
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# You should not need to change anything below this line...!
with open("trace.log", "w") as ts:
	try:
		PathToInputFile = AqwaInputFile
		PathToInputFile = PathToInputFile.replace("\"","")		# Strip any quotes in path
		isHydrostatic = False
		
		# Look for RESTART - do not run AqwaReader if we have only calculated for stages 1-2
		try:
			with open(PathToInputFile, "r") as fdat:
				for line in fdat.readlines():
					if "RESTART  1  2" in line:
						isHydrostatic = True
		
		except Exception as err:
			ts.write(repr(err))
		
		# Set AqwaReader executable path
		workdir = os.path.dirname(PathToInputFile)
		exe = os.path.join(ANSYSInstallDir, "aisol", "bin", "winx64", "AqwaReader.exe")
		
		# Run AqwaReader if not hydrostatic calc
		if not isHydrostatic:
			for resultSet in resultSets:
				argStrings = []
				argVals = []
				args = [exe]

				argStrings.append("--Type")
				argStrings.append("--InFile")
				argStrings.append("--OutFile")
				argStrings.append("--Format")
				argStrings.append("--PLT1")
				argStrings.append("--PLT2")
				argStrings.append("--PLT3")
				argStrings.append("--PLT4")
				
				argVals.append("Graphical")
				argVals.append("{:s}".format(os.path.join(workdir, inputFile)))
				argVals.append("{:s}".format(os.path.join(workdir, resultSet[0])))
				argVals.append("csv")
				argVals.append("{:d}".format(resultSet[1]))
				argVals.append("{:d}".format(resultSet[2]))
				argVals.append("{:d}".format(resultSet[3]))
				argVals.append("{:d}".format(resultSet[4]))
				
				for (string, val) in zip(argStrings, argVals):
					args.extend([string, val])
				
				code = subprocess.call(args, stdout = ts, stderr = ts)
				ts.write("Returned " + str(code) + "\n")

	except Exception as err:
		ts.write(repr(err))