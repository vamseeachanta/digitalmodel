import os
import sys
import shutil

# Write any error messages into a PreSolve log file
with open("PreSolve.log", "w") as sys.stderr:
	
	#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	# Modify this value for your specific case:
	CABIN4OverrideLimit = 2.00E-10
	
	# You should not need to modify anything below this line!
	#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	
	# Temp file for storing updated data
	PathToFile = os.environ["AQWA_INPUT_FILE"]
	TempFile = PathToFile + ".tmp"
	
	# Open temp file
	with open(TempFile, "w") as fout:
	
		# Look for OPTIONS, add DBUG; add CABIN4 debug option card
		with open(PathToFile, "r") as f:
			data = f.readlines()
			optsFound = False
			
			for line in data:
				if line.find("OPTIONS") > -1 and not optsFound:
					options = line.strip().split(" ")
					newopts = [options[0]]
					newopts.append("DBUG")
					
					if len(options) > 1:
						newopts.extend(options[1:])
					
					for opt in newopts:
						fout.write(opt + " ")
					
					fout.write("\n")
					optsFound = True
				
				elif line.find("RESTART") > -1:
					fout.write(line)
					fout.write("CABIN4      0    0    0    0    0    0   {:.3e}\n".format(CABIN4OverrideLimit))
					fout.write("END\n")
				
				else:
					fout.write(line)
	
	# Remove original input file, replace with modified file
	os.remove(PathToFile)
	shutil.move(TempFile, PathToFile)