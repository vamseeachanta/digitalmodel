#######################################################
# Version 1 - December 2018
# Version 2 - January 2019 Include internal tanks
# Version 3 - January 2019 Use subdirectory of user_files
#######################################################

import os
import shutil

uppath = lambda _path,n : os.sep.join(_path.split(os.sep)[:-n])

PathToWork = os.environ['AQWA_WORK_DIR']
UserFilePath = os.path.join(uppath(PathToWork,5),'user_files','HD_files')

# Check if .pac file exists in USER_FILES subdirectory
if os.path.isfile(os.path.join(UserFilePath,'ANALYSIS.PAC')):
    pass

# Since WB undertakes two solves for HD, we need to check that the PAC file exists in the working directory 
# since only created on second pass
# Additional optional files associated with internal tanks. If they do not exist then script exits
else:
    if not os.path.exists(UserFilePath):
        os.makedirs(UserFilePath)
    OriginalPacFile = os.path.join(PathToWork,'ANALYSIS.PAC')
    if os.path.isfile(OriginalPacFile):  
        for extension in ['PAC','HYD','VAC','POT','USS','TPC','TPT','TUS','TVC']:
            targetFile = 'ANALYSIS.' + extension
            originalFile = os.path.join(PathToWork,targetFile)
            if os.path.isfile(originalFile):
                shutil.copyfile(originalFile,os.path.join(UserFilePath,targetFile))
            else:
                break
        

 
