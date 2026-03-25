######################################################################################
# Version 1 - December 2018 
# Version 2 - January 2019 Include PERD and FWDS commands in FDR* data
#           - Allow multiple structures (including interacting)
#           - Ensure checking of input data categories only done after Restart command
# Version 3 - January 2019 Dialogue window added. 
# Version 4 - January 2019 Use subdirectory of user_files
#           - Remove files in user_files if full solve undertaken
######################################################################################

import os
import shutil
import ctypes

uppath = lambda _path,n : os.sep.join(_path.split(os.sep)[:-n])

PathToFile = os.environ['AQWA_INPUT_FILE']
PathToWork = os.environ['AQWA_WORK_DIR']

# Check if .hyd file exists in USER_FILES directory
# If it does then skip solve by using the existing hyd file
UserFilePath = os.path.join(uppath(PathToWork,5),'user_files','HD_files')
UserFileHydPath = os.path.join(UserFilePath,'ANALYSIS.HYD')
UserFileResPath = os.path.join(UserFilePath,'ANALYSIS.RES')

if os.path.isfile(UserFileHydPath):

    hydiList = []

    with open(PathToFile, 'r') as f:
        data = f.readlines()

# Overwrite existing file        
    with open(PathToFile, 'w') as fout:

        datacheck = False
        restartread = False
        ElStruct = ''
# Add CRNM option and path to .hyd file    
        for line in data:
    
            if line[0:1] == '*':
                fout.write(line)
                continue
            
            if line[0:9] == 'NUM_CORES':
                fout.write(line)
                fout.write('OPTIONS CRNM\n')
                continue
                
            if line[0:7] == 'RESTART':
                restartread = True
                if line[12:13] == '2':
                    datacheck = True
                else:
                    ID_Yes = 6
                    ID_No = 7
                    MB_YESNO = 0x4L
                    MB_ICONQUESTION = 0x20L
                    window = ctypes.windll.user32.GetDesktopWindow()
                    result = (ctypes.windll.user32.MessageBoxA(ctypes.c_int(window),"Do you want to use existing .HYD file?\n"
                        "This will reduce solution time. "
                        "Only use if additional damping or mass has been included or modified.\n"
                        "Select No to undertake full solve?","Hydrodynamic Diffraction Solution",MB_YESNO | MB_ICONQUESTION))
                    if result == ID_No:
                        datacheck = True
                        for extension in ['PAC','HYD','VAC','POT','USS','TPC','TPT','TUS','TVC']:
                            targetFile = 'ANALYSIS.' + extension
                            if os.path.isfile(os.path.join(UserFilePath,targetFile)):
                                os.remove(os.path.join(UserFilePath,targetFile))  
                                
            if restartread:
                      
                if line[6:10] == 'DIRN':
                    if not datacheck:
                        continue
        
                if line[6:10] == 'HRTZ':
                    if not datacheck:
                        continue
                    
                if line[6:10] == 'PERD':
                    if not datacheck:
                        continue

                if line[6:10] == 'FWDS':
                    if not datacheck:
                        continue
            
                if line[10:12] == 'FD':
                    fout.write(line)
                    if datacheck:
                        continue
                    else:
                        if line[12:13] == 'R':
                            structNum = ' '+line[13:14]
                        else:
                            structNum = line[12:14]
                        writeCommand = True
                        if len(hydiList) > 0:
                            for structs in hydiList:
                                if structNum == structs[1]:
                                    break
                                if int(structNum) > int(structs[1]) and int(structNum) <= int(structs[0]):
                                    writeCommand = False
                                    break
                        if writeCommand:
                            fout.write('      FILE          '+UserFileHydPath+'\n')
                            fout.write('      CSTR   '+structNum+'\n')
                            fout.write('      CPDB\n')
                        continue
                        
                if line[10:12] == 'EL':
                    if line[12:13] == 'M':
                        ElStruct = ' '+line[13:14]
                    else:
                        ElStruct = line[12:14]
                        
                if line[6:10] == 'HYDI':
                    hydiStruct = line[18:20]
                    hydiList.append([ElStruct,hydiStruct])

            fout.write(line)    
       


 
