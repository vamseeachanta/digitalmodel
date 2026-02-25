from glob import glob
def DrillingRiserPost():
    orcaSimFilesList = glob('*.sim')	#Reading all ".sim" files in folder
    orcaFileNames = [item[:-4] for item in orcaSimFilesList]    #Removing".sim" extension and saving only file names in the list
    
    for simFileCount, (orcaSimFiles, inputorcaFileNames) in enumerate(zip(orcaSimFilesList, orcaFileNames)):
        print("File Number is", simFileCount+1)
        print("File Name is", orcaSimFiles)
        print("print("+ str(simFileCount+1)+ ")", file = out)
        print("model = orca.Model('" + str(inputorcaFileNames) + ".sim')", file=out)
        print("line = model['Riser']", file=out)
        print("UFJ = model['Flex Joint1']", file=out)
        print("LFJ = model['Flex Joint2']", file=out)
        print("TimePeriod = orca.pnLatestWave", file=out)
        print("WH_" + str(inputorcaFileNames) +" = model['Environment'].WaveHeight", file=out)
        print("WT_" + str(inputorcaFileNames) +" = model['Environment'].WavePeriod", file=out)
        print("Offset_" + str(inputorcaFileNames) +" = model['Ship'].InitialX", file = out)
        print("BM_"+ str(inputorcaFileNames) + "= line.RangeGraph('Bend Moment', TimePeriod)", file=out)
        print("VM_"+ str(inputorcaFileNames) + " = line.RangeGraph('Max von MIses Stress', TimePeriod)", file=out)
        print("UFJ_"+ str(inputorcaFileNames) + " = UFJ.TimeHistory('Bend Moment', TimePeriod)", file=out)
        print("LFJ_"+ str(inputorcaFileNames) + " = LFJ.TimeHistory('Bend Moment', TimePeriod)", file=out)
        print("whArray[" +str(simFileCount)+ "] = str(WH_" + str(inputorcaFileNames) + ")", file=out)
        print("wtArray[" +str(simFileCount)+ "] = str(WT_" + str(inputorcaFileNames) + ")", file=out)
        print("offsetArray[" +str(simFileCount)+ "] = str((Offset_" + str(inputorcaFileNames) + "/waterDepth*100))", file=out)
        print("bmArray[" +str(simFileCount)+ "] = str(max(BM_"+ str(inputorcaFileNames)+ ".Max))", file=out)
        print("vmArray[" +str(simFileCount)+ "] = str(max(VM_"+ str(inputorcaFileNames)+ ".Max)/1000)", file=out)
        print("ufjArray[" +str(simFileCount)+ "] = str(max(UFJ_"+ str(inputorcaFileNames)+ "/bendStiffness))", file=out)
        print("lfjArray[" +str(simFileCount)+ "] = str(max(LFJ_"+ str(inputorcaFileNames)+ "/bendStiffness))", file=out)
            
with open('postprocessing.py','w') as out:
    print("import OrcFxAPI as orca", file=out)
    print("import matplotlib.pyplot as pyplot", file=out)
    print("import pandas as pd", file=out)
    print("import numpy as np", file=out)
    print("whArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("wtArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("offsetArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("bmArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("vmArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("ufjArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("lfjArray = np.zeros(shape=(143,1)).ravel()", file=out)
    print("bendStiffness = 2", file=out) #KN-m/deg
    print("waterDepth = 2895.6", file=out)  # Water Depth is 9500 ft
    DrillingRiserPost()
out.close()

with open('postprocessing.py','a') as out:
    print("resultsList = pd.DataFrame(", file=out)
    print("    {'Wave Height': whArray,", file=out)
    print("    'Wave Period': wtArray,", file=out)
    print("    'Vesel Offset%': offsetArray,", file=out)
    print("    'Bending Moment': bmArray,", file=out)
    print("    'von Mises Stress': vmArray,", file=out)
    print("    'UFJ Angle': ufjArray,", file=out)
    print("    'LFJ Angle': lfjArray", file=out)
    print("    })", file=out)
    print("writer = pd.ExcelWriter('Results.xlsx', engine='xlsxwriter')", file=out)
    print("resultsList.to_excel(writer, sheet_name='Sheet1')", file=out)
    print("workbook  = writer.book", file=out)
    print("worksheet = writer.sheets['Sheet1']", file=out)
    print("workbook.close()", file=out)
out.close()
