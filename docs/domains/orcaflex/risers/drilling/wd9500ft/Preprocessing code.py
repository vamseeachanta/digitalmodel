from __future__ import print_function   #Works only with 3+ version of python
def strengthInputFilePreperation():

    for offsetCount, offset in enumerate(range(-10,12,2)):
        
        for seastateCount, (inputwaveType, inputwaveDirection, inputwaveheight, inputtimePeriod) in enumerate(zip(waveType, waveDirection, waveHeight, timePeriod)):
            print ("Offset of Vessel is", offset)
            print("Offset Counter of Vessel is", offsetCount+1)
            print ("SeaState Counter of Vessel is", seastateCount+1)
            print ("# For Sea State " + str(seastateCount+1) + ", "+ str(offset) + "% Offset" , file=out)
            print("environment = model.environment", file=out)
            print("vessel = model['Ship']", file=out)
            print("vessel.InitialX = " + str((offset*waterDepth)/100) , file=out) 
            print("environment.WaveType = '" + str(inputwaveType) +"'", file=out)
            print("environment.WaveHeight = " + str(inputwaveheight), file=out)
            print("environment.WavePeriod = " + str(inputtimePeriod), file=out)
            print("environment.WaveDirection = " + str(inputwaveDirection), file=out)
            print("model.SaveData('SS" + str(seastateCount+1) + "_offset" + str(offsetCount+1) +".dat')", file=out)
            print("model.RunSimulation()", file=out)
            print("model.SaveSimulation('SS" + str(seastateCount+1) + "_offset" + str(offsetCount+1) +".sim')", file=out)
            
import os
import xlrd                                                             
wb = xlrd.open_workbook('environmentLoading.xlsx')  # Open Excel file 
sh1 = wb.sheet_by_name(u'Sheet1')   # Read Sheet
masterFile1 = 'Stones_0.875WT_3000ft.dat'   # name of basemodel
waterDepth = 2895.6  #Water depth of the model is 3000 ft
waveType = sh1.col_values(0)    # Defining wave type in array
waveDirection = sh1.col_values(1)   # Defining wave direction in array
waveHeight = sh1.col_values(2)  # Defining wave height in array
timePeriod = sh1.col_values(3)  # Defining wave period in array

with open('preprocessing.txt','w') as out:
    print("import pandas as pd", file=out)
    print("import OrcFxAPI as orca", file=out)
    print("import matplotlib.pyplot as pyplot", file=out)
    print("import numpy as np", file=out)
    print("import xlwd", file=out)
    print("model = orca.Model('" + (masterFile1) + "')", file=out)
    strengthInputFilePreperation()
out.close()    

