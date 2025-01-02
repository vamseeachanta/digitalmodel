import math

pi = 3.141592653589793
pipeNominalOD_m = 0.127  # outside diameter  
pipeNominalWT_m = 0.0127
pipeMinimumWT_m = 0.005875 # Pipe wall tickness
pipeNominalID = pipeNominalOD_m - 2*pipeNominalWT_m  # inside diameter
print("pipeNominalInsideDiameter : ",pipeNominalID)
pipeFutureCorrosionRateFloor = 0.0762 #minimum corrosion rate floor in mm per year (3 mpy)
pipeAge = 8   # age in years
pipeAsessLengthErrorRatio = 0.01 # Acceptable error ratio in assessment length
pipeDataQualityRatioLimit = 0.2 # data quality ratio to determine data reject threshold when there are too many NANs in a set
pipeAssessmentLengthCeiling = 500 # Averaging length ceiling limit in mm
pipeExternalPressure = 0 # External Pressure
pipeInternalPressure = 41.37E6 # Internal pressure
pipeTensionEffective = 0 # N
pipeMoment = 0 # Global bending moment in pipe
pipeYieldStrength = 551.5796E6 # Pa
AllowableStressFac = 0.666 # Ca Allowable stress factor 
SigmaA = AllowableStressFac*pipeYieldStrength  # Basic
DesignCaseFac = 1.5 #Cf  #Taken from Section4.4 Table-2 under Load Category-Survival  
condition = DesignCaseFac*SigmaA
print("condition : ",condition)


pipeMinimumID_m = pipeNominalOD_m -(2*pipeMinimumWT_m)

pipeA = (pi/4)*(pipeNominalOD_m**2 - pipeMinimumID_m**2)  # Area
print("pipeArea : ",pipeA)

pipeAi = (pi/4)*(pipeMinimumID_m**2)
print("piprAi : ",pipeAi)

pipeAo = (pi/4)*(pipeNominalOD_m**2)
print("piprAo : ",pipeAo)

pipeI = (pi/64)*(pipeNominalOD_m**4 - pipeMinimumID_m**4)  # Moment of Intertia
print("pipeIntertia : ",pipeI)

# True wall tension in pipe at section being analyzed (T)
pipeTensionTrue = pipeTensionEffective + (pipeInternalPressure*pipeAi) - (pipeExternalPressure*pipeAo)
print("pipeTensionTrue : ",pipeTensionTrue)

sigmaRadial = -((pipeExternalPressure*pipeNominalOD_m) + (pipeInternalPressure*pipeMinimumID_m))/(pipeNominalOD_m + pipeMinimumID_m)
print("sigmaRadial : ",sigmaRadial)

sigmaCircuferential = (pipeInternalPressure - pipeExternalPressure)*(pipeNominalOD_m /(2*pipeMinimumWT_m)) - pipeInternalPressure
print("sigmaCircuferential : ",sigmaCircuferential)


sigmaAxial_1 = ((pipeTensionTrue/pipeA) + (pipeMoment/(2*pipeI)))*(pipeNominalOD_m - pipeMinimumWT_m)
print("sigmaAxial_1 : ",sigmaAxial_1)

sigmaAxial_2 = ((pipeTensionTrue/pipeA) - (pipeMoment/(2*pipeI)))*(pipeNominalOD_m - pipeMinimumWT_m)
print("sigmaAxial_2 : ",sigmaAxial_2)

LHS_Squared1 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_2 - sigmaRadial)**2

print("LHS_Squared1 : ",LHS_Squared1)

VMStress = (1/math.sqrt(2))*(math.sqrt(LHS_Squared1))
print("VM Stress : ",VMStress)
