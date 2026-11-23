import math

# Relevant inputs
pi = 3.141592653589793
pipeNominalOD_m = 0.288925  # outside diameter  
pipeNominalWT_m = 0.0508 # Nominal Wall Thickness
pipeMinimumWT_m = 0.0508 # Pipe wall tickness
pipeNominalID = pipeNominalOD_m - 2*pipeNominalWT_m  # inside diameter
print("pipeNominalInsideDiameter : ",pipeNominalID)

pipeExternalPressure = 0 # External Pressure
pipeInternalPressure = 1.03E+08 # Internal pressure
pipeTensionEffective = 556227 # N
pipeMoment = 7679.624316 # Global bending moment in pipe

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
print("pipeAi : ",pipeAi)

pipeAo = (pi/4)*(pipeNominalOD_m**2)
print("pipeAo : ",pipeAo)

pipeI = (pi/64)*(pipeNominalOD_m**4 - pipeMinimumID_m**4)  # Moment of Intertia
print("pipeIntertia : ",pipeI)

# True wall tension in pipe at section being analyzed (T)
pipeTensionTrue = pipeTensionEffective + (pipeInternalPressure*pipeAi) - (pipeExternalPressure*pipeAo)
print("pipeTensionTrue : ",pipeTensionTrue)

sigmaRadial = -((pipeExternalPressure*pipeNominalOD_m) + (pipeInternalPressure*pipeMinimumID_m))/(pipeNominalOD_m + pipeMinimumID_m)
print("sigmaRadial : ",sigmaRadial)

sigmaCircuferential = (pipeInternalPressure - pipeExternalPressure)*(pipeNominalOD_m /(2*pipeMinimumWT_m)) - pipeInternalPressure
print("sigmaCircuferential : ",sigmaCircuferential)


sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipeMoment/(2*pipeI))*(pipeNominalOD_m - pipeMinimumWT_m)
print("sigmaAxial_1 : ",sigmaAxial_1)

sigmaAxial_2 = (pipeTensionTrue/pipeA) - (pipeMoment/(2*pipeI))*(pipeNominalOD_m - pipeMinimumWT_m)
print("sigmaAxial_2 : ",sigmaAxial_2)

LHS_Squared_1 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_1 - sigmaRadial)**2

LHS_Squared_2 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_2 - sigmaRadial)**2
print("LHS_Squared_1: ",LHS_Squared_1)

VMStress_1_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_1))
print("VM Stress 1 in Pa: ",VMStress_1_Pa)
print("VM Stress 1 in ksi: ",VMStress_1_Pa*1.45038E-007)


VMStress_2_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_2))
print("VM Stress 2 in Pa: ",VMStress_2_Pa)
print("VM Stress 2 in ksi: ",VMStress_2_Pa*1.45038E-007)
