import math

# Relevant inputs
pi = 3.141592653589793
pipe1 = {'pipeNominalOD_m':0.24765,'pipeMinimumWT_m':0.034925,'pipeExternalPressure': 0,'pipeInternalPressure':0,'pipeTensionEffective':6674724,
         'pipeMoment':0,'pipeYieldStrength':551.5796E6}
  
##pipeNominalWT_m = 0.034925 # Nominal Wall Thickness
##pipeNominalID = pipeNominalOD_m - 2*pipeNominalWT_m  # inside diameter
##print("pipeNominalInsideDiameter : ",pipeNominalID)

AllowableStressFac = 0.666 # Ca Allowable stress factor 
SigmaA = AllowableStressFac*pipe1['pipeYieldStrength']  # Basic
print("SigmaA : ",SigmaA)
DesignCaseFac = 1.5 #Cf  #Taken from Section4.4 Table-2 under Load Category-Survival  
pipeMinimumID_m = pipe1['pipeNominalOD_m'] -(2*pipe1['pipeMinimumWT_m'])
print('pipeMinimumID_m : ',pipeMinimumID_m )

pipeA = (pi/4)*(pipe1['pipeNominalOD_m']**2 - pipeMinimumID_m**2)  # Area
print("pipeArea : ",pipeA)

pipeAi = (pi/4)*(pipeMinimumID_m**2)
print("pipeAi : ",pipeAi)

pipeAo = (pi/4)*(pipe1['pipeNominalOD_m']**2)
print("pipeAo : ",pipeAo)

pipeI = (pi/64)*(pipe1['pipeNominalOD_m']**4 - pipeMinimumID_m**4)  # Moment of Intertia
print("pipeIntertia : ",pipeI)

# True wall tension in pipe at section being analyzed (T)
pipeTensionTrue = pipe1['pipeTensionEffective'] + (pipe1['pipeInternalPressure']*pipeAi) - (pipe1['pipeExternalPressure']*pipeAo)
print("pipeTensionTrue : ",pipeTensionTrue)

sigmaRadial = -((pipe1['pipeExternalPressure']*pipe1['pipeNominalOD_m']) + (pipe1['pipeInternalPressure']*pipeMinimumID_m))/(pipe1['pipeNominalOD_m'] + pipeMinimumID_m)
print("sigmaRadial : ",sigmaRadial)

sigmaCircuferential = (pipe1['pipeInternalPressure'] - pipe1['pipeExternalPressure'])*(pipe1['pipeNominalOD_m'] /(2*pipe1['pipeMinimumWT_m'])) - pipe1['pipeInternalPressure']
print("sigmaCircuferential : ",sigmaCircuferential)


sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipe1['pipeMoment']/(2*pipeI))*(pipe1['pipeNominalOD_m'] - pipe1['pipeMinimumWT_m'])
print("sigmaAxial_1 : ",sigmaAxial_1)

sigmaAxial_2 = (pipeTensionTrue/pipeA) - (pipe1['pipeMoment']/(2*pipeI))*(pipe1['pipeNominalOD_m'] - pipe1['pipeMinimumWT_m'])
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

SigmaACf = DesignCaseFac*SigmaA  # R.H.S of Equation
print("Stress(R.H.S) in Pa : ",SigmaACf)
print("Stress(R.H.S) in ksi: ",SigmaACf*1.45038E-007)

