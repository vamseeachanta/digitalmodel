import math 

AllowableStressFac = 0.666 # Ca Allowable stress factor 
DesignCaseFac = 1.5 #Cf #Taken from Section4.4 Table-2 under Load Category-Survival
   
def VMStress(pipe1):
    ## Importing class properties from VMStressCalculations_R7,py file
    from VMStressCalculations_R7 import ClassPipe
    
    pipeNominalID = pipe1.pipeNominalOD_m - 2*pipe1.pipeNominalWT_m  # inside diameter
    print("pipeNominalInsideDiameter : ","{:.4e}".format(pipeNominalID))

    SigmaA = AllowableStressFac*pipe1.pipeYieldStrength  # Basic
    print("SigmaA : ","{:.3e}".format(SigmaA))
    
    pipeMinimumID_m = pipe1.pipeNominalOD_m -(2*pipe1.pipeMinimumWT_m)
    print('pipeMinimumID_m : ',pipeMinimumID_m )
    
    pipeA = (math.pi/4)*(pipe1.pipeNominalOD_m**2 - pipeMinimumID_m**2)  # Area
    print("pipeArea : ","{:.3e}".format(pipeA))

    pipeAi = (math.pi/4)*(pipeMinimumID_m**2)
    print("pipeAi : ","{:.3e}".format(pipeAi))

    pipeAo = (math.pi/4)*(pipe1.pipeNominalOD_m**2)
    print("pipeAo : ","{:.3e}".format(pipeAo))

    pipeI = (math.pi/64)*(pipe1.pipeNominalOD_m**4 - pipeMinimumID_m**4)  # Moment of Intertia
    print("pipeIntertia : ","{:.3e}".format(pipeI))

    # True wall tension in pipe at section being analyzed (T)
    pipeTensionTrue = pipe1.pipeTensionEffective + (pipe1.pipeInternalPressure*pipeAi) - (pipe1.pipeExternalPressure*pipeAo)
    print("pipeTensionTrue : ","{:.3e}".format(pipeTensionTrue))

    sigmaRadial = -((pipe1.pipeExternalPressure*pipe1.pipeNominalOD_m) + (pipe1.pipeInternalPressure*pipeMinimumID_m))/(pipe1.pipeNominalOD_m + pipeMinimumID_m)
    print("sigmaRadial : ","{:.3e}".format(sigmaRadial))

    sigmaCircuferential = (pipe1.pipeInternalPressure - pipe1.pipeExternalPressure)*(pipe1.pipeNominalOD_m /(2*pipe1.pipeMinimumWT_m)) - pipe1.pipeInternalPressure
    print("sigmaCircuferential : ","{:.3e}".format(sigmaCircuferential))


    sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipe1.pipeMoment/(2*pipeI))*(pipe1.pipeNominalOD_m - pipe1.pipeMinimumWT_m)
    print("sigmaAxial_1 : ","{:.3e}".format(sigmaAxial_1))

    sigmaAxial_2 = (pipeTensionTrue/pipeA) - (pipe1.pipeMoment/(2*pipeI))*(pipe1.pipeNominalOD_m - pipe1.pipeMinimumWT_m)
    print("sigmaAxial_2 : ","{:.3e}".format(sigmaAxial_2))

    LHS_Squared_1 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_1 - sigmaRadial)**2
    print("LHS_Squared_1 : ","{:.3e}".format(LHS_Squared_1))

    LHS_Squared_2 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_2 - sigmaRadial)**2
    print("LHS_Squared_2 : ","{:.3e}".format(LHS_Squared_2))

    VMStress_1_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_1))
    print("VMStress_1_Pa : ","{:.3e}".format(VMStress_1_Pa))

    VMStress_2_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_2))
    print("VMStress_2_Pa : ","{:.3e}".format(VMStress_2_Pa))

    VMStress_Pa = max(VMStress_2_Pa,VMStress_1_Pa)
    VMStress_MPa = VMStress_Pa * 1E-6
    VMStress_ksi = VMStress_Pa * 1.45038E-007
    print("VM Stress in MPa: ","{0:.2f}".format(VMStress_MPa))
    print("VM Stress in ksi: ","{0:.2f}".format(VMStress_ksi))

    SigmaACf = DesignCaseFac*SigmaA  # R.H.S of Equation
    print("Stress(R.H.S) in MPa : ","{0:.2f}".format(SigmaACf*1E-6))
    print("Stress(R.H.S) in ksi: ","{0:.2f}".format(SigmaACf*1.45038E-007))

    trueWallTensionLimit = DesignCaseFac*pipe1.pipeYieldStrength*pipeA
    print("Tension Limit in kN : ","{0:.2f}".format(trueWallTensionLimit*1E-3))

    bendingMomentLimit = DesignCaseFac*pipe1.pipeYieldStrength*2*pipeI/(pipe1.pipeNominalOD_m - pipe1.pipeNominalWT_m)
    print("Bending Moment Limit in kN.m : ","{0:.2f}".format(bendingMomentLimit*1E-3))

    internalPressureLimit = DesignCaseFac*pipe1.pipeYieldStrength*math.sqrt(2)/math.sqrt( (pipeNominalID/(pipeNominalID + pipe1.pipeNominalOD_m))**2 +(pipe1.pipeNominalOD_m/2/pipe1.pipeNominalWT_m)**2 +((pipeNominalID/(pipeNominalID + pipe1.pipeNominalOD_m))-(pipe1.pipeNominalOD_m/2/pipe1.pipeNominalWT_m) )**2 )
    print("Internal Pressure Limit in MPa : ","{0:.2f}".format(internalPressureLimit*1E-6))
    f = open(pipe1.outputFileName,'w')
    f.write('von Mises Stress is : ')
    f.write(str(VMStress_ksi))#
	
