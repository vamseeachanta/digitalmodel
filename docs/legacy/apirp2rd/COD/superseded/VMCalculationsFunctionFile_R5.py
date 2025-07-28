
def VMStress():
    import math
    AllowableStressFac = 0.666 # Ca Allowable stress factor 
    DesignCaseFac = 1 #Cf #Taken from Section4.4 Table-2 under Load Category-Survival

    ## Importing class properties from VMStressCalculations_R8,py file
    from VMStressCalculations_R10 import pipe

    pipeNominalID = pipe.pipeNominalOD_m - 2*pipe.pipeNominalWT_m  # inside diameter
    print("pipeNominalInsideDiameter : ","{:.4e}".format(pipeNominalID))

    SigmaA = AllowableStressFac*pipe.pipeYieldStrength  # Basic
    print("SigmaA : ","{:.3e}".format(SigmaA))
    
    pipeMinimumID_m = pipe.pipeNominalOD_m -(2*pipe.pipeMinimumWT_m)
    print('pipeMinimumID_m : ',pipeMinimumID_m )
    
    pipeA = (math.pi/4)*(pipe.pipeNominalOD_m**2 - pipeMinimumID_m**2)  # Area
    print("pipeArea : ","{:.3e}".format(pipeA))

    pipeAi = (math.pi/4)*(pipeMinimumID_m**2)
    print("pipeAi : ","{:.3e}".format(pipeAi))

    pipeAo = (math.pi/4)*(pipe.pipeNominalOD_m**2)
    print("pipeAo : ","{:.3e}".format(pipeAo))

    pipeI = (math.pi/64)*(pipe.pipeNominalOD_m**4 - pipeMinimumID_m**4)  # Moment of Intertia
    print("pipeIntertia : ","{:.3e}".format(pipeI))

    # True wall tension in pipe at section being analyzed (T)
    pipeTensionTrue = pipe.pipeTensionEffective + (pipe.pipeInternalPressure*pipeAi) - (pipe.pipeExternalPressure*pipeAo)
##    print("pipeTensionTrue : ","{:.3e}".format(pipeTensionTrue))

    sigmaRadial = -((pipe.pipeExternalPressure*pipe.pipeNominalOD_m) + (pipe.pipeInternalPressure*pipeMinimumID_m))/(pipe.pipeNominalOD_m + pipeMinimumID_m)
##    print("sigmaRadial : ","{:.3e}".format(sigmaRadial))

    sigmaCircuferential = (pipe.pipeInternalPressure - pipe.pipeExternalPressure)*(pipe.pipeNominalOD_m /(2*pipe.pipeMinimumWT_m)) - pipe.pipeInternalPressure
##    print("sigmaCircuferential : ","{:.3e}".format(sigmaCircuferential))


    sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipe.pipeMoment/(2*pipeI))*(pipe.pipeNominalOD_m - pipe.pipeMinimumWT_m)
##    print("sigmaAxial_1 : ","{:.3e}".format(sigmaAxial_1))

    sigmaAxial_2 = (pipeTensionTrue/pipeA) - (pipe.pipeMoment/(2*pipeI))*(pipe.pipeNominalOD_m - pipe.pipeMinimumWT_m)
##    print("sigmaAxial_2 : ","{:.3e}".format(sigmaAxial_2))

    LHS_Squared_1 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_1 - sigmaRadial)**2
##    print("LHS_Squared_1 : ","{:.3e}".format(LHS_Squared_1))

    LHS_Squared_2 = (sigmaRadial - sigmaCircuferential)**2+(sigmaCircuferential - sigmaAxial_2)**2 + (sigmaAxial_2 - sigmaRadial)**2
##    print("LHS_Squared_2 : ","{:.3e}".format(LHS_Squared_2))

    VMStress_1_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_1))
    print("VMStress_1_Pa : ","{:.3e}".format(VMStress_1_Pa))

    VMStress_2_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_2))
    print("VMStress_2_Pa : ","{:.3e}".format(VMStress_2_Pa))

    VMStress_Pa = max(VMStress_2_Pa,VMStress_1_Pa)
    VMStress_MPa = VMStress_Pa * 1E-6
    VMStress_ksi = VMStress_Pa * 1.45038E-007
##    print("VM Stress in MPa: ","{0:.2f}".format(VMStress_MPa))
##    print("VM Stress in ksi: ","{0:.2f}".format(VMStress_ksi))

    SigmaACf = DesignCaseFac*SigmaA  # R.H.S of Equation
    print("Stress(R.H.S) in kPa : ","{0:.2f}".format(SigmaACf*1E-3))
    print("Stress(R.H.S) in ksi: ","{0:.2f}".format(SigmaACf*1.45038E-007))

    trueWallTensionLimit = SigmaACf*pipeA
    print("Tension Limit in kN : ","{0:.2f}".format(trueWallTensionLimit*1E-3))

    bendingMoment = DesignCaseFac*pipe.pipeYieldStrength*pipeI/(pipe.pipeNominalOD_m - pipe.pipeNominalWT_m)
    bendingMomentLimit = math.sqrt(bendingMoment)
    print("Bending Moment Limit in kN.m : ","{0:.2f}".format(bendingMomentLimit*1E-3))

    internalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((-pipeNominalID/(pipeNominalID + pipe.pipeNominalOD_m))-((pipe.pipeNominalOD_m/(2*pipe.pipeMinimumWT_m))-1))**2 +(((pipe.pipeNominalOD_m/(2*pipe.pipeMinimumWT_m))-1)-(pipeAi/pipeA))**2 +((pipeAi/pipeA)+(pipeNominalID/(pipeNominalID + pipe.pipeNominalOD_m)))**2)
    print("internal Pressure Limit in MPa : ","{0:.2f}".format(internalPressureLimit*1E-6))

    externalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((pipe.pipeNominalOD_m/(2*pipe.pipeMinimumWT_m))-(pipe.pipeNominalOD_m/(pipeNominalID + pipe.pipeNominalOD_m)))**2 +((pipe.pipeNominalOD_m/(2*pipe.pipeMinimumWT_m))+(pipeAo/pipeA))**2 +((pipeAo/pipeA)+(pipe.pipeNominalOD_m/(pipeNominalID + pipe.pipeNominalOD_m)))**2)
    print("external Pressure Limit in MPa : ","{0:.2f}".format(externalPressureLimit*1E-6))

    sigmaRadial_1 = -(pipe.pipeInternalPressure*pipeNominalID)/(pipe.pipeNominalOD_m+pipeNominalID)
    print("sigmaRadial_1 : ","{:.3f}".format(sigmaRadial_1))

    sigmaCircuferential_1 = ((pipe.pipeInternalPressure*pipe.pipeNominalOD_m)/(2*pipe.pipeMinimumWT_m))-pipe.pipeInternalPressure
    print("sigmaCircuferential_1 : ", "{:.3f}".format(sigmaCircuferential_1))

    #Tension R.H.S
    Tension_RHS = SigmaACf**2-(sigmaRadial_1 -sigmaCircuferential_1)**2-sigmaCircuferential_1**2-sigmaRadial_1**2
    print("Tension_RHS : ","{:.3f}".format(Tension_RHS))

    #Tension L.H.S
    Tension_LHS = (1/pipeA)**2+(1/pipeA)*((-sigmaRadial_1)-sigmaCircuferential_1)
    #Considering above eqation as quadratic eqn ax^2+bx and passing values to a,b and c value from  Tension_RHS
    a = (1/pipeA)**2
    b = (1/pipeA)*(sigmaRadial_1-sigmaCircuferential_1)
    c = -Tension_RHS
    Tension_positive = (-b+math.sqrt(b**2-(4*a*c)))/(2*a)
    print("Tension_positive : ","{:.3f}".format(Tension_positive)," N ")
    print("Tension_positive : ","{:.3f}".format(Tension_positive*1E-3)," kN ")
    Tension_negative = (-b-math.sqrt(b**2-(4*a*c)))/(2*a)
    print("Tension_negative : ","{:.3f}".format(Tension_negative)," N ")
    print("Tension_negative : ","{:.3f}".format(Tension_negative*1E-3)," kN ")

    #Moment R.H.S
    Moment_RHS = 2*(SigmaACf**2)-(sigmaRadial_1-sigmaCircuferential_1)**2-(sigmaCircuferential_1)**2-(sigmaRadial_1)**2
    print("Moment_RHS : ",Moment_RHS)

    #Moment L.H.S
##    Moment_LHS = 2*(Moment/(2*pipeI(pipe.pipeNominalOD_m-pipe.pipeMinimumWT_m)))**2+2*Moment*(((1/((2*pipeI)*(pipe.pipeNominalOD_m-pipe.pipeMinimumWT_m))*(sigmaRadial_1)))-(sigmaCircuferential_1*(1/((2*pipeI)*(pipe.pipeNominalOD_m-pipe.pipeMinimumWT_m)))))
    #Considering above eqation as quadratic eqn ax^2+bx and passing values to a1,b1 and c1 value from Moment_RHS
    a1 = ((1/(2*pipeI))*(pipe.pipeNominalOD_m-pipe.pipeMinimumWT_m))**2
    b1 = ((1/(2*pipeI))*(pipe.pipeNominalOD_m-pipe.pipeMinimumWT_m))*(sigmaRadial_1-sigmaCircuferential_1)
    c1 = -Moment_RHS/2
    Moment_positive = ((-b1+math.sqrt(b1**2-(4*a1*c1)))/(2*a1))
    print("Moment_positive : ","{:.3f}".format(Moment_positive)," Nm ")
    print("Moment_positive : ","{:.3f}".format(Moment_positive*1E-3)," kNm ")
    Moment_negative = ((-b1-math.sqrt(b1**2-(4*a1*c1)))/(2*a1))
    print("Moment_negative : ","{:.3f}".format(Moment_negative)," Nm ")
    print("Moment_negative : ","{:.3f}".format(Moment_negative*1E-3)," kNm ")
    
VMStress()

 
