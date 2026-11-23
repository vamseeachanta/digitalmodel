
def VMStress():
    import math
    import matplotlib.pyplot as pyplot
    AllowableStressFac = 0.666 # Ca Allowable stress factor 
    DesignCaseFac = 1 #Cf #Taken from Section4.4 Table-2 under Load Category-Survival

    ## Importing class properties from VMStressCalculations_R8,py file
    from VMStressCalculations_R12 import pipe1, pipe2,pipe3,pipe4,pipe5,pipe6,pipe7,pipe8

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
##    print("pipeTensionTrue : ","{:.3e}".format(pipeTensionTrue))

    sigmaRadial = -((pipe1.pipeExternalPressure*pipe1.pipeNominalOD_m) + (pipe1.pipeInternalPressure*pipeMinimumID_m))/(pipe1.pipeNominalOD_m + pipeMinimumID_m)
##    print("sigmaRadial : ","{:.3e}".format(sigmaRadial))

    sigmaCircuferential = (pipe1.pipeInternalPressure - pipe1.pipeExternalPressure)*(pipe1.pipeNominalOD_m /(2*pipe1.pipeMinimumWT_m)) - pipe1.pipeInternalPressure
##    print("sigmaCircuferential : ","{:.3e}".format(sigmaCircuferential))


    sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipe1.pipeMoment/(2*pipeI))*(pipe1.pipeNominalOD_m - pipe1.pipeMinimumWT_m)
##    print("sigmaAxial_1 : ","{:.3e}".format(sigmaAxial_1))

    sigmaAxial_2 = (pipeTensionTrue/pipeA) - (pipe1.pipeMoment/(2*pipeI))*(pipe1.pipeNominalOD_m - pipe1.pipeMinimumWT_m)
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

    bendingMoment = DesignCaseFac*pipe1.pipeYieldStrength*pipeI/(pipe1.pipeNominalOD_m - pipe1.pipeNominalWT_m)
    bendingMomentLimit = math.sqrt(bendingMoment)
    print("Bending Moment Limit in kN.m : ","{0:.2f}".format(bendingMomentLimit*1E-3))

    internalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((-pipeNominalID/(pipeNominalID + pipe1.pipeNominalOD_m))-((pipe1.pipeNominalOD_m/(2*pipe1.pipeMinimumWT_m))-1))**2 +(((pipe1.pipeNominalOD_m/(2*pipe1.pipeMinimumWT_m))-1)-(pipeAi/pipeA))**2 +((pipeAi/pipeA)+(pipeNominalID/(pipeNominalID + pipe1.pipeNominalOD_m)))**2)
    print("internal Pressure Limit in MPa : ","{0:.2f}".format(internalPressureLimit*1E-6))

    externalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((pipe1.pipeNominalOD_m/(2*pipe1.pipeMinimumWT_m))-(pipe1.pipeNominalOD_m/(pipeNominalID + pipe1.pipeNominalOD_m)))**2 +((pipe1.pipeNominalOD_m/(2*pipe1.pipeMinimumWT_m))+(pipeAo/pipeA))**2 +((pipeAo/pipeA)+(pipe1.pipeNominalOD_m/(pipeNominalID + pipe1.pipeNominalOD_m)))**2)
    print("external Pressure Limit in MPa : ","{0:.2f}".format(externalPressureLimit*1E-6))

    sigmaRadial_1 = -((pipe1.pipeExternalPressure*pipe1.pipeNominalOD_m)+(pipe1.pipeInternalPressure*pipeNominalID))/(pipe1.pipeNominalOD_m+pipeNominalID)
    print("sigmaRadial_1 : ","{:.3f}".format(sigmaRadial_1))

    sigmaCircuferential_1 = (((pipe1.pipeInternalPressure-pipe1.pipeExternalPressure)*pipe1.pipeNominalOD_m)/(2*pipe1.pipeMinimumWT_m))-pipe1.pipeInternalPressure
    print("sigmaCircuferential_1 : ", "{:.3f}".format(sigmaCircuferential_1))
    
    #sigmaAxial_1 = (pipeTensionTrue/pipeA) + (pipe1.pipeMoment/(2*pipeI))*(pipe1.pipeNominalOD_m - pipe1.pipeMinimumWT_m)
    #print("sigmaAxial_1 : ","{:.3e}".format(sigmaAxial_1))
    count = 0
    x = range(8)
    list1 = [pipe1, pipe2,pipe3,pipe4,pipe5,pipe6,pipe7,pipe8]
    TensionList = []
    BendMomentList = []
    for i in list1:
        count = 1+count
        print("BendingMoment_pipe"+str(count)+" : ","{:.2f}".format(i.pipeMoment))
        
        Tension_LHS = (2*(((i.pipeMoment/(2*pipeI))*(i.pipeNominalOD_m- i.pipeMinimumWT_m))**2))
        print("Tension_LHS_"+str(count)+" : ","{:.2f}".format(Tension_LHS))

        Tension_RHS = (2*SigmaACf**2)-(sigmaRadial_1 -sigmaCircuferential_1)**2-Tension_LHS
        print("Tension_RHS_"+str(count)+" : ","{:.2f}".format(Tension_RHS))

        Tension1 = math.sqrt((Tension_RHS*pipeA**2)/2)
        print("Tension_"+str(count)+" : ","{:.2f}".format(Tension1)," N ")
        print("Tension_"+str(count)+" : ","{:.2f}".format(Tension1*1E-3)," kN ")
        TensionList.append((Tension1/1000)/4.448)
        BendMomentList.append((i.pipeMoment/1000)*.74)

    #print(TensionList)
    print(BendMomentList)
    #print(-Tension1)
    #print(pipe1.pipeMoment)
    x1 = [-a for a in BendMomentList]
    x2 = x1[::-1] # Reversing x1 values
    x3 = BendMomentList
    x4 = x3[::-1]
    x_axis = []
    x_axis += x1+x2+x3+x4  # Combining all values in one set like x1,x2,x3 and x4
    print(x_axis)

    y1 = TensionList
    y2 = y1[::-1]
    y2 = [-b for b in y2]
    y3 = [-c for c in y1]
    y4 = y1[::-1]
    y_axis = []
    y_axis += y1+y2+y3+y4
    print(y_axis)

    #it creates plot tittle, color and size
    pyplot.title('Effective Tension vs Bending Moment plot', fontsize=14, fontweight='bold', color='black')
    #it creates plot X-axis name, fontsize and colour
    pyplot.xlabel('Bending Moment (kips-ft)', fontsize=12, fontweight='bold', color='black') 
    #it creates plot Y-axis name, fontsize and colour             
    pyplot.ylabel('Effective Tension  (kips)', fontsize=12, fontweight='bold', color='black')
    #it converted Static max.effective tension value KN to Te and it plots these values along with arc length
    pyplot.plot(x_axis, y_axis)
    pyplot.savefig("Tension vs Moment.png",dpi=800)
    pyplot.show()
    #Tension R.H.S
##    Tension_RHS1 = (2*SigmaACf**2)-(sigmaRadial_1 -sigmaCircuferential_1)**2-Tension_LHS1
##    print("Tension_RHS1 : ","{:.3f}".format(Tension_RHS1))

VMStress()
