import math
import matplotlib.pyplot as plt
from scipy.interpolate import spline
import numpy as np

from math import *
from dataManager.loadConfiguration import loadConfiguration

configParams = loadConfiguration('dataManager//APIRP2RD.ini')

def APIRP2RDCalculation(APIRP2RD):
    from APIRP2RD_Calculations import APIRP1, APIRP2,APIRP3,APIRP4,APIRP5,APIRP6,APIRP7,APIRP8

    ## APIRP Calculation

    APIRPNominalID = APIRP2RD.APIRPNominalOD_m - 2*APIRP2RD.APIRPNominalWT_m  # inside diameter
    print("APIRP_NominalInsideDiameter : ","{:.4e}".format(APIRPNominalID))

    APIRPMinimumID_m = APIRP2RD.APIRPNominalOD_m -(2*APIRP2RD.APIRPMinimumWT_m)
    print('APIRP_MinimumID_m : ',APIRPMinimumID_m )
    
    APIRPA = (math.pi/4)*(APIRP2RD.APIRPNominalOD_m**2 - APIRPMinimumID_m**2)  # Area
    print("APIRP_Area : ","{:.3e}".format(APIRPA))

    APIRPAi = (math.pi/4)*(APIRPMinimumID_m**2)
    print("APIRP_Ai : ","{:.3e}".format(APIRPAi))

    APIRPAo = (math.pi/4)*(APIRP2RD.APIRPNominalOD_m**2)
    print("APIRP_Ao : ","{:.3e}".format(APIRPAo)) 

    APIRPI = (math.pi/64)*(APIRP2RD.APIRPNominalOD_m**4 - APIRPMinimumID_m**4)  # Moment of Intertia
    print("APIRP_Intertia : ","{:.3e}".format(APIRPI))

    APIRPTensionTrueT=APIRP2RD.APIRPTensionEffective+(APIRP2RD.APIRPInternalPressure*APIRPAi)-(APIRP2RD.APIRPExternalPressure*APIRPAo)
    print("APIRP_TensionTrue_T : ","{0:.2f}".format(APIRPTensionTrueT))
    
    #Stress Calculation

    
    SigmaA = APIRP2RD.AllowableStressFac*APIRP2RD.APIRPYieldStrength  # Basic
    print("SigmaA : ","{:.3e}".format(SigmaA))

    SigmaACf = APIRP2RD.DesignCaseFac*SigmaA  # R.H.S of Equation
    #print("SigmaACf in ksi  : ","{:.3e}".format(SigmaACf))
    #print("SigmaACf in pa  : ","{:.3e}".format(SigmaACf*6894757.293178))
    print("Stress(R.H.S) in kPa : ","{0:.2f}".format(SigmaACf*1E-3))
    print("Stress(R.H.S) in ksi: ","{0:.2f}".format(SigmaACf*1.45038E-07))
    
    sigmaRadial_1 = -((APIRP2RD.APIRPExternalPressure*APIRP2RD.APIRPNominalOD_m)+(APIRP2RD.APIRPInternalPressure*APIRPNominalID))/(APIRP2RD.APIRPNominalOD_m+APIRPNominalID)
    print("sigmaRadial_1 : ","{:.3f}".format(sigmaRadial_1))

    sigmaCircumferential_1 = (((APIRP2RD.APIRPInternalPressure-APIRP2RD.APIRPExternalPressure)*((APIRP2RD.APIRPNominalOD_m)/(2*APIRP2RD.APIRPMinimumWT_m)))-APIRP2RD.APIRPInternalPressure)
    print("sigmaCircumferential_1 : ", "{:.3f}".format(sigmaCircumferential_1))

    sigmaAxial_1 = (APIRPTensionTrueT/APIRPA)+((APIRP2RD.APIRPMoment)/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m-APIRP2RD.APIRPMinimumWT_m)
    print("sigmaAxial_1 : ","{:.3e}".format(sigmaAxial_1))

    sigmaAxial_2 = (APIRPTensionTrueT/APIRPA)-((APIRP2RD.APIRPMoment)/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m-APIRP2RD.APIRPMinimumWT_m)
    print("sigmaAxial_2 : ","{:.3e}".format(sigmaAxial_2))

    AllowableStress=((sigmaRadial_1-sigmaCircumferential_1)**2)+((sigmaCircumferential_1-sigmaAxial_1)**2)+((sigmaAxial_1-sigmaRadial_1)**2)
    #print("AllowableStress : ","{:.3e}".format((1/sqrt(2))*(sqrt(AllowableStress))))
    #print("AllowableStress_LHS : ","{:.2f}".format((1/sqrt(2))*(sqrt(AllowableStress)*0.000000145037738007)))

    LHS_Squared_1 = (sigmaRadial_1 - sigmaCircumferential_1)**2+(sigmaCircumferential_1 - sigmaAxial_2)**2 + (sigmaAxial_1 - sigmaRadial_1)**2
    #print("LHS_Squared_1 : ","{:.3e}".format(LHS_Squared_1))

    LHS_Squared_2 = (sigmaRadial_1 - sigmaCircumferential_1)**2+(sigmaCircumferential_1 - sigmaAxial_2)**2 + (sigmaAxial_2 - sigmaRadial_1)**2
    #print("LHS_Squared_2 : ","{:.3e}".format(LHS_Squared_2))

    VMStress_1_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_1))
    print("VMStress_1_Pa : ","{:.3e}".format(VMStress_1_Pa))

    VMStress_2_Pa = (1/math.sqrt(2))*(math.sqrt(LHS_Squared_2))
    print("VMStress_2_Pa : ","{:.3e}".format(VMStress_2_Pa))

    VMStress_Pa = max(VMStress_2_Pa,VMStress_1_Pa)
    VMStress_MPa = VMStress_Pa * 1E-6
    VMStress_ksi = VMStress_Pa * 1.45038E-007

    trueWallTensionLimit = SigmaACf*APIRPA
    print("Tension Limit in kN : ","{0:.2f}".format(trueWallTensionLimit*1E-3))

    bendingMoment = APIRP2RD.DesignCaseFac*APIRP2RD.APIRPYieldStrength*APIRPI/(APIRP2RD.APIRPNominalOD_m - APIRP2RD.APIRPNominalWT_m)
    bendingMomentLimit = math.sqrt(bendingMoment)
    print("Bending Moment Limit in kN.m : ","{0:.2f}".format(bendingMomentLimit*1E-3))

    internalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((-APIRPNominalID/(APIRPNominalID + APIRP2RD.APIRPNominalOD_m))-((APIRP2RD.APIRPNominalOD_m/(2*APIRP2RD.APIRPMinimumWT_m))-1))**2 +(((APIRP2RD.APIRPNominalOD_m/(2*APIRP2RD.APIRPMinimumWT_m))-1)-(APIRPAi/APIRPA))**2 +((APIRPAi/APIRPA)+(APIRPNominalID/(APIRPNominalID + APIRP2RD.APIRPNominalOD_m)))**2)
    print("internal Pressure Limit in MPa : ","{0:.2f}".format(internalPressureLimit*1E-6))

    externalPressureLimit = (SigmaACf*math.sqrt(2))/math.sqrt(((APIRP2RD.APIRPNominalOD_m/(2*APIRP2RD.APIRPMinimumWT_m))-(APIRP2RD.APIRPNominalOD_m/(APIRPNominalID + APIRP2RD.APIRPNominalOD_m)))**2 +((APIRP2RD.APIRPNominalOD_m/(2*APIRP2RD.APIRPMinimumWT_m))+(APIRPAo/APIRPA))**2 +((APIRPAo/APIRPA)+(APIRP2RD.APIRPNominalOD_m/(APIRPNominalID + APIRP2RD.APIRPNominalOD_m)))**2)
    print("external Pressure Limit in MPa : ","{0:.2f}".format(externalPressureLimit*1E-6))


    Tension_LHS = 2*(((APIRP2RD.APIRPMoment/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m-APIRP2RD.APIRPMinimumWT_m))**2)
    #Tension_LHS = 2*(((APIRP2RD.APIRPMoment/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m- APIRP2RD.APIRPMinimumWT_m))**2)
    #print("Tension_LHS_"+str(count)+" : ","{:.2f}".format(Tension_LHS))

    Tension_RHS = (2*(SigmaACf**2)-(sigmaRadial_1 -sigmaCircumferential_1)**2-Tension_LHS)/2
    #print("Tension_RHS_"+str(count)+" : ","{:.2f}".format(Tension_RHS))


    ##    #calculating the Equation
    
    count =0
    x = range(8)
    list1 = [APIRP1, APIRP2,APIRP3,APIRP4,APIRP5,APIRP6,APIRP7,APIRP8]
    TensionList1 = []
    TensionList2 = []
    BendMomentList = []
    for i in list1:
        count = 1+count
        print("BendingMoment_APIRP"+str(count)+" : ","{:.2f}".format(i.APIRPMoment))
        #print("BendingMoment_APIRP"+str(count)+" : ","{:.2f}".format(APIRP2RD.APIRPMoment))

        Tension_LHS = 2*(((i.APIRPMoment/(2*APIRPI))*(i.APIRPNominalOD_m-i.APIRPMinimumWT_m))**2)
        #Tension_LHS = 2*(((APIRP2RD.APIRPMoment/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m- APIRP2RD.APIRPMinimumWT_m))**2)
        print("Tension_LHS_"+str(count)+" : ","{:.2f}".format(Tension_LHS))

        Tension_RHS = (2*(SigmaACf**2)-(sigmaRadial_1 -sigmaCircumferential_1)**2-Tension_LHS)/2
        print("Tension_RHS_"+str(count)+" : ","{:.2f}".format(Tension_RHS))


        a = (1/APIRPA)**2
        b = (4*((i.APIRPMoment/(2*APIRPI))*(APIRP2RD.APIRPNominalOD_m- APIRP2RD.APIRPMinimumWT_m)))*(1/APIRPA)
        c = -Tension_RHS
        Tension_positive1 = (-b+(math.sqrt(b**2-(4*a*c))))/(2*a)
        Tension_negative1 = (-b-(math.sqrt(b**2-(4*a*c))))/(2*a)
        
        TensionList1.append(Tension_positive1/1000)
        TensionList2.append(Tension_negative1/1000)
        BendMomentList.append(i.APIRPMoment/1000)

    #print(TensionList1)
    #print(TensionList2)
    print(BendMomentList)

    x1 = [-a for a in BendMomentList]
    x2 = x1[::-1] # Reversing x1 values
    x3 = BendMomentList
    x4 = x3[::-1]
    x_axis = []
    x_axis += x1+x2+x3+x4  # Combining all values in one set like x1,x2,x3 and x4
    print(x_axis)

    y1 = TensionList1
    y2 = y1[::-1]
    y2 = [-b for b in y2]
    y3 = [-c for c in y1]
    y4 = y1[::-1]
    y_axis = []
    y_axis += y1+y2+y3+y4
    print(y_axis)
    

    #it creates plot tittle, color and size
    plt.title('Effective Tension vs Bending Moment plot', fontsize=14, fontweight='bold', color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Bending Moment (kips-ft)', fontsize=12, fontweight='bold', color='black') 
    #it creates plot Y-axis name, fontsize and colour             
    plt.ylabel('Effective Tension  (kips)', fontsize=12, fontweight='bold', color='black')
    #it converted Static max.effective tension value KN to Te and it plots these values along with arc length
    plt.plot(x_axis, y_axis)
    plt.savefig("Tension vs Moment.png",dpi=800)

    plt.xlim(0, 1.1*max(x3))
    plt.ylim(0, 1.1*max(y1))
    plt.plot(x_axis, y_axis)
    plt.savefig("Tension vs Moment_Quad1.png",dpi=800)

APIRP2RDCalculation(configParams.APIRP2RD)

