import math
import matplotlib.pyplot as plt
from dataManager.loadConfiguration import loadConfiguration

configParams = loadConfiguration('dataManager//APISTD2RD.ini')

def APISTD2RDCalculation(APISTD2RD):

    #Loading Calculation

    NominalID = APISTD2RD.NominalOD_1 - 2*APISTD2RD.NominalWT_1  # inside diameter
    print("NominalInsideDiameter : ","{0:.2f}".format(NominalID))

    MinimumID_m = APISTD2RD.NominalOD_1 -(2*APISTD2RD.NominalWT_1)
    print('APIRP_MinimumID_m : ',MinimumID_m )
    
    A = (math.pi/4)*(APISTD2RD.NominalOD_1**2 - MinimumID_m**2)  # Area
    print("CrossSectionalArea : ","{0:.2f}".format(A))

    Ai = (math.pi/4)*(MinimumID_m**2)
    print("InnerCrossSectionalArea : ","{0:.2f}".format(Ai))

    Ao = (math.pi/4)*(APISTD2RD.NominalOD_1**2)
    print("OuterCrossSectionalArea : ","{0:.2f}".format(Ao)) 

    Intertia = (math.pi/64)*(APISTD2RD.NominalOD_1**4 - MinimumID_m**4)  # Moment of Intertia
    print("Intertia : ","{0:.2f}".format(Intertia))

    SigmaA = APISTD2RD.AllowableStressFac*APISTD2RD.SMYS  # Basic
    print("AxialStress : ","{:.2f}".format(SigmaA))
    
    #Burst Pressure Check
   
    BurstPressure_1=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_1))
    print("BurstPressure_1 : ","{0:.2f}".format(BurstPressure_1))

    #Collapse Pressure Check

    #Yield collapse pressure Py

    YieldCollapsePressure_1= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1)
    #print("YieldCollapsePressure_1 : ","{0:.2f}".format(YieldCollapsePressure_1))

    #elastic collapse pressure Pel
    ElasticCollapsePressure_1= 2*APISTD2RD.E*(APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    #print("ElasticCollapsePressure_1 : ","{0:.2f}".format(ElasticCollapsePressure_1))
   
    #plastic collapse pressure Pp

    PlasticCollapsePressure_1=2*APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    #print("PlasticCollapsePressure_1 : ","{0:.2f}".format(PlasticCollapsePressure_1))
    
    #collapse pressure Pc

    CollapsePressure_1=YieldCollapsePressure_1*ElasticCollapsePressure_1/math.sqrt(YieldCollapsePressure_1**2+ElasticCollapsePressure_1**2)
    print("CollapsePressure_1 : ","{0:.2f}".format(CollapsePressure_1))
    
    #collapse pressure may alternatively be calculated as a function of the elastic capacity, plastic capacity and the ovality of the pipe as

    LHS=(CollapsePressure_1-ElasticCollapsePressure_1)*(CollapsePressure_1**2-PlasticCollapsePressure_1**2)
    RHS=CollapsePressure_1*ElasticCollapsePressure_1*PlasticCollapsePressure_1*2*APISTD2RD.ovality*(APISTD2RD.NominalOD_1/APISTD2RD.NominalWT_1)

    if LHS == RHS:
        CollapsePressureCheckPass = True
    else:
        CollapsePressureCheckPass = False

    #print("CollapsePressureCheckPass is: {}" .format(CollapsePressureCheckPass))

    #Tension
    Tension= (APISTD2RD.SMYS*A)
    print("Tension Ty = : ","{0:.2f}".format(Tension))
    
    AxialTension=SigmaA*A
    print("AxialTension Ta = : ","{0:.2f}".format(AxialTension))

    EffectiveTension=AxialTension-APISTD2RD.InternalPressure*Ai+APISTD2RD.ExternalPressure*Ao 
    print("EffectiveTension Te = : ","{0:.2f}".format(EffectiveTension))

    #Moment
    YieldMoment=(math.pi/4)*(APISTD2RD.SMYS*(APISTD2RD.NominalOD_1-APISTD2RD.NominalWT_1)**2*APISTD2RD.NominalWT_1)/10**6
    print("YieldMoment My = : ","{0:.2f}".format(YieldMoment))

    PlasticMoment=((4/math.pi)*YieldMoment)/10**3
    print("PlasticMoment Mp = : ","{0:.2f}".format(PlasticMoment))
    


    #Method-I Calculation
    #Method 1 limit check is met if the combined loads satisfy the following internal overpressure inequality

    Internal_OverPressure_M1=(APISTD2RD.Moment/YieldMoment)/math.sqrt((APISTD2RD.FD_1**2)-((APISTD2RD.InternalPressure-APISTD2RD.ExternalPressure)/BurstPressure_1)**2)-(EffectiveTension/Tension)
    print("Internal_OverPressure_M1 = : ","{0:.2f}".format(Internal_OverPressure_M1))

    External_OverPressure_M1=(APISTD2RD.Moment/YieldMoment)/math.sqrt((APISTD2RD.FD_1**2)-((APISTD2RD.ExternalPressure-APISTD2RD.InternalPressure)/CollapsePressure_1)**2)-(EffectiveTension/Tension)
    print("External_OverPressure_M1 = : ","{0:.2f}".format(External_OverPressure_M1))

    if Internal_OverPressure_M1 <= 1:
        Internal_OverPressure_M1CheckPass = True
    else:
        Internal_OverPressure_M1CheckPass = False

    print("Internal_OverPressure_M1CheckPass is: {}" .format(Internal_OverPressure_M1CheckPass))

    if External_OverPressure_M1 <= 1:
        External_OverPressure_M1CheckPass = True

    else:
        External_OverPressure_M1CheckPass = False

    print("External_OverPressure_M1CheckPass is: {}" .format(External_OverPressure_M1CheckPass))


    #Method-II Calculation
    #Method 2 limit check is met if the combined loads satisfy the following internal overpressure inequality

    Internal_OverPressure_M2=(APISTD2RD.Moment/YieldMoment)/math.sqrt(APISTD2RD.FD_1**2-(APISTD2RD.InternalPressure-APISTD2RD.ExternalPressure)/BurstPressure_1)**2*math.cos(math.pi/2*(EffectiveTension/Tension)/math.sqrt(APISTD2RD.FD_1**2-(APISTD2RD.InternalPressure-APISTD2RD.ExternalPressure)/BurstPressure_1)**2)
    print("Internal_OverPressure_M2 = : ","{0:.2f}".format(Internal_OverPressure_M2))

    External_OverPressure_M2=(APISTD2RD.Moment/YieldMoment)/math.sqrt(APISTD2RD.FD_1**2-(APISTD2RD.ExternalPressure-APISTD2RD.InternalPressure)/CollapsePressure_1)**2*math.cos(math.pi/2*(EffectiveTension/Tension)/math.sqrt(APISTD2RD.FD_1**2-(APISTD2RD.ExternalPressure-APISTD2RD.InternalPressure)/CollapsePressure_1)**2)
    print("External_OverPressure_M2 = : ","{0:.2f}".format(External_OverPressure_M2))

    if Internal_OverPressure_M2 <= 1:
        Internal_OverPressure_M2CheckPass = True
    else:
        Internal_OverPressure_M2CheckPass = False

    print("Internal_OverPressure_M2CheckPass is: {}" .format(Internal_OverPressure_M2CheckPass))

    if External_OverPressure_M2 <= 1:
        External_OverPressure_M2CheckPass = True
    else:
        External_OverPressure_M2CheckPass = False

    print("External_OverPressure_M2CheckPass is: {}" .format(External_OverPressure_M2CheckPass))


    #Method-IV Calculation

    #Method 4 checks for tension and the bending separately. Tension limit check is met if the combined loads satisfy the following internal overpressure inequality

    Method4=math.sqrt(((APISTD2RD.InternalPressure-APISTD2RD.ExternalPressure)/BurstPressure_1)**2+(EffectiveTension/Tension)**2/APISTD2RD.FD_2)
    print("Method4 is: {}" .format(Method4))

    if External_OverPressure_M2 <= 1:
        Method4CheckPass = True
    else:
        Method4CheckPass = False

    print("Method4CheckPass  is: {}" .format(Method4CheckPass))
    
    
APISTD2RDCalculation(configParams.APISTD2RD)    
