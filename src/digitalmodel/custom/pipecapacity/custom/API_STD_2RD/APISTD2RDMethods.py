import logging
import math

import numpy as np
import pandas as pd


def APISTD2RDBurst(data):
# No corrosion allowance considered for test pressure
    Pb = 0.45*(data["S"] + data["U"])*math.log(data["OD"]/data["ID"])
    Pt = data["designFactors"]["internalPressure"]["hydroStaticTest"] * Pb
# Corrosion allowance considered for design and accidental pressure design
    Pb = 0.45*(data["S"] + data["U"])*math.log(data["OD"]/(data["ID"]+2*data["CorrosionAllowance"]))
    Pd = data["designFactors"]["internalPressure"]["design"] * Pb
    Pa = data["designFactors"]["internalPressure"]["incidentalPressure"] * Pb
    
    data.update({"Pb": Pb, "Pd": Pd, "Pa": Pa, "Pt": Pt})
    return(data)

def variableWTBurst(cfg):
    # Prepare input/output dataframe
    header_row = ['OD', 'ID', 't', 'Pd', 'Pa', 'Pt', 'Pb']
    dataDF = pd.DataFrame(columns = header_row)

    if cfg['geometry']['NominalOD'] !=None:
        for t in range(0, len(cfg['geometry']['NominalWT'])):
            newDFRow = [cfg['geometry']['NominalOD'],
                        cfg['geometry']['NominalOD'] - 2*cfg['geometry']['NominalWT'][t],
                        cfg['geometry']['NominalWT'][t], None, None, None, None]
            dataDF.loc[len(dataDF)] = newDFRow

    elif cfg['geometry']['NominalID'] !=None:
        for t in range(0, len(cfg['geometry']['NominalWT'])):
            newDFRow = [cfg['geometry']['NominalID'] + 2*cfg['geometry']['NominalWT'][t],
                        cfg['geometry']['NominalID'],
                        cfg['geometry']['NominalWT'][t], None, None, None, None]
            dataDF.loc[len(dataDF)] = newDFRow

    #  Run the program row by row
    for i in range(0,len(dataDF)):
        inputData = dataDF.loc[i].to_dict()
        inputData.update({"k": cfg['material']['k'], "S": cfg['material']['SMYS'],                          \
                "U": cfg['material']['SMUS'], "CorrosionAllowance": cfg['geometry']['CorrosionAllowance'],  \
                "designFactors": cfg['designFactors']                                                       \
                })
        output = APISTD2RDBurst(inputData)
        dataDF.loc[i]["Pd"] = output["Pd"]
        dataDF.loc[i]["Pa"] = output["Pa"]
        dataDF.loc[i]["Pt"] = output["Pt"]
        dataDF.loc[i]["Pb"] = output["Pb"]

    logging.critical(dataDF.to_string())
    return (cfg, dataDF)

def sectionProperties(data):
    A  = (math.pi/4)*(data["OD"]**2-data["ID"]**2)
    Ai = (math.pi/4)*(data["ID"]**2)
    Ao = (math.pi/4)*(data["OD"]**2)
    I = (math.pi/64)*(data["OD"]**4-data["ID"]**4)

    data.update({"A": A, "Ai": Ai, "Ao": Ao, "I": I})

    return data

def APISTD2RDCollapse(data):
#Collapse Pressure Check
    Py  = 2*data["S"]*(data["t"]/data["ID"])
    #print(data)
    Pel = 2*data["E"]*(data["t"]/data["ID"])**3/(1-data["Poissionsratio"]**2)
    Pp  = 2*(data["t"]/data["OD"])*data["S"]*data["alphafab"]
    Pc  = Py*Pel/math.sqrt(Py**2+Pel**2)

    data.update({"Py": Py, "Pel": Pel, "Pp": Pp, "Pc": Pc})
    return(data)

def APISTD2RDMethod1(data):
#check for internal over or external over pr if i.p to--
# Moment Used in method 1
    #SigmaA = (data["AllowableStressFac"])*(data["S"])
# Tension Check
    Ty = (data["S"]*data["A"])/1000*4.448222 #kN
    #Ta = SigmaA*data["A"]/1000
    #Te = (Ta-data["internalPressure"]*data["Ai"]+data["externalPressure"]*data["Ao"])/1000 # Unit lbs
    My = math.pi/4*(data["S"]*(data["OD"]-data["t"])**2*data["t"])/1000/12*1.355818 # Unit kN.m
    Mp = (4/math.pi)*My
       
#Method 1 Calculation
# Action: Check if internal overpresssure
    FDPressureCorrected = math.sqrt((data["designFactors"]["internalPressure"][data['LimitState']]**2)-((data["internalPressure"]-data["externalPressure"])/data["Pb"])**2)
    Tlimiting = (FDPressureCorrected*Ty)
#Method1 calculation
    Mlimiting1 = FDPressureCorrected*My 

#Method2 calculation
    Mlimiting2 = (FDPressureCorrected*Mp)*math.cos(0)

#Method 4 Calculation
    TensionLimitCheck=(math.sqrt(((data["internalPressure"]-data["externalPressure"])/data["Pb"])**2)+(Tlimiting/Ty)**2)/(data["designFactors"]["internalPressure"][data['LimitState']])
    if TensionLimitCheck <= 1:
        print ("TensionLimitCheckPass = True")
    else:
        print ("TensionLimitCheckPass = False")

# Action:  Convert to Data Frame TypeError
#1 indicates Method1 calculation
#2 indicates Method2 calculation
# Method 1& 2 plotting calculation

    TArray = np.arange(-Tlimiting, Tlimiting, 1)
    #print(TArray)
    MarrayPositive1 = [] 
    MarrayNegative1 = [] 
    MarrayPositive2 = [] 
    MarrayNegative2 = [] 

    for i in range(0, len(TArray)):
        MarrayPositive1.append(My*(FDPressureCorrected - abs(TArray[i]/Ty)))
        MarrayNegative1.append(-My*(FDPressureCorrected - abs(TArray[i]/Ty)))

    #Method 2 Plotting
        MarrayPositive2.append(Mp*(FDPressureCorrected)*math.cos((math.pi/2)*((TArray[i]/Ty)/FDPressureCorrected)))
        MarrayNegative2.append(-Mp*(FDPressureCorrected)*math.cos((math.pi/2)*((TArray[i]/Ty)/FDPressureCorrected)))

    resultMatrix = np.stack((TArray, MarrayPositive1, MarrayNegative1,MarrayPositive2, MarrayNegative2) , axis=-1)
    resultDF = pd.DataFrame(resultMatrix, columns = ['TArray','MarrayPositive1', 'MarrayNegative1','MarrayPositive2', 'MarrayNegative2'])

    return(data, resultDF)

def nominalWTAPISTD2RDMethod1(cfg, inputData):
    
    inputData = sectionProperties(inputData)
    inputData = APISTD2RDBurst(inputData)
    inputData = APISTD2RDCollapse(inputData)
    resultData, resultDF = APISTD2RDMethod1(inputData)
    
    return (resultData, resultDF)

def APISTD2RDMethod1Utilization(cfg):
    #check for internal over or external over pr if i.p to--
# 
    Ty = (cfg['Method1SLS']["S"]*cfg['Method1SLS']["A"])/1000*4.448222   # Unit kN
    My = math.pi/4*(cfg['Method1SLS']["S"]*(cfg['Method1SLS']["OD"]-cfg['Method1SLS']["t"])**2*cfg['Method1SLS']["t"])/1000/12*1.355818 # Unit kN


    for i in range(0, len(cfg['LoadingConditionResults'])):
        loadCategory = cfg['LoadingConditionResults'][i]['LoadCategory']

        if (cfg['LoadingConditionResults'][i]['InternalPressure'] >= cfg['LoadingConditionResults'][i]['ExternalPressure']):
            FDPressureCorrected = math.sqrt((cfg["designFactors"]["internalPressure"][loadCategory]**2)-((cfg['LoadingConditionResults'][i]['InternalPressure']-cfg['LoadingConditionResults'][i]['ExternalPressure'])/cfg['Method1SLS']['Pb'])**2)
            cfg['LoadingConditionResults'][i]['PressureCondition']= "Internal OverPressure"
        else:
            FDPressureCorrected = math.sqrt((cfg["designFactors"]["externalPressure"][loadCategory]**2)-((cfg['LoadingConditionResults'][i]['InternalPressure']-cfg['LoadingConditionResults'][i]['ExternalPressure'])/cfg['Method1SLS']['Pc'])**2)
            cfg['LoadingConditionResults'][i]['PressureCondition']= "Internal OverPressure"


        LHSTensionTerm = cfg['LoadingConditionResults'][0]['Tension']/Ty
        LHSBendingTerm = cfg['LoadingConditionResults'][0]['BendingMoment']/My
        LHS = abs(LHSTensionTerm) + abs(LHSBendingTerm)
        Utilization = LHS/FDPressureCorrected
        cfg['LoadingConditionResults'][i]['Method1Utilization']= Utilization
        cfg['LoadingConditionResults'][i]['DesignFactor']= cfg["designFactors"]["internalPressure"][loadCategory]

    return(cfg)
