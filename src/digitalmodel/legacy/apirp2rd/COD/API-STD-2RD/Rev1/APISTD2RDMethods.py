import math
import logging
import pandas as pd
import numpy as np

def APISTD2RDBurst(data):
# No corrosion allowance considered for test pressure
    Pb = 0.45*(data["S"] + data["U"])*math.log(data["OD"]/data["ID"])
    Pt = data["designFactors"]["internalPressure"]["hydroStaticTest"] * Pb
# Corrosion allowance considered for design and accidental pressure design
    Pb = 0.45*(data["S"] + data["U"])*math.log(data["OD"]/(data["ID"]+2*data["CorrosionAllowance"]))
    Pd = data["designFactors"]["internalPressure"]["design"] * Pb
    Pa = data["designFactors"]["internalPressure"]["incidentalPressure"] * Pb

    data.update({"Pb": Pb, "Pd": Pd, "Pd": Pd, "Pa": Pa, "Pt": Pt})
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

def variableWTCollapse(cfg):
    if cfg['geometry']['NominalOD'] !=None:
        inputData = ({"OD": cfg['geometry']['NominalOD'],
            "ID": cfg['geometry']['NominalOD']- 2*cfg['geometry']['DesignWT'] ,
            "t": cfg['geometry']['DesignWT'],
            "E": cfg['material']['E'], "S": cfg['material']['SMYS'],                            \
            "Poissionsratio": cfg['material']['Poissionsratio'], "alphafab": cfg['material']['alphafab'], \
            "DesignWT": cfg['geometry']['DesignWT'] 
            })
    elif cfg['geometry']['NominalID'] !=None:
        inputData = ({"OD": cfg['geometry']['NominalID'] + 2*cfg['geometry']['DesignWT'],
            "ID": cfg['geometry']['NominalID'] ,
            "t": cfg['geometry']['DesignWT'],
            "E": cfg['material']['E'],
            "Poissionsratio": cfg['material']['Poissionsratio'], "alphafab": cfg['material']['alphafab'], \
            "DesignWT": cfg['geometry']['DesignWT'] 
            })

    return (cfg)

def APISTD2RDMethod1(data, loadCategory):
#check for internal over or external over pr if i.p to--
# Moment Used in method 1
    SigmaA = (data["AllowableStressFac"])*(data["S"])
# Tension Check
    Ty = (data["S"]*data["A"])/1000
    Ta = SigmaA*data["A"]/1000
# CHECK Formula
    Te = (Ta-data["internalPressure"]*data["Ai"]+data["externalPressure"]*data["Ao"])/1000 # Unit lbs
    My = math.pi/4*(data["S"]*(data["OD"]-data["t"])**2*data["t"])/1000/12 # Unit lbs.in
    Mp = (4/math.pi)*My
    
#Method 1 Calculation
# Action: Check if internal overpresssure
    #InternalOverPressure = (data["bendingMoment"]/My)/math.sqrt((data["designFactors"]["collapse"]["SLS"]**2)-((data["internalPressure"]-data["externalPressure"])/data["Pb"])**2)-(Te/Ty)
    #ExternalOverPressure = (data["bendingMoment"]/My)/math.sqrt((data["designFactors"]["collapse"]["SLS"]**2)-((data["externalPressure"]-data["internalPressure"])/data["Pc"])**2)-(Te/Ty)
    FDPressureCorrected = math.sqrt((data["designFactors"]["internalPressure"][loadCategory]**2)-((data["internalPressure"]-data["externalPressure"])/data["Pb"])**2)
    Tlimiting = FDPressureCorrected*Ty
    Mlimiting = FDPressureCorrected*My


# Action:  Convert to Data Frame 
    TArray = np.arange(-Tlimiting, Tlimiting, 1)
    MarrayPositive = []
    MarrayNegative = []

    for i in range(0, len(TArray)):
        MarrayPositive.append(My*(FDPressureCorrected - abs(TArray[i]/Ty)))
        MarrayNegative.append(-My*(FDPressureCorrected - abs(TArray[i]/Ty)))

    resultMatrix = np.stack((TArray, MarrayPositive, MarrayNegative) , axis=-1)
    resultDF = pd.DataFrame(resultMatrix, columns = ['TArray','MarrayPositive', 'MarrayNegative'])

    #print(My)

    return(data, resultDF)

def nominalWTAPISTD2RDMethod1(cfg, loadCategory):
    if cfg['geometry']['NominalOD'] !=None:
        inputData = ({"OD": cfg['geometry']['NominalOD'],
            "ID": cfg['geometry']['NominalOD'] - 2*cfg['geometry']['DesignWT'],
            "t": cfg['geometry']['DesignWT'], "E": cfg['material']['E'],
            "Poissionsratio": cfg['material']['Poissionsratio'], "alphafab": cfg['material']['alphafab'],
            "k": cfg['material']['k'], "S": cfg['material']['SMYS'], "bendingMoment": cfg['load']['bendingMoment'],                        \
            "U": cfg['material']['SMUS'], "CorrosionAllowance": cfg['geometry']['CorrosionAllowance'], "externalPressure": cfg['load']['externalPressure'],  \
            "designFactors": cfg['designFactors'], "AllowableStressFac": cfg['geometry']['AllowableStressFac'], "internalPressure": cfg['load']['internalPressure']                                                        \
            })
    elif cfg['geometry']['NominalID'] !=None:
        inputData = ({"OD": cfg['geometry']['NominalID'] + 2*cfg['geometry']['DesignWT'],
            "ID": cfg['geometry']['NominalID'] ,
            "t": cfg['geometry']['DesignWT'], "E": cfg['material']['E'],
            "Poissionsratio": cfg['material']['Poissionsratio'], "alphafab": cfg['material']['alphafab'],
            "k": cfg['material']['k'], "S": cfg['material']['SMYS'], "bendingMoment": cfg['load']['bendingMoment'],                          \
            "U": cfg['material']['SMUS'], "CorrosionAllowance": cfg['geometry']['CorrosionAllowance'], "externalPressure": cfg['load']['externalPressure'],  \
            "designFactors": cfg['designFactors'], "AllowableStressFac": cfg['geometry']['AllowableStressFac'], "internalPressure": cfg['load']['internalPressure']                                                     \
            })
    
    
    inputData = sectionProperties(inputData)
    inputData = APISTD2RDBurst(inputData)
    inputData = APISTD2RDCollapse(inputData)
    resultData, resultDF = APISTD2RDMethod1(inputData, loadCategory)
    
    return (resultData, resultDF)
