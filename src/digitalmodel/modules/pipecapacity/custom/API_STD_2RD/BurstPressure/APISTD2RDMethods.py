import logging
import math

import pandas as pd


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


    
