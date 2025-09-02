import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
fatigueRawdata = pd.read_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Data\\FatigueRawdata.xlsx") # reading raw data using pandas
fatigueConstant = fatigueRawdata["Log a1~"]                                 # reading column data for slope1 

#Fatigue Constant, Slope, Transfer Cycles, Stress at Transfer Cycles, Fatigue Limit, Reference thickness, Thickness constant calculations for different slopes
# first slope calculations 
fatigueConstant1 = np.power(10,fatigueConstant)                             # calculating fatigue constant using numpy 
fatigueRawdata["Log a1"] = fatigueConstant1                                 # updating column with calculated values
refernceConstantCycleandStressdata = fatigueRawdata["# of Slopes"]          # assigning reference variable
slope = fatigueRawdata["m1"]
transferCycle1 = np.where(refernceConstantCycleandStressdata > 1,
                          fatigueRawdata["Transfer Cycles 1"], np.nan )
fatigueRawdata["Transfer Cycles 1"] = transferCycle1
transferStress1 = np.where(refernceConstantCycleandStressdata  > 1,
                           fatigueRawdata["Transfer Stress 1 (Mpa)"],np.nan)
fatigueRawdata["Transfer Stress 1 (Mpa)"] = transferStress1                 # updating column with calculated values 

# second slope calculations 
def constant(value):
    if value == "-":
        return (np.nan)                                                     # function for calcuating fatigue constant for slopes
    elif 1 < value > 4 :
        return (10**value)
    elif value == " ":
        return (np.nan)
slope1 = fatigueRawdata["m2"]
slope2 = slope1.map(constant)
fatigueRawdata["m2"] = slope2
fatigueConstant2 = fatigueRawdata["Log a2~"]                                # reading column data for slope2
fatigue = fatigueConstant2.map(constant)                                    #applying condition for calculating fatigue constant for slope                
fatigueRawdata["Log a2~"] = fatigue                                         # updating column with calculated values 
transferCycle2 = np.where(refernceConstantCycleandStressdata > 2,
                          fatigueRawdata["Transfer Cycles 2"], np.nan )     # conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["Transfer Cycles 2"] = transferCycle2                        # updating column with calculated values 
transferStress2 = np.where(refernceConstantCycleandStressdata  > 2,
                           fatigueRawdata["Transfer Stress 2 (Mpa)"],np.nan)# conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["Transfer Stress 2 (Mpa)"] = transferStress2                 # updating column with calculated values       

### third slope calculations
fatigueConstant3 =  fatigueRawdata["Log a3~"]                               # assigning reference variable
fatigue1 = fatigueConstant3.map(constant)                                   #applying condition for calculating fatigue constant for slope 
fatigueRawdata["Log a3~"] = fatigue1                                        # updating column with calculated values 
slope3 = np.where(refernceConstantCycleandStressdata  > 2,
                  fatigueRawdata["m3"], np.nan )                            # conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["m3"] = slope3                                               # updating column with calculated values 
transferCycle3 = np.where(refernceConstantCycleandStressdata  > 3,
                          fatigueRawdata["Transfer Cycles 3"], np.nan )     # conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["Transfer Cycles 3"] = transferCycle3                        # updating column with calculated values 
CAFL = np.where(refernceConstantCycleandStressdata  > 3,
                fatigueRawdata["CAFL (Mpa)"], np.nan )                      # conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["CAFL (Mpa)"] = CAFL                                         # updating column with calculated values 

### fourth slope calculations
fatigueConstant4 =  fatigueRawdata["Log a4~"]                               # assigning reference variable
fatigue2 = fatigueConstant4.map(constant)                                   #applying condition for calculating fatigue constant for slope 
fatigueRawdata["Log a4~"] = fatigue2                                        # updating column with calculated values 
slope4 = np.where(refernceConstantCycleandStressdata  > 3,
                  fatigueRawdata["m4"], np.nan )                            # conditional statement using numpy (np.where(if condition, true, false)
fatigueRawdata["m4"] = slope4                                               # updating column with calculated values 


plt.loglog(transferCycle1,transferStress1)

##fatigueRawdata.to_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Cal\\FatiguebasicurveCal.xlsx")

