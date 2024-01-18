import math
from VMCalculationsFunctionFile import VMStress

pipe1 = dict(pipeNominalOD_m = 0.24765,  # Outside diameter
             pipeNominalWT_m = 0.034925,  # Nominal Wall Thickness
             pipeMinimumWT_m = 0.034925,  # Pipe wall Thickness
             pipeExternalPressure = 0,  # External Pressure
             pipeInternalPressure=0,  # Internal Pressure
             pipeTensionEffective = 6674724,  # N
             pipeMoment = 0,  # Global bending moment in pipe
             pipeYieldStrength = 551.5796E6)  # Pa

print(pipe1) #prints pipe1 details

#Below VMcalculations() function is imported from VMCalculatioinsFunctionFile.py file
#For importing external function both should be of same directory for calling

VMStress(**pipe1) 
