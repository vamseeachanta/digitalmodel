
from VMCalculationsFunctionFile_R2 import VMStress

class ClassPipe:
    pipeNominalOD_m = 0.257175  # Outside diameter (m)
    pipeNominalWT_m = 0.034925  # Nominal Wall Thickness (m)
    pipeMinimumWT_m = 0.034925  # Pipe wall Thickness (m)
    pipeExternalPressure = 0  # External Pressure (Pa)
    pipeInternalPressure= 1.5513E+08  # Internal Pressure (Pa)
    pipeTensionEffective = 0  # N
    pipeMoment = 0  # Global bending moment in pipe
    pipeYieldStrength = 551.5796E6  # MPa
    outputFileName = "0158TSJThinend.out"
 
pipe1 = ClassPipe() 
VMStress(pipe1)

class ClassPipe:
    pipeNominalOD_m = 0.288925  # Outside diameter (m)
    pipeNominalWT_m = 0.0508  # Nominal Wall Thickness (m)
    pipeMinimumWT_m = 0.0508  # Pipe wall Thickness (m)
    pipeExternalPressure = 0  # External Pressure (Pa)
    pipeInternalPressure= 1.5513E+08  # Internal Pressure (Pa)
    pipeTensionEffective = 0  # N
    pipeMoment = 0  # Global bending moment in pipe
    pipeYieldStrength = 551.5796E6  # MPa
    outputFileName = "0158TSJThickend.out"
 
pipe2 = ClassPipe() 
VMStress(pipe2)
