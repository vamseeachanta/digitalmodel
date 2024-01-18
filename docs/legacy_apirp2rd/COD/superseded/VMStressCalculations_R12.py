
from VMCalculationsFunctionFile_R7 import VMStress

class MainPipe:
    def __init__(self,pipeNominalOD_m,pipeNominalWT_m,pipeMinimumWT_m,
                 pipeExternalPressure,pipeInternalPressure,
                 pipeTensionEffective,pipeMoment,pipeYieldStrength):
        self.pipeNominalOD_m = pipeNominalOD_m   # Outside diameter
        self.pipeNominalWT_m = pipeNominalWT_m   # Nominal Wall Thickness
        self.pipeMinimumWT_m = pipeMinimumWT_m   # Pipe wall Thickness
        self.pipeExternalPressure = pipeExternalPressure   # External Pressure
        self.pipeInternalPressure= pipeInternalPressure   # Internal Pressure
        self.pipeTensionEffective = pipeTensionEffective   # N
        self.pipeMoment = pipeMoment   # Global bending moment in pipe Nm
        self.pipeYieldStrength = pipeYieldStrength  # pa

## We can define the pipe values by creating a object called 'pipe' 
pipe1 = MainPipe(0.24765,0.034925,0.034925,0,0,0,0,5.52E+08)
pipe2 = MainPipe(0.24765,0.034925,0.034925,0,0,0,10E+04,5.52E+08)
pipe3 = MainPipe(0.24765,0.034925,0.034925,0,0,0,16E+04,5.52E+08)
pipe4 = MainPipe(0.24765,0.034925,0.034925,0,0,0,24E+04,5.52E+08)
pipe5 = MainPipe(0.24765,0.034925,0.034925,0,0,0,30E+04,5.52E+08)
pipe6 = MainPipe(0.24765,0.034925,0.034925,0,0,0,36E+04,5.52E+08)
pipe7 = MainPipe(0.24765,0.034925,0.034925,0,0,0,42E+04,5.52E+08)
pipe8 = MainPipe(0.24765,0.034925,0.034925,0,0,0,46.5E+04,5.52E+08)
