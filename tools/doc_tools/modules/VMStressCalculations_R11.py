

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
        self.pipeMoment = pipeMoment   # Global bending moment in pipe
        self.pipeYieldStrength = pipeYieldStrength  # pa

## We can define the pipe values by creating a object called 'pipe' 
pipe = MainPipe(0.24765,0.034925,0.034925,0,10,6674724,0,5.52E+08) 

