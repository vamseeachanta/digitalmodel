
from VMCalculationsFunctionFile_R3 import VMStress

class MainPipe:
    def __init__(self):
        self.pipeNominalOD_m = 0.24765  # Outside diameter
        self.pipeNominalWT_m = 0.034925  # Nominal Wall Thickness
        self.pipeMinimumWT_m = 0.034925  # Pipe wall Thickness
        self.pipeExternalPressure = 0  # External Pressure
        self.pipeInternalPressure=0  # Internal Pressure
        self.pipeTensionEffective = 6674724  # N
        self.pipeMoment = 0  # Global bending moment in pipe
        self.pipeYieldStrength = 551.5796E6  # Pa
pipe = MainPipe()
