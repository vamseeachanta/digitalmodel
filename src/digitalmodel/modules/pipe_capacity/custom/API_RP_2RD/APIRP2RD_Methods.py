class APIRP:
    def __init__(
        self,
        APIRPNominalOD_m,
        APIRPNominalWT_m,
        APIRPMinimumWT_m,
        APIRPExternalPressure,
        APIRPInternalPressure,
        APIRPTensionEffective,
        APIRPMoment,
        APIRPYieldStrength,
    ):
        self.APIRPNominalOD_m = APIRPNominalOD_m  # Outside diameter
        self.APIRPNominalWT_m = APIRPNominalWT_m  # Nominal Wall Thickness
        self.APIRPMinimumWT_m = APIRPMinimumWT_m  # APIRP wall Thickness
        self.APIRPExternalPressure = APIRPExternalPressure  # External Pressure
        self.APIRPInternalPressure = APIRPInternalPressure  # Internal Pressure
        self.APIRPTensionEffective = APIRPTensionEffective  # N
        self.APIRPMoment = APIRPMoment  # Global bending moment in APIRP Nm
        self.APIRPYieldStrength = APIRPYieldStrength  # pa


## We can define the APIRP values by creating a object called 'APIRP'
APIRP1 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 0, 5.52e08)
APIRP2 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 10e04, 5.52e08)
APIRP3 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 16e04, 5.52e08)
APIRP4 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 24e04, 5.52e08)
APIRP5 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 30e04, 5.52e08)
APIRP6 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 36e04, 5.52e08)
APIRP7 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 42e04, 5.52e08)
APIRP8 = APIRP(0.24765, 0.034925, 0.034925, 0, 0, 0, 46.5e04, 5.52e08)
