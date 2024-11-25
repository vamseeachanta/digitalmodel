import math

# TODO Define a generic class PipeSizing()
# TODO Convert YAML file to take a generic sizing function as input appropriately.
# TODO Require flag for  parametrization of sizing by parametrization for a single method
# TODO Add sizing method + parametrization into a unique chart


def ASMEB314InternalPressure(data):
    DesignPressure = 2*data['S']*data['t']/data['D']*data['F']*data['WeldFactor']*data['T']
    Stress_Hoop = data['S']*data['F']*data['WeldFactor']*data['T']

    if data['D']/data['t'] >= 30:
        ThicknessMinimum_Pressure = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop)
    else:
        ThicknessMinimum_Pressure = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop + (data['Pi']-data['Po']))
    
    data.update({"MaximumDesignPressure": DesignPressure, "MinimumWallThickness_Pressure": ThicknessMinimum_Pressure})
    return data


def ASMEB314LogitudinalStress(data):
    if data['Condition'] == "Restrained":
        Stress_Elongation = -data['E']*data['Alpha']*(data['T1'] - data['T2'])
        Stress_Longitudinal = data['S']*data['F']*data['WeldFactor']*data['T']
        Stress_Moment = 0
        Stress_Axial = 0
        Stress_Hoop = (Stress_Longitudinal - Stress_Elongation - Stress_Moment - Stress_Axial)/data['Poissionsratio']
        
        DbytRatio = data['D']/data['t']
        if  DbytRatio >= 30:
             ThicknessMinimum_Longitudinal = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop)
        else:
            ThicknessMinimum_Longitudinal = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop + (data['Pi']-data['Po']))

        data.update({"MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal, \
        "Stress_Elongation": Stress_Elongation, "Stress_Logitudinal": Stress_Longitudinal,
        "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
        "Stress_Moment": Stress_Moment})

        return data


def ASMEB314EquivalentStress(data):
    if data['Condition'] == "Restrained":
        Stress_Elongation = -data['E']*data['Alpha']*(data['T1'] - data['T2'])
        Stress_Equivalent = data['S']*data['F']*data['WeldFactor']*data['T']
        Stress_Moment = 0
        Stress_Axial = 0
        Stress_Torsion = 0
        Stress_Hoop = ((-1*math.sqrt(Stress_Equivalent**2/2 - Stress_Torsion)/(data['Poissionsratio']-1)) \
                        - Stress_Elongation - Stress_Moment)/(1+data['Poissionsratio'])

        DbytRatio = data['D']/data['t']
        if  DbytRatio >= 30:
            ThicknessMinimum_Equivalent = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop)
        else:
            ThicknessMinimum_Equivalent = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop + (data['Pi']-data['Po']))

        data.update({"MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent, \
        "Stress_Elongation": Stress_Elongation, 
        "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
        "Stress_Moment": Stress_Moment, "Stress_Equivalent" : Stress_Equivalent})

        return data


def ASMEB318InternalPressure(data):
    DesignPressure = 2*data['S']*data['t']/data['D']*data['F']*data['WeldFactor']*data['T']

    DbytRatio = data['D']/data['t']
    if  DbytRatio >= 30:
        ThicknessMinimum_Pressure = (data['Pi']-data['Po'])*data['D']/(2*data['S']*data['F']*data['WeldFactor']*data['T'])
    else:
        ThicknessMinimum_Pressure = (data['Pi']-data['Po'])*data['D']/(2*data['S']*data['F']*data['WeldFactor']*data['T'] + (data['Pi']-data['Po']))

    data.update({"MaximumDesignPressure": DesignPressure, 
                "MinimumWallThickness": ThicknessMinimum_Pressure})

    return data


def ASMEB318LogitudinalStress(data):
    if data['Condition'] == "Restrained":
        Stress_Elongation = -data['E']*data['Alpha']*(data['T1'] - data['T2'])
        Stress_Longitudinal = data['S']*data['F']*data['WeldFactor']*data['T']
        Stress_Moment = 0
        Stress_Axial = 0
        Stress_Hoop = (Stress_Longitudinal - Stress_Elongation - Stress_Moment)/data['Poissionsratio']

        DbytRatio = data['D']/data['t']
        if  DbytRatio >= 30:
            ThicknessMinimum_Longitudinal = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop)
        else:
            ThicknessMinimum_Longitudinal = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop + (data['Pi']-data['Po']))

        data.update({"MinimumWallThickness_Longitudinal": ThicknessMinimum_Longitudinal, \
        "Stress_Elongation": Stress_Elongation, "Stress_Logitudinal": Stress_Longitudinal,
        "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
        "Stress_Moment": Stress_Moment})

        return data


def ASMEB318EquivalentStress(data):
    if data['Condition'] == "Restrained":
        Stress_Elongation = -data['E']*data['Alpha']*(data['T1'] - data['T2'])
        Stress_Equivalent = data['S']*data['F']*data['WeldFactor']*data['T']
        Stress_Moment = 0
        Stress_Axial = 0
        Stress_Torsion = 0
        Stress_Hoop = ((-1*math.sqrt(Stress_Equivalent**2/2 - Stress_Torsion)/(data['Poissionsratio']-1)) \
                        - Stress_Elongation - Stress_Moment)/(1+data['Poissionsratio'])

        DbytRatio = data['D']/data['t']
        if  DbytRatio >= 30:
            ThicknessMinimum_Equivalent = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop)
        else:
            ThicknessMinimum_Equivalent = (data['Pi']-data['Po'])*data['D']/(2*Stress_Hoop + (data['Pi']-data['Po']))

        data.update({"MinimumWallThickness_Equivalent": ThicknessMinimum_Equivalent, \
        "Stress_Elongation": Stress_Elongation, 
        "Stress_Hoop": Stress_Hoop, "Stress_Axial": Stress_Axial,
        "Stress_Moment": Stress_Moment, "Stress_Equivalent" : Stress_Equivalent})

        return data
