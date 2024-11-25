import logging


class General():

    def __init__(self, cfg):
        self.cfg = cfg

    def getGeneralOrcaFlexSetUp(self):
        general = {}
        unit_system = {"UnitsSystem": "SI"}
        general.update(unit_system)

        statics = {
            "StaticsMethod": "Whole System statics",
            "BuoysIncludedInStatics": "Individually Specified",
            "StaticsMaxIterations": 800
        }
        general.update(statics)

        dynamics = {
            "DynamicsSolutionMethod": "Implicit time domain",
            "ImplicitUseVariableTimeStep": "No",
            "ImplicitConstantTimeStep": 0.25,
            "LogPrecision": "Single",
            "TargetLogSampleInterval": 0.1
        }
        general.update(dynamics)

        stage_duration = {"StageDuration": [10, 600]}
        general.update(stage_duration)

        update_cfg = self.cfg.copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return {"General": general}

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
