class Vessel():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        vessel = {}

        general = self.get_general()
        vessel.update(general)

        calculation = self.get_caculation()
        vessel.update(calculation)

        shaded_drawing = {
            "ShadedDrawingCullingMode":
                "Anticlockwise",
        }
        vessel.update(shaded_drawing)

        return vessel

    def get_general(self):
        general = {
            "Name": "shuttleTankerObject",
            "VesselType": "Vessel Type1",
            "Draught": "Draught1",
            "Length": "~",
            "Connection": "Free",
            "InitialPosition": [100, 0, 0],
            "Orientation": [0, 0, 180]
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_general_'
        property_group = general
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return general


    def get_caculation(self):
        calculation = {
            "IncludedInStatics": "None",
            "PrimaryMotion": "Calculated (6 DOF)",
            "SuperimposedMotion": "None",
            "IncludeAppliedLoads": "No",
            "IncludeWaveLoad1stOrder": "Yes",
            "IncludeWaveDriftLoad2ndOrder": "Yes",
            "IncludeWaveDriftDamping": "Yes",
            "IncludeSumFrequencyLoad": "No",
            "IncludeAddedMassAndDamping": "Yes",
            "IncludeManoeuvringLoad": "No",
            "IncludeOtherDamping": "No",
            "IncludeCurrentLoad": "Yes",
            "IncludeWindLoad": "Yes",
            "PrimaryMotionIsTreatedAs": "Both low and wave frequency",
            "PrimaryMotionDividingPeriod": 80,
            "CalculationMode": "Filtering"
        }

        update_cfg = self.cfg['cfg'].copy()
        calculate_function_prefix = 'get_calculation_'
        property_group = calculation
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return calculation

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)
