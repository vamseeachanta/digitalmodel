import logging


class Environment():

    def __init__(self, cfg):
        self.cfg = cfg

    def getOrcaFlexEnvironment(self):

        model = {}
        seaAndSeabed = self.seaAndSeabed()
        model.update(seaAndSeabed)

        model.update(self.wave())
        model.update(self.current())
        model.update(self.wind())

        return {"Environment": model}

    def seaAndSeabed(self):
        seaSeabed = {}
        # Sea
        seaSeabed.update({"WaterSurfaceZ": 0})
        seaSeabed.update({"KinematicViscosity": 1.35E-6})
        seaSeabed.update({"SeaTemperature": 10})
        seaSeabed.update({"ReynoldsNumberCalculation": "Flow Direction"})
        # Sea Density
        seaSeabed.update({"HorizontalWaterDensityFactor": "~"})
        seaSeabed.update({"VerticalDensityVariation": "Constant"})
        seaSeabed.update({"Density": 1.025})
        # Seabed
        seaSeabed.update({"SeabedType": "Flat"})
        seaSeabed.update({"SeabedOrigin": [0, 0]})
        seaSeabed.update({"WaterDepth": 30})
        seaSeabed.update({"SeabedSlopeDirection": 0})
        seaSeabed.update({"SeabedSlope": 0})
        seaSeabed.update({"SeabedModel": "Linear"})
        seaSeabed.update({"SeabedNormalStiffness": 100})
        seaSeabed.update({"SeabedShearStiffness": "~"})

        update_cfg = self.cfg.copy()
        calculate_function_prefix = 'get_seaSeabed_'
        property_group = seaSeabed
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return seaSeabed

    def wave(self):
        wave = {}
        wave.update({
            "SimulationTimeOrigin": 0,
            "WaveKinematicsCutoffDepth": 200,
            "WaveCalculationMethod": "Instantaneous Position(exact)",
            "WaveCalculationTimeInterval": 0,
            "WaveCalculationSpatialInterval": 0
        })
        wave.update({
            "WaveTrains": [{
                "Name": "Wave1",
                "WaveType": "Dean Stream",
                "WaveDirection": 180,
                "WaveHeight": 1,
                "WavePeriod": 8,
                "WaveOrigin": [0, 0],
                "WaveTimeOrigin": 0,
                "WaveStreamFunctionOrder": 5
            }]
        })


        update_cfg = self.cfg['Wave'][0].copy()
        calculate_function_prefix = 'get_wave_'
        property_group = wave
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        WaveTrains = self.get_wave_waveTrains(self.cfg['Wave'][0]['WaveTrains'])
        wave.update({"WaveTrains": WaveTrains})

        return wave

    def get_wave_waveTrains(self, WaveTrains):
        for waveTrain in WaveTrains:
            if (waveTrain.__contains__('WaveGamma')) and (waveTrain['WaveGamma'] == 'Calculated'):
                Hs = waveTrain['WaveHs']
                Tp = waveTrain['WaveTp']
                waveTrain['WaveGamma'] = self.getJONSWAPGamma(Hs, Tp)

        return WaveTrains

    def getJONSWAPGamma(self, Hs, Tp):
        import math
        if Tp > 5*(Hs ** 0.5):
            JONSWAPGamma = 1
        else:
            JONSWAPGamma = math.exp(6-1.2*Tp/(Hs ** 0.5))
            if JONSWAPGamma > 5:
                JONSWAPGamma = 5

        return JONSWAPGamma.__round__(3)

    def current(self):
        current = {}
        current.update({
            "MultipleCurrentDataCanBeDefined": "No",
            "CurrentRamp": "No",
            "HorizontalCurrentFactor": "~",
            "CurrentMethod": "Interpolated",
            "RefCurrentSpeed": 0,
            "RefCurrentDirection": 0,
            "CurrentDepth, CurrentFactor, CurrentRotation": [[0, 1, 0], [28, 0.173, 0]]
        })

        update_cfg = self.cfg['Current'][0].copy()
        calculate_function_prefix = 'get_current_'
        property_group = current
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return current

    def wind(self):
        wind = {}
        # Wind
        wind.update({
            "IncludeVesselWindLoads": "Yes",
            "IncludeLineWindLoads": "Yes",
            "IncludeBuoyWingWindLoads": "Yes",
            "VerticalWindVariationFactor": "~",
            "AirDensity": 0.00128,
            "WindType": "Constant",
            "WindSpeed": 0,
            "WindDirection": 0
        })

        update_cfg = self.cfg['Wind'][0].copy()
        calculate_function_prefix = 'get_wind_'
        property_group = wind
        self.update_property_group(calculate_function_prefix, property_group, update_cfg)

        return wind

    def update_with_load_variation(self, load_variation):
        search_key = 'Wave'
        load_cfg = dict(filter(lambda item: search_key in item[0], load_variation.items()))
        self.cfg['Wave'][0]['WaveTrains'][0].update(load_cfg)
        search_key = 'Current'
        load_cfg = dict(filter(lambda item: search_key in item[0], load_variation.items()))
        self.cfg['Current'][0].update(load_cfg)
        search_key = 'Wind'
        load_cfg = dict(filter(lambda item: search_key in item[0], load_variation.items()))
        self.cfg['Wind'][0].update(load_cfg)
        fea_data = self.getOrcaFlexEnvironment()
        return fea_data

    def update_property_group(self, calculate_function_prefix, property_group, cfg):
        for key_item in property_group.keys():
            if key_item in cfg and cfg[key_item] is not None:
                if cfg[key_item] != 'Calculated':
                    property_group[key_item] = cfg[key_item]
                else:
                    calculate_function = getattr(self, calculate_function_prefix + key_item)
                    property_group[key_item] = calculate_function(cfg)

    def RayleighDampingCoefficients(self):
        # TODO self.customize_loading_data
        if self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Period1'] == "WavePeriod1":
            if self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Dean Stream":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['WaveTz'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "JONSWAP":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['WaveTp'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
            elif self.riser_model.cfg['EnvironmentLoad'][self.fea_type][
                    self.loading_index]['Wave']['WaveTrains'][0]['WaveType'] == "Ochi-Hubble":
                customData = {
                    "Name":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                    "Mode":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                    "DampingRatio":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                    "Period1":
                        1 / self.riser_model.cfg['EnvironmentLoad'][self.fea_type][self.loading_index]['Wave']
                        ['WaveTrains'][0]['Wavefm1'],
                    "ApplyToGeometricStiffness":
                        self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
                }
        else:
            customData = {
                "Name":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Name'],
                "Mode":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Mode'],
                "DampingRatio":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['DampingRatio'],
                "Period1":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['Period1'],
                "ApplyToGeometricStiffness":
                    self.riser_model.cfg['FEASettings']['Damping'][self.fea_type]['ApplyToGeometricStiffness']
            }

        model = {}

        model.update({"Name": customData['Name']})
        model.update({"Mode": customData['Mode']})
        model.update({"DampingRatio": customData['DampingRatio']})
        model.update({"Period1": customData['Period1']})
        model.update({"ApplyToGeometricStiffness": customData['ApplyToGeometricStiffness']})

        return model
