import logging
import math


class PipeSizing():
    def __init__(self, cfg):
        self.cfg = cfg
        self.cfg['equivalent_pipe'] = {}

    def evaluate_pipe_system_properties(self):
        if self.cfg['Outer_Pipe'] != None:
            self.pipe_properties(pipe_flag = 'Outer_Pipe')

        if self.cfg['Inner_Pipe'] != None:
            self.pipe_properties(pipe_flag = 'Inner_Pipe')

        self.system_properties()


    def pipe_properties(self, pipe_flag):
        if self.cfg[pipe_flag]['Geometry']['Nominal_ID'] == None:
            self.cfg[pipe_flag]['Geometry']['Nominal_ID'] = self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                                              - 2*self.cfg[pipe_flag]['Geometry']['Design_WT']

        if self.cfg[pipe_flag]['Geometry']['Nominal_OD'] == None:
            self.cfg[pipe_flag]['Geometry']['Nominal_OD'] = self.cfg[pipe_flag]['Geometry']['Nominal_ID'] \
                                              + 2*self.cfg[pipe_flag]['Geometry']['Design_WT']

        if self.cfg[pipe_flag]['Geometry']['Design_WT'] == None:
            self.cfg[pipe_flag]['Geometry']['Design_WT'] = (self.cfg[pipe_flag]['Geometry']['Nominal_OD'] \
                                              - self.cfg[pipe_flag]['Geometry']['Nominal_ID'])/2

        self.pipe_section_properties(pipe_flag)
        self.get_fea_properties(pipe_flag)

    def pipe_section_properties(self, pipe_flag):
        Ao = (math.pi / 4) * (self.cfg[pipe_flag]['Geometry']['Nominal_OD'] ** 2)
        Ai = (math.pi / 4) * (self.cfg[pipe_flag]['Geometry']['Nominal_ID'] ** 2)
        Io = (math.pi / 64) * (self.cfg[pipe_flag]['Geometry']['Nominal_OD'] ** 4)
        Ii = (math.pi / 64) * (self.cfg[pipe_flag]['Geometry']['Nominal_ID'] ** 4)
        Jo = (math.pi / 32) * (self.cfg[pipe_flag]['Geometry']['Nominal_OD'] ** 4)
        Ji = (math.pi / 32) * (self.cfg[pipe_flag]['Geometry']['Nominal_ID'] ** 4)

        A = Ao - Ai
        I = Io - Ii
        J = Jo - Ji


        # TODO Where to get the material properties?


        self.cfg[pipe_flag]['section_properties'] = ({'pipe':{
                                                      "geometry": self.cfg[pipe_flag]['Geometry'],
                                                      "Ao": Ao, "Ai": Ai,
                                                      "Io": Io, "Ii": Ii,
                                                      "Jo": Jo, "Ji": Ji,
                                                      "A": A, "I": I,  "J": J}})


    def get_fea_properties(self, pipe_flag):
        material = self.cfg[pipe_flag]['Material']['Material']
        material_grade = self.cfg[pipe_flag]['Material']['Material_Grade']
        try:
            self.cfg['Material'][material]['G']
        except:
            self.cfg['Material'][material]['G'] = self.cfg['Material'][material]['E']/(2*(1+self.cfg['Material'][material]['Poissionsratio']))
        MassPerUnitLength = self.cfg[pipe_flag]['section_properties']['pipe']['A']*self.cfg['Material'][material]['Rho']
        EI = self.cfg[pipe_flag]['section_properties']['pipe']['I']*self.cfg['Material'][material]['E']
        EA = self.cfg[pipe_flag]['section_properties']['pipe']['A']*self.cfg['Material'][material]['E']
        GJ = self.cfg[pipe_flag]['section_properties']['pipe']['J'] * self.cfg['Material'][material]['G']

        self.cfg[pipe_flag]['section_properties']['pipe'].update({"MassPerUnitLength": MassPerUnitLength,
                                                    "EI": EI, "EA": EA, "GJ": GJ, "E": self.cfg['Material'][material]['E'],
                                                    "SMYS": self.cfg['Material'][material]['Grades'][material_grade]['SMYS'],
                                                    "SMUS": self.cfg['Material'][material]['Grades'][material_grade]['SMUS'],
                                                    "PoissonRatio": self.cfg['Material'][material]['Poissionsratio']})

    def system_properties(self):
        if self.cfg['Inner_Pipe'] != None:
            MassPerUnitLength = self.cfg['Outer_Pipe']['section_properties']['pipe']['MassPerUnitLength'] + \
                                self.cfg['Inner_Pipe']['section_properties']['pipe']['MassPerUnitLength']
            EI = self.cfg['Outer_Pipe']['section_properties']['pipe']['EI'] + \
                                self.cfg['Inner_Pipe']['section_properties']['pipe']['EI']
            EA = self.cfg['Outer_Pipe']['section_properties']['pipe']['EA'] + \
                                self.cfg['Inner_Pipe']['section_properties']['pipe']['EA']
            GJ = self.cfg['Outer_Pipe']['section_properties']['pipe']['GJ'] + \
                                self.cfg['Inner_Pipe']['section_properties']['pipe']['GJ']

            self.cfg['equivalent_pipe']['section_properties']['pipe'].update({"MassPerUnitLength": MassPerUnitLength,
                                                           "EI": EI, "EA": EA,
                                                           "GJ": GJ})

        else:
            self.cfg['equivalent_pipe']['section_properties'] = self.cfg['Outer_Pipe']['section_properties']


    def get_pipe_system_properties(self):
        self.evaluate_pipe_system_properties()
        return self.cfg['equivalent_pipe']


    # TODO Refactor following functions for Catenary Riser Stuff and integrate them
    def pipeProperties(self, cfg, FluidDensity, Buoyancy = False):
        cfg = self.steelSectionProperties(cfg)
        cfg = self.insulationSectionProperties(cfg)
        if Buoyancy == True:
            cfg = self.buoyancySectionProperties(cfg)
        else:
            cfg['BuoyancySection'] = None
        cfg = self.equivalentPipe(cfg, FluidDensity, Buoyancy)

        return(cfg)

    def steelSectionProperties(self, cfg):
        OD = cfg["geometry"]["NominalOD"]
        ID = cfg["geometry"]["NominalID"]
        data = {"OD": OD, "ID": ID}
        cfg['SteelSection'] = self.sectionProperties(data)

        return cfg

    def insulationSectionProperties(self, cfg):
        OD = cfg["geometry"]["NominalOD"]+2*cfg["geometry"]["ExternalCoating"]['Thickness']
        ID = cfg["geometry"]["NominalOD"]
        data = {"OD": OD, "ID": ID}
        cfg['InsulationSection'] = sectionProperties(data)

        return cfg

    def buoyancySectionProperties(self, cfg):
        OD = cfg['InsulationSection']["OD"]+2*cfg["LazyWaveCatenaryDefinition"]["UniformBuoyancy"]['Thickness']
        ID = cfg['InsulationSection']["OD"]
        data = {"OD": OD, "ID": ID}
        cfg['BuoyancySection'] = sectionProperties(data)

        return cfg

    def sectionProperties(self, data):

        A  = (math.pi/4)*(data['OD']**2-data['ID']**2)
        Ai = (math.pi/4)*(data['ID']**2)
        Ao = (math.pi/4)*(data['OD']**2)
        I = (math.pi/64)*(data['OD']**4-data['ID']**4)

        data.update({"A": A, "Ai": Ai, "Ao": Ao, "I": I})

        return data

    def equivalentPipe(self, cfg, FluidDensity, Buoyancy = False):
        if (cfg['geometry']['Strakes']['BaseThickness'] == None and Buoyancy == False):
            massPerUnitLength = cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2) \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)

            weightPerUnitLength = (cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2)  \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)     \
                            - cfg['InsulationSection']['Ao']*cfg['Material']['SeaWater']['Rho']*(0.0254**2) \
                            )*cfg['default']['Constants']['g']
        elif(cfg['geometry']['Strakes']['BaseThickness'] != None and Buoyancy == False):
            massPerUnitLength = cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2) \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['Material']['Strakes']['MassPerUnitLength']                           \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)

            weightPerUnitLength = (cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2)  \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)     \
                            - cfg['InsulationSection']['Ao']*cfg['Material']['SeaWater']['Rho']*(0.0254**2) \
                            )*cfg['default']['Constants']['g']                                  \
                            + cfg['Material']['Strakes']['WeightPerUnitLength']
        elif(Buoyancy == True):
            massPerUnitLength = cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2) \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['BuoyancySection']['A']*cfg['Material']['Buoyancy']['Rho']*(0.0254**2) \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)

            weightPerUnitLength = (cfg['SteelSection']['A']*cfg['Material']['Steel']['Rho']*(0.0254**2)  \
                            + cfg['InsulationSection']['A']*cfg['Material']['ExternalCoating']['Rho']*(0.0254**2) \
                            + cfg['BuoyancySection']['A']*cfg['Material']['Buoyancy']['Rho']*(0.0254**2) \
                            + cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)     \
                            - cfg['BuoyancySection']['Ao']*cfg['Material']['SeaWater']['Rho']*(0.0254**2) \
                            )*cfg['default']['Constants']['g']                                  \

        cfg['equivalentPipe'] = {"weightPerUnitLength": weightPerUnitLength, 'massPerUnitLength': massPerUnitLength}

        return(cfg)



    def buoyancyFactorAndDiameter(self):
        pass

