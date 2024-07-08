# Standard library imports
import math


class PipeProperties:
    def __init__(self) -> None:
        pass
    
    def get_properties(self, pipe_cfg):
        pass

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


    def pipeProperties_method1(self, pipe):

        ## calculate these properties as a new class and add to pipe properties

        pipe.NominalID = pipe.NominalOD - 2*pipe.NominalWT  # inside diameter
        logging.info( "pipeNominalInsideDiameter : "  +  str(round(pipe.NominalID,5)) )

        
        pipe.MinimumID = pipe.NominalOD -(2*pipe.MinimumWT)
        logging.info( 'pipeMinimumID_m : ' + str(round(pipe.MinimumID,5)) )
        
        pipe.A = (math.pi/4)*(pipe.NominalOD**2 - pipe.MinimumID**2)  # Area
        logging.info( "pipeArea : " + str(round(pipe.A,5)) )

        pipe.Ai = (math.pi/4)*(pipe.MinimumID**2)
        logging.info( "pipeAi : "  + str(round(pipe.Ai,5)) )

        pipe.Ao = (math.pi/4)*(pipe.NominalOD**2)
        logging.info( "pipeAo : " + str(round(pipe.Ao,5)) )

        pipe.I = (math.pi/64)*(pipe.NominalOD**4 - pipe.MinimumID**4)  # Moment of Intertia
        logging.info( "pipeIntertia : " + str(round(pipe.I,5)) )

        return pipe