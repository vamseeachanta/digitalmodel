import math

from ConfigurationManager import ConfigurationManager
configuration_manager = ConfigurationManager('API-RD-2RD.ini')

class APIRP2RD:
    allowableStressfactor = None
    DesignCaseFactor = None

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

class APIRP2RDPipe(APIRP2RD):
    
    def __init__(self,pipeNominalOD_m,pipeNominalWT_m,pipeMinimumWT_m,pipeExternalPressure,pipeInternalPressure,pipeTensionEffective,pipeMoment,pipeYieldStrength,Parameters = None):
        super().__init__(pipeNominalOD_m,pipeNominalWT_m,pipeMinimumWT_m,pipeExternalPressure,pipeInternalPressure,pipeTensionEffective,pipeMoment,pipeYieldStrength)
        if Parameters is None:
            self.Parameters= configuration_manager.get_configured_values()
            #self.Parameters.allowableStressfactor= ast.literal_eval(self.Parameters.allowableStressfactor)
            #self.Parameters.DesignCaseFactor = ast.literal_eval(self.Parameters.DesignCaseFactor)

# ACTION: Find reference for AllowableStressFactor per APIRP2RD standards
    def updateAllowableStressFactor(self, allowableStressfactor=None):
        if self.allowableStressfactor is None:
            self.allowableStressfactor = allowableStressfactor

# ACTION: Find reference for DesignCaseFactor per APIRP2RD standards or default value
    def updateDesignCaseFactor(self, DesignCaseFactor=None):
        if self.DesignCaseFactor is None:
            self.DesignCaseFactor = DesignCaseFactor

## calculate these properties as a new class and add to pipe properties

    def pipeNominalID(self, pipe):
        self.pipeNominalID = self.pipeNominalOD_m - 2*self.pipeNominalWT_m  # inside diameter
        #logging.info( "pipeNominalInsideDiameter : "  +  str(round(self.nominalInnerDiameter,5)) )

    def pipeMinimumID(self, pipe):
        self.pipeMinimumID_m = self.pipeNominalOD_m -(2*self.pipeMinimumWT_m)
        #logging.info( 'pipeMinimumID_m : ' + str(round(self.minimumInnerDiameter,5)) )
        
    def pipeA(self, pipe):
        self.pipeA = (math.pi/4)*(self.pipeNominalOD_m**2 - self.pipeMinimumID_m**2)  # Area
        #logging.info( "pipeArea : " + str(round(self.pipe.A,5)) )
        
    def pipeAi(self, pipe):
        self.pipe.Ai = (math.pi/4)*(self.pipeMinimumID_m**2)
        #logging.info( "pipeAi : "  + str(round(self.pipe.Ai,5)) )
        
    def pipeAo(self, pipe):
        self.pipe.Ao = (math.pi/4)*(self.pipeNominalOD_m**2)
        #logging.info( "pipeAo : " + str(round(self.pipe.Ao,5)) )
        
    def pipeI(self, pipe):
        self.pipe.I = (math.pi/64)*(self.pipeNominalOD_m**4 - self.pipeMinimumID_m**4)  # Moment of Intertia
        #logging.info( "pipeIntertia : " + str(round(self.pipe.I,5)) )
        
    def SigmaA(self, pipe, loading, codeParameters):
        self.SigmaA = self.codeParameters.self.AllowableStressFac*self.pipeYieldStrength  # Basic
        #logging.info( "SigmaA : " + str(round(self.SigmaA,5)) )

    def SigmaACf(self, pipe, loading, codeParameters):
        self.SigmaACf = self.codeParameters.self.DesignCaseFactor*self.SigmaA  # R.H.S of Equation
        #logging.info("Stress(R.H.S) in kPa : " + str(round(self.SigmaACf*1E-3,5)) )
        #logging.info("Stress(R.H.S) in ksi: " + str(round(self.SigmaACf*1.45038E-007,5)))

    def sigmaRadial_1(self, pipe, loading, codeParameters):
        self.sigmaRadial_1 = -((self.pipeExternalPressure*self.pipeNominalOD_m)+(self.pipeInternalPressure*self.pipeNominalID))/(self.pipeNominalOD_m+self.pipeNominalID)
        #logging.info("sigmaRadial_1 : " + str(round(self.sigmaRadial_1,5)) )

    def sigmaCircuferential_1(self, pipe, loading, codeParameters):
        self.sigmaCircuferential_1 = (((pipeInternalPressure-self.pipeExternalPressure)*self.pipeNominalOD_m)/(2*self.pipeMinimumWT_m))-self.pipeInternalPressure
        #logging.info("self.sigmaCircuferential_1 : " + str(round(self.sigmaCircuferential_1,5)))
    
        #logging.info("self.BendingMoment_pipe : " + str(round(self.loading.Moment,5)))
    
    def Tension_LHS(self, pipe, loading, codeParameters):
        self.Tension_LHS = 2*(((self.pipeMoment/(2*self.pipeI))*(self.pipeNominalOD_m- self.pipeMinimumWT_m))**2)
        #logging.info("self.Tension_LHS : " + str(round(self.Tension_LHS,5)))

    def Tension_RHS(self, pipe, loading, codeParameters):
        self.Tension_RHS = (2*self.SigmaACf**2)-(self.sigmaRadial_1 -self.sigmaCircuferential_1)**2-self.Tension_LHS
        #logging.info("self.Tension_RHS : " + str(round(self.Tension_RHS,5)))
        a = 2*((1/self.pipe.A)**2)
        b = (4*((self.pipeMoment/(2*self.pipe.I))*(self.pipeNominalOD_m- self.pipeMinimumWT_m)))*(1/self.pipe.A)
        c = -Tension_RHS
        self.Tension_positive1 = (-b+math.sqrt(b**2-(4*a*c)))/(2*a)
        self.Tension_negative1 = (-b-math.sqrt(b**2-(4*a*c)))/(2*a)
        self.Tension1 = math.sqrt((self.Tension_RHS*self.pipeA**2)/2)
        self.TensionList1.append(self.Tension_positive1/1000)
        self.TensionList2.append(self.Tension_negative1/1000)
        self.BendMomentList.append(i.self.pipeMoment/1000)
        
