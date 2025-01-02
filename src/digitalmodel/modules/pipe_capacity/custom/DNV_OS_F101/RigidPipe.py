'''
Author : Vamsee Achanta
Date: 2018-07-12
Objective: To define pipe attributes (data) for offshore structural analysis
'''
import ast
import logging
import math

from custom.DNV_OS_F101.ConfigurationManager import ConfigurationManager
from custom.DNV_OS_F101.solvePolynomialEquation import solvePolynomialEquation

configuration_manager = ConfigurationManager('custom\\DNV_OS_F101\\DNV-OS-F101.ini')

class RigidPipe:
    thicknessFabricationTolerance = None
    corrosionAllowance = None
    erosionAllowance = None
    ovality = None

    def __init__(self, nominalDiameter, nominalWallThickness):
        self.nominalDiameter = nominalDiameter
        self.nominalWallThickness = nominalWallThickness


class DNVOSF101Pipe(RigidPipe):

    def __init__(self,nominalDiameter, nominalWallThickness, Parameters = None):
        super().__init__(nominalDiameter, nominalWallThickness)
        if Parameters is None:
            self.Parameters= configuration_manager.get_configured_values()
            self.Parameters.pressureTestSafetyFactors.system = ast.literal_eval(self.Parameters.pressureTestSafetyFactors.system)
            self.Parameters.pressureTestSafetyFactors.mill = ast.literal_eval(self.Parameters.pressureTestSafetyFactors.mill)
            self.Parameters.pressureContainment.safetyClass = ast.literal_eval(self.Parameters.pressureContainment.safetyClass)
            self.Parameters.materialFabrication.factors = ast.literal_eval(self.Parameters.materialFabrication.factors)
            self.Parameters.other.safetyClass = ast.literal_eval(self.Parameters.other.safetyClass)
            self.Parameters.loadEffectFactors.values = ast.literal_eval(self.Parameters.loadEffectFactors.values)
            self.Parameters.load.values = ast.literal_eval(self.Parameters.load.values)
            self.Parameters.conditionLoadEffectFactors.values = ast.literal_eval(self.Parameters.conditionLoadEffectFactors.values)

        self.thickness1 = {}
        self.thickness2 = {}
        self.thickness3 = {}

    # ACTION: Find reference for fabrication tolerance per DNV standards
    def updateThicknessFabricationTolerance(self, thicknessFabricationTolerance=None):
        if self.thicknessFabricationTolerance is None:
            self.thicknessFabricationTolerance = thicknessFabricationTolerance

    # ACTION: Find reference for corrosion allowance per DNV standards or default value
    def updateCorrosionAllowance(self, CorrosionAllowance=None):
        if self.corrosionAllowance is None:
            self.corrosionAllowance = CorrosionAllowance

    # ACTION: Find reference for Erosion allowance per DNV standards or default value
    def updateErosionAllowance(self, ErosionAllowance=None):
        if self.erosionAllowance is None:
            self.erosionAllowance = ErosionAllowance

    # ACTION: Find reference for ovality per DNV standards. Default is 3%
    def updateOvality(self, Dmax=None, Dmin=None):
        if self.ovality is None:
            if (Dmax == None) or (Dmin == None):
                self.ovality = self.Parameters.defaults.ovality
            else:
                self.ovality = (Dmax - Dmin) / self.nominalDiameter
        if self.ovality < 0.005:
            self.ovality = 0.005

    # Temperature Derating
    def temperatureDeratingValues(self, fluidTemperature = None):
        if fluidTemperature is None:
            if (self.Parameters.loadPressure.fluidTemperature > self.Parameters.materialProperties.temperatureDerateLimit):
                self.fy_temp = 1.14 - self.Parameters.loadPressure.fluidTemperature / 850  # Temperature Derating Factor for the Yield Stress.
    # Action:   Add ultimate Stress drating
                self.fu_temp = self.fy_temp
            else:
                self.fy_temp = 0  # Temperature Derating Factor for the Yield Stress
                self.fu_temp = self.fy_temp
        else:
            if fluidTemperature > self.Parameters.materialProperties.temperatureDerateLimit:
                self.fy_temp = 1.14 - fluidTemperature / 850  # Temperature Derating Factor for the Yield Stress
                #     Add ultimate Stress drating
                self.fu_temp = self.fy_temp
            else:
                self.fy_temp = 0  # Temperature Derating Factor for the Yield Stress
                self.fu_temp = self.fy_temp

    # Material strength values and derated values
    def updateStrengthValues(self,specifiedMinimumYieldStrength=None, specifiedMinimumTensileStrength=None):
        if specifiedMinimumYieldStrength is not None:
            self.Parameters.materialProperties.specifiedMinimumYieldStrength = specifiedMinimumYieldStrength
        self.minimumYieldStrength = (self.Parameters.materialProperties.specifiedMinimumYieldStrength - self.fy_temp)*self.Parameters.materialStrengthFactors.Normally

        if specifiedMinimumYieldStrength is not None:
            self.Parameters.materialProperties.specifiedMinimumTensileStrength = specifiedMinimumTensileStrength
        self.minimumTensileStrength = (self.Parameters.materialProperties.specifiedMinimumTensileStrength - self.fu_temp)*self.Parameters.materialStrengthFactors.Normally

    def DNVThickness(self):
        self.thickness1["installation"] = self.nominalWallThickness - self.thicknessFabricationTolerance
        self.thickness1["operation"] = self.nominalWallThickness - self.thicknessFabricationTolerance - self.corrosionAllowance
        self.thickness2["installation"] = self.nominalWallThickness
        self.thickness2["operation"] = self.nominalWallThickness - self.corrosionAllowance
        self.thickness3["installation"] = self.nominalWallThickness
        self.thickness3["operation"] = self.nominalWallThickness - 0.5 * self.corrosionAllowance

    def pressureContainmentResistance(self, thickness = None):
        yield_ContainmentOrBurst = min(self.Parameters.materialProperties.specifiedMinimumYieldStrength, self.Parameters.materialProperties.specifiedMinimumTensileStrength/1.15)
        if thickness is None:
            self.pressureContainmentResistance = 2 * self.nominalWallThickness / (self.nominalDiameter - self.nominalWallThickness) *yield_ContainmentOrBurst*2/math.sqrt(3)
        else:
            self.pressureContainmentResistance = 2 * thickness / (self.nominalDiameter - thickness) *yield_ContainmentOrBurst*2/math.sqrt(3)

    # ACTION : Need formulas for plt and ph
    def pressureContainment(self, operatingCondition, safetyClass="high", limitState="ULS"):
        thickness = self.thickness1[operatingCondition]
        self.pressureContainmentResistance(thickness)
        quantity1 = self.pressureContainmentResistance / self.Parameters.pressureTestSafetyFactors.system[safetyClass] / self.Parameters.materialResistanceFactors[limitState]
        quantity2 = self.Parameters.loadPressure.plt / self.Parameters.pressureTestSafetyFactors.system[safetyClass] - self.Parameters.loadPressure.external
        quantity3 = self.Parameters.loadPressure.ph * self.Parameters.materialStrengthFactors.Normally / self.Parameters.pressureTestSafetyFactors.mill[safetyClass]

        LHS = self.Parameters.loadPressure.pli - self.Parameters.loadPressure.external
        RHS = min(quantity1, quantity2, quantity3)
        self.incidentPressureburstcheckMargin = (RHS - LHS) / RHS
        if LHS <= RHS:
            self.incidentPressureburstCheckPass = True
        else:
            self.incidentPressureburstCheckPass = False

        LHS = self.Parameters.loadPressure.plt - self.Parameters.loadPressure.external
        RHS = min(quantity1, self.Parameters.loadPressure.ph)
        self.incidentPressureburstcheckMargin = (RHS - LHS) / RHS
        if LHS <= RHS:
            self.testPressureburstCheckPass = True
        else:
            self.testPressureburstCheckPass = False

    # DNV-OS-F101, Section 13 D700, Local Buckling - Collapse Check. 
    def collapse(self, thickness, pipeFabricationMethod):
        self.dOverTRatio = self.nominalDiameter / thickness
        self.tOverDRatio = 1/self.dOverTRatio
        self.elasticCollapseCapacity = 2*self.Parameters.materialProperties.youngsModulus*self.tOverDRatio**3/(1-self.Parameters.materialProperties.poissionsratio**2)
        self.plasticCollapseCapacity = self.minimumYieldStrength*self.Parameters.materialFabrication.factors[pipeFabricationMethod]*2*self.tOverDRatio

        b = -self.elasticCollapseCapacity
        c = -(self.plasticCollapseCapacity**2 + self.plasticCollapseCapacity*self.elasticCollapseCapacity*self.ovality*self.dOverTRatio)
        d = self.elasticCollapseCapacity*(self.plasticCollapseCapacity**2)
        u = 1/3 * (-b**2/3 + c)
        v = 1/2 * (2/27*(b**3) - b*c/3 + d)
        phi= math.acos(-v/math.sqrt(-u**3))
        y = -2 * math.sqrt(-u) * math.cos(phi/3+ 60*math.pi/180)

        self.collapsePressure = y-b/3

    # Local Buckling - System Collapse Check (DNV
    def localBucklingUnderExternalPressure_polyMethod(self, thickness, pipeFabricationMethod):
        self.collapse(thickness, pipeFabricationMethod)
        coefficientX3 = 1
        coefficientX2 = -self.elasticCollapseCapacity
        coefficientX1 = -(self.plasticCollapseCapacity**2 + self.elasticCollapseCapacity*self.plasticCollapseCapacity*self.ovality*self.dOverTRatio)
        coefficientX0 = self.elasticCollapseCapacity * self.plasticCollapseCapacity**2
        polynomialCoefficients = [coefficientX3, coefficientX2, coefficientX1, coefficientX0]
        potentialRoots = solvePolynomialEquation(polynomialCoefficients)
        print(potentialRoots)

    def localBucklingUnderExternalPressure(self, thickness, pipeFabricationMethod, safetyClass="high", limitState="ULS"):
        # Evalaute collapse pressure
        self.collapse(thickness, pipeFabricationMethod)

        LHS = self.Parameters.loadPressure.external - self.Parameters.loadPressure.minimumInternal
        RHS = self.collapsePressure/self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState]
        self.localBucklingExternalPressureCheckMargin = (RHS - LHS) / RHS
        if LHS <= RHS:
            self.localBucklingExternalPressureCheckPass = True
        else:
            self.localBucklingExternalPressureCheckPass = False

    def propagationBuckling(self, thickness, pipeFabricationMethod, safetyClass="high", limitState="ULS", pipeCondition="Cold"):
        # Update the wall thickness ratio for provided wall thickness
        self.collapse(thickness, pipeFabricationMethod)

        if pipeCondition == "Cold":
            yieldStrength = self.Parameters.materialProperties.specifiedMinimumYieldStrength
        else:
            yieldStrength = self.minimumYieldStrength

        if self.dOverTRatio < 15 or  self.dOverTRatio < 45:
            self.propagationPressure = 35*yieldStrength * self.Parameters.materialFabrication.factors[pipeFabricationMethod] * (self.tOverDRatio**2.5)
        else:
            self.propagationPressure = None

        if self.propagationPressure is not None:
            LHS = self.Parameters.loadPressure.external - self.Parameters.loadPressure.minimumInternal
            RHS = self.propagationPressure / self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState]
            self.propagationBucklingCheckMargin = (RHS - LHS) / RHS
            if LHS <= RHS:
                self.propagationBucklingCheckPass = True
            else:
                self.propagationBucklingCheckPass = False
        else:
                self.propagationBucklingCheckMargin = None
                self.propagationBucklingCheckPass = "Pipe out of code limits. Detailed FEA may be required to ascertain pipe bending capacity"

# Action : Add buckle arrestor capacity design equations

    def localBucklingCombinedLoadingLoadControlled(self, thickness, pipeFabricationMethod, operatingCondition, safetyClass="high", limitState="ULS", pipeCondition="Cold", conditionEffect ="Other"):
        # Update the wall thickness ratio for provided wall thickness
        self.collapse(thickness, pipeFabricationMethod)
        # self.pressureContainment(operatingCondition, safetyClass)

        pInternal = self.Parameters.loadPressure.internal
        pExternal = self.Parameters.loadPressure.external
        if (pInternal >= pExternal):
            logging.critical("Internal Overpressure")
            overPressureRatio = (pInternal - pExternal)/self.pressureContainmentResistance
        else:
            logging.critical("External Overpressure")
            overPressureRatio = (pExternal - self.Parameters.loadPressure.minimumInternal)/self.collapsePressure

        if pipeCondition == "Cold":
            yieldStrength = self.Parameters.materialProperties.specifiedMinimumYieldStrength
        else:
            yieldStrength = self.minimumYieldStrength
        gammac = self.Parameters.conditionLoadEffectFactors.values[conditionEffect]

        Msd1 = self.Parameters.load.values["bendingMoment"]["functional"] * self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["bendingMoment"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["bendingMoment"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["bendingMoment"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac
        Ssd1 = self.Parameters.load.values["effectiveTension"]["functional"] * self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["effectiveTension"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["effectiveTension"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["effectiveTension"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac

        Msd2 = self.Parameters.load.values["bendingMoment"]["functional"] / self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["bendingMoment"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["bendingMoment"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["bendingMoment"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac
        Ssd2 = self.Parameters.load.values["effectiveTension"]["functional"] / self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["effectiveTension"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["effectiveTension"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["effectiveTension"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac

        self.Msd = max(Msd1, Msd2) 
        self.Ssd = max(Ssd1, Ssd2) 

        plasticTensionCapacity = yieldStrength * math.pi * (self.nominalDiameter - thickness) * thickness
        plasticMomentCapacity = yieldStrength * math.pi * ((self.nominalDiameter - thickness)**2) * thickness

        beta = (60 - self.dOverTRatio) / 90
        alphaC = (1-beta) + beta*(self.Parameters.materialProperties.tensileStrength)/yieldStrength
        if overPressureRatio < 2/3:
            alphaP = 1 - beta
        else:
            alphaP = 1 - 3*beta*(1-overPressureRatio)
        LHS_Term1a = self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState]*self.Msd/alphaC/plasticMomentCapacity
        LHS_Term1b = self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState]*self.Ssd/alphaC/plasticTensionCapacity

        if (pInternal >= pExternal):
            LHS_Term2 = alphaP/alphaC * overPressureRatio
        else:
            LHS_Term2 = self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState] * overPressureRatio

        interactionRatio = (LHS_Term1a + LHS_Term1b ** 2)**2 + LHS_Term2 **2
        RHS = 1
        if (self.dOverTRatio <= 15) or  (self.dOverTRatio <= 45):
            logging.critical("CAUTION: D/t ratio outside of code limits. Adjust platic loads with FEA findings prior to use")
        if (self.Ssd/plasticTensionCapacity >= 0.4):
            logging.critical("CAUTION: Tension loading ratio outside of code limits. Refer to code")

        self.localBucklingCombinedLoadingLoadControlledCheckMargin = (RHS - interactionRatio) / RHS
        if interactionRatio <= RHS:
            self.localBucklingCombinedLoadingLoadControlledCheckPass = True
        else:
            self.localBucklingCombinedLoadingLoadControlledCheckPass = False

    def localBucklingCombinedLoadingDisplacementControlled(self, thickness, pipeFabricationMethod, operatingCondition, safetyClass="high", limitState="ULS", pipeCondition="Cold", conditionEffect ="Other"):
        # Update the wall thickness ratio for provided wall thickness
        self.collapse(thickness, pipeFabricationMethod)

        logging.critical("Update alphah in .ini file to re-run Displacement controlled method")
        if (self.dOverTRatio <= 45):
            logging.critical("CAUTION: D/t ratio outside of code limits. Refer to Code")

        gammac = self.Parameters.conditionLoadEffectFactors.values[conditionEffect]
        esd1 = self.Parameters.load.values["strain"]["functional"] * self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["strain"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["strain"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["strain"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac
        esd2 = self.Parameters.load.values["strain"]["functional"] / self.Parameters.loadEffectFactors.values[limitState]["functional"] * gammac + \
               self.Parameters.load.values["strain"]["environmental"] * self.Parameters.loadEffectFactors.values[limitState]["environmental"]  + \
               self.Parameters.load.values["strain"]["interference"] * self.Parameters.loadEffectFactors.values[limitState]["interference"] * gammac + \
               self.Parameters.load.values["strain"]["accidental"] * self.Parameters.loadEffectFactors.values[limitState]["accidental"] * gammac
        esd = max(esd1, esd2)

        pInternal = self.Parameters.loadPressure.internal
        pExternal = self.Parameters.loadPressure.external
        if (pInternal >= pExternal):
            ec = 0.78 * (self.tOverDRatio - 0.01) * (1 + 5.75 * (self.Parameters.loadPressure.minimumInternal - pExternal)/self.pressureContainmentResistance ) * self.Parameters.defaults.alphah**-1.5 * self.Parameters.defaults.alphagw 
            logging.critical("Internal Overpressure")

            self.localBucklingCombinedLoadingDisplacementControlledCheckMargin = (esd - ec) / ec
            if esd <= ec:
                self.localBucklingCombinedLoadingDisplacementControlledCheckPass = True
            else:
                self.localBucklingCombinedLoadingDisplacementControlledCheckPass = False

        else:
            ec = 0.78 * (self.tOverDRatio - 0.01) * alphah**-1.5 * alphagw 
            logging.critical("External Overpressure")
            LHS = (esd/ec*self.Parameters.resistanceFactors.gammae[safetyClass]) ** 0.8 +  \
                        (pExternal - self.Parameters.loadPressure.minimumInternal)/self.collapsePressure*self.Parameters.other.safetyClass[safetyClass]/self.Parameters.materialResistanceFactors[limitState] 
            RHS =1
            self.localBucklingCombinedLoadingDisplacementControlledCheckMargin = (LHS - RHS) / RHS
            if LHS <= RHS:
                self.localBucklingCombinedLoadingDisplacementControlledCheckPass = True
            else:
                self.localBucklingCombinedLoadingDisplacementControlledCheckPass = False
