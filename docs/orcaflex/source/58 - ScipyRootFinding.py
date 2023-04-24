import scipy.optimize
import OrcFxAPI

model = OrcFxAPI.Model()

riserType = model.CreateObject(OrcFxAPI.ObjectType.LineType)
riserType.Name = "Riser type"
buoyedType = model.CreateObject(OrcFxAPI.ObjectType.LineType)
buoyedType.Name = "Buoyed type"
riser = model.CreateObject(OrcFxAPI.ObjectType.Line)
riser.Name = "Riser"

riser.LineType = riserType.Name, buoyedType.Name, riserType.Name
riser.Length = 90.0, 80.0, 100.0
riser.TargetSegmentLength = 2.0, 2.0, 2.0
riser.EndBX = riser.EndAX + 160.0
riser.EndBConnection = "Anchored"
riser.EndBHeightAboveSeabed = 0.0

buoyedType.WizardCalculation = "Line with Floats"
buoyedType.FloatBaseLineType = riserType.Name
buoyedType.FloatDiameter = 1.1
buoyedType.FloatLength = 2.0
buoyedType.FloatPitch = 8.0
buoyedType.InvokeWizard()

targetZ = -50.0
initialFloatLength = buoyedType.FloatLength
arclengthRange = OrcFxAPI.arSpecifiedSections(2, 2)

def calcHogBendZ(floatLength):
    buoyedType.FloatLength = floatLength[0]
    buoyedType.InvokeWizard()
    model.CalculateStatics()
    Z = riser.RangeGraph("Z", arclengthRange=arclengthRange).Mean
    hogBendZ = max(Z) # Z value at high point of hog bend
    print(f"Float length = {floatLength[0]}, hog bend Z = {hogBendZ}")
    return hogBendZ - targetZ

scipy.optimize.fsolve(calcHogBendZ, initialFloatLength)
