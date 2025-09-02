import numpy
import scipy.interpolate
import OrcFxAPI

model = OrcFxAPI.Model()
stiffness = model.CreateObject(OrcFxAPI.ObjectType.BendingStiffness)
stiffness.IndependentValue = 0.0, 0.012, 0.021, 0.074, 0.28
stiffness.DependentValue = 0.0, 1200.0, 1450.0, 1700.0, 1850.0
interp = scipy.interpolate.interp1d(
    stiffness.IndependentValue,
    stiffness.DependentValue
)
for C in numpy.linspace(interp.x[0], interp.x[-1], num=10):
    M = interp(C)
    print(f"C = {C} M = {M}")
