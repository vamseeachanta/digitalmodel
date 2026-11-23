import h5py
import numpy
import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.TargetSegmentLength = 1.0,
model.CalculateStatics()

varNames = [
    "Effective tension",
    "Bend moment",
    "Declination",
    "Depth"
]
varUnits = {}
for details in line.varDetails(OrcFxAPI.ResultType.RangeGraph):
    varUnits[details.VarName] = details.VarUnits

f = h5py.File("scratch/serialization_advanced.hdf5", "w")
for varName in varNames:
    rg = line.RangeGraph(varName)
    dataset = f.create_dataset(varName, data=numpy.column_stack([rg.X, rg.Mean]))
    dataset.attrs["x_axis_title"] = f"Arclength ({varUnits['X']})"
    dataset.attrs["y_axis_title"] = f"{varName} ({varUnits[varName]})"

f.close()
