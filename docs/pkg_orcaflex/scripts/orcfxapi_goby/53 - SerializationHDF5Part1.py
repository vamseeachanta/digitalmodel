import h5py
import numpy
import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.TargetSegmentLength = 1.0,
model.CalculateStatics()
Te = line.RangeGraph("Effective tension")

f = h5py.File("scratch/serialization.hdf5", "w")
dataset = f.create_dataset("values", data=numpy.column_stack([Te.X, Te.Mean]))
dataset.attrs["title"] = "Effective tension in static state"
dataset.attrs["x_axis_title"] = "Arclength (m)"
dataset.attrs["y_axis_title"] = "Te (kN)"
f.close()
