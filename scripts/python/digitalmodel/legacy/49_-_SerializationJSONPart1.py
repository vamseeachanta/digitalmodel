import json
import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.TargetSegmentLength = 1.0,
model.CalculateStatics()
Te = line.RangeGraph("Effective tension")

data = {
    "title": "Effective tension in static state",
    "x_axis_title": "Arclength (m)",
    "y_axis_title": "Te (kN)",
    "x_axis_values": Te.X.tolist(),
    "y_axis_values": Te.Mean.tolist()
}
with open("scratch/serialization.json", "w") as f:
    json.dump(data, f)
