import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()

rg = line.RangeGraph("Effective tension")
print("{:>5} {:>7}".format("z", "Te"))
for z, Te in zip(rg.X, rg.Mean):
    print(f"{z:5.1f} {Te:7.3f}")
