import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()

inner = line.RangeGraph("von Mises Stress", objectExtra=
    OrcFxAPI.oeLine(RadialPos=OrcFxAPI.RadialPos.Inner, Theta=0.0))
outer = line.RangeGraph("von Mises Stress", objectExtra=
    OrcFxAPI.oeLine(RadialPos=OrcFxAPI.RadialPos.Outer, Theta=0.0))
print("{:>5} {:>7} {:>7}".format("z", "inner", "outer"))
for z, vmsInner, vmsOuter in zip(inner.X, inner.Mean, outer.Mean):
    print(f"{z:5.1f} {vmsInner:7.2f} {vmsOuter:7.2f}")
