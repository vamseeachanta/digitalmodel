import OrcFxAPI

model = OrcFxAPI.Model()

vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)

line.EndAConnection = vessel.Name
line.EndAX = 45.0
line.EndAZ = -5.0
line.EndBX = 110.0
line.EndBZ = -30.0
# skip setting Y values of position, leave at default value of zero

model.RunSimulation()

tension = line.TimeHistory("Effective tension",
    objectExtra=OrcFxAPI.oeEndA)
print(f"Max tension = {max(tension)}")
print(f"Min tension = {min(tension)}")
