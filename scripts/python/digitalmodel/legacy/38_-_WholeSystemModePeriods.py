import OrcFxAPI

model = OrcFxAPI.Model()
line1 = model.CreateObject(OrcFxAPI.ObjectType.Line)
line2 = model.CreateObject(OrcFxAPI.ObjectType.Line)
buoy = model.CreateObject(OrcFxAPI.ObjectType.Buoy3D)
line1.EndAX, line1.EndBX = -45, -5
line2.EndAX, line2.EndBX = 5, 45
line1.EndBConnection, line2.EndAConnection = buoy.Name, buoy.Name
model.CalculateStatics()
modes = OrcFxAPI.Modes(model)
print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
print(modes.period[:5])
