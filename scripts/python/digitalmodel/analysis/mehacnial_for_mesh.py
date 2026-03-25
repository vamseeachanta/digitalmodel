#region Details View Action
#body_20 = DataModel.GetObjectById(20)
#body_20.Thickness = Quantity(.6, "m")
#endregion

#region Details View Action
mesh_15 = ExtAPI.DataModel.Project.Model.Mesh
mesh_15.PhysicsPreference = MeshPhysicsPreferenceType.Hydrodynamics
#endregion

#region Details View Action
mesh_15.ElementSize = Quantity(1, "m")
#endregion

#region Details View Action
mesh_15.ElementSize = Quantity(2, "m")
#endregion

#region Context Menu Action
mesh_15.GenerateMesh()
#endregion


#region Details View Action
for i in range(1,10000):
    body=DataModel.GetObjectById(i)
    try:
        if str(body.GeometryType)=="Surface":
            #print(i)
            body.Thickness = Quantity(1.12, "mm")
    except AttributeError:
        # Handle objects that don't have the GeometryType attribute here
        # You can skip them or perform other actions as needed
        pass
#endregion
