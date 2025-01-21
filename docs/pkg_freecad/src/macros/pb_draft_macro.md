# -*- coding: utf-8 -*-

# Macro Begin: C:\Users\CHINNI\AppData\Roaming\FreeCAD\Macro\Twisted part.FCMacro +++++++++++++++++++++++++++++++++++++++++++++++++
import FreeCAD
import PartDesign
import PartDesignGui
import Sketcher
import Edit

# Gui.runCommand('Std_DlgMacroRecord',0)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').addObject('PartDesign::Body','Body')
# Gui.getDocument('Unnamed').ActiveView.setActiveObject('pdbody',App.getDocument('Unnamed').getObject('Body'),'')
### End command PartDesign_NewSketch
# Gui.Selection.addSelection('Unnamed','Body','Origin.XY_Plane.')
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch')
App.getDocument('Unnamed').getObject('Sketch').Support = (App.getDocument('Unnamed').getObject('XY_Plane'),[''])
App.getDocument('Unnamed').getObject('Sketch').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CompCreateCircle',0)
App.getDocument('Unnamed').getObject('Sketch').addGeometry(Part.Circle(App.Vector(0.000000,0.000000,0),App.Vector(0,0,1),16.491366),False)
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Coincident',0,3,-1,1)) 
App.ActiveDocument.recompute()
# Gui.runCommand('Sketcher_CompCreateArc',0)
App.getDocument('Unnamed').getObject('Sketch').addGeometry(Part.ArcOfCircle(Part.Circle(App.Vector(0.000000,0.000000,0),App.Vector(0,0,1),31.780330),1.600999,4.702000),False)
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Coincident',1,3,0,3)) 
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('PointOnObject',1,1,-2)) 
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('PointOnObject',1,2,-2)) 
App.ActiveDocument.recompute()
# Gui.runCommand('Sketcher_CreatePolyline',0)
App.getDocument('Unnamed').getObject('Sketch').addGeometry(Part.LineSegment(App.Vector(0.000000,31.759251,0),App.Vector(33.205379,31.759251,0)),False)
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Tangent',1,1,2,1)) 
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Sketch').addGeometry(Part.LineSegment(App.Vector(33.205379,31.759251,0),App.Vector(35.316692,-32.533588,0)),False)
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Coincident',2,2,3,1)) 
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Vertical',3)) 
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Sketch').addGeometry(Part.LineSegment(App.Vector(33.205379,-32.533588,0),App.Vector(0.191939,-31.381958,0)),False)
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Coincident',3,2,4,1)) 
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Coincident',4,2,1,2)) 
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Horizontal',4)) 
App.ActiveDocument.recompute()
# Gui.Selection.addSelection('Unnamed','Body','Sketch.Edge1',9.57775,13.3856,0.008,False)
### Begin command Sketcher_CompConstrainRadDia
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Diameter',0,32.982732))
App.getDocument('Unnamed').getObject('Sketch').setDatum(10,App.Units.Quantity('25.000000 mm'))
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
### End command Sketcher_CompConstrainRadDia
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch.Edge2',-7.86081,30.6158,0.008,False)
### Begin command Sketcher_CompConstrainRadDia
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('Diameter',1,63.217774))
App.getDocument('Unnamed').getObject('Sketch').setDatum(11,App.Units.Quantity('40.000000 mm'))
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
### End command Sketcher_CompConstrainRadDia
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch.Edge3',18.618,20,0.008,False)
### Begin command Sketcher_ConstrainDistanceX
App.getDocument('Unnamed').getObject('Sketch').addConstraint(Sketcher.Constraint('DistanceX',2,1,2,2,33.205388))
App.getDocument('Unnamed').getObject('Sketch').setDatum(12,App.Units.Quantity('25.000000 mm'))
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
### End command Sketcher_ConstrainDistanceX
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch.')
App.getDocument('Unnamed').recompute()
### Begin command PartDesign_Pad
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::Pad','Pad')
App.getDocument('Unnamed').getObject('Pad').Profile = App.getDocument('Unnamed').getObject('Sketch')
App.getDocument('Unnamed').getObject('Pad').Length = 10
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Pad').ReferenceAxis = (App.getDocument('Unnamed').getObject('Sketch'),['N_Axis'])
App.getDocument('Unnamed').getObject('Sketch').Visibility = False
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('Pad').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('Body').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('Pad').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('Pad').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('Body').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('Pad').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('Pad').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('Body').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('Pad').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('Pad').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('Body').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('Pad').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('Pad').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('Body').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('Pad').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Pad.')
# Gui.Selection.clearSelection()
### End command PartDesign_Pad
# Gui.Selection.clearSelection()
App.getDocument('Unnamed').getObject('Pad').Length = 10.000000
App.getDocument('Unnamed').getObject('Pad').TaperAngle = 0.000000
App.getDocument('Unnamed').getObject('Pad').UseCustomVector = 0
App.getDocument('Unnamed').getObject('Pad').Direction = (0, 0, 1)
App.getDocument('Unnamed').getObject('Pad').ReferenceAxis = (App.getDocument('Unnamed').getObject('Sketch'), ['N_Axis'])
App.getDocument('Unnamed').getObject('Pad').AlongSketchNormal = 1
App.getDocument('Unnamed').getObject('Pad').Type = 0
App.getDocument('Unnamed').getObject('Pad').UpToFace = None
App.getDocument('Unnamed').getObject('Pad').Reversed = 0
App.getDocument('Unnamed').getObject('Pad').Midplane = 1
App.getDocument('Unnamed').getObject('Pad').Offset = 0
App.getDocument('Unnamed').recompute()
# Gui.getDocument('Unnamed').resetEdit()
App.getDocument('Unnamed').getObject('Sketch').Visibility = False
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,-4.91358,-0.803616)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch001')
App.getDocument('Unnamed').getObject('Sketch001').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch001').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch001.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch001')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch001.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch001')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_External',0)
# Gui.Selection.addSelection('Unnamed','Body','Pad.Edge10',25,-6.90979,5)
App.getDocument('Unnamed').getObject('Sketch001').addExternal("Pad","Edge10")
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Edge5',25,-20,-1.43954)
App.getDocument('Unnamed').getObject('Sketch001').addExternal("Pad","Edge5")
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CompCreateRectangles',0)
geoList = []
geoList.append(Part.LineSegment(App.Vector(-34.357006,20.441460,0),App.Vector(40.499031,20.441460,0)))
geoList.append(Part.LineSegment(App.Vector(40.499031,20.441460,0),App.Vector(40.499031,-16.986565,0)))
geoList.append(Part.LineSegment(App.Vector(40.499031,-16.986565,0),App.Vector(-34.357006,-16.986565,0)))
geoList.append(Part.LineSegment(App.Vector(-34.357006,-16.986565,0),App.Vector(-34.357006,20.441460,0)))
App.getDocument('Unnamed').getObject('Sketch001').addGeometry(geoList,False)
conList = []
conList.append(Sketcher.Constraint('Coincident',0,2,1,1))
conList.append(Sketcher.Constraint('Coincident',1,2,2,1))
conList.append(Sketcher.Constraint('Coincident',2,2,3,1))
conList.append(Sketcher.Constraint('Coincident',3,2,0,1))
conList.append(Sketcher.Constraint('Horizontal',0))
conList.append(Sketcher.Constraint('Horizontal',2))
conList.append(Sketcher.Constraint('Vertical',1))
conList.append(Sketcher.Constraint('Vertical',3))
App.getDocument('Unnamed').getObject('Sketch001').addConstraint(conList)
del geoList, conList

App.ActiveDocument.recompute()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',25.008,-8.63723,20.4415,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.ExternalEdge1',25.008,-5.9501,5,False)
### Begin command Sketcher_ConstrainEqual
App.getDocument('Unnamed').getObject('Sketch001').addConstraint(Sketcher.Constraint('Equal',0,-3))
App.ActiveDocument.recompute()
### End command Sketcher_ConstrainEqual
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge4',25.008,-8.05696,13.5317,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.ExternalEdge2',25.008,-20,0.863725,False)
### Begin command Sketcher_ConstrainEqual
App.getDocument('Unnamed').getObject('Sketch001').addConstraint(Sketcher.Constraint('Equal',3,-4))
App.ActiveDocument.recompute()
### End command Sketcher_ConstrainEqual
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Vertex1',25.013,-8.05696,1.56943,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Vertex4',25.013,31.943,-8.43057,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Constraint9',25.018,0,-0.287908,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.RootPoint',25.01,0,0,False)
# Gui.runCommand('Sketcher_ConstrainSymmetric',0)
# Gui.runCommand('Sketcher_ConstrainSymmetric',0)
# Gui.runCommand('Sketcher_ConstrainSymmetric',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Vertex1',25.013,-8.05696,1.56943,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Vertex4',25.013,31.943,-8.43057,False)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.RootPoint',25.01,0,0,False)
### Begin command Sketcher_ConstrainSymmetric
App.getDocument('Unnamed').getObject('Sketch001').addConstraint(Sketcher.Constraint('Symmetric',0,1,1,2,-1,1))
App.ActiveDocument.recompute()
### End command Sketcher_ConstrainSymmetric
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch001')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch001').AttachmentOffset = App.Placement(App.Vector(0,0,2),App.Rotation(App.Vector(0,0,1),0))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,1.22846,-1.48206)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch002')
App.getDocument('Unnamed').getObject('Sketch002').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch002').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch002.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch002')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch002.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch002')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-11.5163,5)
App.getDocument('Unnamed').getObject('Sketch002').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch002')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch002.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch002').AttachmentOffset = App.Placement(App.Vector(0,0,2),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch002').AttachmentOffset = App.Placement(App.Vector(0,0,26),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch002').AttachmentOffset = App.Placement(App.Vector(0,0,26),App.Rotation(App.Vector(0,0,1),4))

FreeCAD.getDocument('Unnamed').getObject('Sketch002').AttachmentOffset = App.Placement(App.Vector(0,0,26),App.Rotation(App.Vector(0,0,1),45))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,-1.07481,-0.55399)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch003')
App.getDocument('Unnamed').getObject('Sketch003').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch003').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch003.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch003')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch003.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch003')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-9.75409,5)
App.getDocument('Unnamed').getObject('Sketch003').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch003')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch003.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch003').AttachmentOffset = App.Placement(App.Vector(0,0,5),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch003').AttachmentOffset = App.Placement(App.Vector(0,0,50),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch003').AttachmentOffset = App.Placement(App.Vector(0,0,50),App.Rotation(App.Vector(0,0,1),9))

FreeCAD.getDocument('Unnamed').getObject('Sketch003').AttachmentOffset = App.Placement(App.Vector(0,0,50),App.Rotation(App.Vector(0,0,1),90))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,10.7158,-1.33216)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch004')
App.getDocument('Unnamed').getObject('Sketch004').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch004').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch004.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch004')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch004.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch004')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-11.7085,5)
App.getDocument('Unnamed').getObject('Sketch004').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch004')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch004.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch004').AttachmentOffset = App.Placement(App.Vector(0,0,5),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch004').AttachmentOffset = App.Placement(App.Vector(0,0,52),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch004').AttachmentOffset = App.Placement(App.Vector(0,0,52),App.Rotation(App.Vector(0,0,1),9))

FreeCAD.getDocument('Unnamed').getObject('Sketch004').AttachmentOffset = App.Placement(App.Vector(0,0,52),App.Rotation(App.Vector(0,0,1),90))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,-3.87158,-0.687398)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch005')
App.getDocument('Unnamed').getObject('Sketch005').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch005').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch005.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch005')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch005.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch005')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-10.5569,5)
App.getDocument('Unnamed').getObject('Sketch005').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch005')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,7),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),13))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),135))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,3.80598,-2.06267)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch006')
App.getDocument('Unnamed').getObject('Sketch006').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch006').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch006.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch006')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch006.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch006')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-16.507,5)
App.getDocument('Unnamed').getObject('Sketch006').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch006')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch006.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,1),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,10),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,102),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,102),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,102),App.Rotation(App.Vector(0,0,1),18))

FreeCAD.getDocument('Unnamed').getObject('Sketch006').AttachmentOffset = App.Placement(App.Vector(0,0,102),App.Rotation(App.Vector(0,0,1),180))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,1.88659,-1.60169)
### Begin command PartDesign_NewSketch
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch007')
App.getDocument('Unnamed').getObject('Sketch007').Support = (App.getDocument('Unnamed').getObject('Pad'),['Face3',])
App.getDocument('Unnamed').getObject('Sketch007').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch007.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch007')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch007.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch007')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
### End command PartDesign_NewSketch
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-15.5473,5)
App.getDocument('Unnamed').getObject('Sketch007').carbonCopy("Sketch001",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch007')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch007.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch007').AttachmentOffset = App.Placement(App.Vector(0,0,1),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch007').AttachmentOffset = App.Placement(App.Vector(0,0,10),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch007').AttachmentOffset = App.Placement(App.Vector(0,0,104),App.Rotation(App.Vector(0,0,1),0))

# Gui.Selection.clearSelection()
# Gui.runCommand('PartDesign_AdditiveLoft',0)
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,5.30853,-2.40061)
### Begin command PartDesign_AdditiveLoft
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::AdditiveLoft','AdditiveLoft')
App.getDocument('Unnamed').getObject('AdditiveLoft').Profile = (App.getDocument('Unnamed').getObject('Pad'), ['Face3',])
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'AdditiveLoft.')
# Gui.Selection.clearSelection()
### End command PartDesign_AdditiveLoft
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,-6.61118,5)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch002.Edge1',51,-3.31543,3.75564)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch003.Edge4',75,2.81854,-20)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch004.Edge4',77,1.26865,-20)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge2',103,-13.8873,14.397)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge4',103,15.0848,-13.1994)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge1',103,-6.63356,-0.437512)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge3',103,-0.769821,7.84089)
# Gui.Selection.clearSelection()
App.getDocument('Unnamed').recompute()
# Gui.getDocument('Unnamed').resetEdit()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge1',103,-5.86948,-1.20159)
FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),4))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),45))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),4))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),9))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),13))

FreeCAD.getDocument('Unnamed').getObject('Sketch005').AttachmentOffset = App.Placement(App.Vector(0,0,78),App.Rotation(App.Vector(0,0,1),135))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch007.Edge1',129,1.26879,5)
### Begin command PartDesign_AdditiveLoft
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::AdditiveLoft','AdditiveLoft')
App.getDocument('Unnamed').getObject('AdditiveLoft').Profile = App.getDocument('Unnamed').getObject('Sketch007')
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Sketch007').Visibility = False
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'AdditiveLoft.')
# Gui.Selection.clearSelection()
### End command PartDesign_AdditiveLoft
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch006.Edge3',127,-2.5837,5)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge4',103,14.7599,-13.5244)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch004.Edge3',77,5,-3.20935)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch003.Edge1',75,-5,8.1248)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch002.Edge3',51,5.23283,-1.83824)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge3',27,4.53055,-5)
# Gui.Selection.clearSelection()
App.getDocument('Unnamed').recompute()
App.getDocument('Unnamed').getObject('Pad').Visibility = False
# Gui.getDocument('Unnamed').resetEdit()
App.getDocument('Unnamed').getObject('Sketch007').Visibility = False
App.getDocument('Unnamed').getObject('Sketch006').Visibility = False
App.getDocument('Unnamed').getObject('Sketch005').Visibility = False
App.getDocument('Unnamed').getObject('Sketch004').Visibility = False
App.getDocument('Unnamed').getObject('Sketch003').Visibility = False
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Sketch007.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Sketch006.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Sketch005.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Sketch004.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Sketch003.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.Face3',25,3.7347,-1.66456)
### Begin command PartDesign_AdditiveLoft
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::AdditiveLoft','AdditiveLoft001')
App.getDocument('Unnamed').getObject('AdditiveLoft001').Profile = (App.getDocument('Unnamed').getObject('AdditiveLoft'), ['Face3',])
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('AdditiveLoft001').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'AdditiveLoft001.')
# Gui.Selection.clearSelection()
### End command PartDesign_AdditiveLoft
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,3.71372,5)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch002.Edge1',51,-0.131253,6.93982)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch003.Edge1',75,-5,2.74714)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch004.Edge1',77,-5,-0.0287433)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge1',103,-2.32562,-4.74545)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch006.Edge3',127,5.91542,5)
# Gui.Selection.clearSelection()
App.getDocument('Unnamed').recompute()
# Gui.getDocument('Unnamed').resetEdit()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.')
### Begin command Std_Delete
App.getDocument('Unnamed').removeObject('AdditiveLoft')
App.getDocument('Unnamed').recompute()
### End command Std_Delete
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch003.Edge1',75,-5,4.9675)
### Begin command Std_Delete
App.getDocument('Unnamed').removeObject('Sketch003')
App.getDocument('Unnamed').recompute()
### End command Std_Delete
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Face3',25,6.92375,-1.60296)
### Begin command PartDesign_AdditiveLoft
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::AdditiveLoft','AdditiveLoft')
App.getDocument('Unnamed').getObject('AdditiveLoft').Profile = (App.getDocument('Unnamed').getObject('Pad'), ['Face3',])
App.ActiveDocument.recompute()
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('Pad').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'AdditiveLoft.')
# Gui.Selection.clearSelection()
### End command PartDesign_AdditiveLoft
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch001.Edge1',27,2.90543,5)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch002.Edge1',51,-6.87531,0.195762)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch004.Edge1',77,-5,2.46954)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch005.Edge1',103,-6.72927,-0.341799)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch006.Edge3',127,0.846894,5)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch007.Edge1',129,-2.3932,5)
# Gui.Selection.clearSelection()
App.getDocument('Unnamed').recompute()
App.getDocument('Unnamed').getObject('Pad').Visibility = False
# Gui.getDocument('Unnamed').resetEdit()
App.getDocument('Unnamed').getObject('Pad').Visibility = False
App.getDocument('Unnamed').getObject('Sketch001').Visibility = False
App.getDocument('Unnamed').getObject('Sketch002').Visibility = False
App.getDocument('Unnamed').getObject('Sketch004').Visibility = False
App.getDocument('Unnamed').getObject('Sketch005').Visibility = False
App.getDocument('Unnamed').getObject('Sketch006').Visibility = False
App.getDocument('Unnamed').getObject('Sketch007').Visibility = False
# Gui.Selection.addSelection('Unnamed','Body','Pad.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Pad.Sketch.')
### Begin command PartDesign_DuplicateSelection
FreeCADGui.runCommand('Std_DuplicateSelection')
App.getDocument('Unnamed').getObject('Body').addObject(App.getDocument('Unnamed').getObject('Sketch008'))
App.getDocument('Unnamed').getObject('Sketch008').Visibility = False
App.getDocument('Unnamed').getObject('Sketch008').Visibility = True
App.ActiveDocument.recompute()
### End command PartDesign_DuplicateSelection
# Gui.runCommand('PartDesign_NewSketch',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Origin.XZ_Plane.')
App.getDocument('Unnamed').getObject('Body').newObject('Sketcher::SketchObject','Sketch009')
App.getDocument('Unnamed').getObject('Sketch009').Support = (App.getDocument('Unnamed').getObject('XZ_Plane'),[''])
App.getDocument('Unnamed').getObject('Sketch009').MapMode = 'FlatFace'
App.ActiveDocument.recompute()
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Sketch009.')
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch009')
# tv = Show.TempoVis(App.ActiveDocument, tag= ActiveSketch.ViewObject.TypeId)
# ActiveSketch.ViewObject.TempoVis = tv
# if ActiveSketch.ViewObject.EditingWorkbench:
#   tv.activateWorkbench(ActiveSketch.ViewObject.EditingWorkbench)
# if ActiveSketch.ViewObject.HideDependent:
#   tv.hide(tv.get_all_dependent(App.getDocument('Unnamed').getObject('Body'), 'Sketch009.'))
# if ActiveSketch.ViewObject.ShowSupport:
#   tv.show([ref[0] for ref in ActiveSketch.Support if not ref[0].isDerivedFrom("PartDesign::Plane")])
# if ActiveSketch.ViewObject.ShowLinks:
#   tv.show([ref[0] for ref in ActiveSketch.ExternalGeometry])
# tv.sketchClipPlane(ActiveSketch, ActiveSketch.ViewObject.SectionView)
# tv.hide(ActiveSketch)
# del(tv)
# del(ActiveSketch)
# 
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch009')
# if ActiveSketch.ViewObject.RestoreCamera:
#   ActiveSketch.ViewObject.TempoVis.saveCamera()
#   if ActiveSketch.ViewObject.ForceOrtho:
#     ActiveSketch.ViewObject.Document.ActiveView.setCameraType('Orthographic')
# 
# Gui.Selection.clearSelection()
# Gui.runCommand('Sketcher_CarbonCopy',0)
# Gui.Selection.addSelection('Unnamed','Body','Sketch008.Edge3',6.30273,-20,0)
App.getDocument('Unnamed').getObject('Sketch009').carbonCopy("Sketch008",False)
App.ActiveDocument.recompute()
# Gui.Selection.clearSelection()
# Gui.getDocument('Unnamed').resetEdit()
App.ActiveDocument.recompute()
# ActiveSketch = App.getDocument('Unnamed').getObject('Sketch009')
# tv = ActiveSketch.ViewObject.TempoVis
# if tv:
#   tv.restore()
# ActiveSketch.ViewObject.TempoVis = None
# del(tv)
# del(ActiveSketch)
# 
# Gui.Selection.addSelection('Unnamed','Body','Sketch009.')
App.getDocument('Unnamed').recompute()
FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,1,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,2,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,3,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,4,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,5,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,6,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,7,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,8,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,9,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,10,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,11,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,12,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,13,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,14,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,15,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,16,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(1,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(2,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(3,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(4,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(5,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(6,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(7,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(8,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(9,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(10,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(11,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(12,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(13,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(14,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(15,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(16,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(17,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(18,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(19,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(20,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(21,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(22,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(23,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(24,0,1),0))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),0))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch008.')
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch009.')
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch008.')
# Gui.runCommand('Std_Delete',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch008.')
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.runCommand('Std_ToggleVisibility',0)
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch009.')
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch008.')
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.')
App.getDocument("Unnamed").getObject("Body").removeObject(App.getDocument("Unnamed").getObject("Sketch008"))
# App.getDocument('Unnamed').getObject('Sketch008').adjustRelativeLinks(App.getDocument('Unnamed').getObject('AdditiveLoft'))
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.dropObject(App.getDocument('Unnamed').getObject('Sketch008'),None,'',[])
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Sketch008')
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','AdditiveLoft.')
# App.getDocument('Unnamed').getObject('Sketch008').adjustRelativeLinks(App.getDocument('Unnamed').getObject('AdditiveLoft'))
# App.getDocument('Unnamed').getObject('AdditiveLoft').ViewObject.dropObject(App.getDocument('Unnamed').getObject('Sketch008'),None,'',[])
# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Sketch008')
FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),18))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(1,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(2,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(3,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(4,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(5,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(6,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(7,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(8,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(9,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(10,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(11,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(12,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(13,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(14,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(15,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(16,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(17,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(18,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(19,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(20,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(21,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(22,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(23,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(24,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(25,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(26,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(27,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(28,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(29,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(30,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(31,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(32,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(33,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(34,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(35,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(36,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(37,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(38,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(39,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(40,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(41,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(42,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(43,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(44,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(45,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(46,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(47,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(48,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(49,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(50,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch008').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),180))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch009.')
FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),2))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),3))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),4))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),5))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),6))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),7))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),8))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),9))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),10))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),11))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),12))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),13))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),14))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),15))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),16))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),17))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),18))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),19))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),20))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),21))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),22))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),23))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),24))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),25))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),26))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),27))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),28))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),29))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),30))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),31))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),32))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),33))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),34))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),35))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),36))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),37))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),38))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),39))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),40))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),41))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),42))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),43))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),44))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),45))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),46))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),47))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),48))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),49))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),50))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),51))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),52))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),53))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),54))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),55))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),56))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),57))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),58))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),59))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),60))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),61))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),62))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),63))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),64))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),65))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),66))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),67))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),68))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),69))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),70))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),71))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),72))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),73))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),74))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),75))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),76))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),77))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),78))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),79))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),80))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),81))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),82))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),83))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),84))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),85))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),86))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),87))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),88))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),89))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),90))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),1))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),18))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,2,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,3,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,4,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,5,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,6,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,7,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,8,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,7,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,6,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,5,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,4,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,3,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,2,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,0,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(0,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(1,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(2,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(3,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(4,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(5,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(6,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(7,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(8,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(9,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(10,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(11,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(12,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(13,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(14,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(15,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(16,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(17,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(18,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(19,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(20,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(21,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(22,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(23,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(24,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(25,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(26,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(27,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(28,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(29,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(30,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(31,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(32,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(33,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(34,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(35,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(36,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(37,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(38,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(39,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(40,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(41,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(42,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(43,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(44,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(45,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(46,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(47,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(48,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(49,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(50,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(51,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(52,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(53,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(54,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(55,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(56,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(57,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(58,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(59,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(60,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(61,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(62,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(63,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(64,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(65,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(66,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(67,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(68,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(69,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(70,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(71,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(72,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(73,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(74,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(75,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(76,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(77,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(78,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(79,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(80,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(81,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(82,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(83,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(84,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(85,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(86,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(87,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(88,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(89,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(90,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(91,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(92,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(93,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(94,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(95,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(96,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(97,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(98,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(99,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(100,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(101,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(102,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(103,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(104,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(105,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(106,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(107,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(108,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(109,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(110,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(111,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(112,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(113,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(114,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(115,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(116,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(117,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(118,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(119,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(120,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(121,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(122,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(123,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(124,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(125,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(126,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(127,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(128,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(129,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(130,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(131,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(132,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(133,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(134,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(135,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(136,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(137,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(138,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(139,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(140,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(141,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(142,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(143,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(144,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(145,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(146,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(147,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(148,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(149,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(150,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(151,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(152,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(153,0,0),App.Rotation(App.Vector(0,1,1),180))

FreeCAD.getDocument('Unnamed').getObject('Sketch009').AttachmentOffset = App.Placement(App.Vector(154,0,0),App.Rotation(App.Vector(0,1,1),180))

# Gui.Selection.clearSelection()
# Gui.Selection.addSelection('Unnamed','Body','Sketch009.')
### Begin command Std_Delete
App.getDocument('Unnamed').removeObject('Sketch009')
App.getDocument('Unnamed').recompute()
### End command Std_Delete
# Gui.Selection.clearSelection()
# Gui.runCommand('Std_Undo',0)
### Begin command PartDesign_Pad
App.getDocument('Unnamed').getObject('Body').newObject('PartDesign::Pad','Pad001')
App.getDocument('Unnamed').getObject('Pad001').Profile = App.getDocument('Unnamed').getObject('Sketch009')
App.getDocument('Unnamed').getObject('Pad001').Length = 10
App.ActiveDocument.recompute()
App.getDocument('Unnamed').getObject('Pad001').ReferenceAxis = (App.getDocument('Unnamed').getObject('Sketch009'),['N_Axis'])
App.getDocument('Unnamed').getObject('Sketch009').Visibility = False
App.ActiveDocument.recompute()
# App.getDocument('Unnamed').getObject('Pad001').ViewObject.ShapeColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'ShapeColor',App.getDocument('Unnamed').getObject('Pad001').ViewObject.ShapeColor)
# App.getDocument('Unnamed').getObject('Pad001').ViewObject.LineColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'LineColor',App.getDocument('Unnamed').getObject('Pad001').ViewObject.LineColor)
# App.getDocument('Unnamed').getObject('Pad001').ViewObject.PointColor=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'PointColor',App.getDocument('Unnamed').getObject('Pad001').ViewObject.PointColor)
# App.getDocument('Unnamed').getObject('Pad001').ViewObject.Transparency=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'Transparency',App.getDocument('Unnamed').getObject('Pad001').ViewObject.Transparency)
# App.getDocument('Unnamed').getObject('Pad001').ViewObject.DisplayMode=getattr(App.getDocument('Unnamed').getObject('AdditiveLoft').getLinkedObject(True).ViewObject,'DisplayMode',App.getDocument('Unnamed').getObject('Pad001').ViewObject.DisplayMode)
# Gui.getDocument('Unnamed').setEdit(App.getDocument('Unnamed').getObject('Body'), 0, 'Pad001.')
# Gui.Selection.clearSelection()
### End command PartDesign_Pad
App.getDocument('Unnamed').getObject('Pad001').Length = 10.000000
App.getDocument('Unnamed').getObject('Pad001').TaperAngle = 0.000000
App.getDocument('Unnamed').getObject('Pad001').UseCustomVector = 0
App.getDocument('Unnamed').getObject('Pad001').Direction = (0, 0, 1)
App.getDocument('Unnamed').getObject('Pad001').ReferenceAxis = (App.getDocument('Unnamed').getObject('Sketch009'), ['N_Axis'])
App.getDocument('Unnamed').getObject('Pad001').AlongSketchNormal = 1
App.getDocument('Unnamed').getObject('Pad001').Type = 0
App.getDocument('Unnamed').getObject('Pad001').UpToFace = None
App.getDocument('Unnamed').getObject('Pad001').Reversed = 0
App.getDocument('Unnamed').getObject('Pad001').Midplane = 1
App.getDocument('Unnamed').getObject('Pad001').Offset = 0
App.getDocument('Unnamed').recompute()
App.getDocument('Unnamed').getObject('AdditiveLoft').Visibility = False
# Gui.getDocument('Unnamed').resetEdit()
App.getDocument('Unnamed').getObject('Sketch009').Visibility = False
# Gui.Selection.addSelection('Unnamed','Sketch008')
# Gui.runCommand('Std_ToggleVisibility',0)
# Macro End: C:\Users\CHINNI\AppData\Roaming\FreeCAD\Macro\Twisted part.FCMacro +++++++++++++++++++++++++++++++++++++++++++++++++
