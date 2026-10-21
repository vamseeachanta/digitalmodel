# Introduction

Software:
<https://www.freecad.org/>

Create objects in freeCAD?  
<https://www.youtube.com/@FreeCADAcademy>

### Summary

Freecad Scripting Channel
<https://www.youtube.com/watch?v=KujiAIHNRKQ&list=PLWuyJLVUNtc15o92Bo6SgtYzXt7zlSIsh>

### ?

### Lessons

#### Basic Scripts

- Print Hello World
- Load Document
- Simple User input

## Training

A good introduction or training video for FreeCAD3D
<https://docs.google.com/presentation/d/1mx08eQhsbkhgh_vZBNE2pLhkdCRrDWuX/edit#slide=id.p1>

## Other Tools

Open source for use in non-Autodesk CAD programs: .dxf &  .iges

What is .step files?

Option 1: Add ODA File Converter to freeCAD to open dwg drawings.
<https://www.opendesign.com/guestfiles/oda_file_converter>
**ODA Converter is NOT Working Seamlessly in FreeCAD**

A file extension comparisons with features.

Option 2:

## Macro Resources

https://github.com/FreeCAD/FreeCAD-macros

## References

<https://all3dp.com/2/freecad-dwg-import-dwg-files/>

[Batch export of files](<https://cadplus.xarial.com/batch/>)
[File Formats Supported for Exporting SOLIDWORKS Files](https://help.solidworks.com/2022/English/SWConnected/swdotworks/t_export_sw_non_sw_file.htm#:~:text=SLDASM%2C%20or%20*.,export%20options%20and%20click%20OK.)

<https://mechnexus.com/category/freecad-articles/>

## Parametric CAD judge pilot

The [flanged spool pilot](../../../examples/freecad/flanged_spool/README.md)
under #2804 and #2272 joins the FreeCAD design-tools and gmsh meshing modules.
It documents an independent annulus/frustum steel-volume calculation, geometry
measurements, fixed mesh-quality limits, and a planted thickness defect test.
Native CAD/mesh acceptance remains unverified when those dependencies are absent;
the comparator and judge rejection tests run separately.
