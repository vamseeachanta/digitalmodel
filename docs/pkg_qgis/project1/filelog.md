
## Folder and File Description

- folder, _ss : superseded files
- folder, inputs - input files
  - folder, lines : line files
  - folder, terrain : combined terrain files
- intermediates - intermediate outputs
  - DEM_20px_m : DEM files with 20px resolution. For detailed analysis and final outputs.
  - DEM_m : DEM files with 100px resolution. For fast analysis to check output format, sanity checks etc.
- outputs - output files
- py - python scripts
  - load_dxf_layer.py : Load dxf as a layer in QGIS
  - load_lakach_lines.py : Load all dxf liens of the project in QGIS

## QGIS Files


| File Name | Description | Comments |
| --- | --- | --- |
| 2024_12_11_Lakach_TIN.qgz | QGIS project file with all lines loaded |  |
| layer_board.csv | Board layer |  Contains all reference input files|
