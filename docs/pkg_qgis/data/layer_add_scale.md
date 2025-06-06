### Set up Data Scale for Layer

<https://qgis.org/pyqgis/master/core/QgsMapLayerElevationProperties.html>

### Set up Rendering Scale for a Layer

To set up a scale for a layer in QGIS, right-click on the layer in the Layers panel, select "Properties", then navigate to the "Rendering" tab where you can enable "Scale Dependent Visibility" and define the minimum and maximum scales at which the layer should be visible on the map; essentially, controlling when the layer appears or disappears based on the zoom level.
Key steps:
Access Layer Properties: Right-click on the desired layer in the Layers panel and select "Properties".
Go to Rendering Tab: In the Layer Properties dialog, navigate to the "Rendering" tab.
Enable Scale Dependent Visibility: Check the box next to "Scale Dependent Visibility".
Set Minimum and Maximum Scales:
Minimum Scale: Enter the scale at which the layer should start appearing on the map.
Maximum Scale: Enter the scale at which the layer should become fully visible.

### Set Z Scale for a Layer in QGIS

To set the Z scale for a layer in QGIS, you can access the layer properties and use the "zScale" attribute within the "QgsMapLayerElevationProperties" class; this allows you to manually adjust the scaling factor applied to the Z values of your layer, effectively changing the vertical exaggeration of the data.
How to do it:
Select your layer:
In the Layers panel, click on the layer you want to modify the Z scale for.
Access layer properties:
Right-click on the layer and choose "Properties".
Alternatively, you can use the "Layer" menu and select "Properties".
Find the Z scale setting:
Within the layer properties dialog, navigate to the relevant section related to elevation data (usually under "Data Source" or "Geometry").
Look for the "Z Scale" option, which will allow you to input a numerical value to adjust the vertical scaling.
Key points to remember:
Scaling factor:
A Z scale value of 1 represents no change to the Z values, while a value greater than 1 will vertically exaggerate the data, and a value less than 1 will compress it.
Python scripting:
If you are working with Python scripting in QGIS, you can directly access the "zScale" attribute of a layer object to modify the Z scale programmatically.

Alternative methods for adjusting Z values in QGIS:
Drape tool:
Use the "Drape (set Z value from raster)" processing tool to automatically assign Z values to a vector layer based on a raster elevation model.
Feature Z Setter plugin:
This plugin allows you to set Z values for individual features based on a reference DEM layer with additional offset options.
