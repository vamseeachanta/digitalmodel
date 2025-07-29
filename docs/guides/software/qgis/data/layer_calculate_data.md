
### Processing ToolBox



### Reference Information

Editing Data Methods:

- Method 1:
- you can use Geoprocessing toolbox ? set Z value (might be called slightly different), and set your polygons Z coordinate to 0 (or even 77).
- If the geometries are flat but you actually want them to be elevated, you can set Z value in the same way, choosing ?set value from the field? and point to ?ELEVATION? field.
- Finally, you can add Speckle ?Transformation? to extrude your polygons from whenever they currently are, and choose the field ?DERIVED HEIGHT? for extrusion.
- Additionally, if you want your building elevation to be correct relative to each other, but adjusted to 77m as ?zero level?, just add a New Field in the attribute table, using the Field Calculator formula ?elevation - 77?. Then you can use this new field instead of ELEVATION.

<https://speckle.community/t/qgis-workflow-reference-elevation-based-on-attribute-table/9999>
