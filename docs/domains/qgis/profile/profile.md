
### Summary

- Software plugin:
  - Need a profile tool plugin.
- Add existing route projection and then extract profile.

- Changing Units:
  - <https://www.youtube.com/watch?v=UvMlX24kuMk>
  - Bottom right corner of the screen - UTM 32735 to convert to meeters


### References

<https://www.youtube.com/watch?v=ZikXG-SDzSY>

### Log

### python commands

processing.run("qgis:tininterpolation", {'INTERPOLATION_DATA':'//dgi-hou-fs01/Projects/61863 Talos Lakach FEED/999 Work Space/ARosales/Terrain.shp|layername=Terrain::~::0::~::7::~::0','METHOD':0,'EXTENT':'184001.545700000,306395.715600000,2067807.136300000,2113760.718900000 [EPSG:32615]','PIXEL_SIZE':10,'OUTPUT':'C:/Temp/qgis/dem.tif'})

Error:
Feature (154) from ?Extracted? has invalid geometry. Please fix the geometry or change the ?Invalid features filtering? option for this input or globally in Processing settings.

Setings ->

 Options -> Processing -> General -> Invalid features filtering -> Ignore features with invalid geometries

Execution failed after 0.31 seconds
