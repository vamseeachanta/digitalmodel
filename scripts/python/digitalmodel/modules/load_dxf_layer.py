from qgis.core import QgsVectorLayer, QgsProject

# Load a DXF layer
def load_dxf_layer(dxf_filename, layer_name):
    uri = dxf_filename+"|layername=entities|geometrytype=LineString|uniqueGeometryType=yes"
    vlayer = QgsVectorLayer(uri, layer_name, "ogr")
    if vlayer.isValid():
        QgsProject.instance().addMapLayer(vlayer)
