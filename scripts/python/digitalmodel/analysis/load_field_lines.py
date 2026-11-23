from qgis.core import QgsVectorLayer, QgsProject

def delete_layer_if_exists(layer_name):
    project = QgsProject.instance()
    layer = project.mapLayersByName(layer_name)

    if layer:
        project.removeMapLayer(layer[0].id())
        print(f"Layer '{layer_name}' deleted.")
    else:
        print(f"Layer '{layer_name}' not found.")

# Load a DXF layer
def load_dxf_layer(dxf_filename, layer_name):
    delete_layer_if_exists(layer_name)
    uri = dxf_filename+"|layername=entities|geometrytype=LineString|uniqueGeometryType=yes"
    vlayer = QgsVectorLayer(uri, layer_name, "ogr")
    if vlayer.isValid():
        QgsProject.instance().addMapLayer(vlayer)


# Input Files: the DXF folder is DIGITALMODEL_PRIVATE_DATA.
import os

DXF_DIR = os.environ.get("DIGITALMODEL_PRIVATE_DATA")
if not DXF_DIR:
    raise RuntimeError(
        "Set DIGITALMODEL_PRIVATE_DATA to the folder holding the field-line DXF files."
    )

layer_name = "18inch_flowline_main"
dxf_filename = os.path.join(DXF_DIR, "18inch Flowline_4.dxf")
load_dxf_layer(dxf_filename, layer_name)

layer_name = "18inch_flowline_infield"
dxf_filename = os.path.join(DXF_DIR, "18inch Flowline_4.1.dxf")
load_dxf_layer(dxf_filename, layer_name)

layer_name = "umbilical_main"
dxf_filename = os.path.join(DXF_DIR, "Main Umbilical_2.1.dxf")
load_dxf_layer(dxf_filename, layer_name)

layer_name = "umbilical_infield"
dxf_filename = os.path.join(DXF_DIR, "Main Infield Umbilical_2.1.dxf")
load_dxf_layer(dxf_filename, layer_name)

layer_name = "4inch_ct"
dxf_filename = os.path.join(DXF_DIR, "4inch Coiled Pipeline_2.1.dxf")
load_dxf_layer(dxf_filename, layer_name)

layer_name = "4inch_saline"
dxf_filename = os.path.join(DXF_DIR, "4inch Saline Diffuser Line_2.1.dxf")
load_dxf_layer(dxf_filename, layer_name)

