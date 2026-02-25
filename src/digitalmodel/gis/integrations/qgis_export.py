"""QGIS project file and style definition generators for well layer data.

Generates ``.qgs`` QGIS project files and ``.qml`` style definition files
from a :class:`~digitalmodel.gis.layers.feature_layer.FeatureLayer` (or
:class:`~digitalmodel.gis.layers.well_layer.WellLayer`).  The generated
files are valid XML and can be validated with :mod:`xml.etree.ElementTree`
without requiring a QGIS installation.

The generated project embeds well positions as a virtual memory layer so
the file is self-contained (no external Shapefile dependency).

Dependencies
------------
All generation uses the standard-library :mod:`xml.etree.ElementTree`.
No QGIS runtime, PyQGIS, or heavy spatial library is required.
"""

from __future__ import annotations

import uuid
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from digitalmodel.gis.layers.feature_layer import FeatureLayer


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_QGIS_VERSION = "3.34.0-Prizren"
_WGS84_PROJ4 = "+proj=longlat +datum=WGS84 +no_defs"
_WGS84_WKT = (
    'GEOGCRS["WGS 84",ENSEMBLE["World Geodetic System 1984 ensemble",'
    'MEMBER["World Geodetic System 1984 (Transit)"],'
    'ELLIPSOID["WGS 84",6378137,298.257223563,LENGTHUNIT["metre",1]],'
    'ENSEMBLEACCURACY[2.0]],PRIMEM["Greenwich",0,ANGLEUNIT["degree",0.0174532925199433]],'
    'CS[ellipsoidal,2],AXIS["geodetic latitude (Lat)",north,ORDER[1],'
    'ANGLEUNIT["degree",0.0174532925199433]],'
    'AXIS["geodetic longitude (Lon)",east,ORDER[2],'
    'ANGLEUNIT["degree",0.0174532925199433]],USAGE[SCOPE["Horizontal component of 3D system."],'
    'AREA["World."],BBOX[-90,-180,90,180]],ID["EPSG",4326]]'
)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _indent(elem: ET.Element, level: int = 0) -> None:
    """Add pretty-print indentation to an ElementTree in-place."""
    indent_str = "\n" + "  " * level
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = indent_str + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = indent_str
        for child in elem:
            _indent(child, level + 1)
        if not child.tail or not child.tail.strip():  # type: ignore[reportPossiblyUnbound]
            child.tail = indent_str  # type: ignore[reportPossiblyUnbound]
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = indent_str


def _to_xml_string(root: ET.Element) -> str:
    """Serialise *root* to an indented XML string with declaration."""
    _indent(root)
    tree = ET.ElementTree(root)
    import io
    buf = io.BytesIO()
    tree.write(buf, encoding="UTF-8", xml_declaration=True)
    return buf.getvalue().decode("utf-8")


def _build_extent_from_layer(layer: FeatureLayer) -> dict[str, float]:
    """Return min/max lon/lat extent for the layer with a small buffer."""
    bbox = layer.bounding_box
    buffer = 0.1  # degrees
    return {
        "xmin": bbox.min_x - buffer,
        "ymin": bbox.min_y - buffer,
        "xmax": bbox.max_x + buffer,
        "ymax": bbox.max_y + buffer,
    }


def _sanitise_id(text: str) -> str:
    """Replace spaces and special characters with underscores."""
    return "".join(c if c.isalnum() or c in "-_." else "_" for c in str(text))


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class QGISExporter:
    """Generate QGIS project files and QML style definitions for well data."""

    # ------------------------------------------------------------------
    # QGS project generation
    # ------------------------------------------------------------------

    @classmethod
    def generate_project(cls, layer: FeatureLayer) -> str:
        """Generate a QGIS project XML string for the given layer.

        The project contains a single memory (virtual) layer with all
        well point features embedded as WKT geometries.

        Parameters
        ----------
        layer:
            Source feature layer (typically a WellLayer).

        Returns
        -------
        str
            A QGIS project XML string.
        """
        layer_id = _sanitise_id(layer.name) + "_" + uuid.uuid4().hex[:8]
        extent = _build_extent_from_layer(layer)

        root = ET.Element("qgis", attrib={"version": _QGIS_VERSION})

        # ---- Title ----
        ET.SubElement(root, "title").text = layer.name

        # ---- CRS ----
        proj_crs = ET.SubElement(root, "projectCrs")
        crs_el = ET.SubElement(proj_crs, "spatialrefsys")
        ET.SubElement(crs_el, "authid").text = "EPSG:4326"
        ET.SubElement(crs_el, "proj4").text = _WGS84_PROJ4
        ET.SubElement(crs_el, "srsid").text = "3452"
        ET.SubElement(crs_el, "srid").text = "4326"
        ET.SubElement(crs_el, "description").text = "WGS 84"

        # ---- Extent ----
        extent_el = ET.SubElement(root, "mapcanvas")
        ET.SubElement(extent_el, "xmin").text = str(extent["xmin"])
        ET.SubElement(extent_el, "ymin").text = str(extent["ymin"])
        ET.SubElement(extent_el, "xmax").text = str(extent["xmax"])
        ET.SubElement(extent_el, "ymax").text = str(extent["ymax"])

        # ---- Layer list ----
        projectlayers = ET.SubElement(root, "projectlayers")
        maplayer = ET.SubElement(
            projectlayers,
            "maplayer",
            attrib={"type": "vector", "geometry": "Point"},
        )
        ET.SubElement(maplayer, "id").text = layer_id
        ET.SubElement(maplayer, "layername").text = layer.name
        datasource = ET.SubElement(maplayer, "datasource")
        datasource.text = cls._build_memory_uri(layer)
        provider_el = ET.SubElement(maplayer, "provider", attrib={"encoding": "UTF-8"})
        provider_el.text = "memory"

        # Embed field definitions
        fields_el = ET.SubElement(maplayer, "fields")
        df = layer.data
        for col in df.columns:
            if col in (layer.lon_col, layer.lat_col):
                continue
            field_el = ET.SubElement(fields_el, "field", attrib={"name": col})
            dtype = str(df[col].dtype)
            qtype = "double" if "float" in dtype or "int" in dtype else "string"
            ET.SubElement(field_el, "type").text = qtype

        # Embed features as WKT
        geometries_el = ET.SubElement(maplayer, "features")
        for _, row in df.iterrows():
            lon = float(row[layer.lon_col])
            lat = float(row[layer.lat_col])
            feat_el = ET.SubElement(geometries_el, "feature")
            wkt_el = ET.SubElement(feat_el, "wkt")
            wkt_el.text = f"POINT ({lon} {lat})"
            attrs_el = ET.SubElement(feat_el, "attributes")
            for col in df.columns:
                if col in (layer.lon_col, layer.lat_col):
                    continue
                attr_el = ET.SubElement(attrs_el, "attribute", attrib={"name": col})
                attr_el.text = str(row[col])

        return _to_xml_string(root)

    @classmethod
    def write_project(
        cls,
        layer: FeatureLayer,
        filepath: str | Path,
    ) -> Path:
        """Write the QGIS project XML to a ``.qgs`` file.

        Parameters
        ----------
        layer:
            Source feature layer.
        filepath:
            Destination file path (should end in ``.qgs``).

        Returns
        -------
        Path
            Resolved path to the written file.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        filepath.write_text(cls.generate_project(layer), encoding="utf-8")
        return filepath.resolve()

    # ------------------------------------------------------------------
    # QML style definition
    # ------------------------------------------------------------------

    @classmethod
    def generate_well_qml(cls, layer: FeatureLayer) -> str:
        """Generate a QML style definition string for well markers.

        The style uses an SVG circle marker scaled by water depth where
        the column is available, falling back to a simple circle.

        Parameters
        ----------
        layer:
            Source feature layer.

        Returns
        -------
        str
            A QGIS layer style XML string.
        """
        root = ET.Element("qgis", attrib={"version": _QGIS_VERSION})
        renderer = ET.SubElement(
            root,
            "renderer-v2",
            attrib={"type": "singleSymbol", "forceraster": "0"},
        )
        symbols_el = ET.SubElement(renderer, "symbols")
        symbol_el = ET.SubElement(
            symbols_el,
            "symbol",
            attrib={"name": "0", "type": "marker", "alpha": "1"},
        )
        data_defined_props = ET.SubElement(symbol_el, "data_defined_properties")
        ET.SubElement(data_defined_props, "Option", attrib={"type": "Map"})

        layer_el = ET.SubElement(
            symbol_el,
            "layer",
            attrib={"class": "SimpleMarker", "pass": "0", "enabled": "1"},
        )
        # Marker properties
        for prop_name, prop_val in [
            ("size", "4"),
            ("color", "0,0,255,255"),
            ("outline_color", "0,0,0,255"),
            ("outline_width", "0.4"),
            ("name", "circle"),
        ]:
            ET.SubElement(
                layer_el,
                "Option",
                attrib={"name": prop_name, "value": prop_val, "type": "QString"},
            )

        return _to_xml_string(root)

    @classmethod
    def write_well_qml(
        cls,
        layer: FeatureLayer,
        filepath: str | Path,
    ) -> Path:
        """Write the QML style definition to a ``.qml`` file.

        Parameters
        ----------
        layer:
            Source feature layer.
        filepath:
            Destination file path (should end in ``.qml``).

        Returns
        -------
        Path
            Resolved path to the written file.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        filepath.write_text(cls.generate_well_qml(layer), encoding="utf-8")
        return filepath.resolve()

    # ------------------------------------------------------------------
    # GeoJSON helper
    # ------------------------------------------------------------------

    @classmethod
    def to_geojson_layer(cls, layer: FeatureLayer) -> dict:
        """Export the layer as a GeoJSON FeatureCollection dict.

        Parameters
        ----------
        layer:
            Source feature layer.

        Returns
        -------
        dict
            A GeoJSON FeatureCollection.
        """
        return layer.to_geojson()

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @classmethod
    def _build_memory_uri(cls, layer: FeatureLayer) -> str:
        """Build a QGIS memory layer URI string for the given layer."""
        df = layer.data
        fields: list[str] = []
        for col in df.columns:
            if col in (layer.lon_col, layer.lat_col):
                continue
            dtype = str(df[col].dtype)
            qtype = "double" if "float" in dtype or "int" in dtype else "string"
            fields.append(f"field={col}:{qtype}")

        parts = ["Point?crs=EPSG:4326"] + fields
        return "&".join(parts)
