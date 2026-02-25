"""Generate Python scripts for the QGIS Processing framework.

This module produces script *text* for QGIS. It does NOT import qgis itself.
"""
from __future__ import annotations

import textwrap
from pathlib import Path


class QGISScriptGenerator:
    """Generate QGIS Processing Python scripts."""

    @classmethod
    def generate_load_layer_script(
        cls,
        filepath: str,
        layer_name: str = "layer",
    ) -> str:
        """Generate a QGIS script to load a vector layer.

        Supports GeoJSON, Shapefile, KML and other OGR-compatible formats.
        """
        return textwrap.dedent(f"""\
            from qgis.core import QgsVectorLayer, QgsProject

            layer = QgsVectorLayer({filepath!r}, {layer_name!r}, "ogr")
            if not layer.isValid():
                raise RuntimeError(f"Failed to load layer from {filepath!r}")

            QgsProject.instance().addMapLayer(layer)
            print(f"Loaded layer '{{layer.name()}}' with {{layer.featureCount()}} features")
        """)

    @classmethod
    def generate_buffer_analysis_script(
        cls,
        input_layer: str,
        distance_m: float,
        output_path: str,
    ) -> str:
        """Generate a buffer analysis script using ``processing.run``."""
        return textwrap.dedent(f"""\
            import processing

            result = processing.run(
                "native:buffer",
                {{
                    "INPUT": {input_layer!r},
                    "DISTANCE": {distance_m!r},
                    "SEGMENTS": 5,
                    "END_CAP_STYLE": 0,
                    "JOIN_STYLE": 0,
                    "MITER_LIMIT": 2,
                    "DISSOLVE": False,
                    "OUTPUT": {output_path!r},
                }},
            )
            print(f"Buffer output: {{result['OUTPUT']}}")
        """)

    @classmethod
    def generate_heatmap_script(
        cls,
        input_layer: str,
        radius: float,
        output_path: str,
    ) -> str:
        """Generate a heatmap raster script using ``processing.run``."""
        return textwrap.dedent(f"""\
            import processing

            result = processing.run(
                "qgis:heatmapkerneldensityestimation",
                {{
                    "INPUT": {input_layer!r},
                    "RADIUS": {radius!r},
                    "PIXEL_SIZE": 10,
                    "OUTPUT": {output_path!r},
                }},
            )
            print(f"Heatmap output: {{result['OUTPUT']}}")
        """)

    @classmethod
    def save_script(cls, script: str, filepath) -> Path:
        """Write *script* text to a ``.py`` file and return its path."""
        path = Path(filepath)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(script, encoding="utf-8")
        return path
