"""Blender Python script generator for geospatial well visualization.

Generates a self-contained Blender Python script that, when executed inside
Blender's scripting workspace, places well markers in 3D space with proper
geospatial positioning.  The generated script is pure Python and can be
validated with :func:`ast.parse` without a Blender installation.

Coordinate mapping
------------------
Geographic coordinates (WGS84 longitude/latitude) are first projected to a
projected coordinate system (UTM, defaulting to the zone of the layer
centroid), then offset relative to a scene origin anchor so that the
centroid maps to (0, 0) in Blender scene units.  A configurable
``scale_factor`` converts metres to Blender units (default 1 m = 0.001 units,
i.e. km scale).

No external libraries (pyproj, etc.) are required at generation time.  The
generated script itself does not depend on pyproj inside Blender.
"""

from __future__ import annotations

import math
import textwrap
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from digitalmodel.gis.layers.feature_layer import FeatureLayer


_DEFAULT_SCALE = 0.001  # 1 m = 0.001 Blender units (km scale)


# ---------------------------------------------------------------------------
# Coordinate helpers
# ---------------------------------------------------------------------------


def _wgs84_to_utm_simple(
    longitude: float,
    latitude: float,
) -> tuple[float, float, int, str]:
    """Simplified WGS84 → UTM conversion (no pyproj required).

    Uses the standard UTM projection formulas with the WGS84 ellipsoid.
    Accuracy is suitable for scene positioning (< 1 m error over a field).

    Returns
    -------
    tuple[float, float, int, str]
        ``(easting, northing, zone, hemisphere)``
    """
    a = 6_378_137.0
    f = 1.0 / 298.257_223_563
    b = a * (1 - f)
    e2 = 1 - (b / a) ** 2
    e_prime2 = e2 / (1 - e2)
    k0 = 0.9996

    zone = int(math.floor((longitude + 180.0) / 6.0)) + 1
    zone = min(zone, 60)
    central_meridian = math.radians((zone - 1) * 6 - 180 + 3)

    lat_r = math.radians(latitude)
    lon_r = math.radians(longitude)

    n = a / math.sqrt(1 - e2 * math.sin(lat_r) ** 2)
    t = math.tan(lat_r) ** 2
    c = e_prime2 * math.cos(lat_r) ** 2
    A = math.cos(lat_r) * (lon_r - central_meridian)

    M = a * (
        (1 - e2 / 4 - 3 * e2**2 / 64 - 5 * e2**3 / 256) * lat_r
        - (3 * e2 / 8 + 3 * e2**2 / 32 + 45 * e2**3 / 1024) * math.sin(2 * lat_r)
        + (15 * e2**2 / 256 + 45 * e2**3 / 1024) * math.sin(4 * lat_r)
        - (35 * e2**3 / 3072) * math.sin(6 * lat_r)
    )

    easting = (
        k0
        * n
        * (
            A
            + (1 - t + c) * A**3 / 6
            + (5 - 18 * t + t**2 + 72 * c - 58 * e_prime2) * A**5 / 120
        )
        + 500_000.0
    )

    northing_raw = k0 * (
        M
        + n
        * math.tan(lat_r)
        * (
            A**2 / 2
            + (5 - t + 9 * c + 4 * c**2) * A**4 / 24
            + (61 - 58 * t + t**2 + 600 * c - 330 * e_prime2) * A**6 / 720
        )
    )

    hemisphere = "N" if latitude >= 0 else "S"
    northing = northing_raw if latitude >= 0 else northing_raw + 10_000_000.0

    return easting, northing, zone, hemisphere


def _wgs84_to_utm_zone(
    longitude: float,
    latitude: float,
    zone: int,
    hemisphere: str,
) -> tuple[float, float, int, str]:
    """Project WGS84 to UTM using a specified zone (for cross-zone consistency).

    Same formulas as :func:`_wgs84_to_utm_simple` but with the zone and
    hemisphere explicitly provided rather than auto-detected.
    """
    a = 6_378_137.0
    f = 1.0 / 298.257_223_563
    b = a * (1 - f)
    e2 = 1 - (b / a) ** 2
    e_prime2 = e2 / (1 - e2)
    k0 = 0.9996

    central_meridian = math.radians((zone - 1) * 6 - 180 + 3)

    lat_r = math.radians(latitude)
    lon_r = math.radians(longitude)

    n = a / math.sqrt(1 - e2 * math.sin(lat_r) ** 2)
    t = math.tan(lat_r) ** 2
    c = e_prime2 * math.cos(lat_r) ** 2
    A = math.cos(lat_r) * (lon_r - central_meridian)

    M = a * (
        (1 - e2 / 4 - 3 * e2**2 / 64 - 5 * e2**3 / 256) * lat_r
        - (3 * e2 / 8 + 3 * e2**2 / 32 + 45 * e2**3 / 1024) * math.sin(2 * lat_r)
        + (15 * e2**2 / 256 + 45 * e2**3 / 1024) * math.sin(4 * lat_r)
        - (35 * e2**3 / 3072) * math.sin(6 * lat_r)
    )

    easting = (
        k0
        * n
        * (
            A
            + (1 - t + c) * A**3 / 6
            + (5 - 18 * t + t**2 + 72 * c - 58 * e_prime2) * A**5 / 120
        )
        + 500_000.0
    )

    northing_raw = k0 * (
        M
        + n
        * math.tan(lat_r)
        * (
            A**2 / 2
            + (5 - t + 9 * c + 4 * c**2) * A**4 / 24
            + (61 - 58 * t + t**2 + 600 * c - 330 * e_prime2) * A**6 / 720
        )
    )

    northing = northing_raw if hemisphere == "N" else northing_raw + 10_000_000.0

    return easting, northing, zone, hemisphere


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class BlenderExporter:
    """Generate Blender Python scripts for geospatial well visualization."""

    @staticmethod
    def geo_to_blender(
        easting: float,
        northing: float,
        origin_easting: float,
        origin_northing: float,
        scale_factor: float = _DEFAULT_SCALE,
    ) -> tuple[float, float]:
        """Convert projected coordinates to Blender scene units.

        Offsets the point from a scene origin and applies the scale factor.

        Parameters
        ----------
        easting:
            UTM easting of the point in metres.
        northing:
            UTM northing of the point in metres.
        origin_easting:
            UTM easting of the scene origin anchor in metres.
        origin_northing:
            UTM northing of the scene origin anchor in metres.
        scale_factor:
            Metres-to-Blender-units multiplier (default: 0.001 = km scale).

        Returns
        -------
        tuple[float, float]
            ``(blender_x, blender_y)`` in Blender scene units.
        """
        bx = (easting - origin_easting) * scale_factor
        by = (northing - origin_northing) * scale_factor
        return bx, by

    @classmethod
    def generate_well_script(
        cls,
        layer: FeatureLayer,
        scale_factor: float = _DEFAULT_SCALE,
        well_height_units: float = 0.05,
    ) -> str:
        """Generate a Blender Python script that places well markers in 3D space.

        Parameters
        ----------
        layer:
            A :class:`~digitalmodel.gis.layers.feature_layer.FeatureLayer`
            or :class:`~digitalmodel.gis.layers.well_layer.WellLayer` whose
            rows represent well locations.
        scale_factor:
            Metres-to-Blender-units multiplier (default 0.001).
        well_height_units:
            Height of the generated cylinder marker in Blender units.

        Returns
        -------
        str
            A self-contained Python script for use inside Blender.
        """
        centroid = layer.centroid
        origin_e, origin_n, _zone, _hemi = _wgs84_to_utm_simple(
            centroid.x, centroid.y
        )

        # Use the centroid's UTM zone for ALL points to ensure consistent
        # projected coordinates even when data spans UTM zone boundaries.
        wells: list[dict] = []
        df = layer.data
        for _, row in df.iterrows():
            lon = float(row[layer.lon_col])
            lat = float(row[layer.lat_col])
            # Force centroid zone so cross-zone points stay consistent
            e, n, _, _ = _wgs84_to_utm_zone(lon, lat, _zone, _hemi)
            bx, by = cls.geo_to_blender(e, n, origin_e, origin_n, scale_factor)
            well_record: dict = {
                "bx": round(bx, 6),
                "by": round(by, 6),
                "longitude": lon,
                "latitude": lat,
            }
            for col in df.columns:
                if col not in (layer.lon_col, layer.lat_col):
                    val = row[col]
                    well_record[str(col)] = (
                        val if not hasattr(val, "item") else val.item()
                    )
            wells.append(well_record)

        script = textwrap.dedent(f"""\
            # Blender GIS Well Visualisation Script
            # Generated by digitalmodel.gis.integrations.blender_export
            #
            # Scene origin (anchor):
            #   Longitude: {centroid.x}
            #   Latitude:  {centroid.y}
            # Scale factor: {scale_factor} (Blender units per metre)
            #
            # Run this script in Blender's scripting workspace to place
            # well markers in 3D space.

            import bpy
            import math

            # ---------------------------------------------------------------------------
            # Configuration
            # ---------------------------------------------------------------------------

            SCALE_FACTOR = {scale_factor}
            WELL_HEIGHT = {well_height_units}
            WELL_RADIUS = {well_height_units * 0.1}

            # Scene origin anchor coordinates (WGS84)
            ORIGIN_LON = {centroid.x}
            ORIGIN_LAT = {centroid.y}

            # ---------------------------------------------------------------------------
            # Well data (projected coordinates relative to scene origin)
            # ---------------------------------------------------------------------------

            WELLS = {wells!r}

            # ---------------------------------------------------------------------------
            # Helper: clear existing well objects
            # ---------------------------------------------------------------------------

            def clear_existing_wells():
                for obj in list(bpy.data.objects):
                    if obj.get("gis_well_marker"):
                        bpy.data.objects.remove(obj, do_unlink=True)

            # ---------------------------------------------------------------------------
            # Helper: create a single well marker cylinder
            # ---------------------------------------------------------------------------

            def create_well_marker(name, bx, by, custom_properties):
                bpy.ops.mesh.primitive_cylinder_add(
                    radius=WELL_RADIUS,
                    depth=WELL_HEIGHT,
                    location=(bx, by, WELL_HEIGHT / 2.0),
                )
                obj = bpy.context.active_object
                obj.name = name
                obj["gis_well_marker"] = True
                for key, value in custom_properties.items():
                    obj[key] = value
                return obj

            # ---------------------------------------------------------------------------
            # Main: place wells
            # ---------------------------------------------------------------------------

            def main():
                clear_existing_wells()
                for well in WELLS:
                    name = str(well.get("well_name", well.get("name", "well")))
                    bx = well["bx"]
                    by = well["by"]
                    custom_properties = {{
                        k: v for k, v in well.items()
                        if k not in ("bx", "by", "well_name", "name")
                    }}
                    custom_properties["water_depth_m"] = well.get("water_depth_m", 0)
                    create_well_marker(name, bx, by, custom_properties)
                print(f"Placed {{len(WELLS)}} well markers in scene.")

            if __name__ == "__main__":
                main()
        """)
        return script

    @classmethod
    def write_well_script(
        cls,
        layer: FeatureLayer,
        filepath: str | Path,
        scale_factor: float = _DEFAULT_SCALE,
        well_height_units: float = 0.05,
    ) -> Path:
        """Write the Blender well script to a file.

        Parameters
        ----------
        layer:
            Source feature layer.
        filepath:
            Destination ``.py`` file path.
        scale_factor:
            Metres-to-Blender-units multiplier.
        well_height_units:
            Height of each well cylinder in Blender units.

        Returns
        -------
        Path
            Resolved path to the written script.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        script = cls.generate_well_script(
            layer,
            scale_factor=scale_factor,
            well_height_units=well_height_units,
        )
        filepath.write_text(script, encoding="utf-8")
        return filepath.resolve()
