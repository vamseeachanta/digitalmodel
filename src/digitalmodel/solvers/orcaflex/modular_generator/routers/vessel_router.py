"""Router for vessel specifications.

Converts vessel engineering specs into GenericModel-compatible dicts
for processing by GenericModelBuilder.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from .base_router import BaseRouter


class VesselRouter(BaseRouter):
    """Route vessel specs to GenericModel-compatible dicts.

    Converts vessel principal dimensions, mass properties, and RAO data
    into VesselType and Vessel dicts compatible with GenericModelBuilder.

    Supports two input modes:
    1. Equipment.Vessel spec (from ProjectInputSpec)
    2. Hull catalog entry + RAO file path

    Output keys: vessel_types, vessels
    """

    def route(self, spec: Any) -> dict[str, Any]:
        """Convert vessel specification to GenericModel-compatible dict.

        Args:
            spec: Vessel specification object with attributes:
                - name: str
                - loa: float (m)
                - beam: float (m)
                - depth: float (m)
                - draft: float (m)
                - mass: float (te)
                - gmt: float (m) — transverse metacentric height
                - cog: list[float] — center of gravity [x, y, z]
                - gyration_radii: object with roll, pitch, yaw (m)
                - position: list[float] — initial position [x, y, z]
                - raos: object | None — RAO data source

        Returns:
            Dict with vessel_types and vessels keys.
        """
        vessel_type = self._build_vessel_type(spec)
        vessel = self._build_vessel(spec)

        return {
            "vessel_types": [vessel_type],
            "vessels": [vessel],
        }

    @classmethod
    def from_hull_catalog(
        cls,
        catalog_entry: dict[str, Any],
        rao_file: Path | None = None,
        position: list[float] | None = None,
        heading: float = 0.0,
    ) -> dict[str, Any]:
        """Create vessel dicts from hull catalog entry.

        Args:
            catalog_entry: Dict from hull_panel_catalog.yaml with keys:
                hull_id, hull_type, name, length_m, beam_m, draft_m, etc.
            rao_file: Path to RAO data file (OrcaWave .owr or AQWA .LIS).
            position: Initial vessel position [x, y, z] in meters.
            heading: Initial heading in degrees.

        Returns:
            Dict with vessel_types and vessels keys.
        """
        name = catalog_entry.get("name", catalog_entry["hull_id"])
        type_name = f"{name}_type"

        vessel_type: dict[str, Any] = {
            "name": type_name,
            "properties": {
                "Length": catalog_entry.get("length_m", 0),
                "Draught": catalog_entry.get("draft_m", 0),
            },
        }

        # Add RAO reference if available
        if rao_file is not None:
            rao_path = Path(rao_file)
            if rao_path.suffix.lower() == ".owr":
                vessel_type["properties"]["WavesReferredToBy"] = "frequency (rad/s)"
                vessel_type["properties"]["RAOOrigin"] = [0, 0, 0]
            vessel_type["properties"]["DisplacementRAOCalculationFile"] = str(rao_path)

        vessel: dict[str, Any] = {
            "name": name,
            "properties": {
                "VesselType": type_name,
                "InitialX": position[0] if position else 0,
                "InitialY": position[1] if position else 0,
                "InitialZ": position[2] if position else 0,
                "InitialHeading": heading,
                "IncludedInStatics": "None",
                "PrimaryMotion": "None",
                "SuperimposedMotion": "RAOs + harmonics",
            },
        }

        return {
            "vessel_types": [vessel_type],
            "vessels": [vessel],
        }

    def _build_vessel_type(self, spec: Any) -> dict[str, Any]:
        """Build VesselType dict from vessel spec.

        Maps engineering properties to OrcaFlex VesselType fields:
        - LOA → Length
        - Draft → Draught
        - Mass → Mass
        - COG → CentreOfMass
        - GMT → GML (approximate)
        - Gyration radii → RadiiOfGyration[Rx, Ry, Rz]
        """
        type_name = getattr(spec, "type_name", f"{spec.name}_type")

        properties: dict[str, Any] = {
            "Length": spec.loa,
            "Draught": spec.draft,
        }

        # Mass properties
        if hasattr(spec, "mass") and spec.mass:
            properties["Mass"] = spec.mass

        # Centre of mass
        if hasattr(spec, "cog") and spec.cog:
            properties["CentreOfMassX"] = spec.cog[0]
            properties["CentreOfMassY"] = spec.cog[1]
            properties["CentreOfMassZ"] = spec.cog[2]

        # Gyration radii
        if hasattr(spec, "gyration_radii") and spec.gyration_radii:
            gr = spec.gyration_radii
            properties["RadiiOfGyrationRx"] = gr.roll
            properties["RadiiOfGyrationRy"] = gr.pitch
            properties["RadiiOfGyrationRz"] = gr.yaw

        # Metacentric height
        if hasattr(spec, "gmt") and spec.gmt:
            properties["TransverseMetacentricHeight"] = spec.gmt

        # RAO data source
        if hasattr(spec, "raos") and spec.raos is not None:
            rao_props = self._build_rao_properties(spec.raos)
            properties.update(rao_props)

        return {
            "name": type_name,
            "properties": properties,
        }

    def _build_vessel(self, spec: Any) -> dict[str, Any]:
        """Build Vessel instance dict from vessel spec.

        Maps vessel position and motion settings to OrcaFlex Vessel fields.
        """
        type_name = getattr(spec, "type_name", f"{spec.name}_type")
        position = getattr(spec, "position", [0, 0, 0])
        orientation = getattr(spec, "orientation", [0, 0, 0])

        properties: dict[str, Any] = {
            "VesselType": type_name,
            "InitialX": position[0],
            "InitialY": position[1],
            "InitialZ": position[2],
            "InitialHeading": orientation[0],
            "InitialTrim": orientation[1] if len(orientation) > 1 else 0,
            "InitialHeel": orientation[2] if len(orientation) > 2 else 0,
        }

        # Motion settings
        primary_motion = getattr(spec, "primary_motion", "None")
        superimposed = getattr(spec, "superimposed_motion", "RAOs + harmonics")
        included_statics = getattr(spec, "included_in_statics", "None")

        properties["PrimaryMotion"] = primary_motion
        properties["SuperimposedMotion"] = superimposed
        properties["IncludedInStatics"] = included_statics

        return {
            "name": spec.name,
            "properties": properties,
        }

    @staticmethod
    def _build_rao_properties(raos: Any) -> dict[str, Any]:
        """Build RAO-related properties for VesselType.

        Handles RAO data from various sources:
        - Direct frequency/heading/amplitude data
        - File reference (OrcaWave .owr, AQWA export)
        """
        properties: dict[str, Any] = {}

        # If raos has a file reference
        if hasattr(raos, "file") and raos.file:
            properties["DisplacementRAOCalculationFile"] = str(raos.file)
            return properties

        # If raos has direct data
        if hasattr(raos, "reference_heading"):
            properties["RAOHeading"] = raos.reference_heading

        if hasattr(raos, "frequencies") and raos.frequencies:
            properties["WavesReferredToBy"] = "frequency (rad/s)"
            properties["RAOResponseUnits"] = "m/m"
            properties["RAOWaveUnit"] = "m"

        return properties
