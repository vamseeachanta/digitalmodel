"""Router for mooring system specifications.

Converts MooringSystem domain objects into GenericModel-compatible dicts
for processing by GenericModelBuilder.
"""

from __future__ import annotations

import math
from typing import Any

from ..schema.mooring import (
    ChainGrade,
    MooringLine,
    MooringSegment,
    MooringSegmentType,
    MooringSystem,
)
from .base_router import BaseRouter

# Studless chain properties per DNV-OS-E302 Table 2-2 (simplified).
# Key: ChainGrade, Value: dict with MBL coefficient and weight coefficient.
# MBL (kN) = mbl_coeff * d^2   where d = diameter in mm
# Mass (te/m) = mass_coeff * d^2   where d = diameter in mm
# EA (kN) = ea_coeff * d^2   where d = diameter in mm
CHAIN_DATABASE: dict[str, dict[str, float]] = {
    "R3": {
        "mbl_coeff": 0.0249,  # MBL = 0.0249 * d^2 (kN, d in mm)
        "mass_coeff": 0.0219e-3,  # mass/m = 0.0219 * d^2 (kg/m, d in mm) -> te/m
        "ea_coeff": 0.854,  # EA = 0.854 * d^2 * 1000 (kN, d in mm)
        "cd": 2.4,  # Normal drag coefficient for chain
        "ca": 1.0,  # Added mass coefficient
    },
    "R3S": {
        "mbl_coeff": 0.0274,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R4": {
        "mbl_coeff": 0.0304,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R4S": {
        "mbl_coeff": 0.0330,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R5": {
        "mbl_coeff": 0.0358,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
}

# Default wire rope properties (6-strand, fiber core)
WIRE_ROPE_DEFAULTS: dict[str, float] = {
    "mass_coeff": 0.0340e-3,  # mass/m = 0.034 * d^2 (kg/m, d in mm) -> te/m
    "ea_ratio": 0.45,  # EA/MBL ratio (typical for spiral strand)
    "cd": 1.2,
    "ca": 1.0,
}

# Default polyester rope properties
POLYESTER_DEFAULTS: dict[str, float] = {
    "mass_coeff": 0.0080e-3,  # mass/m approx, d in mm -> te/m
    "ea_ratio": 0.15,  # EA/MBL ratio (typical for polyester)
    "cd": 1.2,
    "ca": 1.0,
}


class MooringRouter(BaseRouter):
    """Route mooring system specs to GenericModel-compatible dicts.

    Converts MooringSystem objects into dicts with:
    - line_types: OrcaFlex LineType dicts (General category)
    - lines: OrcaFlex Line dicts with multi-section definitions
    - winches: OrcaFlex Winch dicts (when pretension is specified)

    Line types are deduplicated by (segment_type, diameter, grade) tuple.
    """

    def route(self, spec: MooringSystem) -> dict[str, Any]:
        """Convert MooringSystem to GenericModel-compatible dict.

        Args:
            spec: MooringSystem specification.

        Returns:
            Dict with line_types, lines, and optionally winches keys.
        """
        line_types: list[dict[str, Any]] = []
        lines: list[dict[str, Any]] = []
        winches: list[dict[str, Any]] = []
        seen_lt_names: set[str] = set()

        for ml in spec.lines:
            # Build and deduplicate line types
            for seg in ml.segments:
                lt = self._segment_to_linetype(seg)
                if lt["name"] not in seen_lt_names:
                    line_types.append(lt)
                    seen_lt_names.add(lt["name"])

            # Build multi-section line
            line = self._build_line(ml)
            lines.append(line)

            # Pretension winch if specified
            if ml.pretension is not None:
                winch = self._build_winch(ml)
                winches.append(winch)

        result: dict[str, Any] = {
            "line_types": line_types,
            "lines": lines,
        }
        if winches:
            result["winches"] = winches

        return result

    def _segment_to_linetype(self, seg: MooringSegment) -> dict[str, Any]:
        """Convert a mooring segment to an OrcaFlex LineType dict.

        Args:
            seg: MooringSegment specification.

        Returns:
            Dict with 'name' and 'properties' for GenericLineType.
        """
        if seg.type == MooringSegmentType.CHAIN:
            return self._chain_linetype(seg)
        elif seg.type == MooringSegmentType.WIRE_ROPE:
            return self._wire_linetype(seg)
        else:
            return self._polyester_linetype(seg)

    def _chain_linetype(self, seg: MooringSegment) -> dict[str, Any]:
        """Build LineType for chain segment from grade database."""
        grade_key = seg.grade.value if seg.grade else "R3"
        db = CHAIN_DATABASE[grade_key]
        d_mm = seg.diameter * 1000  # Convert m to mm

        mbl = db["mbl_coeff"] * d_mm ** 2
        mass = db["mass_coeff"] * d_mm ** 2
        ea = seg.axial_stiffness if seg.axial_stiffness else db["ea_coeff"] * d_mm ** 2 * 1000

        # Chain OD for hydrodynamics = 2 * bar diameter (studless convention)
        hydro_od = seg.diameter * 2

        name = self._linetype_name(seg)

        return {
            "name": name,
            "properties": {
                "Category": "General",
                "OD": hydro_od,
                "ID": 0,
                "MassPerUnitLength": mass,
                "EA": ea,
                "EI": [0, None],
                "GJ": 0,
                "PoissonRatio": 0.5,
                "Cd": [db["cd"], None, 0.008],
                "Ca": [db["ca"], None, 0],
                "Cm": [None, None, None],
                "CompressionIsLimited": "No",
                "AllowableTension": mbl,
                "SeabedLateralFrictionCoefficient": 1.0,
                "SeabedAxialFrictionCoefficient": None,
                "RayleighDampingCoefficients": "(no damping)",
            },
        }

    def _wire_linetype(self, seg: MooringSegment) -> dict[str, Any]:
        """Build LineType for wire rope segment."""
        d_mm = seg.diameter * 1000
        mbl = seg.breaking_load if seg.breaking_load else 0
        mass = WIRE_ROPE_DEFAULTS["mass_coeff"] * d_mm ** 2
        ea = seg.axial_stiffness if seg.axial_stiffness else (
            WIRE_ROPE_DEFAULTS["ea_ratio"] * mbl if mbl else 0
        )

        name = self._linetype_name(seg)

        return {
            "name": name,
            "properties": {
                "Category": "General",
                "OD": seg.diameter,
                "ID": 0,
                "MassPerUnitLength": mass,
                "EA": ea,
                "EI": [0, None],
                "GJ": 0,
                "PoissonRatio": 0.5,
                "Cd": [WIRE_ROPE_DEFAULTS["cd"], None, 0.008],
                "Ca": [WIRE_ROPE_DEFAULTS["ca"], None, 0],
                "Cm": [None, None, None],
                "CompressionIsLimited": "No",
                "AllowableTension": mbl if mbl else None,
                "SeabedLateralFrictionCoefficient": 0.5,
                "SeabedAxialFrictionCoefficient": None,
                "RayleighDampingCoefficients": "(no damping)",
            },
        }

    def _polyester_linetype(self, seg: MooringSegment) -> dict[str, Any]:
        """Build LineType for polyester rope segment."""
        d_mm = seg.diameter * 1000
        mbl = seg.breaking_load if seg.breaking_load else 0
        mass = POLYESTER_DEFAULTS["mass_coeff"] * d_mm ** 2
        ea = seg.axial_stiffness if seg.axial_stiffness else (
            POLYESTER_DEFAULTS["ea_ratio"] * mbl if mbl else 0
        )

        name = self._linetype_name(seg)

        return {
            "name": name,
            "properties": {
                "Category": "General",
                "OD": seg.diameter,
                "ID": 0,
                "MassPerUnitLength": mass,
                "EA": ea,
                "EI": [0, None],
                "GJ": 0,
                "PoissonRatio": 0.5,
                "Cd": [POLYESTER_DEFAULTS["cd"], None, 0.008],
                "Ca": [POLYESTER_DEFAULTS["ca"], None, 0],
                "Cm": [None, None, None],
                "CompressionIsLimited": "No",
                "AllowableTension": mbl if mbl else None,
                "SeabedLateralFrictionCoefficient": 0.3,
                "SeabedAxialFrictionCoefficient": None,
                "RayleighDampingCoefficients": "(no damping)",
            },
        }

    def _build_line(self, ml: MooringLine) -> dict[str, Any]:
        """Build an OrcaFlex Line dict from a MooringLine.

        Creates a multi-section line with End A at anchor and End B at fairlead.

        Args:
            ml: MooringLine specification.

        Returns:
            Dict with 'name' and 'properties' for GenericLine.
        """
        # Build section data: [LineType, Length, SegmentCount] per segment
        section_data = []
        for seg in ml.segments:
            lt_name = self._linetype_name(seg)
            num_segments = max(1, round(seg.length / seg.segment_length))
            section_data.append([lt_name, seg.length, num_segments])

        # End A = anchor, End B = fairlead
        end_a_conn = self._endpoint_connection(ml.anchor)
        end_b_conn = self._endpoint_connection(ml.fairlead)

        properties: dict[str, Any] = {
            "EndAConnection": end_a_conn,
            "EndBConnection": end_b_conn,
            "EndAX": ml.anchor.position[0],
            "EndAY": ml.anchor.position[1],
            "EndAZ": ml.anchor.position[2],
            "EndBX": ml.fairlead.position[0],
            "EndBY": ml.fairlead.position[1],
            "EndBZ": ml.fairlead.position[2],
            "NumberOfSections": len(section_data),
            "LineType, Length, NumberOfSegments": section_data,
            "StaticsStep1": "Catenary",
        }

        if ml.pretension is not None:
            # When pretensioned, Line End B connects to the winch
            # (winch sits on the vessel, line connects to winch)
            winch_name = f"Winch_{ml.name}"
            properties["EndBConnection"] = winch_name
            properties["EndBX"] = 0
            properties["EndBY"] = 0
            properties["EndBZ"] = 0
        elif ml.fairlead.vessel:
            properties["EndBConnection"] = ml.fairlead.vessel

        if ml.lay_azimuth is not None:
            properties["LayAzimuth"] = ml.lay_azimuth

        return {
            "name": ml.name,
            "properties": properties,
        }

    def _build_winch(self, ml: MooringLine) -> dict[str, Any]:
        """Build a pretension winch for a mooring line.

        OrcaFlex winch topology for mooring pretension:
        - Winch sits on the vessel (Connection = vessel name)
        - Line End B connects to the winch (set in _build_line)
        - Winch controls tension via "Specified tension" mode

        Args:
            ml: MooringLine with pretension specified.

        Returns:
            Dict with 'name' and 'properties' for GenericWinch.
        """
        vessel = ml.fairlead.vessel or "Fixed"
        return {
            "name": f"Winch_{ml.name}",
            "properties": {
                "Connection": vessel,
                "ConnectionX": ml.fairlead.position[0],
                "ConnectionY": ml.fairlead.position[1],
                "ConnectionZ": ml.fairlead.position[2],
                "WinchControl": "Specified tension",
                "Tension": ml.pretension,
            },
        }

    @staticmethod
    def _endpoint_connection(endpoint) -> str:
        """Map endpoint type to OrcaFlex connection string."""
        if endpoint.type == "anchor" or endpoint.type == "fixed":
            return "Fixed"
        elif endpoint.type == "fairlead" and endpoint.vessel:
            return endpoint.vessel
        return "Free"

    @staticmethod
    def _linetype_name(seg: MooringSegment) -> str:
        """Generate a consistent line type name for deduplication.

        Format: {type}_{diameter_mm}mm[_{grade}]
        """
        d_mm = round(seg.diameter * 1000)
        if seg.type == MooringSegmentType.CHAIN:
            grade = seg.grade.value if seg.grade else "R3"
            return f"chain_{d_mm}mm_{grade}"
        elif seg.type == MooringSegmentType.WIRE_ROPE:
            return f"wire_{d_mm}mm"
        else:
            return f"polyester_{d_mm}mm"
