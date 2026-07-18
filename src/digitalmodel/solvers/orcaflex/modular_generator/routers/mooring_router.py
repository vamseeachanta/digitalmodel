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

# Studless chain properties per DNV-OS-E302 (breaking load formula).
# Key: ChainGrade, Value: dict with MBL coefficient and weight coefficient.
# MBL (kN) = mbl_coeff * d^2 * (44 - 0.08*d)   where d = diameter in mm
#   (DNV-OS-E302 grade coefficients: R3=0.0223, R3S=0.0249, R4=0.0274,
#    R4S=0.0304, R5=0.0320; e.g. 76mm R3 -> 4884 kN)
# Mass (te/m) = mass_coeff * d^2   where d = diameter in mm
# EA (kN) = ea_coeff * d^2 * 100   where d = diameter in mm
#   (equivalent to 0.854e8 * d^2 kN with d in m; matches curated library
#    component docs/domains/orcaflex/library/line_types/chain_76mm_r4.yml
#    EA = 490000 kN for 76mm)
CHAIN_DATABASE: dict[str, dict[str, float]] = {
    "R3": {
        "mbl_coeff": 0.0223,  # MBL = 0.0223 * d^2 * (44 - 0.08d) (kN, d in mm)
        "mass_coeff": 0.0219e-3,  # mass/m = 0.0219 * d^2 (kg/m, d in mm) -> te/m
        "ea_coeff": 0.854,  # EA = 0.854 * d^2 * 100 (kN, d in mm)
        "cd": 2.4,  # Normal drag coefficient for chain
        "ca": 1.0,  # Added mass coefficient
    },
    "R3S": {
        "mbl_coeff": 0.0249,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R4": {
        "mbl_coeff": 0.0274,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R4S": {
        "mbl_coeff": 0.0304,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
    "R5": {
        "mbl_coeff": 0.0320,
        "mass_coeff": 0.0219e-3,
        "ea_coeff": 0.854,
        "cd": 2.4,
        "ca": 1.0,
    },
}

# Default wire rope properties (6x36 IWRC, as in the curated library
# components docs/domains/orcaflex/library/line_types/wire_rope_*.yml).
# EA (kN) = ea_coeff * d^2 * 100   where d = diameter in mm
#   (equivalent to 0.78e8 * d^2 kN with d in m; matches wire_rope_64mm
#    EA=320000, wire_rope_76mm EA=450000, wire_rope_84mm EA=550000)
WIRE_ROPE_DEFAULTS: dict[str, float] = {
    "mass_coeff": 0.0340e-3,  # mass/m = 0.034 * d^2 (kg/m, d in mm) -> te/m
    "ea_coeff": 0.78,  # EA = 0.78 * d^2 * 100 (kN, d in mm)
    "cd": 1.2,
    "ca": 1.0,
}

# Default polyester rope properties (as in the curated library components
# docs/domains/orcaflex/library/line_types/polyester_rope_*.yml).
# EA (kN) = ea_coeff * d^2 * 100   where d = diameter in mm
#   (equivalent to 0.07e8 * d^2 kN with d in m; matches polyester_rope_120mm
#    EA=100000, polyester_rope_140mm EA=140000, polyester_rope_160mm EA=180000)
POLYESTER_DEFAULTS: dict[str, float] = {
    "mass_coeff": 0.0080e-3,  # mass/m approx, d in mm -> te/m
    "ea_coeff": 0.07,  # EA = 0.07 * d^2 * 100 (kN, d in mm)
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

        # DNV-OS-E302 breaking load: MBL = c * d^2 * (44 - 0.08d) (kN, d in mm)
        mbl = seg.breaking_load if seg.breaking_load else (
            db["mbl_coeff"] * d_mm ** 2 * (44 - 0.08 * d_mm)
        )
        mass = db["mass_coeff"] * d_mm ** 2
        ea = seg.axial_stiffness if seg.axial_stiffness else db["ea_coeff"] * d_mm ** 2 * 100

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
        # EA from diameter (0.78e8 * d^2 kN, d in m), not from MBL: an
        # EA/MBL ratio of 0.45 would imply >200% elongation at break.
        ea = seg.axial_stiffness if seg.axial_stiffness else (
            WIRE_ROPE_DEFAULTS["ea_coeff"] * d_mm ** 2 * 100
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
        # EA from diameter (0.07e8 * d^2 kN, d in m), not from MBL.
        ea = seg.axial_stiffness if seg.axial_stiffness else (
            POLYESTER_DEFAULTS["ea_coeff"] * d_mm ** 2 * 100
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

        # Lines cannot connect to winch objects in OrcaFlex; the fairlead
        # connection stays on the vessel and the pretension winch (if any)
        # spans vessel -> line End B (see _build_winch).
        if ml.fairlead.vessel:
            properties["EndBConnection"] = ml.fairlead.vessel

        if ml.lay_azimuth is not None:
            properties["LayAzimuth"] = ml.lay_azimuth

        return {
            "name": ml.name,
            "properties": properties,
        }

    def _build_winch(self, ml: MooringLine) -> dict[str, Any]:
        """Build a pretension winch for a mooring line.

        OrcaFlex winch topology for mooring pretension (same list-form
        connection as the proven S-lay tensioner in
        ``builders/winch_builder.py``):
        - Winch spans two connection points: the vessel (at the fairlead
          position) and the mooring line at End B
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
                "Connection": [vessel, ml.name],
                "ConnectionPoint": [list(ml.fairlead.position), [0, 0, 0]],
                "ConnectionzRelativeTo": [None, "End B"],
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
