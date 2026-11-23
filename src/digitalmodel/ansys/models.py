# ABOUTME: ANSYS FEA domain dataclasses — materials, sections, design points, journals
# ABOUTME: Pure data containers; no ANSYS software dependency required

"""
ANSYS FEA Domain Models
=======================

Data containers for parsed ANSYS file content:

    APDLMaterial    — material properties from MP commands (.inp)
    APDLSection     — beam/rod section from SECTYPE/SECDATA blocks (.inp)
    DesignPoint     — single parametric design point row (DesignPointLog.csv)
    ParametricStudy — full DesignXplorer parametric study
    WBJNJournal     — IronPython Workbench journal metadata (.wbjn)
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class APDLMaterial:
    """Material properties extracted from APDL MP commands."""
    mat_id: int
    elastic_modulus_mpa: float | None = None
    poissons_ratio: float | None = None
    density_kg_mm3: float | None = None


@dataclass
class APDLSection:
    """Beam/rod section extracted from SECTYPE + SECDATA commands."""
    section_id: int
    section_type: str           # e.g. "BEAM", "ROD"
    section_subtype: str        # e.g. "ASEC"
    area_mm2: float | None = None
    iz_mm4: float | None = None     # 2nd moment about Z
    iy_mm4: float | None = None     # 2nd moment about Y
    j_mm4: float | None = None      # polar moment (torsion)


@dataclass
class DesignPoint:
    """Single parametric design point from DesignXplorer log."""
    name: str               # e.g. "DP 0"
    index: int
    parameters: dict[str, float] = field(default_factory=dict)


@dataclass
class ParametricStudy:
    """Full ANSYS DesignXplorer parametric study from DesignPointLog.csv."""
    project_name: str
    creation_date: str
    parameter_labels: dict[str, str] = field(default_factory=dict)
    design_points: list[DesignPoint] = field(default_factory=list)


@dataclass
class WBJNJournal:
    """Key metadata extracted from an IronPython Workbench journal (.wbjn)."""
    release_version: str
    filepath: str
    design_point_operations: list[str] = field(default_factory=list)
    system_names: list[str] = field(default_factory=list)
    parameter_expressions: list[tuple[str, str]] = field(default_factory=list)
