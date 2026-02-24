#!/usr/bin/env python3
"""
ABOUTME: Data models for OpenFOAM CFD case configuration including case types,
boundary conditions, solver settings, turbulence models, and domain geometry.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional


# ============================================================================
# Enumerations
# ============================================================================


class CaseType(Enum):
    """Marine/offshore CFD analysis case types."""

    WAVE_LOADING = "wave_loading"
    CURRENT_LOADING = "current_loading"
    GREENWATER = "greenwater"
    SLOSHING = "sloshing"
    VIV = "viv"


class BoundaryType(Enum):
    """OpenFOAM boundary condition types."""

    FIXED_VALUE = "fixedValue"
    ZERO_GRADIENT = "zeroGradient"
    NO_SLIP = "noSlip"
    EMPTY = "empty"
    SYMMETRY = "symmetry"
    CYCLIC = "cyclic"
    WAVE_INLET = "waveInlet"
    INLET_OUTLET = "inletOutlet"
    PRESSURE_INLET_OUTLET_VELOCITY = "pressureInletOutletVelocity"
    TOTAL_PRESSURE = "totalPressure"


class TurbulenceType(Enum):
    """Turbulence modelling options."""

    LAMINAR = "laminar"
    K_EPSILON = "kEpsilon"
    K_OMEGA_SST = "kOmegaSST"
    LES_SMAGORINSKY = "lesSmagorinsky"


# ============================================================================
# BoundaryCondition
# ============================================================================


@dataclass
class BoundaryCondition:
    """Boundary condition specification for one patch and one field.

    Attributes:
        patch_name: Name of the boundary patch.
        bc_type: OpenFOAM BC type keyword.
        field: Field name (e.g. 'U', 'p', 'k').
        value: Value string (e.g. 'uniform (1 0 0)'). Optional.
        extra: Additional key-value pairs for the BC dict.
    """

    patch_name: str
    bc_type: BoundaryType
    field: str
    value: Optional[str] = None
    extra: Dict[str, Any] = field(default_factory=dict)

    def to_foam_dict(self) -> Dict[str, Any]:
        """Serialise to an OpenFOAM dict block keyed by patch name."""
        bc_dict: Dict[str, Any] = {"type": self.bc_type.value}
        if self.value is not None:
            bc_dict["value"] = self.value
        bc_dict.update(self.extra)
        return {self.patch_name: bc_dict}


# ============================================================================
# TurbulenceModel
# ============================================================================


@dataclass
class TurbulenceModel:
    """Turbulence model configuration.

    Attributes:
        turbulence_type: Model selection.
        intensity: Turbulent intensity (fraction), default 0.05.
        length_scale: Turbulent length scale (m).
    """

    turbulence_type: TurbulenceType = TurbulenceType.K_OMEGA_SST
    intensity: float = 0.05
    length_scale: Optional[float] = None

    def to_dict(self) -> Dict[str, Any]:
        """Return turbulenceProperties dict entries."""
        if self.turbulence_type == TurbulenceType.LAMINAR:
            return {
                "simulationType": "laminar",
            }
        sim_type = (
            "LES"
            if self.turbulence_type == TurbulenceType.LES_SMAGORINSKY
            else "RAS"
        )
        return {
            "simulationType": sim_type,
            sim_type: {
                "model": self.turbulence_type.value,
                "turbulence": "on",
                "printCoeffs": "on",
            },
        }


# ============================================================================
# SolverConfig
# ============================================================================


@dataclass
class SolverConfig:
    """Solver selection and time-stepping configuration.

    Attributes:
        solver_name: OpenFOAM application name.
        start_time: Simulation start time (s).
        end_time: Simulation end time (s).
        delta_t: Time step (s).
        write_interval: Write frequency (steps or time units).
        adjustable_time_step: Use adjustable Courant-based delta_t.
        max_co: Maximum Courant number (for adjustable stepping).
        is_multiphase: Whether this is a VOF (two-phase) simulation.
    """

    solver_name: str = "simpleFoam"
    start_time: float = 0.0
    end_time: float = 1000.0
    delta_t: float = 1.0
    write_interval: int = 100
    adjustable_time_step: bool = False
    max_co: float = 0.9
    is_multiphase: bool = False
    purge_write: int = 0

    def to_control_dict(self) -> Dict[str, Any]:
        """Return controlDict field values."""
        return {
            "application": self.solver_name,
            "startFrom": "startTime",
            "startTime": self.start_time,
            "stopAt": "endTime",
            "endTime": self.end_time,
            "deltaT": self.delta_t,
            "writeControl": "timeStep",
            "writeInterval": self.write_interval,
            "purgeWrite": self.purge_write,
            "writeFormat": "ascii",
            "writePrecision": 6,
            "writeCompression": "off",
            "timeFormat": "general",
            "timePrecision": 6,
            "runTimeModifiable": "true",
            "adjustTimeStep": "yes" if self.adjustable_time_step else "no",
            "maxCo": self.max_co,
        }


# ============================================================================
# DomainConfig
# ============================================================================


@dataclass
class DomainConfig:
    """Computational domain bounding box and cell sizing.

    Attributes:
        min_coords: [xmin, ymin, zmin] of the domain (m).
        max_coords: [xmax, ymax, zmax] of the domain (m).
        base_cell_size: Background mesh cell size (m).
        n_cells: Override for [nx, ny, nz] cell counts.
    """

    min_coords: List[float] = field(default_factory=lambda: [-100.0, -50.0, -30.0])
    max_coords: List[float] = field(default_factory=lambda: [200.0, 50.0, 10.0])
    base_cell_size: float = 5.0
    n_cells: Optional[List[int]] = None

    @classmethod
    def from_hull_geometry(
        cls,
        hull_length: float,
        hull_beam: float,
        hull_draft: float,
        upstream_factor: float = 2.5,
        downstream_factor: float = 6.0,
        lateral_factor: float = 3.5,
        depth_factor: float = 2.5,
        freeboard_factor: float = 0.5,
        base_cell_size: float = 5.0,
    ) -> "DomainConfig":
        """Auto-size domain from hull principal dimensions.

        The domain is centred on the hull midship with:
        - upstream:   upstream_factor * L (negative x)
        - downstream: downstream_factor * L (positive x)
        - lateral:    lateral_factor * B on each side
        - depth:      depth_factor * T below keel
        - height:     freeboard_factor * L above waterplane
        """
        x_min = -upstream_factor * hull_length
        x_max = downstream_factor * hull_length
        y_min = -lateral_factor * hull_beam
        y_max = lateral_factor * hull_beam
        z_min = -depth_factor * hull_draft
        z_max = freeboard_factor * hull_length

        return cls(
            min_coords=[x_min, y_min, z_min],
            max_coords=[x_max, y_max, z_max],
            base_cell_size=base_cell_size,
        )

    def cell_counts(self) -> tuple[int, int, int]:
        """Compute cell counts from domain extent and base cell size."""
        if self.n_cells is not None:
            return tuple(self.n_cells)  # type: ignore[return-value]
        lengths = [
            self.max_coords[i] - self.min_coords[i] for i in range(3)
        ]
        nx = max(1, round(lengths[0] / self.base_cell_size))
        ny = max(1, round(lengths[1] / self.base_cell_size))
        nz = max(1, round(lengths[2] / self.base_cell_size))
        return nx, ny, nz

    def estimate_cell_count(self) -> int:
        """Estimate total cell count for the background mesh."""
        nx, ny, nz = self.cell_counts()
        return nx * ny * nz

    def block_mesh_vertices(self) -> List[List[float]]:
        """Return the 8 corner vertices in OpenFOAM hex ordering.

        OpenFOAM hex block vertex order (right-hand rule, z-up):
        0: (xmin, ymin, zmin)  4: (xmin, ymin, zmax)
        1: (xmax, ymin, zmin)  5: (xmax, ymin, zmax)
        2: (xmax, ymax, zmin)  6: (xmax, ymax, zmax)
        3: (xmin, ymax, zmin)  7: (xmin, ymax, zmax)
        """
        x0, y0, z0 = self.min_coords
        x1, y1, z1 = self.max_coords
        return [
            [x0, y0, z0],
            [x1, y0, z0],
            [x1, y1, z0],
            [x0, y1, z0],
            [x0, y0, z1],
            [x1, y0, z1],
            [x1, y1, z1],
            [x0, y1, z1],
        ]


# ============================================================================
# OpenFOAMCase
# ============================================================================

# Solver selection by case type
_CASE_SOLVER_MAP: Dict[CaseType, str] = {
    CaseType.WAVE_LOADING: "interFoam",
    CaseType.CURRENT_LOADING: "simpleFoam",
    CaseType.GREENWATER: "interFoam",
    CaseType.SLOSHING: "interFoam",
    CaseType.VIV: "pimpleFoam",
}

_MULTIPHASE_CASES = {
    CaseType.WAVE_LOADING,
    CaseType.GREENWATER,
    CaseType.SLOSHING,
}


@dataclass
class OpenFOAMCase:
    """Top-level OpenFOAM case configuration.

    Attributes:
        name: Case identifier (used as directory name).
        case_type: Marine application type.
        solver_config: Solver selection and time-stepping settings.
        turbulence_model: Turbulence closure model.
        domain: Computational domain definition.
        boundary_conditions: List of boundary conditions.
        metadata: Free-form metadata dict.
    """

    name: str
    case_type: CaseType
    solver_config: SolverConfig = field(default_factory=SolverConfig)
    turbulence_model: TurbulenceModel = field(
        default_factory=lambda: TurbulenceModel(TurbulenceType.K_OMEGA_SST)
    )
    domain: DomainConfig = field(default_factory=DomainConfig)
    boundary_conditions: List[BoundaryCondition] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

    @classmethod
    def for_case_type(cls, case_type: CaseType, name: str) -> "OpenFOAMCase":
        """Create a pre-configured case for the given marine application.

        Selects the appropriate solver, multiphase flag, and default
        time-stepping parameters for each case type.

        Args:
            case_type: Marine CFD application type.
            name: Case directory name.

        Returns:
            OpenFOAMCase with sensible defaults for the application.
        """
        solver_name = _CASE_SOLVER_MAP[case_type]
        is_multiphase = case_type in _MULTIPHASE_CASES

        # Transient default delta_t for time-domain solvers
        if solver_name in ("interFoam", "pimpleFoam"):
            delta_t = 0.01
            end_time = 50.0
            write_interval = 50
        else:
            delta_t = 1.0
            end_time = 1000.0
            write_interval = 100

        solver_cfg = SolverConfig(
            solver_name=solver_name,
            is_multiphase=is_multiphase,
            delta_t=delta_t,
            end_time=end_time,
            write_interval=write_interval,
        )

        return cls(
            name=name,
            case_type=case_type,
            solver_config=solver_cfg,
        )

    def add_boundary_condition(self, bc: BoundaryCondition) -> None:
        """Append a boundary condition to this case."""
        self.boundary_conditions.append(bc)
