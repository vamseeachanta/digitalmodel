"""
ABOUTME: Data models for Capytaine BEM hydrodynamic analysis module.
ABOUTME: Defines input configuration and output result structures.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple, Union

import numpy as np


class MeshFormat(str, Enum):
    """Supported mesh file formats for Capytaine import."""
    STL = "stl"
    GDF = "gdf"
    MSH = "msh"
    MAR = "mar"
    MESHIO = "meshio"
    AUTO = "auto"


class DOF(str, Enum):
    """Rigid body degrees of freedom."""
    SURGE = "Surge"
    SWAY = "Sway"
    HEAVE = "Heave"
    ROLL = "Roll"
    PITCH = "Pitch"
    YAW = "Yaw"

    @classmethod
    def all_dofs(cls) -> List[DOF]:
        return list(cls)

    @classmethod
    def translational(cls) -> List[DOF]:
        return [cls.SURGE, cls.SWAY, cls.HEAVE]

    @classmethod
    def rotational(cls) -> List[DOF]:
        return [cls.ROLL, cls.PITCH, cls.YAW]


@dataclass
class MeshConfig:
    """Mesh source configuration."""
    path: Optional[Path] = None
    format: MeshFormat = MeshFormat.AUTO
    name: str = "body"
    # For predefined shapes (testing)
    predefined_shape: Optional[str] = None  # "sphere", "cylinder", "box"
    shape_params: Dict = field(default_factory=dict)
    # Symmetry
    use_symmetry: bool = False
    symmetry_plane: Optional[str] = None  # "xOz" or "yOz"

    def __post_init__(self):
        if self.path is not None:
            self.path = Path(self.path)


@dataclass
class BodyConfig:
    """Floating body configuration for BEM analysis."""
    mesh: MeshConfig
    dofs: List[DOF] = field(default_factory=DOF.all_dofs)
    center_of_mass: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    rotation_center: Optional[Tuple[float, float, float]] = None
    # Physical properties for RAO (optional, needed for RAO computation)
    mass: Optional[float] = None  # kg
    inertia_matrix: Optional[np.ndarray] = None  # 6x6
    hydrostatic_stiffness: Optional[np.ndarray] = None  # 6x6
    # Lid mesh for irregular frequency removal
    use_lid: bool = False
    lid_z: Optional[float] = None

    @property
    def rotation_center_or_com(self) -> Tuple[float, float, float]:
        return self.rotation_center if self.rotation_center is not None else self.center_of_mass


@dataclass
class WaveConditions:
    """Wave environment specification (DNV-RP-C205 §3.3)."""
    periods: Optional[np.ndarray] = None  # s
    omegas: Optional[np.ndarray] = None  # rad/s
    headings: np.ndarray = field(default_factory=lambda: np.array([0.0]))  # rad
    water_depth: float = np.inf  # m, inf = deep water
    rho: float = 1025.0  # kg/m^3 seawater density (DNV-RP-C205 §2.3.1)
    g: float = 9.81  # m/s^2

    def __post_init__(self):
        if self.periods is not None and self.omegas is not None:
            raise ValueError("Specify either periods or omegas, not both")
        if self.periods is None and self.omegas is None:
            raise ValueError("Must specify either periods or omegas")
        self.headings = np.atleast_1d(np.asarray(self.headings, dtype=float))

    @property
    def omega_array(self) -> np.ndarray:
        if self.omegas is not None:
            return np.asarray(self.omegas, dtype=float)
        return 2.0 * np.pi / np.asarray(self.periods, dtype=float)

    @property
    def period_array(self) -> np.ndarray:
        if self.periods is not None:
            return np.asarray(self.periods, dtype=float)
        return 2.0 * np.pi / np.asarray(self.omegas, dtype=float)

    @classmethod
    def from_period_range(
        cls,
        t_min: float,
        t_max: float,
        n_periods: int = 30,
        headings_deg: Sequence[float] = (0.0,),
        water_depth: float = np.inf,
        rho: float = 1025.0,
    ) -> WaveConditions:
        return cls(
            periods=np.linspace(t_min, t_max, n_periods),
            headings=np.deg2rad(headings_deg),
            water_depth=water_depth,
            rho=rho,
        )

    @classmethod
    def from_omega_range(
        cls,
        omega_min: float,
        omega_max: float,
        n_omega: int = 30,
        headings_deg: Sequence[float] = (0.0,),
        water_depth: float = np.inf,
        rho: float = 1025.0,
    ) -> WaveConditions:
        return cls(
            omegas=np.linspace(omega_min, omega_max, n_omega),
            headings=np.deg2rad(headings_deg),
            water_depth=water_depth,
            rho=rho,
        )


@dataclass
class SolverConfig:
    """BEM solver configuration."""
    # Green function
    green_function: str = "Delhommeau"  # "Delhommeau", "XieStokesChu"
    # Engine
    engine: str = "BasicMatrixEngine"
    linear_solver: str = "lu_decomposition"
    matrix_cache_size: int = 1
    # Parallelization
    n_jobs: int = 1  # joblib parallelism for fill_dataset
    # Method
    method: str = "indirect"  # "indirect" or "direct"


@dataclass
class BEMResult:
    """Results from a BEM hydrodynamic analysis."""
    # Raw xarray dataset from Capytaine
    dataset: object = None  # xarray.Dataset
    # Extracted numpy arrays for direct access
    added_mass: Optional[np.ndarray] = None  # (n_omega, n_dof, n_dof)
    radiation_damping: Optional[np.ndarray] = None  # (n_omega, n_dof, n_dof)
    excitation_force: Optional[np.ndarray] = None  # (n_omega, n_heading, n_dof) complex
    diffraction_force: Optional[np.ndarray] = None  # (n_omega, n_heading, n_dof) complex
    froude_krylov_force: Optional[np.ndarray] = None  # (n_omega, n_heading, n_dof) complex
    # Coordinate arrays
    omegas: Optional[np.ndarray] = None
    periods: Optional[np.ndarray] = None
    headings: Optional[np.ndarray] = None
    dof_names: Optional[List[str]] = None
    # Hydrostatics
    hydrostatic_stiffness: Optional[np.ndarray] = None  # 6x6
    inertia_matrix: Optional[np.ndarray] = None  # 6x6
    # Metadata
    body_name: str = ""
    water_depth: float = np.inf
    rho: float = 1025.0
    g: float = 9.81


@dataclass
class RAOResult:
    """Response Amplitude Operators from post-processing."""
    rao: Optional[np.ndarray] = None  # (n_omega, n_heading, n_dof) complex
    rao_amplitude: Optional[np.ndarray] = None  # magnitude
    rao_phase: Optional[np.ndarray] = None  # degrees
    omegas: Optional[np.ndarray] = None
    periods: Optional[np.ndarray] = None
    headings: Optional[np.ndarray] = None
    dof_names: Optional[List[str]] = None
    # Source dataset
    dataset: object = None  # xarray.Dataset with RAO data
