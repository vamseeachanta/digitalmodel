"""
ABOUTME: Capytaine BEM hydrodynamic analysis module for digitalmodel.
ABOUTME: Open-source potential flow solver for added mass, damping, excitation forces, and RAOs.
"""

__version__ = "0.1.0"

from .models import (
    BEMResult,
    BodyConfig,
    DOF,
    MeshConfig,
    MeshFormat,
    RAOResult,
    SolverConfig,
    WaveConditions,
)
from .mesh_adapter import create_floating_body
from .solver import CapytaineSolver, run_bem_analysis
from .rao import compute_rao, compute_rao_manual
from .results import (
    added_mass_table,
    excitation_force_table,
    export_netcdf,
    plot_added_mass_damping,
    plot_excitation_force,
    plot_rao,
)

__all__ = [
    # Models
    "BEMResult",
    "BodyConfig",
    "DOF",
    "MeshConfig",
    "MeshFormat",
    "RAOResult",
    "SolverConfig",
    "WaveConditions",
    # Mesh
    "create_floating_body",
    # Solver
    "CapytaineSolver",
    "run_bem_analysis",
    # RAO
    "compute_rao",
    "compute_rao_manual",
    # Results
    "added_mass_table",
    "excitation_force_table",
    "export_netcdf",
    "plot_added_mass_damping",
    "plot_excitation_force",
    "plot_rao",
]
