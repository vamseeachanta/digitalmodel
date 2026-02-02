"""
Data Models for Structural Analysis

Defines data structures for stress states, material properties,
geometry, and analysis results.
"""

from dataclasses import dataclass
from typing import Dict, Tuple
import numpy as np


@dataclass
class StressState:
    """Complete stress state at a point."""
    sigma_x: float = 0.0    # Normal stress in x direction (MPa)
    sigma_y: float = 0.0    # Normal stress in y direction (MPa)
    sigma_z: float = 0.0    # Normal stress in z direction (MPa)
    tau_xy: float = 0.0     # Shear stress xy (MPa)
    tau_xz: float = 0.0     # Shear stress xz (MPa)
    tau_yz: float = 0.0     # Shear stress yz (MPa)

    def von_mises(self) -> float:
        """Calculate Von Mises equivalent stress."""
        return np.sqrt(
            0.5 * (
                (self.sigma_x - self.sigma_y)**2 +
                (self.sigma_y - self.sigma_z)**2 +
                (self.sigma_z - self.sigma_x)**2 +
                6 * (self.tau_xy**2 + self.tau_xz**2 + self.tau_yz**2)
            )
        )

    def principal_stresses(self) -> Tuple[float, float, float]:
        """Calculate principal stresses."""
        # Build stress tensor
        tensor = np.array([
            [self.sigma_x, self.tau_xy, self.tau_xz],
            [self.tau_xy, self.sigma_y, self.tau_yz],
            [self.tau_xz, self.tau_yz, self.sigma_z]
        ])

        # Eigenvalues are principal stresses
        eigenvalues = np.linalg.eigvalsh(tensor)
        return tuple(sorted(eigenvalues, reverse=True))

    def max_shear(self) -> float:
        """Calculate maximum shear stress."""
        s1, s2, s3 = self.principal_stresses()
        return (s1 - s3) / 2


@dataclass
class MaterialProperties:
    """Material properties for structural analysis."""
    yield_strength: float     # MPa
    ultimate_strength: float  # MPa
    youngs_modulus: float     # MPa
    poissons_ratio: float
    density: float            # kg/m³
    name: str = "Steel"


# Common structural steels
STEEL_S275 = MaterialProperties(
    yield_strength=275,
    ultimate_strength=430,
    youngs_modulus=210000,
    poissons_ratio=0.3,
    density=7850,
    name="S275"
)

STEEL_S355 = MaterialProperties(
    yield_strength=355,
    ultimate_strength=510,
    youngs_modulus=210000,
    poissons_ratio=0.3,
    density=7850,
    name="S355"
)

STEEL_S420 = MaterialProperties(
    yield_strength=420,
    ultimate_strength=520,
    youngs_modulus=210000,
    poissons_ratio=0.3,
    density=7850,
    name="S420"
)


@dataclass
class PlateGeometry:
    """Plate geometry for buckling analysis."""
    length: float       # a (mm)
    width: float        # b (mm)
    thickness: float    # t (mm)


@dataclass
class BucklingResult:
    """Results from buckling analysis."""
    critical_stress: float    # MPa
    applied_stress: float     # MPa
    utilization: float
    safety_factor: float
    mode: str
    passes: bool


@dataclass
class CapacityResult:
    """Results from capacity check."""
    capacity: float
    demand: float
    utilization: float
    governing_mode: str
    passes: bool
    details: Dict
