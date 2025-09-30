"""
Plate Capacity Analysis Module

This module provides comprehensive plate buckling analysis capabilities based on
DNV standards and engineering best practices. It consolidates functionality from
the original platecapacity domain modules with improved structure and error handling.

Author: Migrated and modernized from original work by Sai Venkatesh
Date: 2025-01-15
Standards: DNV-RP-C201, API standards
"""

from typing import Dict, List, Optional, Tuple, Union, Any
from dataclasses import dataclass, field
from enum import Enum
import math
import logging
from pathlib import Path

# Set up logging
logger = logging.getLogger(__name__)


class BoundaryCondition(Enum):
    """Enumeration for plate boundary conditions."""
    SIMPLY_SUPPORTED = "simply_supported"
    SIDES_CLAMPED = "sides_clamped"
    ALL_EDGES_CLAMPED = "all_edges_clamped"
    FREE_EDGE = "free_edge"


class LoadingType(Enum):
    """Enumeration for plate loading types."""
    UNIFORM_COMPRESSION = "uniform_compression"
    EDGE_COMPRESSION = "edge_compression"
    BIAXIAL = "biaxial"
    SHEAR = "shear"
    COMBINED = "combined"


@dataclass
class PlateProperties:
    """Data class for plate geometric and material properties."""

    # Geometric properties
    length: float  # Plate length (m or in)
    breadth: float  # Plate breadth/width (m or in)
    thickness: float  # Plate thickness (m or in)

    # Material properties
    youngs_modulus: float  # Young's modulus (Pa or psi)
    poisson_ratio: float = 0.3  # Poisson's ratio (dimensionless)
    yield_strength: float = 0.0  # Yield strength (Pa or psi)
    tangent_modulus: Optional[float] = None  # Tangent modulus (Pa or psi)

    # Environmental properties
    water_depth: float = 0.0  # Water depth (m or ft)

    # Units
    length_unit: str = "m"
    stress_unit: str = "Pa"

    def __post_init__(self):
        """Validate input parameters after initialization."""
        if self.length <= 0:
            raise ValueError("Plate length must be positive")
        if self.breadth <= 0:
            raise ValueError("Plate breadth must be positive")
        if self.thickness <= 0:
            raise ValueError("Plate thickness must be positive")
        if self.youngs_modulus <= 0:
            raise ValueError("Young's modulus must be positive")
        if not 0 < self.poisson_ratio < 0.5:
            raise ValueError("Poisson's ratio must be between 0 and 0.5")
        if self.yield_strength < 0:
            raise ValueError("Yield strength must be non-negative")
        if self.water_depth < 0:
            raise ValueError("Water depth must be non-negative")


@dataclass
class AppliedLoads:
    """Data class for applied loads on the plate."""

    longitudinal_stress: float = 0.0  # σx stress (Pa or psi)
    transverse_stress: float = 0.0   # σy stress (Pa or psi)
    shear_stress: float = 0.0        # τxy shear stress (Pa or psi)

    # Optional: for more complex loading
    pressure_load: float = 0.0       # Normal pressure (Pa or psi)

    def __post_init__(self):
        """Validate applied loads."""
        # Convert to absolute values for checking but preserve signs
        pass  # Stresses can be positive (tension) or negative (compression)


@dataclass
class BucklingConstants:
    """Data class for buckling analysis constants."""

    # Buckling coefficients from tables/standards
    k1: float = 0.425  # Edge compression constant
    k2: float = 5.34   # Shear buckling constant
    k3: float = 4.0    # Additional buckling constant
    k4: float = 1.0    # Material/geometric factor
    k5: float = 0.425  # Stress curve constant

    # Boundary condition factors
    c_xx: float = 4.0     # Longitudinal BC factor
    c_yy: float = 1.0     # Transverse BC factor
    c_tau: float = 5.34   # Shear BC factor

    # Safety factors
    material_factor: float = 1.15  # Material safety factor (γM)

    def __post_init__(self):
        """Validate buckling constants."""
        if self.material_factor <= 0:
            raise ValueError("Material factor must be positive")


@dataclass
class BucklingResults:
    """Data class to store buckling analysis results."""

    # Critical stresses
    critical_stress_longitudinal: float = 0.0
    critical_stress_transverse: float = 0.0
    critical_stress_shear: float = 0.0
    critical_stress_equivalent: float = 0.0

    # Reduced slenderness ratios
    lambda_x: float = 0.0
    lambda_y: float = 0.0
    lambda_tau: float = 0.0
    lambda_e: float = 0.0

    # Usage factors
    usage_factor_longitudinal: float = 0.0
    usage_factor_transverse: float = 0.0
    usage_factor_shear: float = 0.0
    usage_factor_equivalent: float = 0.0
    usage_factor_biaxial: float = 0.0

    # Status flags
    is_safe: bool = True
    failure_mode: Optional[str] = None

    # Additional results
    von_mises_stress: float = 0.0
    elastic_buckling_stresses: Dict[str, float] = field(default_factory=dict)
    characteristic_resistances: Dict[str, float] = field(default_factory=dict)


class PlateBucklingAnalyzer:
    """
    Main class for plate buckling analysis according to DNV standards.

    This class performs comprehensive plate buckling analysis including:
    - Elastic buckling calculations
    - Ultimate strength analysis
    - Serviceability checks
    - Usage factor calculations
    """

    def __init__(self,
                 plate_props: PlateProperties,
                 applied_loads: AppliedLoads,
                 buckling_constants: Optional[BucklingConstants] = None,
                 boundary_condition: BoundaryCondition = BoundaryCondition.SIMPLY_SUPPORTED):
        """
        Initialize the plate buckling analyzer.

        Args:
            plate_props: Plate geometric and material properties
            applied_loads: Applied stress loads
            buckling_constants: Buckling analysis constants (optional)
            boundary_condition: Plate boundary conditions
        """
        self.plate_props = plate_props
        self.applied_loads = applied_loads
        self.buckling_constants = buckling_constants or BucklingConstants()
        self.boundary_condition = boundary_condition

        # Calculated geometric ratios
        self._calculate_geometric_ratios()

        logger.info(f"Initialized plate buckling analyzer for {plate_props.length_unit} units")

    def _calculate_geometric_ratios(self) -> None:
        """Calculate common geometric ratios used in analysis."""
        self.aspect_ratio = self.plate_props.length / self.plate_props.breadth  # l/b
        self.breadth_ratio = self.plate_props.breadth / self.plate_props.length  # b/l
        self.thickness_ratio = self.plate_props.thickness / self.plate_props.breadth  # t/b
        self.slenderness_ratio = self.plate_props.breadth / self.plate_props.thickness  # b/t

        logger.debug(f"Geometric ratios - Aspect: {self.aspect_ratio:.3f}, "
                    f"Slenderness: {self.slenderness_ratio:.3f}")

    def calculate_von_mises_stress(self) -> float:
        """
        Calculate the equivalent von Mises stress from applied loads.

        Returns:
            von Mises stress value
        """
        σx = self.applied_loads.longitudinal_stress
        σy = self.applied_loads.transverse_stress
        τxy = self.applied_loads.shear_stress

        von_mises = math.sqrt(σx**2 + σy**2 - σx*σy + 3*τxy**2)

        logger.debug(f"Von Mises stress: {von_mises:.2f} {self.plate_props.stress_unit}")
        return von_mises

    def calculate_elastic_buckling_stresses(self) -> Dict[str, float]:
        """
        Calculate elastic buckling stresses for different loading modes.

        Returns:
            Dictionary of elastic buckling stresses
        """
        # Base factor for elastic buckling
        E = self.plate_props.youngs_modulus
        ν = self.plate_props.poisson_ratio
        t = self.plate_props.thickness
        b = self.plate_props.breadth

        base_factor = (math.pi**2 * E) / (12 * (1 - ν**2)) * (t/b)**2

        # Buckling coefficients based on boundary conditions
        if self.boundary_condition == BoundaryCondition.SIMPLY_SUPPORTED:
            k_xx = self.buckling_constants.c_xx
            k_yy = (1 + self.breadth_ratio**2)**2
            k_tau = 5.34 + 4 * self.breadth_ratio**2
        elif self.boundary_condition == BoundaryCondition.SIDES_CLAMPED:
            k_xx = 7.0  # Typical value for clamped edges
            k_yy = 1 + 2.5 * self.breadth_ratio**2 + 5 * self.breadth_ratio**4
            k_tau = 9 + 5.6 * self.breadth_ratio**2
        else:
            # Default to simply supported
            k_xx = self.buckling_constants.c_xx
            k_yy = (1 + self.breadth_ratio**2)**2
            k_tau = 5.34 + 4 * self.breadth_ratio**2

        elastic_stresses = {
            'longitudinal': base_factor * k_xx,
            'transverse': base_factor * k_yy,
            'shear': base_factor * k_tau
        }

        logger.debug(f"Elastic buckling stresses calculated: {elastic_stresses}")
        return elastic_stresses

    def calculate_reduced_slenderness_ratios(self, elastic_stresses: Dict[str, float]) -> Dict[str, float]:
        """
        Calculate reduced slenderness ratios for stability analysis.

        Args:
            elastic_stresses: Elastic buckling stresses

        Returns:
            Dictionary of slenderness ratios
        """
        # Characteristic material resistances
        fy = self.plate_props.yield_strength
        σkx = σky = fy  # Material resistance in x and y directions
        τk = fy / math.sqrt(3)  # Shear resistance

        # Calculate slenderness ratios
        λx = math.sqrt(σkx / elastic_stresses['longitudinal']) if elastic_stresses['longitudinal'] > 0 else 0
        λy = math.sqrt(σky / elastic_stresses['transverse']) if elastic_stresses['transverse'] > 0 else 0
        λτ = math.sqrt(τk / elastic_stresses['shear']) if elastic_stresses['shear'] > 0 else 0

        # Equivalent slenderness for combined loading
        σx = abs(self.applied_loads.longitudinal_stress)
        σy = abs(self.applied_loads.transverse_stress)
        τxy = abs(self.applied_loads.shear_stress)

        c = 2 - self.breadth_ratio  # Interaction exponent

        if fy > 0:
            λe_term = ((σx/elastic_stresses['longitudinal'])**c +
                      (σy/elastic_stresses['transverse'])**c +
                      (τxy/elastic_stresses['shear'])**c)**(1/c)
            λe = math.sqrt(fy / self.calculate_von_mises_stress() * λe_term) if λe_term > 0 else 0
        else:
            λe = 0

        slenderness_ratios = {
            'lambda_x': λx,
            'lambda_y': λy,
            'lambda_tau': λτ,
            'lambda_e': λe
        }

        logger.debug(f"Slenderness ratios: {slenderness_ratios}")
        return slenderness_ratios

    def calculate_characteristic_resistances(self, slenderness_ratios: Dict[str, float]) -> Dict[str, float]:
        """
        Calculate characteristic buckling resistances.

        Args:
            slenderness_ratios: Reduced slenderness ratios

        Returns:
            Dictionary of characteristic resistances
        """
        fy = self.plate_props.yield_strength
        σkx = σky = fy
        τk = fy / math.sqrt(3)

        λx = slenderness_ratios['lambda_x']
        λy = slenderness_ratios['lambda_y']
        λτ = slenderness_ratios['lambda_tau']
        λe = slenderness_ratios['lambda_e']

        # Serviceability resistances
        σscr_x = σkx / math.sqrt(1 + λx**4) if λx > 0 else σkx
        σscr_y = σky / math.sqrt(1 + λy**4) if λy > 0 else σky
        τscr = τk / math.sqrt(1 + λτ**4) if λτ > 0 else τk
        σescr = fy / math.sqrt(1 + λe**4) if λe > 0 else fy

        # Ultimate resistances
        if λx < 1:
            σucr_x = σkx / math.sqrt(1 + λx**4)
        else:
            σucr_x = σkx / (math.sqrt(2) * λx)

        if λy < 1:
            σucr_y = σky / math.sqrt(1 + λy**4)
        else:
            σucr_y = σky / (math.sqrt(2) * λy)

        if λτ < 1:
            τucr = τk / math.sqrt(1 + λτ**4)
        else:
            τucr = τk / (math.sqrt(2) * λτ)

        if λe < 1:
            σeucr = fy / math.sqrt(1 + λe**4)
        else:
            σeucr = fy / (math.sqrt(2) * λe)

        resistances = {
            'serviceability': {
                'longitudinal': σscr_x,
                'transverse': σscr_y,
                'shear': τscr,
                'equivalent': σescr
            },
            'ultimate': {
                'longitudinal': σucr_x,
                'transverse': σucr_y,
                'shear': τucr,
                'equivalent': σeucr
            }
        }

        logger.debug(f"Characteristic resistances calculated")
        return resistances

    def calculate_usage_factors(self, resistances: Dict[str, Dict[str, float]]) -> Dict[str, float]:
        """
        Calculate usage factors for serviceability and ultimate limit states.

        Args:
            resistances: Characteristic resistances

        Returns:
            Dictionary of usage factors
        """
        σx = abs(self.applied_loads.longitudinal_stress)
        σy = abs(self.applied_loads.transverse_stress)
        τxy = abs(self.applied_loads.shear_stress)
        von_mises = self.calculate_von_mises_stress()

        # Serviceability usage factors
        ηs_x = σx / resistances['serviceability']['longitudinal'] if resistances['serviceability']['longitudinal'] > 0 else 0
        ηs_y = σy / resistances['serviceability']['transverse'] if resistances['serviceability']['transverse'] > 0 else 0
        ηs_τ = τxy / resistances['serviceability']['shear'] if resistances['serviceability']['shear'] > 0 else 0
        ηs_e = von_mises / resistances['serviceability']['equivalent'] if resistances['serviceability']['equivalent'] > 0 else 0

        # Ultimate usage factors
        ηu_x = σx / resistances['ultimate']['longitudinal'] if resistances['ultimate']['longitudinal'] > 0 else 0
        ηu_y = σy / resistances['ultimate']['transverse'] if resistances['ultimate']['transverse'] > 0 else 0
        ηu_τ = τxy / resistances['ultimate']['shear'] if resistances['ultimate']['shear'] > 0 else 0
        ηu_e = von_mises / resistances['ultimate']['equivalent'] if resistances['ultimate']['equivalent'] > 0 else 0

        # Biaxial interaction usage factor (if applicable)
        γM = self.buckling_constants.material_factor
        ci = 1 - self.plate_props.breadth / (120 * self.plate_props.thickness)
        ci = max(0, ci)  # Ensure non-negative

        σxrd = resistances['ultimate']['longitudinal'] / γM
        σyrd = resistances['ultimate']['transverse'] / γM
        τrd = resistances['ultimate']['shear'] / γM

        if σxrd > 0 and σyrd > 0 and τrd > 0:
            biaxial_term = ((σx/σxrd)**2 + (σy/σyrd)**2 -
                           ci*(σx/σxrd)*(σy/σyrd) + (τxy/τrd)**2)
            ηu_biaxial = math.sqrt(max(0, biaxial_term))
        else:
            ηu_biaxial = 0

        usage_factors = {
            'serviceability': {
                'longitudinal': ηs_x,
                'transverse': ηs_y,
                'shear': ηs_τ,
                'equivalent': ηs_e
            },
            'ultimate': {
                'longitudinal': ηu_x,
                'transverse': ηu_y,
                'shear': ηu_τ,
                'equivalent': ηu_e,
                'biaxial': ηu_biaxial
            }
        }

        logger.debug(f"Usage factors calculated")
        return usage_factors

    def perform_analysis(self) -> BucklingResults:
        """
        Perform complete plate buckling analysis.

        Returns:
            BucklingResults object with all analysis results
        """
        logger.info("Starting plate buckling analysis")

        # Calculate von Mises stress
        von_mises = self.calculate_von_mises_stress()

        # Calculate elastic buckling stresses
        elastic_stresses = self.calculate_elastic_buckling_stresses()

        # Calculate slenderness ratios
        slenderness_ratios = self.calculate_reduced_slenderness_ratios(elastic_stresses)

        # Calculate characteristic resistances
        resistances = self.calculate_characteristic_resistances(slenderness_ratios)

        # Calculate usage factors
        usage_factors = self.calculate_usage_factors(resistances)

        # Determine safety status
        max_usage_ultimate = max([
            usage_factors['ultimate']['longitudinal'],
            usage_factors['ultimate']['transverse'],
            usage_factors['ultimate']['shear'],
            usage_factors['ultimate']['equivalent'],
            usage_factors['ultimate']['biaxial']
        ])

        is_safe = max_usage_ultimate <= 1.0
        failure_mode = None

        if not is_safe:
            # Determine dominant failure mode
            usage_vals = usage_factors['ultimate']
            if usage_vals['biaxial'] == max_usage_ultimate:
                failure_mode = "Biaxial interaction"
            elif usage_vals['longitudinal'] == max_usage_ultimate:
                failure_mode = "Longitudinal buckling"
            elif usage_vals['transverse'] == max_usage_ultimate:
                failure_mode = "Transverse buckling"
            elif usage_vals['shear'] == max_usage_ultimate:
                failure_mode = "Shear buckling"
            else:
                failure_mode = "Equivalent stress"

        # Create results object
        results = BucklingResults(
            critical_stress_longitudinal=elastic_stresses['longitudinal'],
            critical_stress_transverse=elastic_stresses['transverse'],
            critical_stress_shear=elastic_stresses['shear'],
            critical_stress_equivalent=von_mises,

            lambda_x=slenderness_ratios['lambda_x'],
            lambda_y=slenderness_ratios['lambda_y'],
            lambda_tau=slenderness_ratios['lambda_tau'],
            lambda_e=slenderness_ratios['lambda_e'],

            usage_factor_longitudinal=usage_factors['ultimate']['longitudinal'],
            usage_factor_transverse=usage_factors['ultimate']['transverse'],
            usage_factor_shear=usage_factors['ultimate']['shear'],
            usage_factor_equivalent=usage_factors['ultimate']['equivalent'],
            usage_factor_biaxial=usage_factors['ultimate']['biaxial'],

            is_safe=is_safe,
            failure_mode=failure_mode,
            von_mises_stress=von_mises,
            elastic_buckling_stresses=elastic_stresses,
            characteristic_resistances=resistances
        )

        logger.info(f"Analysis complete. Safety status: {'SAFE' if is_safe else 'UNSAFE'}")
        if failure_mode:
            logger.warning(f"Failure mode: {failure_mode}")

        return results


def create_plate_from_legacy_data(legacy_data: Dict[str, Any]) -> Tuple[PlateProperties, AppliedLoads, BucklingConstants]:
    """
    Create modern data structures from legacy parameter dictionaries.

    Args:
        legacy_data: Dictionary containing legacy parameter data

    Returns:
        Tuple of (PlateProperties, AppliedLoads, BucklingConstants)
    """
    # Extract plate properties
    plate_props = PlateProperties(
        length=legacy_data.get('PlateLength', 0),
        breadth=legacy_data.get('PlateBreadth', 0),
        thickness=legacy_data.get('PlateThickness', 0),
        youngs_modulus=legacy_data.get('YoungsModulus', 0),
        poisson_ratio=legacy_data.get('PoissionsRatio', 0.3),
        yield_strength=legacy_data.get('YieldStrength', 0),
        tangent_modulus=legacy_data.get('TangentModulus'),
        water_depth=legacy_data.get('AverageWaterDepth', 0),
        length_unit=legacy_data.get('PlateLength_unit', 'm'),
        stress_unit=legacy_data.get('YieldStrength_unit', 'Pa')
    )

    # Extract applied loads
    applied_loads = AppliedLoads(
        longitudinal_stress=legacy_data.get('LongtudinalStress', 0),
        transverse_stress=legacy_data.get('TransverseStress', 0),
        shear_stress=legacy_data.get('ShearStress', 0)
    )

    # Extract buckling constants
    buckling_constants = BucklingConstants(
        k1=legacy_data.get('constantvalueTable1', 0.425),
        k3=legacy_data.get('constantvalueTable3', 4.0),
        k5=legacy_data.get('constantvalueTable5', 0.425),
        material_factor=legacy_data.get('Resulting material factor', 1.15)
    )

    return plate_props, applied_loads, buckling_constants


# Example usage and testing functions
def run_example_analysis() -> None:
    """Run an example plate buckling analysis."""

    # Define plate properties (using SI units)
    plate_props = PlateProperties(
        length=2.69,  # m
        breadth=0.70,  # m
        thickness=0.014,  # m
        youngs_modulus=210e9,  # Pa (210 GPa for steel)
        poisson_ratio=0.3,
        yield_strength=235e6,  # Pa (235 MPa for mild steel)
        water_depth=40.0,  # m
        length_unit="m",
        stress_unit="Pa"
    )

    # Define applied loads
    applied_loads = AppliedLoads(
        longitudinal_stress=0.5e6,  # Pa (0.5 MPa)
        transverse_stress=0.5e6,   # Pa (0.5 MPa)
        shear_stress=0.7e6         # Pa (0.7 MPa)
    )

    # Create analyzer and run analysis
    analyzer = PlateBucklingAnalyzer(
        plate_props=plate_props,
        applied_loads=applied_loads,
        boundary_condition=BoundaryCondition.SIMPLY_SUPPORTED
    )

    results = analyzer.perform_analysis()

    # Print results
    print(f"Plate Buckling Analysis Results:")
    print(f"================================")
    print(f"Von Mises Stress: {results.von_mises_stress/1e6:.2f} MPa")
    print(f"Safety Status: {'SAFE' if results.is_safe else 'UNSAFE'}")
    if results.failure_mode:
        print(f"Failure Mode: {results.failure_mode}")
    print(f"\nUsage Factors (Ultimate):")
    print(f"  Longitudinal: {results.usage_factor_longitudinal:.3f}")
    print(f"  Transverse: {results.usage_factor_transverse:.3f}")
    print(f"  Shear: {results.usage_factor_shear:.3f}")
    print(f"  Biaxial: {results.usage_factor_biaxial:.3f}")


if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.INFO)

    # Run example analysis
    run_example_analysis()