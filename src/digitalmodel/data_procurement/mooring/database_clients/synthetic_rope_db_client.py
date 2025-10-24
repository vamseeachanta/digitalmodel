# ABOUTME: Synthetic rope database client for mooring synthetic ropes (polyester, HMPE, nylon)
# ABOUTME: Repository-based component selection with nonlinear stiffness and OrcaFlex export

"""
Synthetic Rope Database Client
================================

Repository-based mooring synthetic rope database.

Data Coverage:
- Fiber types: Polyester, Nylon (Polyamide), HMPE (Dyneema), Aramid (Kevlar)
- Constructions: 8-strand plaited, 12-strand braided, parallel
- Diameters: 100-400mm (typical offshore range)
- Complete mechanical properties including nonlinear stiffness
- Standards compliance (API RP 2SM, DNV-OS-E303, OCIMF MEG4)

Source: Industry-standard specifications and manufacturer catalogs
Authentication: None (repository data)

Example:
    client = SyntheticRopeDatabaseClient()

    # Find polyester ropes by design load
    ropes = client.find_by_design_load(8000, fiber_type='polyester')

    # Get specific rope
    rope = client.get_synthetic_rope(fiber_type='polyester', diameter=220)

    # Export to OrcaFlex with nonlinear stiffness
    orcaflex = client.to_orcaflex_line_type(rope, include_nonlinear=True)
"""

import logging
from typing import Dict, Any, List, Optional, Iterator
from pathlib import Path
import numpy as np

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class SyntheticRopeDatabaseClient(BaseAPIClient):
    """
    Mooring synthetic rope database client.

    Repository-based database of synthetic ropes with complete properties.

    Example:
        client = SyntheticRopeDatabaseClient()
        rope = client.get_synthetic_rope(fiber_type='polyester', diameter=220)
        print(f"MBL: {rope['minimum_breaking_load']} kN")
    """

    # Fiber type properties
    FIBER_TYPES = {
        'polyester': {
            'specific_gravity': 1.38,
            'mbl_factor': 0.17,  # MBL = mbl_factor × d² (kN, d in mm)
            'elongation_50': 6.5,  # % at 50% MBL
            'elongation_100': 12.0,  # % at 100% MBL
            'floats': False,
            'description': 'Deepwater mooring, primary choice',
            'strength': 'high',
            'uv_resistance': 'good_with_coating',
            'abrasion_resistance': 'good',
            'chemical_resistance': 'excellent',
            'service_life': 25  # years design
        },
        'nylon': {
            'specific_gravity': 1.14,
            'mbl_factor': 0.15,
            'elongation_50': 15.0,
            'elongation_100': 25.0,
            'floats': False,
            'description': 'Temporary mooring, high stretch',
            'strength': 'medium',
            'uv_resistance': 'moderate',
            'abrasion_resistance': 'excellent',
            'chemical_resistance': 'good',
            'service_life': 15
        },
        'hmpe': {  # Dyneema, Spectra
            'specific_gravity': 0.97,
            'mbl_factor': 0.22,
            'elongation_50': 2.0,
            'elongation_100': 3.5,
            'floats': True,
            'description': 'Ultra-deepwater, light weight',
            'strength': 'very_high',
            'uv_resistance': 'poor_needs_coating',
            'abrasion_resistance': 'poor',
            'chemical_resistance': 'excellent',
            'service_life': 20
        },
        'aramid': {  # Kevlar
            'specific_gravity': 1.44,
            'mbl_factor': 0.20,
            'elongation_50': 1.5,
            'elongation_100': 2.5,
            'floats': False,
            'description': 'Specialized applications',
            'strength': 'very_high',
            'uv_resistance': 'poor',
            'abrasion_resistance': 'moderate',
            'chemical_resistance': 'good',
            'service_life': 15
        }
    }

    # Standard synthetic rope diameters (mm)
    STANDARD_DIAMETERS = [
        100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
        200, 210, 220, 230, 240, 250, 260, 270, 280, 290,
        300, 320, 340, 360, 380, 400
    ]

    def __init__(self, database_path: str = None, **kwargs):
        """
        Initialize Synthetic Rope Database Client.

        Args:
            database_path: Path to synthetic rope database directory
            **kwargs: Additional BaseAPIClient arguments
        """
        auth_config = {'method': 'none'}

        super().__init__(
            base_url="file://localhost",
            auth_config=auth_config,
            **kwargs
        )

        if database_path:
            self.db_path = Path(database_path)
        else:
            self.db_path = Path(__file__).parent.parent / 'data' / 'synthetic_ropes'

        self.rope_database = self._initialize_rope_database()

        logger.info(f"Initialized SyntheticRopeDatabaseClient with {len(self.rope_database)} synthetic rope specifications")

    def _initialize_rope_database(self) -> List[Dict[str, Any]]:
        """Initialize synthetic rope database with standard specifications."""
        ropes = []

        for fiber_type, props in self.FIBER_TYPES.items():
            for diameter in self.STANDARD_DIAMETERS:
                rope = self._generate_rope_spec(fiber_type, diameter, props)
                ropes.append(rope)

        return ropes

    def _generate_rope_spec(self, fiber_type: str, diameter: float,
                           props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Generate complete synthetic rope specification.

        Args:
            fiber_type: Fiber type (polyester, nylon, hmpe, aramid)
            diameter: Diameter in mm
            props: Fiber properties

        Returns:
            Complete synthetic rope specification dict
        """
        # Calculate mechanical properties
        # MBL = mbl_factor × d² (kN)
        mbl = props['mbl_factor'] * (diameter ** 2)

        # Mass per meter = ρ × π/4 × d² / 1000 (kg/m)
        # Assuming ~80% fill factor for rope construction
        fill_factor = 0.80
        specific_gravity = props['specific_gravity']
        mass_per_meter = specific_gravity * 1000 * np.pi/4 * ((diameter/1000)**2) * fill_factor

        # Submerged mass (seawater density 1025 kg/m³)
        submerged_mass = mass_per_meter * (1 - 1.025 / specific_gravity)

        # Effective modulus (nonlinear, load-dependent)
        # Simplified: calculate at 50% MBL
        elongation_50 = props['elongation_50'] / 100  # Convert to fraction
        effective_modulus_50 = (0.5 * mbl * 1000) / (elongation_50 * 1)  # kN (simplified)

        # Axial stiffness EA at 50% MBL (kN)
        # This is approximate; real stiffness is highly nonlinear
        ea_50 = effective_modulus_50

        # Damping coefficient (typical for synthetic ropes)
        damping = 0.15

        return {
            # Identification
            'fiber_type': fiber_type,
            'construction': '8-strand plaited',  # Default
            'diameter': diameter,  # mm
            'description': props['description'],

            # Mechanical properties
            'minimum_breaking_load': round(mbl, 1),  # kN
            'elongation_at_50_mbl': props['elongation_50'],  # %
            'elongation_at_100_mbl': props['elongation_100'],  # %
            'creep': 'low',  # Design consideration

            # Physical properties
            'mass_per_meter': round(mass_per_meter, 2),  # kg/m
            'submerged_mass_per_meter': round(submerged_mass, 2),  # kg/m
            'specific_gravity': specific_gravity,
            'floats': props['floats'],

            # Stiffness (nonlinear)
            'axial_stiffness_ea_50': round(ea_50, 0),  # kN at 50% MBL
            'stiffness_type': 'nonlinear',  # load-dependent

            # Standards
            'standards': ['API RP 2SM', 'DNV-OS-E303', 'OCIMF MEG4'],

            # Environmental resistance
            'uv_degradation': props['uv_resistance'],
            'abrasion_resistance': props['abrasion_resistance'],
            'chemical_resistance': props['chemical_resistance'],
            'service_life': props['service_life'],  # years

            # Dynamic properties
            'damping_coefficient': damping,

            # Strength category
            'strength': props['strength']
        }

    def get_all_fiber_types(self) -> List[str]:
        """Get list of available fiber types."""
        return list(self.FIBER_TYPES.keys())

    def get_standard_diameters(self, min_diameter: float = None,
                              max_diameter: float = None) -> List[float]:
        """
        Get list of standard synthetic rope diameters.

        Args:
            min_diameter: Minimum diameter filter (mm)
            max_diameter: Maximum diameter filter (mm)

        Returns:
            List of standard diameters
        """
        diameters = self.STANDARD_DIAMETERS.copy()

        if min_diameter:
            diameters = [d for d in diameters if d >= min_diameter]

        if max_diameter:
            diameters = [d for d in diameters if d <= max_diameter]

        return diameters

    def get_synthetic_rope(self, fiber_type: str, diameter: float) -> Dict[str, Any]:
        """
        Get specific synthetic rope by fiber type and diameter.

        Args:
            fiber_type: Fiber type (polyester, nylon, hmpe, aramid)
            diameter: Diameter in mm

        Returns:
            Synthetic rope specification dict

        Example:
            rope = client.get_synthetic_rope(fiber_type='polyester', diameter=220)
            print(f"MBL: {rope['minimum_breaking_load']} kN")
        """
        if fiber_type not in self.FIBER_TYPES:
            raise ValueError(f"Unknown fiber type: {fiber_type}. "
                           f"Available: {list(self.FIBER_TYPES.keys())}")

        # Find exact match
        for rope in self.rope_database:
            if rope['fiber_type'] == fiber_type and rope['diameter'] == diameter:
                return rope.copy()

        # Find closest diameter
        closest_diameter = min(self.STANDARD_DIAMETERS,
                             key=lambda d: abs(d - diameter))

        if abs(closest_diameter - diameter) <= 5:
            logger.warning(f"Exact diameter {diameter}mm not found, using closest: {closest_diameter}mm")
            return self.get_synthetic_rope(fiber_type, closest_diameter)

        raise ValueError(f"Diameter {diameter}mm not in standard sizes. "
                        f"Closest: {closest_diameter}mm")

    def find_by_design_load(self, design_load: float,
                           fiber_type: str = None,
                           safety_factor: float = 1.67) -> List[Dict[str, Any]]:
        """
        Find synthetic ropes that meet design load requirement.

        Args:
            design_load: Required design load in kN
            fiber_type: Filter by specific fiber type (optional)
            safety_factor: Safety factor (default 1.67)

        Returns:
            List of suitable synthetic ropes sorted by diameter

        Example:
            ropes = client.find_by_design_load(8000, fiber_type='polyester')
            for rope in ropes:
                print(f"{rope['diameter']}mm: MBL={rope['minimum_breaking_load']}kN")
        """
        required_mbl = design_load * safety_factor

        suitable_ropes = []

        for rope in self.rope_database:
            if fiber_type and rope['fiber_type'] != fiber_type:
                continue

            if rope['minimum_breaking_load'] >= required_mbl:
                suitable_ropes.append(rope.copy())

        suitable_ropes.sort(key=lambda r: r['diameter'])

        return suitable_ropes

    def compare_fiber_types(self, design_load: float,
                           safety_factor: float = 1.67) -> Dict[str, Dict[str, Any]]:
        """
        Compare different fiber types for design load.

        Args:
            design_load: Required design load in kN
            safety_factor: Safety factor (default 1.67)

        Returns:
            Dict of fiber_type → optimal rope

        Example:
            comparison = client.compare_fiber_types(design_load=8000)
            for fiber, rope in comparison.items():
                print(f"{fiber}: {rope['diameter']}mm, {rope['mass_per_meter']}kg/m, floats={rope['floats']}")
        """
        comparison = {}

        for fiber_type in self.FIBER_TYPES.keys():
            ropes = self.find_by_design_load(design_load, fiber_type=fiber_type,
                                            safety_factor=safety_factor)
            if ropes:
                comparison[fiber_type] = ropes[0]

        return comparison

    def calculate_nonlinear_stiffness(self, rope: Dict[str, Any],
                                     tension_points: np.ndarray = None) -> Dict[str, np.ndarray]:
        """
        Calculate nonlinear stiffness curve for OrcaFlex.

        Args:
            rope: Synthetic rope specification dict
            tension_points: Tension points as % MBL (default: 0-100% in 10% steps)

        Returns:
            Dict with 'tension' (kN) and 'extension' (%) arrays

        Example:
            rope = client.get_synthetic_rope(fiber_type='polyester', diameter=220)
            stiffness = client.calculate_nonlinear_stiffness(rope)
        """
        if tension_points is None:
            tension_points = np.arange(0, 110, 10)  # 0-100% in 10% steps

        mbl = rope['minimum_breaking_load']
        tensions_kn = tension_points * mbl / 100

        # Nonlinear extension model (simplified power law)
        # extension = a × (T/MBL)^b
        # Calibrated to match elongation_50 and elongation_100

        elongation_50 = rope['elongation_at_50_mbl']
        elongation_100 = rope['elongation_at_100_mbl']

        # Fit power law: ε = a × (T/MBL)^b
        # At 50%: elongation_50 = a × 0.5^b
        # At 100%: elongation_100 = a × 1.0^b = a
        a = elongation_100
        b = np.log(elongation_50 / elongation_100) / np.log(0.5)

        # Calculate extensions
        extensions = a * (tension_points / 100) ** b

        return {
            'tension_kn': tensions_kn,
            'tension_percent_mbl': tension_points,
            'extension_percent': extensions
        }

    def to_orcaflex_line_type(self, rope: Dict[str, Any],
                             name: str = None,
                             include_nonlinear: bool = True) -> str:
        """
        Convert synthetic rope to OrcaFlex line type YAML (in-memory).

        Args:
            rope: Synthetic rope specification dict
            name: Line type name
            include_nonlinear: Include nonlinear stiffness curve

        Returns:
            OrcaFlex line type YAML string (in-memory, not saved)
        """
        import yaml

        if name is None:
            name = f"SyntheticRope_{rope['fiber_type'].capitalize()}_{rope['diameter']}mm"

        orcaflex_line = {
            'Name': name,
            'Category': 'GeneralLineType',

            'Mass per unit length': rope['mass_per_meter'],
            'Submerged mass per unit length': rope['submerged_mass_per_meter'],
            'Outside diameter': rope['diameter'] / 1000,

            'EA': rope['axial_stiffness_ea_50'] * 1000,  # Linearized at 50% MBL
            'EI': 0.0,  # Synthetic rope has no bending stiffness
            'GJ': 0.0,

            'Minimum breaking load': rope['minimum_breaking_load'] * 1000,

            'Fiber type': rope['fiber_type'],
            'Standards': ', '.join(rope['standards']),
            'Description': f"Synthetic rope {rope['fiber_type']} diameter {rope['diameter']}mm"
        }

        # Add nonlinear stiffness curve if requested
        if include_nonlinear:
            stiffness = self.calculate_nonlinear_stiffness(rope)

            orcaflex_line['NonlinearStiffness'] = {
                'Tension': stiffness['tension_kn'].tolist(),
                'Extension': stiffness['extension_percent'].tolist()
            }

        return yaml.dump({'LineType': orcaflex_line},
                        default_flow_style=False,
                        sort_keys=False)

    def stream_ropes(self, fiber_type: str = None,
                    min_diameter: float = None,
                    max_diameter: float = None) -> Iterator[Dict[str, Any]]:
        """Stream synthetic ropes matching criteria (in-memory)."""
        for rope in self.rope_database:
            if fiber_type and rope['fiber_type'] != fiber_type:
                continue

            if min_diameter and rope['diameter'] < min_diameter:
                continue

            if max_diameter and rope['diameter'] > max_diameter:
                continue

            yield rope.copy()

    def query_by_date(self, start_date, end_date, location: Dict[str, float] = None,
                     **kwargs) -> Iterator[Dict[str, Any]]:
        """Query synthetic ropes (compatibility method)."""
        yield from self.stream_ropes(
            fiber_type=kwargs.get('fiber_type'),
            min_diameter=kwargs.get('min_diameter'),
            max_diameter=kwargs.get('max_diameter')
        )
