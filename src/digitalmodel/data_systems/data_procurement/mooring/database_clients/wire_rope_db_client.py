# ABOUTME: Wire rope database client for mooring wire ropes (IWRC, FC constructions)
# ABOUTME: Repository-based component selection with property calculation and OrcaFlex export

"""
Wire Rope Database Client
==========================

Repository-based mooring wire rope database.

Data Coverage:
- Constructions: 6x36 IWRC, 6x41 IWRC, 8x19 FC, 6x19 IWRC
- Diameters: 50-150mm (typical offshore range)
- Complete mechanical properties
- Standards compliance (API RP 2SM, API Spec 9A, EN 12385)

Source: Industry-standard specifications and manufacturer catalogs
Authentication: None (repository data)

Example:
    client = WireRopeDatabaseClient()

    # Find wire ropes by design load
    ropes = client.find_by_design_load(4000, construction='6x36 IWRC')

    # Get specific wire rope
    rope = client.get_wire_rope(construction='6x36 IWRC', diameter=76)

    # Export to OrcaFlex
    orcaflex = client.to_orcaflex_line_type(rope)
"""

import logging
from typing import Dict, Any, List, Optional, Iterator
from pathlib import Path
import numpy as np

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class WireRopeDatabaseClient(BaseAPIClient):
    """
    Mooring wire rope database client.

    Repository-based database of wire ropes with complete properties.

    Example:
        client = WireRopeDatabaseClient()
        rope = client.get_wire_rope(construction='6x36 IWRC', diameter=76)
        print(f"MBL: {rope['minimum_breaking_load']} kN")
    """

    # Wire rope construction factors (MBL coefficient)
    CONSTRUCTION_FACTORS = {
        '6x36 IWRC': {
            'mbl_factor': 0.72,  # MBL = mbl_factor × d² (kN, d in mm)
            'mass_factor': 4.0,  # Mass = mass_factor × d² / 1000 (kg/m)
            'modulus': 100,  # GPa effective
            'flexibility': 'standard',
            'description': 'Standard offshore taut-leg mooring'
        },
        '6x41 IWRC': {
            'mbl_factor': 0.68,
            'mass_factor': 3.8,
            'modulus': 95,
            'flexibility': 'flexible',
            'description': 'Flexible, fatigue-resistant deepwater mooring'
        },
        '8x19 FC': {
            'mbl_factor': 0.50,
            'mass_factor': 2.8,
            'modulus': 80,
            'flexibility': 'very_flexible',
            'description': 'Very flexible temporary mooring (fiber core)'
        },
        '6x19 IWRC': {
            'mbl_factor': 0.75,
            'mass_factor': 4.2,
            'modulus': 105,
            'flexibility': 'rotation_resistant',
            'description': 'Rotation-resistant single-leg mooring'
        }
    }

    # Standard wire rope diameters (mm)
    STANDARD_DIAMETERS = [
        50, 52, 56, 60, 64, 68, 72, 76, 80, 84,
        88, 90, 92, 96, 100, 104, 108, 112, 116, 120,
        124, 128, 132, 136, 140, 144, 150
    ]

    def __init__(self, database_path: str = None, **kwargs):
        """
        Initialize Wire Rope Database Client.

        Args:
            database_path: Path to wire rope database directory
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
            self.db_path = Path(__file__).parent.parent / 'data' / 'wire_ropes'

        self.rope_database = self._initialize_rope_database()

        # Build indexes for O(1) lookup (Phase 1 optimization)
        self._construction_index = {}
        self._diameter_index = {}
        for rope in self.rope_database:
            construction = rope['construction']
            diameter = rope['diameter']

            # Construction index
            if construction not in self._construction_index:
                self._construction_index[construction] = []
            self._construction_index[construction].append(rope)

            # Diameter index
            if diameter not in self._diameter_index:
                self._diameter_index[diameter] = []
            self._diameter_index[diameter].append(rope)

        logger.info(f"Initialized WireRopeDatabaseClient with {len(self.rope_database)} wire rope specifications")

    def _initialize_rope_database(self) -> List[Dict[str, Any]]:
        """Initialize wire rope database with standard specifications."""
        ropes = []

        for construction, props in self.CONSTRUCTION_FACTORS.items():
            for diameter in self.STANDARD_DIAMETERS:
                rope = self._generate_rope_spec(construction, diameter, props)
                ropes.append(rope)

        return ropes

    def _generate_rope_spec(self, construction: str, diameter: float,
                           props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Generate complete wire rope specification.

        Args:
            construction: Wire rope construction
            diameter: Diameter in mm
            props: Construction properties

        Returns:
            Complete wire rope specification dict
        """
        # Calculate mechanical properties
        # MBL = mbl_factor × d² (kN)
        mbl = props['mbl_factor'] * (diameter ** 2)

        # Mass per meter = mass_factor × d² / 1000 (kg/m)
        mass_per_meter = props['mass_factor'] * (diameter ** 2) / 1000

        # Submerged mass (steel density 7850, seawater 1025)
        submerged_mass = mass_per_meter * (1 - 1.025 / 7.85)

        # Axial stiffness EA (kN)
        # Effective area ≈ π/4 × d² × fill_factor
        fill_factor = 0.55  # Wire rope has ~55% solid area
        area_mm2 = np.pi / 4 * (diameter ** 2) * fill_factor
        modulus_gpa = props['modulus']
        ea = area_mm2 * modulus_gpa * 1000 / 1000  # Convert to kN

        # Elongation at 50% MBL (typical)
        elongation_50 = 0.35 if 'IWRC' in construction else 0.50  # %

        # Minimum bend ratio (for fatigue)
        min_bend_ratio = 25 if 'IWRC' in construction else 20

        return {
            # Identification
            'construction': construction,
            'diameter': diameter,  # mm
            'flexibility': props['flexibility'],
            'description': props['description'],

            # Mechanical properties
            'minimum_breaking_load': round(mbl, 1),  # kN
            'elongation_at_50_mbl': elongation_50,  # %
            'modulus_of_elasticity': modulus_gpa,  # GPa

            # Physical properties
            'mass_per_meter': round(mass_per_meter, 2),  # kg/m
            'submerged_mass_per_meter': round(submerged_mass, 2),  # kg/m
            'axial_stiffness_ea': round(ea, 0),  # kN
            'lay_direction': 'regular',  # or 'lang'
            'grade': 'improved_plow_steel',  # or 'extra_improved'

            # Standards
            'standards': ['API RP 2SM', 'API Spec 9A', 'EN 12385'],

            # Termination
            'termination': {
                'type': 'spelter_socket',  # or 'swaged', 'mechanical'
                'efficiency': 0.95  # % of MBL retained
            },

            # Fatigue
            't_curve': 'Wire rope T-curve',
            'minimum_bend_ratio': min_bend_ratio  # × diameter
        }

    def get_all_constructions(self) -> List[str]:
        """Get list of available wire rope constructions."""
        return list(self.CONSTRUCTION_FACTORS.keys())

    def get_standard_diameters(self, min_diameter: float = None,
                              max_diameter: float = None) -> List[float]:
        """
        Get list of standard wire rope diameters.

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

    def get_wire_rope(self, construction: str, diameter: float) -> Dict[str, Any]:
        """
        Get specific wire rope by construction and diameter.

        Args:
            construction: Wire rope construction
            diameter: Diameter in mm

        Returns:
            Wire rope specification dict

        Example:
            rope = client.get_wire_rope(construction='6x36 IWRC', diameter=76)
            print(f"MBL: {rope['minimum_breaking_load']} kN")
        """
        if construction not in self.CONSTRUCTION_FACTORS:
            raise ValueError(f"Unknown construction: {construction}. "
                           f"Available: {list(self.CONSTRUCTION_FACTORS.keys())}")

        # Find exact match
        for rope in self.rope_database:
            if rope['construction'] == construction and rope['diameter'] == diameter:
                return rope.copy()

        # Find closest diameter
        closest_diameter = min(self.STANDARD_DIAMETERS,
                             key=lambda d: abs(d - diameter))

        if abs(closest_diameter - diameter) <= 2:
            logger.warning(f"Exact diameter {diameter}mm not found, using closest: {closest_diameter}mm")
            return self.get_wire_rope(construction, closest_diameter)

        raise ValueError(f"Diameter {diameter}mm not in standard sizes. "
                        f"Closest: {closest_diameter}mm")

    def find_by_design_load(self, design_load: float,
                           construction: str = None,
                           safety_factor: float = 1.67) -> List[Dict[str, Any]]:
        """
        Find wire ropes that meet design load requirement.

        Args:
            design_load: Required design load in kN
            construction: Filter by specific construction (optional)
            safety_factor: Safety factor (default 1.67)

        Returns:
            List of suitable wire ropes sorted by diameter

        Example:
            ropes = client.find_by_design_load(4000, construction='6x36 IWRC')
            for rope in ropes:
                print(f"{rope['diameter']}mm: MBL={rope['minimum_breaking_load']}kN")
        """
        required_mbl = design_load * safety_factor

        # Use construction index if specified (O(1) lookup instead of O(n))
        if construction:
            candidates = self._construction_index.get(construction, [])
        else:
            candidates = self.rope_database

        suitable_ropes = []
        for rope in candidates:
            if rope['minimum_breaking_load'] >= required_mbl:
                suitable_ropes.append(rope.copy())

        suitable_ropes.sort(key=lambda r: r['diameter'])

        return suitable_ropes

    def compare_constructions(self, design_load: float,
                            safety_factor: float = 1.67) -> Dict[str, Dict[str, Any]]:
        """
        Compare different constructions for design load.

        Args:
            design_load: Required design load in kN
            safety_factor: Safety factor (default 1.67)

        Returns:
            Dict of construction → optimal rope

        Example:
            comparison = client.compare_constructions(design_load=4000)
            for construction, rope in comparison.items():
                print(f"{construction}: {rope['diameter']}mm, {rope['mass_per_meter']}kg/m")
        """
        comparison = {}

        for construction in self.CONSTRUCTION_FACTORS.keys():
            ropes = self.find_by_design_load(design_load, construction=construction,
                                            safety_factor=safety_factor)
            if ropes:
                comparison[construction] = ropes[0]

        return comparison

    def to_orcaflex_line_type(self, rope: Dict[str, Any],
                             name: str = None) -> str:
        """
        Convert wire rope to OrcaFlex line type YAML (in-memory).

        Args:
            rope: Wire rope specification dict
            name: Line type name

        Returns:
            OrcaFlex line type YAML string (in-memory, not saved)
        """
        import yaml

        if name is None:
            name = f"WireRope_{rope['construction'].replace(' ', '_')}_{rope['diameter']}mm"

        orcaflex_line = {
            'Name': name,
            'Category': 'GeneralLineType',

            'Mass per unit length': rope['mass_per_meter'],
            'Submerged mass per unit length': rope['submerged_mass_per_meter'],
            'Outside diameter': rope['diameter'] / 1000,

            'EA': rope['axial_stiffness_ea'] * 1000,
            'EI': 0.0,  # Wire rope has minimal bending stiffness
            'GJ': 0.0,  # Minimal torsional stiffness

            'Minimum breaking load': rope['minimum_breaking_load'] * 1000,

            'Construction': rope['construction'],
            'Standards': ', '.join(rope['standards']),
            'Description': f"Wire rope {rope['construction']} diameter {rope['diameter']}mm"
        }

        return yaml.dump({'LineType': orcaflex_line},
                        default_flow_style=False,
                        sort_keys=False)

    def stream_ropes(self, construction: str = None,
                    min_diameter: float = None,
                    max_diameter: float = None) -> Iterator[Dict[str, Any]]:
        """Stream wire ropes matching criteria (in-memory)."""
        for rope in self.rope_database:
            if construction and rope['construction'] != construction:
                continue

            if min_diameter and rope['diameter'] < min_diameter:
                continue

            if max_diameter and rope['diameter'] > max_diameter:
                continue

            yield rope.copy()

    def query_by_date(self, start_date, end_date, location: Dict[str, float] = None,
                     **kwargs) -> Iterator[Dict[str, Any]]:
        """Query wire ropes (compatibility method)."""
        yield from self.stream_ropes(
            construction=kwargs.get('construction'),
            min_diameter=kwargs.get('min_diameter'),
            max_diameter=kwargs.get('max_diameter')
        )
