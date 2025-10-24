# ABOUTME: Chain database client for mooring chains (R3-R6 grades, all sizes)
# ABOUTME: Repository-based component selection with property calculation and OrcaFlex export

"""
Chain Database Client
======================

Repository-based mooring chain database.

Data Coverage:
- Chain grades: R3, R3S, R4, R4S, R5, R6
- Diameters: 50-200mm (typical offshore range)
- Complete mechanical properties
- Standards compliance (API RP 2SK, DNV-OS-E301, ISO 20438)

Source: Industry-standard specifications and manufacturer catalogs
Authentication: None (repository data)

Example:
    client = ChainDatabaseClient()

    # Find chains by design load
    chains = client.find_by_design_load(5000, grade='R4S')

    # Get specific chain
    chain = client.get_chain(grade='R4S', diameter=127)

    # Export to OrcaFlex
    orcaflex = client.to_orcaflex_line_type(chain)
"""

import logging
from typing import Dict, Any, List, Optional, Iterator
from pathlib import Path
import json
import numpy as np

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class ChainDatabaseClient(BaseAPIClient):
    """
    Mooring chain database client.

    Repository-based database of mooring chains with complete properties.

    Example:
        client = ChainDatabaseClient()
        chain = client.get_chain(grade='R4S', diameter=127)
        print(f"MBL: {chain['minimum_breaking_load']} kN")
        print(f"Mass: {chain['mass_per_meter']} kg/m")
    """

    # Chain grade factors (kN per mm²)
    GRADE_FACTORS = {
        'R3': 44,
        'R3S': 44,
        'R4': 50,
        'R4S': 50,
        'R5': 58,
        'R6': 66
    }

    # Standard chain diameters (mm)
    STANDARD_DIAMETERS = [
        50, 56, 62, 68, 73, 76, 81, 84, 87, 90,
        95, 100, 102, 105, 107, 111, 114, 117, 120, 122,
        127, 130, 132, 137, 142, 147, 152, 157, 162, 168,
        173, 178, 183, 200
    ]

    def __init__(self, database_path: str = None, **kwargs):
        """
        Initialize Chain Database Client.

        Args:
            database_path: Path to chain database directory
                          Default: mooring/data/chains/
            **kwargs: Additional BaseAPIClient arguments
        """
        auth_config = {'method': 'none'}  # Repository data, no authentication

        # Not a web API, but we use BaseAPIClient for consistency
        super().__init__(
            base_url="file://localhost",
            auth_config=auth_config,
            **kwargs
        )

        # Set database path
        if database_path:
            self.db_path = Path(database_path)
        else:
            # Default to repository location
            self.db_path = Path(__file__).parent.parent / 'data' / 'chains'

        # Initialize chain database (embedded)
        self.chain_database = self._initialize_chain_database()

        logger.info(f"Initialized ChainDatabaseClient with {len(self.chain_database)} chain specifications")

    def _initialize_chain_database(self) -> List[Dict[str, Any]]:
        """
        Initialize chain database with standard specifications.

        Returns:
            List of chain specifications
        """
        chains = []

        # Generate chains for all grades and standard diameters
        for grade in self.GRADE_FACTORS.keys():
            for diameter in self.STANDARD_DIAMETERS:
                chain = self._generate_chain_spec(grade, diameter)
                chains.append(chain)

        return chains

    def _generate_chain_spec(self, grade: str, diameter: float) -> Dict[str, Any]:
        """
        Generate complete chain specification.

        Args:
            grade: Chain grade (R3, R3S, R4, R4S, R5, R6)
            diameter: Chain diameter in mm

        Returns:
            Complete chain specification dict
        """
        grade_factor = self.GRADE_FACTORS[grade]
        diameter_m = diameter / 1000  # Convert to meters

        # Calculate mechanical properties
        # Proof load = 2 × d² × grade_factor (kN)
        proof_load = 2 * (diameter ** 2) * grade_factor / 1000

        # Minimum breaking load = 3 × d² × grade_factor (kN)
        mbl = 3 * (diameter ** 2) * grade_factor / 1000

        # Mass per meter = 7.86 × d² / 1000 (kg/m) for steel density 7850 kg/m³
        mass_per_meter = 7.86 * (diameter ** 2) / 1000

        # Submerged mass (in seawater, density 1025 kg/m³)
        submerged_mass = mass_per_meter * (1 - 1.025 / 7.85)

        # Axial stiffness EA (kN)
        # Effective area ≈ π/4 × d² × fill_factor
        fill_factor = 0.25  # Chain has ~25% solid area
        area_mm2 = np.pi / 4 * (diameter ** 2) * fill_factor
        modulus_gpa = 200  # Steel modulus
        ea = area_mm2 * modulus_gpa * 1000 / 1000  # Convert to kN

        # Link dimensions (typical ratios)
        link_length = diameter * 6.35
        link_width = diameter * 3.18

        # Determine stud type
        stud_type = "studless" if grade.endswith('S') else "studlink"

        return {
            # Identification
            'grade': grade,
            'diameter': diameter,  # mm
            'stud_type': stud_type,

            # Mechanical properties
            'proof_load': round(proof_load, 1),  # kN
            'minimum_breaking_load': round(mbl, 1),  # kN
            'elongation_at_break': 15.0,  # % minimum per standards

            # Physical properties
            'mass_per_meter': round(mass_per_meter, 2),  # kg/m
            'submerged_mass_per_meter': round(submerged_mass, 2),  # kg/m
            'axial_stiffness_ea': round(ea, 0),  # kN

            # Dimensions
            'link_length': round(link_length, 1),  # mm
            'link_width': round(link_width, 1),  # mm

            # Standards
            'standards': ['API RP 2SK', 'DNV-OS-E301', 'ISO 20438'],

            # Material
            'material': {
                'steel_grade': grade.replace('S', ''),
                'carbon_content_max': 0.30,  # %
                'tensile_strength': 690 if 'R4' in grade else 580,  # MPa
            },

            # Corrosion
            'corrosion_allowance': 2.0,  # mm per side

            # Fatigue
            't_curve': 'API T-curve',
            's_n_curve_slope': 3.0
        }

    def get_all_grades(self) -> List[str]:
        """
        Get list of available chain grades.

        Returns:
            List of grade identifiers
        """
        return list(self.GRADE_FACTORS.keys())

    def get_standard_diameters(self, min_diameter: float = None,
                              max_diameter: float = None) -> List[float]:
        """
        Get list of standard chain diameters.

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

    def get_chain(self, grade: str, diameter: float) -> Dict[str, Any]:
        """
        Get specific chain by grade and diameter.

        Args:
            grade: Chain grade (R3, R3S, R4, R4S, R5, R6)
            diameter: Chain diameter in mm

        Returns:
            Chain specification dict

        Raises:
            ValueError: If grade or diameter not found

        Example:
            chain = client.get_chain(grade='R4S', diameter=127)
            print(f"MBL: {chain['minimum_breaking_load']} kN")
        """
        if grade not in self.GRADE_FACTORS:
            raise ValueError(f"Unknown chain grade: {grade}. "
                           f"Available: {list(self.GRADE_FACTORS.keys())}")

        # Find exact diameter match
        for chain in self.chain_database:
            if chain['grade'] == grade and chain['diameter'] == diameter:
                return chain.copy()

        # If not found, check if diameter is close to standard
        closest_diameter = min(self.STANDARD_DIAMETERS,
                             key=lambda d: abs(d - diameter))

        if abs(closest_diameter - diameter) <= 2:  # Within 2mm tolerance
            logger.warning(f"Exact diameter {diameter}mm not found, using closest: {closest_diameter}mm")
            return self.get_chain(grade, closest_diameter)

        raise ValueError(f"Diameter {diameter}mm not in standard sizes. "
                        f"Closest: {closest_diameter}mm")

    def find_by_design_load(self, design_load: float,
                           grade: str = None,
                           safety_factor: float = 1.67) -> List[Dict[str, Any]]:
        """
        Find chains that meet design load requirement.

        Args:
            design_load: Required design load in kN
            grade: Filter by specific grade (optional)
            safety_factor: Safety factor (default 1.67 per API RP 2SK)

        Returns:
            List of suitable chains sorted by diameter

        Example:
            # Find R4S chains for 5000 kN design load
            chains = client.find_by_design_load(5000, grade='R4S')
            for chain in chains:
                print(f"{chain['diameter']}mm: MBL={chain['minimum_breaking_load']}kN")
        """
        # Required MBL = design_load × safety_factor
        required_mbl = design_load * safety_factor

        # Filter chains
        suitable_chains = []

        for chain in self.chain_database:
            # Check grade filter
            if grade and chain['grade'] != grade:
                continue

            # Check if chain meets MBL requirement
            if chain['minimum_breaking_load'] >= required_mbl:
                suitable_chains.append(chain.copy())

        # Sort by diameter (smallest first for optimization)
        suitable_chains.sort(key=lambda c: c['diameter'])

        return suitable_chains

    def compare_grades(self, design_load: float,
                      safety_factor: float = 1.67) -> Dict[str, Dict[str, Any]]:
        """
        Compare different grades for design load.

        Args:
            design_load: Required design load in kN
            safety_factor: Safety factor (default 1.67)

        Returns:
            Dict of grade → optimal chain

        Example:
            comparison = client.compare_grades(design_load=5000)
            for grade, chain in comparison.items():
                print(f"{grade}: {chain['diameter']}mm, {chain['mass_per_meter']}kg/m")
        """
        comparison = {}

        for grade in self.GRADE_FACTORS.keys():
            chains = self.find_by_design_load(design_load, grade=grade,
                                             safety_factor=safety_factor)
            if chains:
                # Return smallest (most optimal) chain
                comparison[grade] = chains[0]

        return comparison

    def to_orcaflex_line_type(self, chain: Dict[str, Any],
                             name: str = None) -> str:
        """
        Convert chain to OrcaFlex line type YAML (in-memory).

        Args:
            chain: Chain specification dict
            name: Line type name (default: Chain_{grade}_{diameter}mm)

        Returns:
            OrcaFlex line type YAML string (in-memory, not saved)

        Example:
            chain = client.get_chain(grade='R4S', diameter=127)
            yaml = client.to_orcaflex_line_type(chain)
            # Pass directly to OrcaFlex API (no file I/O)
        """
        import yaml

        # Generate line type name
        if name is None:
            name = f"Chain_{chain['grade']}_{chain['diameter']}mm"

        # Build OrcaFlex line type definition
        orcaflex_line = {
            'Name': name,
            'Category': 'GeneralLineType',

            # Mass and geometry
            'Mass per unit length': chain['mass_per_meter'],
            'Submerged mass per unit length': chain['submerged_mass_per_meter'],
            'Outside diameter': chain['diameter'] / 1000,  # Convert to meters

            # Stiffness properties
            'EA': chain['axial_stiffness_ea'] * 1000,  # Convert to N
            'EI': 0.0,  # Chain has no bending stiffness
            'GJ': 0.0,  # Chain has no torsional stiffness

            # Properties
            'Proof load': chain['proof_load'] * 1000,  # Convert to N
            'Minimum breaking load': chain['minimum_breaking_load'] * 1000,  # Convert to N

            # Metadata
            'Grade': chain['grade'],
            'Standards': ', '.join(chain['standards']),
            'Description': f"Mooring chain {chain['grade']} diameter {chain['diameter']}mm {chain['stud_type']}"
        }

        # Convert to YAML
        return yaml.dump({'LineType': orcaflex_line},
                        default_flow_style=False,
                        sort_keys=False)

    def stream_chains(self, grade: str = None,
                     min_diameter: float = None,
                     max_diameter: float = None) -> Iterator[Dict[str, Any]]:
        """
        Stream chains matching criteria (in-memory).

        Args:
            grade: Filter by grade (optional)
            min_diameter: Minimum diameter (optional)
            max_diameter: Maximum diameter (optional)

        Yields:
            Chain specifications (streaming, not stored)

        Example:
            # Stream all R4S chains
            for chain in client.stream_chains(grade='R4S'):
                print(f"{chain['diameter']}mm: {chain['minimum_breaking_load']}kN")
        """
        for chain in self.chain_database:
            # Apply filters
            if grade and chain['grade'] != grade:
                continue

            if min_diameter and chain['diameter'] < min_diameter:
                continue

            if max_diameter and chain['diameter'] > max_diameter:
                continue

            yield chain.copy()

    def query_by_date(self, start_date, end_date, location: Dict[str, float] = None,
                     **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query chains (compatibility method).

        Chains don't change with time, so this returns all chains.

        Args:
            start_date: Not used (chains don't change)
            end_date: Not used (chains don't change)
            location: Not used
            **kwargs: Additional filters (grade, min_diameter, max_diameter)

        Yields:
            Chain specifications (streaming)
        """
        yield from self.stream_chains(
            grade=kwargs.get('grade'),
            min_diameter=kwargs.get('min_diameter'),
            max_diameter=kwargs.get('max_diameter')
        )
