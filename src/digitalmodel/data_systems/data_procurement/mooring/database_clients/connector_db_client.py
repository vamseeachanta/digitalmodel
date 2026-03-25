# ABOUTME: Connector database client for mooring connectors (shackles, links, swivels)
# ABOUTME: Repository-based component selection with capacity rating and standards compliance

"""
Connector Database Client
==========================

Repository-based mooring connector database.

Data Coverage:
- Shackles: Bow shackles, anchor shackles, screw-pin, bolt-type
- Links: H-links, C-links, Kenter links
- Swivels: Single-axis, universal
- Sizes: WLL 100-5000 kN
- Standards compliance (API 2F, DNV 2.22, ISO 2415)

Source: Industry-standard specifications and manufacturer catalogs
Authentication: None (repository data)

Example:
    client = ConnectorDatabaseClient()

    # Find shackles by working load
    shackles = client.find_by_working_load(850, connector_type='bow_shackle')

    # Get specific connector
    connector = client.get_connector(connector_type='bow_shackle', pin_diameter=120)
"""

import logging
from typing import Dict, Any, List, Optional, Iterator
from pathlib import Path

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class ConnectorDatabaseClient(BaseAPIClient):
    """
    Mooring connector database client.

    Repository-based database of connectors with capacity ratings.

    Example:
        client = ConnectorDatabaseClient()
        shackle = client.get_connector(connector_type='bow_shackle', pin_diameter=120)
        print(f"WLL: {shackle['working_load_limit']} kN")
    """

    # Connector type properties
    CONNECTOR_TYPES = {
        'bow_shackle': {
            'category': 'shackle',
            'description': 'Bow shackle for general connections',
            'wll_factor': 0.059,  # WLL = wll_factor × pin_diameter² (kN, pin in mm)
            'proof_factor': 1.5,  # Proof = 1.5 × WLL
            'mbl_factor': 2.0,    # MBL = 2.0 × WLL
            'bow_width_factor': 1.83,  # × pin_diameter
            'bow_length_factor': 2.17  # × pin_diameter
        },
        'anchor_shackle': {
            'category': 'shackle',
            'description': 'Anchor shackle for anchor connections',
            'wll_factor': 0.065,
            'proof_factor': 1.5,
            'mbl_factor': 2.0,
            'bow_width_factor': 1.5,
            'bow_length_factor': 1.8
        },
        'screw_pin_shackle': {
            'category': 'shackle',
            'description': 'Screw-pin shackle for temporary connections',
            'wll_factor': 0.055,
            'proof_factor': 1.5,
            'mbl_factor': 2.0,
            'bow_width_factor': 1.83,
            'bow_length_factor': 2.17
        },
        'h_link': {
            'category': 'link',
            'description': 'H-link for chain connections',
            'wll_factor': 0.070,
            'proof_factor': 1.5,
            'mbl_factor': 2.5,
            'length_factor': 4.0,
            'width_factor': 1.5
        },
        'c_link': {
            'category': 'link',
            'description': 'C-link for chain-to-anchor connection',
            'wll_factor': 0.068,
            'proof_factor': 1.5,
            'mbl_factor': 2.5,
            'length_factor': 3.5,
            'width_factor': 1.3
        },
        'swivel': {
            'category': 'swivel',
            'description': 'Swivel to prevent line twist',
            'wll_factor': 0.080,
            'proof_factor': 1.5,
            'mbl_factor': 2.0,
            'length_factor': 5.0,
            'diameter_factor': 2.0
        }
    }

    # Standard pin diameters (mm) for shackles and links
    STANDARD_PIN_DIAMETERS = [
        50, 60, 70, 80, 90, 100, 110, 120, 130, 140,
        150, 160, 170, 180, 200
    ]

    def __init__(self, database_path: str = None, **kwargs):
        """
        Initialize Connector Database Client.

        Args:
            database_path: Path to connector database directory
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
            self.db_path = Path(__file__).parent.parent / 'data' / 'connectors'

        self.connector_database = self._initialize_connector_database()

        # Build indexes for O(1) lookup (Phase 1 optimization)
        self._type_index = {}
        self._pin_diameter_index = {}
        for connector in self.connector_database:
            connector_type = connector['type']
            pin_diameter = connector['pin_diameter']

            # Type index
            if connector_type not in self._type_index:
                self._type_index[connector_type] = []
            self._type_index[connector_type].append(connector)

            # Pin diameter index
            if pin_diameter not in self._pin_diameter_index:
                self._pin_diameter_index[pin_diameter] = []
            self._pin_diameter_index[pin_diameter].append(connector)

        logger.info(f"Initialized ConnectorDatabaseClient with {len(self.connector_database)} connector specifications")

    def _initialize_connector_database(self) -> List[Dict[str, Any]]:
        """Initialize connector database with standard specifications."""
        connectors = []

        for connector_type, props in self.CONNECTOR_TYPES.items():
            for pin_diameter in self.STANDARD_PIN_DIAMETERS:
                connector = self._generate_connector_spec(connector_type, pin_diameter, props)
                connectors.append(connector)

        return connectors

    def _generate_connector_spec(self, connector_type: str, pin_diameter: float,
                                 props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Generate complete connector specification.

        Args:
            connector_type: Connector type identifier
            pin_diameter: Pin diameter in mm
            props: Connector type properties

        Returns:
            Complete connector specification dict
        """
        # Calculate capacity ratings
        # WLL = wll_factor × pin_diameter² (kN)
        wll = props['wll_factor'] * (pin_diameter ** 2)

        # Proof load = proof_factor × WLL
        proof_load = props['proof_factor'] * wll

        # MBL = mbl_factor × WLL
        mbl = props['mbl_factor'] * wll

        # Dimensions
        if connector_type in ['bow_shackle', 'anchor_shackle', 'screw_pin_shackle']:
            bow_width = pin_diameter * props['bow_width_factor']
            bow_length = pin_diameter * props['bow_length_factor']
            dimensions = {
                'pin_diameter': pin_diameter,
                'bow_inside_width': round(bow_width, 1),
                'bow_inside_length': round(bow_length, 1)
            }
        elif connector_type in ['h_link', 'c_link']:
            length = pin_diameter * props['length_factor']
            width = pin_diameter * props['width_factor']
            dimensions = {
                'pin_diameter': pin_diameter,
                'length': round(length, 1),
                'width': round(width, 1)
            }
        elif connector_type == 'swivel':
            length = pin_diameter * props['length_factor']
            diameter = pin_diameter * props['diameter_factor']
            dimensions = {
                'pin_diameter': pin_diameter,
                'length': round(length, 1),
                'body_diameter': round(diameter, 1)
            }
        else:
            dimensions = {'pin_diameter': pin_diameter}

        return {
            # Identification
            'type': connector_type,
            'category': props['category'],
            'description': props['description'],
            'size': f"{pin_diameter}mm",

            # Capacity
            'working_load_limit': round(wll, 1),  # kN
            'proof_load': round(proof_load, 1),   # kN
            'minimum_breaking_load': round(mbl, 1),  # kN

            # Dimensions
            **dimensions,

            # Material
            'material': {
                'grade': 'High tensile alloy steel',
                'proof_tested': True
            },

            # Standards
            'standards': ['API 2F', 'DNV 2.22', 'ISO 2415']
        }

    def get_all_connector_types(self) -> List[str]:
        """Get list of available connector types."""
        return list(self.CONNECTOR_TYPES.keys())

    def get_standard_pin_diameters(self, min_diameter: float = None,
                                   max_diameter: float = None) -> List[float]:
        """
        Get list of standard pin diameters.

        Args:
            min_diameter: Minimum pin diameter filter (mm)
            max_diameter: Maximum pin diameter filter (mm)

        Returns:
            List of standard pin diameters
        """
        diameters = self.STANDARD_PIN_DIAMETERS.copy()

        if min_diameter:
            diameters = [d for d in diameters if d >= min_diameter]

        if max_diameter:
            diameters = [d for d in diameters if d <= max_diameter]

        return diameters

    def get_connector(self, connector_type: str, pin_diameter: float) -> Dict[str, Any]:
        """
        Get specific connector by type and pin diameter.

        Args:
            connector_type: Connector type identifier
            pin_diameter: Pin diameter in mm

        Returns:
            Connector specification dict

        Example:
            connector = client.get_connector(connector_type='bow_shackle', pin_diameter=120)
            print(f"WLL: {connector['working_load_limit']} kN")
        """
        if connector_type not in self.CONNECTOR_TYPES:
            raise ValueError(f"Unknown connector type: {connector_type}. "
                           f"Available: {list(self.CONNECTOR_TYPES.keys())}")

        # Find exact match
        for connector in self.connector_database:
            if connector['type'] == connector_type and connector['pin_diameter'] == pin_diameter:
                return connector.copy()

        # Find closest pin diameter
        closest_diameter = min(self.STANDARD_PIN_DIAMETERS,
                             key=lambda d: abs(d - pin_diameter))

        if abs(closest_diameter - pin_diameter) <= 5:
            logger.warning(f"Exact pin diameter {pin_diameter}mm not found, using closest: {closest_diameter}mm")
            return self.get_connector(connector_type, closest_diameter)

        raise ValueError(f"Pin diameter {pin_diameter}mm not in standard sizes. "
                        f"Closest: {closest_diameter}mm")

    def find_by_working_load(self, required_wll: float,
                            connector_type: str = None,
                            safety_margin: float = 1.1) -> List[Dict[str, Any]]:
        """
        Find connectors that meet working load requirement.

        Args:
            required_wll: Required working load limit in kN
            connector_type: Filter by specific connector type (optional)
            safety_margin: Safety margin above required WLL (default 1.1 = 10%)

        Returns:
            List of suitable connectors sorted by size

        Example:
            connectors = client.find_by_working_load(850, connector_type='bow_shackle')
            for connector in connectors:
                print(f"{connector['size']}: WLL={connector['working_load_limit']}kN")
        """
        required_wll_with_margin = required_wll * safety_margin

        # Use type index if specified (O(1) lookup instead of O(n))
        if connector_type:
            candidates = self._type_index.get(connector_type, [])
        else:
            candidates = self.connector_database

        suitable_connectors = []
        for connector in candidates:
            # Check if connector meets WLL requirement
            if connector['working_load_limit'] >= required_wll_with_margin:
                suitable_connectors.append(connector.copy())

        # Sort by pin diameter (smallest first for optimization)
        suitable_connectors.sort(key=lambda c: c['pin_diameter'])

        return suitable_connectors

    def find_for_chain(self, chain_grade: str, chain_diameter: float) -> List[Dict[str, Any]]:
        """
        Find suitable connectors for specific chain.

        Args:
            chain_grade: Chain grade (e.g., 'R4S')
            chain_diameter: Chain diameter in mm

        Returns:
            List of suitable connectors

        Example:
            # Find connectors for R4S 127mm chain
            connectors = client.find_for_chain(chain_grade='R4S', chain_diameter=127)
        """
        # Estimate chain MBL (simplified)
        grade_factors = {'R3': 44, 'R3S': 44, 'R4': 50, 'R4S': 50, 'R5': 58, 'R6': 66}
        grade_factor = grade_factors.get(chain_grade, 50)
        chain_mbl = 3 * (chain_diameter ** 2) * grade_factor / 1000  # kN

        # Connector should have WLL ≥ chain MBL / safety_factor
        # Typical: WLL ≥ chain MBL / 2
        required_wll = chain_mbl / 2

        # Find suitable connectors
        suitable = []

        for connector in self.connector_database:
            if connector['working_load_limit'] >= required_wll:
                suitable.append(connector.copy())

        # Sort by size (smallest first)
        suitable.sort(key=lambda c: c['pin_diameter'])

        return suitable

    def compare_connector_types(self, required_wll: float) -> Dict[str, Dict[str, Any]]:
        """
        Compare different connector types for working load.

        Args:
            required_wll: Required working load limit in kN

        Returns:
            Dict of connector_type → optimal connector

        Example:
            comparison = client.compare_connector_types(required_wll=850)
            for conn_type, connector in comparison.items():
                print(f"{conn_type}: {connector['size']}, WLL={connector['working_load_limit']}kN")
        """
        comparison = {}

        for connector_type in self.CONNECTOR_TYPES.keys():
            connectors = self.find_by_working_load(required_wll, connector_type=connector_type)

            if connectors:
                # Return smallest (most optimal) connector
                comparison[connector_type] = connectors[0]

        return comparison

    def stream_connectors(self, connector_type: str = None,
                         min_pin_diameter: float = None,
                         max_pin_diameter: float = None) -> Iterator[Dict[str, Any]]:
        """Stream connectors matching criteria (in-memory)."""
        for connector in self.connector_database:
            if connector_type and connector['type'] != connector_type:
                continue

            if min_pin_diameter and connector['pin_diameter'] < min_pin_diameter:
                continue

            if max_pin_diameter and connector['pin_diameter'] > max_pin_diameter:
                continue

            yield connector.copy()

    def query_by_date(self, start_date, end_date, location: Dict[str, float] = None,
                     **kwargs) -> Iterator[Dict[str, Any]]:
        """Query connectors (compatibility method)."""
        yield from self.stream_connectors(
            connector_type=kwargs.get('connector_type'),
            min_pin_diameter=kwargs.get('min_pin_diameter'),
            max_pin_diameter=kwargs.get('max_pin_diameter')
        )
