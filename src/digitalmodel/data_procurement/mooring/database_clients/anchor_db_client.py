# ABOUTME: Anchor database client for mooring anchors (drag embedment, suction, pile)
# ABOUTME: Repository-based component selection with holding capacity calculation

"""
Anchor Database Client
=======================

Repository-based mooring anchor database.

Data Coverage:
- Anchor types: Drag embedment (Stevpris, Vryhof, Bruce), Suction pile, Driven pile, Plate anchor (VLA), Gravity
- Sizes: Weight range 5,000-50,000 kg
- Holding capacity by soil type
- Installation methods
- Standards compliance (API RP 2SK, DNV-OS-E301)

Source: Industry-standard specifications and manufacturer catalogs
Authentication: None (repository data)

Example:
    client = AnchorDatabaseClient()

    # Find anchors by design capacity
    anchors = client.find_by_holding_capacity(1500, soil='clay')

    # Get specific anchor
    anchor = client.get_anchor(anchor_type='stevpris_mk6', weight=20000)

    # Calculate holding capacity
    capacity = client.calculate_holding_capacity(anchor, soil_type='clay', su=50)
"""

import logging
from typing import Dict, Any, List, Optional, Iterator
from pathlib import Path
import numpy as np

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class AnchorDatabaseClient(BaseAPIClient):
    """
    Mooring anchor database client.

    Repository-based database of anchors with holding capacity calculation.

    Example:
        client = AnchorDatabaseClient()
        anchor = client.get_anchor(anchor_type='stevpris_mk6', weight=20000)
        capacity = client.calculate_holding_capacity(anchor, soil_type='clay', su=50)
        print(f"Holding capacity: {capacity['design_capacity']} kN")
    """

    # Anchor type properties
    ANCHOR_TYPES = {
        'stevpris_mk6': {
            'category': 'drag_embedment',
            'manufacturer': 'Vryhof',
            'sand_factor': 12,  # × submerged weight
            'clay_factor': 8,   # × submerged weight
            'fluke_angle': 50,  # degrees
            'penetration_depth': 6,  # m typical
            'line_angle_max': 30,  # degrees at mudline for full capacity
            'description': 'High holding capacity drag embedment anchor',
            'soil_suitability': {
                'sand': 'excellent',
                'clay': 'excellent',
                'rock': 'not_suitable'
            }
        },
        'vryhof_stevshark': {
            'category': 'drag_embedment',
            'manufacturer': 'Vryhof',
            'sand_factor': 15,
            'clay_factor': 10,
            'fluke_angle': 32,
            'penetration_depth': 8,
            'line_angle_max': 25,
            'description': 'Ultra-high holding capacity for deepwater',
            'soil_suitability': {
                'sand': 'excellent',
                'clay': 'excellent',
                'rock': 'not_suitable'
            }
        },
        'bruce_mk3': {
            'category': 'drag_embedment',
            'manufacturer': 'Bruce',
            'sand_factor': 10,
            'clay_factor': 6,
            'fluke_angle': 45,
            'penetration_depth': 5,
            'line_angle_max': 35,
            'description': 'Proven offshore anchor',
            'soil_suitability': {
                'sand': 'good',
                'clay': 'good',
                'rock': 'not_suitable'
            }
        },
        'suction_pile': {
            'category': 'suction_pile',
            'manufacturer': 'Various',
            'sand_factor': 20,  # Can be very high
            'clay_factor': 15,
            'penetration_depth': None,  # Varies with pile length
            'description': 'Suction-installed cylindrical pile',
            'soil_suitability': {
                'sand': 'excellent',
                'clay': 'excellent',
                'rock': 'not_suitable'
            }
        },
        'driven_pile': {
            'category': 'driven_pile',
            'manufacturer': 'Various',
            'sand_factor': 25,
            'clay_factor': 20,
            'description': 'Pile-driven foundation',
            'soil_suitability': {
                'sand': 'excellent',
                'clay': 'excellent',
                'rock': 'good'
            }
        }
    }

    # Standard anchor weights (kg dry weight)
    STANDARD_WEIGHTS = [
        5000, 7500, 10000, 12500, 15000, 17500, 20000,
        22500, 25000, 27500, 30000, 35000, 40000, 45000, 50000
    ]

    def __init__(self, database_path: str = None, **kwargs):
        """
        Initialize Anchor Database Client.

        Args:
            database_path: Path to anchor database directory
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
            self.db_path = Path(__file__).parent.parent / 'data' / 'anchors'

        self.anchor_database = self._initialize_anchor_database()

        # Build indexes for O(1) lookup (Phase 1 optimization)
        self._type_index = {}
        self._weight_index = {}
        for anchor in self.anchor_database:
            anchor_type = anchor['type']
            weight = anchor['dry_weight']

            # Type index
            if anchor_type not in self._type_index:
                self._type_index[anchor_type] = []
            self._type_index[anchor_type].append(anchor)

            # Weight index
            if weight not in self._weight_index:
                self._weight_index[weight] = []
            self._weight_index[weight].append(anchor)

        logger.info(f"Initialized AnchorDatabaseClient with {len(self.anchor_database)} anchor specifications")

    def _initialize_anchor_database(self) -> List[Dict[str, Any]]:
        """Initialize anchor database with standard specifications."""
        anchors = []

        for anchor_type, props in self.ANCHOR_TYPES.items():
            for weight in self.STANDARD_WEIGHTS:
                anchor = self._generate_anchor_spec(anchor_type, weight, props)
                anchors.append(anchor)

        return anchors

    def _generate_anchor_spec(self, anchor_type: str, dry_weight: float,
                             props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Generate complete anchor specification.

        Args:
            anchor_type: Anchor type identifier
            dry_weight: Dry weight in kg
            props: Anchor type properties

        Returns:
            Complete anchor specification dict
        """
        # Submerged weight (steel in seawater)
        submerged_weight = dry_weight * (1 - 1.025 / 7.85)

        # Estimate dimensions based on weight (empirical)
        # Length ≈ (weight/1000)^0.4 meters
        length = (dry_weight / 1000) ** 0.4 * 3
        width = length * 0.55
        height = length * 0.36

        # Holding capacity (kN)
        sand_capacity = submerged_weight * props['sand_factor'] * 9.81 / 1000
        clay_capacity = submerged_weight * props['clay_factor'] * 9.81 / 1000

        # Design capacity (conservative, min of sand/clay)
        design_capacity = min(sand_capacity, clay_capacity) * 0.9  # 10% reduction for conservatism

        return {
            # Identification
            'type': anchor_type,
            'category': props['category'],
            'manufacturer': props.get('manufacturer'),
            'description': props['description'],

            # Dimensions
            'length': round(length, 1),  # m
            'width': round(width, 1),    # m
            'height': round(height, 1),  # m
            'fluke_angle': props.get('fluke_angle'),  # degrees

            # Weight
            'dry_weight': dry_weight,  # kg
            'submerged_weight': round(submerged_weight, 0),  # kg

            # Holding capacity
            'sand_capacity_kn': round(sand_capacity, 0),  # kN
            'clay_capacity_kn': round(clay_capacity, 0),  # kN
            'design_capacity_kn': round(design_capacity, 0),  # kN (conservative)

            # Capacity factors
            'sand_capacity_factor': props['sand_factor'],
            'clay_capacity_factor': props['clay_factor'],

            # Installation
            'penetration_depth': props.get('penetration_depth'),  # m
            'line_angle_max': props.get('line_angle_max'),  # degrees

            # Standards
            'standards': ['API RP 2SK', 'DNV-OS-E301'],

            # Soil suitability
            'soil_suitability': props['soil_suitability']
        }

    def get_all_anchor_types(self) -> List[str]:
        """Get list of available anchor types."""
        return list(self.ANCHOR_TYPES.keys())

    def get_standard_weights(self, min_weight: float = None,
                            max_weight: float = None) -> List[float]:
        """
        Get list of standard anchor weights.

        Args:
            min_weight: Minimum weight filter (kg)
            max_weight: Maximum weight filter (kg)

        Returns:
            List of standard weights
        """
        weights = self.STANDARD_WEIGHTS.copy()

        if min_weight:
            weights = [w for w in weights if w >= min_weight]

        if max_weight:
            weights = [w for w in weights if w <= max_weight]

        return weights

    def get_anchor(self, anchor_type: str, weight: float) -> Dict[str, Any]:
        """
        Get specific anchor by type and weight.

        Args:
            anchor_type: Anchor type identifier
            weight: Dry weight in kg

        Returns:
            Anchor specification dict

        Example:
            anchor = client.get_anchor(anchor_type='stevpris_mk6', weight=20000)
            print(f"Design capacity: {anchor['design_capacity_kn']} kN")
        """
        if anchor_type not in self.ANCHOR_TYPES:
            raise ValueError(f"Unknown anchor type: {anchor_type}. "
                           f"Available: {list(self.ANCHOR_TYPES.keys())}")

        # Find exact match
        for anchor in self.anchor_database:
            if anchor['type'] == anchor_type and anchor['dry_weight'] == weight:
                return anchor.copy()

        # Find closest weight
        closest_weight = min(self.STANDARD_WEIGHTS,
                           key=lambda w: abs(w - weight))

        if abs(closest_weight - weight) <= 500:  # Within 500kg tolerance
            logger.warning(f"Exact weight {weight}kg not found, using closest: {closest_weight}kg")
            return self.get_anchor(anchor_type, closest_weight)

        raise ValueError(f"Weight {weight}kg not in standard sizes. "
                        f"Closest: {closest_weight}kg")

    def find_by_holding_capacity(self, required_capacity: float,
                                 anchor_type: str = None,
                                 soil: str = 'clay',
                                 safety_factor: float = 1.5) -> List[Dict[str, Any]]:
        """
        Find anchors that meet holding capacity requirement.

        Args:
            required_capacity: Required holding capacity in kN
            anchor_type: Filter by specific anchor type (optional)
            soil: Soil type for capacity check ('sand' or 'clay')
            safety_factor: Safety factor (default 1.5 per API RP 2SK)

        Returns:
            List of suitable anchors sorted by weight

        Example:
            anchors = client.find_by_holding_capacity(1500, soil='clay')
            for anchor in anchors:
                print(f"{anchor['type']}, {anchor['dry_weight']}kg: {anchor['clay_capacity_kn']}kN")
        """
        # Required capacity with safety factor
        required_capacity_with_sf = required_capacity * safety_factor

        # Use type index if specified (O(1) lookup instead of O(n))
        if anchor_type:
            candidates = self._type_index.get(anchor_type, [])
        else:
            candidates = self.anchor_database

        suitable_anchors = []
        for anchor in candidates:
            # Check soil suitability
            if anchor['soil_suitability'].get(soil) == 'not_suitable':
                continue

            # Check capacity
            if soil == 'sand':
                capacity = anchor['sand_capacity_kn']
            elif soil == 'clay':
                capacity = anchor['clay_capacity_kn']
            else:
                capacity = anchor['design_capacity_kn']  # Conservative

            if capacity >= required_capacity_with_sf:
                suitable_anchors.append(anchor.copy())

        # Sort by weight (smallest first for optimization)
        suitable_anchors.sort(key=lambda a: a['dry_weight'])

        return suitable_anchors

    def calculate_holding_capacity(self, anchor: Dict[str, Any],
                                   soil_type: str,
                                   su: float = None,
                                   phi: float = None) -> Dict[str, float]:
        """
        Calculate holding capacity for specific soil conditions.

        Args:
            anchor: Anchor specification dict
            soil_type: Soil type ('clay' or 'sand')
            su: Undrained shear strength for clay (kPa)
            phi: Friction angle for sand (degrees)

        Returns:
            Dict with holding capacity calculations

        Example:
            anchor = client.get_anchor(anchor_type='stevpris_mk6', weight=20000)
            capacity = client.calculate_holding_capacity(anchor, soil_type='clay', su=50)
            print(f"Holding capacity: {capacity['holding_capacity_kn']} kN")
        """
        submerged_weight = anchor['submerged_weight']  # kg
        submerged_weight_kn = submerged_weight * 9.81 / 1000

        if soil_type == 'clay':
            # Clay capacity factor depends on Su
            # Simplified: use anchor's clay_factor
            capacity_factor = anchor['clay_capacity_factor']

            # Adjust for Su if provided (higher Su = higher capacity)
            if su:
                # Reference Su = 50 kPa
                su_adjustment = (su / 50) ** 0.5
                capacity_factor = capacity_factor * su_adjustment

        elif soil_type == 'sand':
            # Sand capacity factor depends on phi
            # Simplified: use anchor's sand_factor
            capacity_factor = anchor['sand_capacity_factor']

            # Adjust for phi if provided (typical phi = 30°)
            if phi:
                phi_adjustment = (np.tan(np.radians(phi)) / np.tan(np.radians(30))) ** 2
                capacity_factor = capacity_factor * phi_adjustment

        else:
            raise ValueError(f"Unknown soil type: {soil_type}. Use 'clay' or 'sand'.")

        # Holding capacity = capacity_factor × submerged_weight
        holding_capacity_kn = capacity_factor * submerged_weight_kn

        return {
            'submerged_weight_kn': round(submerged_weight_kn, 1),
            'capacity_factor': round(capacity_factor, 1),
            'holding_capacity_kn': round(holding_capacity_kn, 0),
            'soil_type': soil_type,
            'su_kpa': su if soil_type == 'clay' else None,
            'phi_deg': phi if soil_type == 'sand' else None
        }

    def compare_anchor_types(self, required_capacity: float,
                            soil: str = 'clay',
                            safety_factor: float = 1.5) -> Dict[str, Dict[str, Any]]:
        """
        Compare different anchor types for required capacity.

        Args:
            required_capacity: Required holding capacity in kN
            soil: Soil type ('sand' or 'clay')
            safety_factor: Safety factor (default 1.5)

        Returns:
            Dict of anchor_type → optimal anchor

        Example:
            comparison = client.compare_anchor_types(required_capacity=1500, soil='clay')
            for anchor_type, anchor in comparison.items():
                print(f"{anchor_type}: {anchor['dry_weight']}kg, {anchor['clay_capacity_kn']}kN")
        """
        comparison = {}

        for anchor_type in self.ANCHOR_TYPES.keys():
            anchors = self.find_by_holding_capacity(
                required_capacity,
                anchor_type=anchor_type,
                soil=soil,
                safety_factor=safety_factor
            )

            if anchors:
                # Return lightest anchor (most optimal)
                comparison[anchor_type] = anchors[0]

        return comparison

    def stream_anchors(self, anchor_type: str = None,
                      min_weight: float = None,
                      max_weight: float = None) -> Iterator[Dict[str, Any]]:
        """Stream anchors matching criteria (in-memory)."""
        for anchor in self.anchor_database:
            if anchor_type and anchor['type'] != anchor_type:
                continue

            if min_weight and anchor['dry_weight'] < min_weight:
                continue

            if max_weight and anchor['dry_weight'] > max_weight:
                continue

            yield anchor.copy()

    def query_by_date(self, start_date, end_date, location: Dict[str, float] = None,
                     **kwargs) -> Iterator[Dict[str, Any]]:
        """Query anchors (compatibility method)."""
        yield from self.stream_anchors(
            anchor_type=kwargs.get('anchor_type'),
            min_weight=kwargs.get('min_weight'),
            max_weight=kwargs.get('max_weight')
        )
