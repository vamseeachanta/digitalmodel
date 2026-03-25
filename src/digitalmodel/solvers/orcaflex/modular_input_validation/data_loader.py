"""
ABOUTME: CALM buoy reference data loader
ABOUTME: Loads and caches CSV reference data for physical consistency validation
"""

from pathlib import Path
from typing import Dict, Optional, Tuple
from dataclasses import dataclass
from functools import lru_cache
import csv


@dataclass
class ParameterRange:
    """Range specification for a parameter."""
    parameter: str
    min_value: Optional[float]
    max_value: Optional[float]
    reference_basis: str
    notes: str


@dataclass
class CALMBuoyReferenceData:
    """Container for CALM buoy reference data."""
    hull_geometry: Dict[str, ParameterRange]
    metocean: Dict[str, ParameterRange]
    mooring_capacity: Dict[str, ParameterRange]
    environmental_conditions: Dict[str, Dict]
    mooring_line_properties: Dict[str, Dict]


class CALMBuoyDataLoader:
    """
    Loader for CALM buoy reference data from CSV files.

    Implements caching to avoid repeated file reads.
    """

    def __init__(self, data_dir: Path):
        """
        Initialize data loader.

        Args:
            data_dir: Root directory for CALM buoy data
        """
        self.data_dir = Path(data_dir)
        self._cache = {}

    def load_all(self) -> CALMBuoyReferenceData:
        """
        Load all CALM buoy reference data.

        Returns:
            Complete reference data container
        """
        return CALMBuoyReferenceData(
            hull_geometry=self.load_hull_geometry_ranges(),
            metocean=self.load_metocean_ranges(),
            mooring_capacity=self.load_mooring_capacity_ranges(),
            environmental_conditions=self.load_environmental_conditions(),
            mooring_line_properties=self.load_mooring_line_properties()
        )

    @lru_cache(maxsize=10)
    def load_hull_geometry_ranges(self) -> Dict[str, ParameterRange]:
        """
        Load hull geometry parameter ranges.

        Returns:
            Dictionary of parameter name to ParameterRange
        """
        file_path = self.data_dir / 'raw' / 'calm_buoy' / 'generic_range' / 'hull_geometry_ranges.csv'
        return self._load_ranges_csv(file_path)

    @lru_cache(maxsize=10)
    def load_metocean_ranges(self) -> Dict[str, ParameterRange]:
        """
        Load metocean design parameter ranges.

        Returns:
            Dictionary of condition name to ParameterRange with multiple parameters
        """
        file_path = self.data_dir / 'raw' / 'calm_buoy' / 'generic_range' / 'metocean_design_ranges.csv'

        ranges = {}
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    condition = row['condition']

                    # Create ranges for each metocean parameter
                    for param_type in ['hs', 'tp', 'surface_current', 'wind_speed']:
                        param_name = f"{condition}_{param_type}"
                        min_col = f"{param_type}_min"
                        max_col = f"{param_type}_max"

                        if min_col in row and max_col in row:
                            ranges[param_name] = ParameterRange(
                                parameter=param_name,
                                min_value=float(row[min_col]) if row[min_col] else None,
                                max_value=float(row[max_col]) if row[max_col] else None,
                                reference_basis=row.get('reference_basis', ''),
                                notes=f"{condition} condition"
                            )
        except FileNotFoundError:
            # Return empty dict if file doesn't exist (graceful degradation)
            pass
        except Exception as e:
            print(f"Warning: Error loading metocean ranges: {e}")

        return ranges

    @lru_cache(maxsize=10)
    def load_mooring_capacity_ranges(self) -> Dict[str, ParameterRange]:
        """
        Load mooring component capacity ranges.

        Returns:
            Dictionary of component name to ParameterRange
        """
        file_path = self.data_dir / 'raw' / 'calm_buoy' / 'generic_range' / 'mooring_capacity_ranges.csv'

        ranges = {}
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    component = row['component']

                    # Capacity range
                    capacity_param = f"{component}_capacity"
                    ranges[capacity_param] = ParameterRange(
                        parameter=capacity_param,
                        min_value=float(row['min_capacity']) if row.get('min_capacity') else None,
                        max_value=float(row['max_capacity']) if row.get('max_capacity') else None,
                        reference_basis=row.get('reference_basis', ''),
                        notes=row.get('notes', '')
                    )

                    # Safety factor range
                    sf_param = f"{component}_safety_factor"
                    ranges[sf_param] = ParameterRange(
                        parameter=sf_param,
                        min_value=float(row['safety_factor_min']) if row.get('safety_factor_min') else None,
                        max_value=float(row['safety_factor_max']) if row.get('safety_factor_max') else None,
                        reference_basis=row.get('reference_basis', ''),
                        notes=f"Safety factor for {component}"
                    )
        except FileNotFoundError:
            pass
        except Exception as e:
            print(f"Warning: Error loading mooring capacity ranges: {e}")

        return ranges

    def load_environmental_conditions(self) -> Dict[str, Dict]:
        """
        Load project-specific environmental conditions.

        Returns:
            Dictionary of sea state ID to environmental parameters
        """
        file_path = self.data_dir / 'results' / 'calm_buoy' / 'project_specific' / 'environmental_conditions.csv'

        conditions = {}
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    sea_state_id = row.get('sea_state_id', '')
                    conditions[sea_state_id] = {
                        'hs': float(row['hs']) if row.get('hs') else None,
                        'tp': float(row['tp']) if row.get('tp') else None,
                        'wave_direction': float(row['wave_direction']) if row.get('wave_direction') else None,
                        'wind_speed': float(row['wind_speed']) if row.get('wind_speed') else None,
                        'wind_direction': float(row['wind_direction']) if row.get('wind_direction') else None,
                        'surface_current_speed': float(row['surface_current_speed']) if row.get('surface_current_speed') else None,
                        'bottom_current_speed': float(row['bottom_current_speed']) if row.get('bottom_current_speed') else None,
                        'return_period': row.get('return_period', ''),
                        'probability': float(row['probability']) if row.get('probability') else None
                    }
        except FileNotFoundError:
            pass
        except Exception as e:
            print(f"Warning: Error loading environmental conditions: {e}")

        return conditions

    def load_mooring_line_properties(self) -> Dict[str, Dict]:
        """
        Load project-specific mooring line properties.

        Returns:
            Dictionary of line ID to properties
        """
        file_path = self.data_dir / 'results' / 'calm_buoy' / 'project_specific' / 'mooring_line_properties.csv'

        lines = {}
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    line_id = row.get('line_id', '')
                    segment = row.get('segment', '')
                    key = f"{line_id}_{segment}" if segment else line_id

                    lines[key] = {
                        'length': float(row['length']) if row.get('length') else None,
                        'diameter': float(row['diameter']) if row.get('diameter') else None,
                        'ea': float(row['ea']) if row.get('ea') else None,
                        'mbl': float(row['mbl']) if row.get('mbl') else None,
                        'submerged_weight': float(row['submerged_weight']) if row.get('submerged_weight') else None,
                        'fairlead_angle': float(row['fairlead_angle']) if row.get('fairlead_angle') else None,
                        'touchdown_distance': float(row['touchdown_distance']) if row.get('touchdown_distance') else None
                    }
        except FileNotFoundError:
            pass
        except Exception as e:
            print(f"Warning: Error loading mooring line properties: {e}")

        return lines

    def _load_ranges_csv(self, file_path: Path) -> Dict[str, ParameterRange]:
        """
        Load parameter ranges from CSV file.

        Args:
            file_path: Path to CSV file

        Returns:
            Dictionary of parameter name to ParameterRange
        """
        ranges = {}

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    param = row.get('parameter', '')
                    if not param:
                        continue

                    ranges[param] = ParameterRange(
                        parameter=param,
                        min_value=float(row['min_value']) if row.get('min_value') else None,
                        max_value=float(row['max_value']) if row.get('max_value') else None,
                        reference_basis=row.get('reference_basis', ''),
                        notes=row.get('notes', '')
                    )
        except FileNotFoundError:
            # Gracefully handle missing files
            print(f"Warning: Reference data file not found: {file_path}")
        except Exception as e:
            print(f"Warning: Error loading ranges from {file_path}: {e}")

        return ranges

    def get_parameter_range(self, parameter: str) -> Optional[ParameterRange]:
        """
        Get range for a specific parameter across all data sources.

        Args:
            parameter: Parameter name

        Returns:
            ParameterRange if found, None otherwise
        """
        # Check all range sources
        for ranges in [
            self.load_hull_geometry_ranges(),
            self.load_metocean_ranges(),
            self.load_mooring_capacity_ranges()
        ]:
            if parameter in ranges:
                return ranges[parameter]

        return None
