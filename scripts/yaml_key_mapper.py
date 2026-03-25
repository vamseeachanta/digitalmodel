#!/usr/bin/env python3
"""
ABOUTME: Flexible YAML key mapper - handles both human-friendly and technical keys
ABOUTME: Provides bidirectional mapping between readable labels and technical keys
"""

from typing import Dict, Any, Optional, List
import re


class YAMLKeyMapper:
    """
    Maps between human-friendly keys and technical keys for YAML configuration.

    Supports multiple formats:
    - Human-friendly: "Project Information", "Buoy Specifications"
    - Technical: "project_information", "buoy_specifications"
    - Code-style: "project_info", "buoy_specs"
    """

    # Primary mapping: Technical key -> Human-friendly label
    KEY_MAPPINGS = {
        # Top-level sections
        'human_input': 'Human Input',
        'ai_generated': 'AI Generated',
        'generation': 'Generation Settings',
        'multi_fidelity': 'Multi-Fidelity Analysis',

        # Human Input subsections
        'project': 'Project Information',
        'standards': 'Standards & Codes',
        'site': 'Site Conditions',
        'buoy': 'Buoy Specifications',
        'mooring': 'Mooring System',
        'offloading': 'Offloading System',
        'analysis': 'Analysis Settings',

        # Project fields
        'name': 'Name',
        'code': 'Code',
        'client': 'Client',
        'operator': 'Operator',
        'location': 'Location',
        'latitude': 'Latitude',
        'longitude': 'Longitude',
        'created_date': 'Created Date',
        'engineer': 'Engineer',
        'reviewer': 'Reviewer',
        'version': 'Version',
        'description': 'Description',

        # Standards fields
        'primary_code': 'Primary Code',
        'mooring_standard': 'Mooring Standard',
        'buoy_standard': 'Buoy Standard',
        'classification_society': 'Classification Society',
        'national_regulations': 'National Regulations',
        'industry_guidelines': 'Industry Guidelines',

        # Site fields
        'water_depth': 'Water Depth',
        'seabed_type': 'Seabed Type',
        'seabed_slope': 'Seabed Slope',
        'seabed_friction_coefficient': 'Seabed Friction Coefficient',
        'metocean_source': 'Metocean Data Source',
        'metocean_override': 'Metocean Conditions',
        'operating_conditions': 'Operating',
        'design_1yr_conditions': 'Design (1-year)',
        'design_10yr_conditions': 'Design (10-year)',
        'extreme_conditions': 'Extreme (100-year)',

        # Operating condition fields
        'hs_max': 'Significant Wave Height (Hs)',
        'tp_max': 'Peak Period (Tp)',
        'wind_speed_max': 'Wind Speed',
        'current_speed_surface': 'Surface Current',
        'current_speed_seabed': 'Seabed Current',

        # 1-year return period fields
        'hs_1yr': 'Significant Wave Height (Hs)',
        'tp_1yr': 'Peak Period (Tp)',
        'tz_1yr': 'Zero-Crossing Period (Tz)',
        'wind_speed_1yr': 'Wind Speed (1-hour mean)',
        'wind_gust_1yr': 'Wind Gust (3-second)',
        'current_speed_surface_1yr': 'Surface Current',
        'current_speed_mid_1yr': 'Mid-Depth Current',
        'current_speed_seabed_1yr': 'Near-Bed Current',

        # 10-year return period fields
        'hs_10yr': 'Significant Wave Height (Hs)',
        'tp_10yr': 'Peak Period (Tp)',
        'tz_10yr': 'Zero-Crossing Period (Tz)',
        'wind_speed_10yr': 'Wind Speed (1-hour mean)',
        'wind_gust_10yr': 'Wind Gust (3-second)',
        'current_speed_surface_10yr': 'Surface Current',

        # 100-year return period fields
        'hs_100yr': 'Significant Wave Height (Hs)',
        'tp_100yr': 'Peak Period (Tp)',
        'tz_100yr': 'Zero-Crossing Period (Tz)',
        'wind_speed_100yr': 'Wind Speed (1-hour mean)',
        'wind_gust_100yr': 'Wind Gust (3-second)',
        'current_speed_surface_100yr': 'Surface Current',

        # Site parameters
        'tidal_range': 'Tidal Range',
        'marine_growth_thickness': 'Marine Growth Thickness',
        'water_depth': 'Water Depth',

        # Buoy fields
        'type': 'Type',
        'outer_diameter': 'Outer Diameter',
        'draft': 'Draft',
        'freeboard': 'Freeboard',
        'skirt_diameter': 'Skirt Diameter',
        'displacement': 'Displacement',
        'mass_dry': 'Dry Mass',
        'mass_operating': 'Operating Mass',
        'centre_of_gravity': 'Centre of Gravity',
        'inertia_roll': 'Roll Inertia',
        'inertia_pitch': 'Pitch Inertia',
        'inertia_yaw': 'Yaw Inertia',
        'hydrodynamic_database': 'Hydrodynamic Database',
        'rao_source': 'RAO Source',
        'material': 'Steel Grade',
        'coating': 'Coating',
        'design_life': 'Design Life',

        # Mooring fields
        'pattern': 'Pattern',
        'number_of_lines': 'Number of Lines',
        'azimuth_spacing': 'Azimuth Spacing',
        'line_segments': 'Mooring Lines',
        'segment_name': 'Segment Name',
        'grade': 'Grade',
        'nominal_diameter': 'Diameter',
        'length': 'Length',
        'mbl': 'Minimum Breaking Load',
        'mass_per_meter': 'Mass per Meter',
        'ea': 'Axial Stiffness (EA)',
        'fairlead_height': 'Fairlead Height',
        'anchor_type': 'Anchor Type',
        'anchor_holding_capacity': 'Anchor Holding Capacity',
        'safety_factor_intact': 'Safety Factor (Intact)',
        'safety_factor_damaged': 'Safety Factor (Damaged)',
        'pretension': 'Pretension',

        # Offloading fields
        'tanker_type': 'Tanker Type',
        'tanker_dwt': 'Deadweight Tonnage',
        'tanker_loa': 'Length Overall',
        'tanker_beam': 'Beam',
        'tanker_draft_laden': 'Draft (Laden)',
        'hose_type': 'Hose Type',
        'hose_diameter': 'Hose Diameter',
        'hose_length': 'Hose Length',
        'hose_segments': 'Number of Hose Segments',
        'hose_mbl': 'Hose Minimum Breaking Load',
        'hawser_type': 'Hawser Type',
        'hawser_diameter': 'Hawser Diameter',
        'hawser_length': 'Hawser Length',
        'hawser_mbl': 'Hawser Minimum Breaking Load',
        'transfer_rate': 'Transfer Rate',
        'product_density': 'Product Density',

        # Analysis fields
        'run_preliminary': 'Run Preliminary',
        'run_detailed': 'Run Detailed',
        'run_sensitivity': 'Run Sensitivity',
        'load_cases': 'Load Cases',
        'condition': 'Condition',
        'wave_direction': 'Wave Direction',
        'current_direction': 'Current Direction',
        'wind_direction': 'Wind Direction',
        'simulation_duration': 'Simulation Duration',
        'time_step': 'Time Step',
        'ramp_time': 'Ramp Time',
        'output_mooring_tension': 'Output Mooring Tension',
        'output_buoy_motions': 'Output Buoy Motions',
        'output_hose_curvature': 'Output Hose Curvature',
        'output_clearances': 'Output Clearances',
        'output_statistics': 'Output Statistics',

        # AI Generated fields
        'validation': 'Validation Results',
        'derived': 'Derived Parameters',
        'recommendations': 'Recommendations',
        'data_sources': 'Data Sources',
        'validated_by_ai': 'Validated by AI',
        'validation_date': 'Validation Date',
        'validation_version': 'Validation Version',
        'confidence_score': 'Confidence Score',
        'checks': 'Checks Performed',
        'geometry_within_ranges': 'Geometry Within Ranges',
        'metocean_realistic': 'Metocean Realistic',
        'mooring_capacity_adequate': 'Mooring Capacity Adequate',
        'safety_factors_met': 'Safety Factors Met',
        'orcaflex_syntax_valid': 'OrcaFlex Syntax Valid',
        'mooring_footprint_radius': 'Mooring Footprint Radius',
        'total_mooring_mass': 'Total Mooring Mass',
        'mooring_stiffness_surge': 'Mooring Stiffness (Surge)',
        'mooring_stiffness_sway': 'Mooring Stiffness (Sway)',
        'watch_circle_radius': 'Watch Circle Radius',
        'buoy_natural_periods': 'Buoy Natural Periods',
        'surge': 'Surge',
        'heave': 'Heave',
        'pitch': 'Pitch',
        'max_line_tension_operating': 'Max Line Tension (Operating)',
        'max_line_tension_extreme': 'Max Line Tension (Extreme)',
        'utilization_ratio_intact': 'Utilization Ratio (Intact)',
        'utilization_ratio_damaged': 'Utilization Ratio (Damaged)',
        'warnings': 'Warnings',
        'suggestions': 'Suggestions',
    }

    def __init__(self):
        """Initialize mapper with reverse lookup dictionary."""
        # Create reverse mapping: Human-friendly -> Technical
        self.reverse_map = {}
        for tech_key, human_key in self.KEY_MAPPINGS.items():
            # Add exact match
            self.reverse_map[human_key.lower()] = tech_key
            # Add normalized version (no spaces, no special chars)
            normalized = self._normalize_key(human_key)
            self.reverse_map[normalized] = tech_key

    def _normalize_key(self, key: str) -> str:
        """
        Normalize key for flexible matching.

        Examples:
            "Project Information" -> "projectinformation"
            "Centre of Gravity" -> "centreofgravity"
            "Safety Factor (Intact)" -> "safetyfactorintact"
        """
        # Remove special characters, convert to lowercase, remove spaces
        normalized = re.sub(r'[^a-z0-9]', '', key.lower())
        return normalized

    def to_technical(self, human_key: str) -> str:
        """
        Convert human-friendly key to technical key.

        Args:
            human_key: Human-friendly key (e.g., "Project Information")

        Returns:
            Technical key (e.g., "project_information")

        Examples:
            >>> mapper.to_technical("Project Information")
            'project_information'
            >>> mapper.to_technical("Buoy Specifications")
            'buoy'
            >>> mapper.to_technical("buoy")  # Already technical
            'buoy'
        """
        # Check if already technical key
        if human_key in self.KEY_MAPPINGS:
            return human_key

        # Try exact match (case-insensitive)
        normalized = human_key.lower()
        if normalized in self.reverse_map:
            return self.reverse_map[normalized]

        # Try normalized match
        normalized = self._normalize_key(human_key)
        if normalized in self.reverse_map:
            return self.reverse_map[normalized]

        # Fallback: convert to snake_case
        return self._to_snake_case(human_key)

    def to_human(self, tech_key: str) -> str:
        """
        Convert technical key to human-friendly label.

        Args:
            tech_key: Technical key (e.g., "project_information")

        Returns:
            Human-friendly label (e.g., "Project Information")

        Examples:
            >>> mapper.to_human("project_information")
            'Project Information'
            >>> mapper.to_human("buoy")
            'Buoy Specifications'
        """
        if tech_key in self.KEY_MAPPINGS:
            return self.KEY_MAPPINGS[tech_key]

        # Fallback: convert snake_case to Title Case
        return tech_key.replace('_', ' ').title()

    def _to_snake_case(self, text: str) -> str:
        """
        Convert text to snake_case.

        Examples:
            "Project Information" -> "project_information"
            "Centre of Gravity" -> "centre_of_gravity"
        """
        # Remove special characters except spaces and underscores
        text = re.sub(r'[^\w\s]', '', text)
        # Replace spaces with underscores
        text = text.replace(' ', '_')
        # Convert to lowercase
        return text.lower()

    def convert_dict_to_technical(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Recursively convert all keys in dictionary to technical format.

        Args:
            data: Dictionary with human-friendly or mixed keys

        Returns:
            Dictionary with technical keys
        """
        if not isinstance(data, dict):
            return data

        result = {}
        for key, value in data.items():
            tech_key = self.to_technical(key)

            if isinstance(value, dict):
                result[tech_key] = self.convert_dict_to_technical(value)
            elif isinstance(value, list):
                result[tech_key] = [
                    self.convert_dict_to_technical(item) if isinstance(item, dict) else item
                    for item in value
                ]
            else:
                result[tech_key] = value

        return result

    def convert_dict_to_human(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Recursively convert all keys in dictionary to human-friendly format.

        Args:
            data: Dictionary with technical keys

        Returns:
            Dictionary with human-friendly keys
        """
        if not isinstance(data, dict):
            return data

        result = {}
        for key, value in data.items():
            human_key = self.to_human(key)

            if isinstance(value, dict):
                result[human_key] = self.convert_dict_to_human(value)
            elif isinstance(value, list):
                result[human_key] = [
                    self.convert_dict_to_human(item) if isinstance(item, dict) else item
                    for item in value
                ]
            else:
                result[human_key] = value

        return result

    def get_nested_value(self, data: Dict[str, Any], path: str) -> Any:
        """
        Get nested value using dot notation, supporting both key formats.

        Args:
            data: Dictionary to search
            path: Dot-notation path (e.g., "human_input.buoy.outer_diameter")

        Returns:
            Value at path, or None if not found

        Examples:
            >>> mapper.get_nested_value(config, "Human Input.Buoy Specifications.Outer Diameter")
            12.0
            >>> mapper.get_nested_value(config, "human_input.buoy.outer_diameter")
            12.0
        """
        keys = path.split('.')
        current = data

        for key in keys:
            if not isinstance(current, dict):
                return None

            # Try technical key first
            tech_key = self.to_technical(key)
            if tech_key in current:
                current = current[tech_key]
                continue

            # Try human key
            if key in current:
                current = current[key]
                continue

            # Try all possible keys in current dict
            found = False
            for dict_key in current.keys():
                if self._normalize_key(dict_key) == self._normalize_key(key):
                    current = current[dict_key]
                    found = True
                    break

            if not found:
                return None

        return current


# Convenience function
def create_mapper() -> YAMLKeyMapper:
    """Create and return a YAMLKeyMapper instance."""
    return YAMLKeyMapper()


if __name__ == "__main__":
    # Test the mapper
    mapper = YAMLKeyMapper()

    print("Testing key conversions:")
    print(f"  'Project Information' -> '{mapper.to_technical('Project Information')}'")
    print(f"  'Buoy Specifications' -> '{mapper.to_technical('Buoy Specifications')}'")
    print(f"  'project_information' -> '{mapper.to_human('project_information')}'")
    print(f"  'buoy' -> '{mapper.to_human('buoy')}'")

    print("\nTesting flexible matching:")
    print(f"  'projectinformation' -> '{mapper.to_technical('projectinformation')}'")
    print(f"  'PROJECT INFORMATION' -> '{mapper.to_technical('PROJECT INFORMATION')}'")
    print(f"  'Project Info' -> '{mapper.to_technical('Project Info')}'")

    print("\nTesting dictionary conversion:")
    test_dict = {
        "Project Information": {
            "Name": "Test Project",
            "Code": "TEST_001"
        },
        "Buoy Specifications": {
            "Outer Diameter": 12.0,
            "Draft": 10.0
        }
    }

    technical = mapper.convert_dict_to_technical(test_dict)
    print(f"  Human -> Technical: {list(technical.keys())}")

    human = mapper.convert_dict_to_human(technical)
    print(f"  Technical -> Human: {list(human.keys())}")
