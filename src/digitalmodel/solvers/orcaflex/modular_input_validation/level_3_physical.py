"""
ABOUTME: Level 3 physical consistency validation
ABOUTME: Validates YAML parameters against CALM buoy reference data and project-specific values
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Any
from .models import (
    Level3Result,
    ValidationStatus,
    PhysicalConsistencyCheck as _BasePhysicalConsistencyCheck,
    Severity,
)


@dataclass
class PhysicalConsistencyCheck(_BasePhysicalConsistencyCheck):
    """models.PhysicalConsistencyCheck plus the reporting fields consumed
    by validator._collect_issues_from_level_3 and the markdown/console
    reporters (message, expected_range, expected_value, reference_basis).

    The base dataclass in models.py lacks these fields, so constructing a
    check with them raised TypeError — previously undetected because the
    old dot-path probes never matched a real model and no check was ever
    built.
    """
    message: str = ""
    expected_range: Optional[str] = None
    expected_value: Optional[Any] = None
    reference_basis: str = ""
from .data_loader import CALMBuoyDataLoader, ParameterRange
from .utils import (
    load_yaml_file,
    extract_parameter_from_yaml,
    is_within_range,
    is_within_tolerance,
    calculate_percentage_difference,
    resolve_include_path,
    extract_includefiles
)
from .config import ValidationConfig
import re

_PATH_INDEX_RE = re.compile(r'^(?P<name>[^\[\]]+)(?P<indices>(?:\[\d+\])*)$')


def _extract_path(content: Any, path: str, default: Any = None) -> Any:
    """Extract a value from nested YAML using dot notation with list indices.

    Unlike utils.extract_parameter_from_yaml (dict-only key walk), this
    supports list traversal, e.g. 'Environment.WaveTrains[0].WaveHs' or
    '6DBuoys[0].Cylinders[0].CylinderOuterDiameter', which is how real
    OrcaFlex models store these parameters (see
    docs/domains/orcaflex/examples/modular/C06/.../includes/03_environment.yml
    and 08_buoys.yml).
    """
    current = content
    for part in path.split('.'):
        m = _PATH_INDEX_RE.match(part)
        if not m:
            return default
        name = m.group('name')
        if not (isinstance(current, dict) and name in current):
            return default
        current = current[name]
        for idx_str in re.findall(r'\[(\d+)\]', m.group('indices')):
            idx = int(idx_str)
            if not (isinstance(current, list) and idx < len(current)):
                return default
            current = current[idx]
    return current


def _extract_first(content: Any, paths: List[str], default: Any = None) -> Any:
    """Return the first non-None value found among candidate paths."""
    for path in paths:
        value = _extract_path(content, path)
        if value is not None:
            return value
    return default


class Level3PhysicalValidator:
    """
    Level 3 validator for physical consistency.

    Validates:
    - Hull geometry parameter ranges
    - Metocean design parameters
    - Mooring component capacities
    - Environmental conditions
    - Mooring line properties
    - Project-specific value comparisons
    """

    def __init__(self, config: Optional[ValidationConfig] = None):
        """
        Initialize Level 3 validator.

        Args:
            config: Validation configuration
        """
        self.config = config or ValidationConfig()
        self.data_loader = CALMBuoyDataLoader(self.config.calm_buoy_data_dir)

    def validate(self, file_path: Path) -> Level3Result:
        """
        Run Level 3 physical consistency validation.

        Args:
            file_path: Path to YAML file

        Returns:
            Level3Result with validation findings
        """
        file_path = Path(file_path)
        result = Level3Result()

        # Check file existence
        if not file_path.exists():
            result.status = ValidationStatus.FAIL
            return result

        # Load and merge YAML content (base + includefiles)
        merged_content = self._load_and_merge_yaml(file_path)
        if not merged_content:
            result.status = ValidationStatus.FAIL
            return result

        # Load reference data
        reference_data = self.data_loader.load_all()

        # Validate hull geometry
        hull_checks = self._validate_hull_geometry(
            merged_content,
            reference_data.hull_geometry
        )
        result.hull_geometry_checks.extend(hull_checks)

        # Validate metocean parameters
        metocean_checks = self._validate_metocean(
            merged_content,
            reference_data.metocean
        )
        result.metocean_checks.extend(metocean_checks)

        # Validate mooring capacity
        mooring_checks = self._validate_mooring_capacity(
            merged_content,
            reference_data.mooring_capacity
        )
        result.mooring_capacity_checks.extend(mooring_checks)

        # Compare to project-specific data
        project_checks = self._compare_to_project_data(
            merged_content,
            reference_data.environmental_conditions,
            reference_data.mooring_line_properties
        )
        result.project_comparison_checks.extend(project_checks)

        # Count issues by severity
        result.parameters_validated = (
            len(result.hull_geometry_checks) +
            len(result.metocean_checks) +
            len(result.mooring_capacity_checks) +
            len(result.project_comparison_checks)
        )

        for check in (result.hull_geometry_checks +
                     result.metocean_checks +
                     result.mooring_capacity_checks +
                     result.project_comparison_checks):
            if check.severity == Severity.CRITICAL:
                result.critical_issues += 1
            elif check.severity == Severity.WARNING:
                result.warnings += 1

        # Determine overall status
        if result.critical_issues > 0:
            result.status = ValidationStatus.FAIL
        elif result.warnings > 0:
            result.status = ValidationStatus.WARN
        elif result.parameters_validated > 0:
            result.status = ValidationStatus.PASS
        else:
            result.status = ValidationStatus.UNKNOWN

        return result

    def _load_and_merge_yaml(self, file_path: Path) -> Optional[Dict]:
        """
        Load YAML file and merge all includefiles.

        Args:
            file_path: Path to base YAML file

        Returns:
            Merged YAML content dictionary
        """
        # Load base file
        valid, base_content, error = load_yaml_file(file_path)
        if not valid or not base_content:
            return None

        # Get includefiles
        includefiles = extract_includefiles(base_content)

        # Merge included files
        merged = base_content.copy()

        for include_file in includefiles:
            include_path = resolve_include_path(file_path, include_file)

            if not include_path.exists():
                continue

            valid, include_content, _ = load_yaml_file(include_path)
            if valid and include_content:
                # Deep merge - include content adds to/overrides base
                merged = self._deep_merge_dicts(merged, include_content)

        return merged

    def _deep_merge_dicts(self, base: Any, override: Any) -> Any:
        """
        Deep merge two data structures (dicts or lists).

        Args:
            base: Base data structure (dict, list, or primitive)
            override: Override data structure (dict, list, or primitive)

        Returns:
            Merged data structure
        """
        # If override is not a dict, return it as-is (replacement behavior)
        if not isinstance(override, dict):
            return override

        # If base is not a dict but override is, return override
        if not isinstance(base, dict):
            return override

        # Both are dicts - perform deep merge
        result = base.copy()

        for key, value in override.items():
            if key in result:
                # Both values are dicts - recurse
                if isinstance(result[key], dict) and isinstance(value, dict):
                    result[key] = self._deep_merge_dicts(result[key], value)
                # Both values are lists - extend
                elif isinstance(result[key], list) and isinstance(value, list):
                    result[key] = result[key] + value
                # Otherwise - override replaces base
                else:
                    result[key] = value
            else:
                # Key doesn't exist in base - add it
                result[key] = value

        return result

    def _validate_hull_geometry(
        self,
        content: Dict,
        ranges: Dict[str, ParameterRange]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Validate hull geometry parameters.

        Args:
            content: Merged YAML content
            ranges: Hull geometry parameter ranges

        Returns:
            List of physical checks
        """
        checks = []

        # Common hull geometry parameters to check.  Real models store buoy
        # data in the '6DBuoys' list (spar-buoy cylinders carry the outer
        # diameter — see docs/domains/orcaflex/examples/modular/C06/
        # 'C06 CALM buoy'/includes/08_buoys.yml); legacy flat 'Buoy.*' paths
        # are kept as fallbacks for synthetic project files.
        hull_params = {
            'buoy_diameter': [
                '6DBuoys[0].Cylinders[0].CylinderOuterDiameter',
                'Buoy.OuterDiameter',
            ],
            'buoy_draft': ['Buoy.Draft'],
            'buoy_mass': ['6DBuoys[0].Mass', 'Buoy.Mass'],
            'buoy_displacement': ['Buoy.Displacement'],
        }

        for param_name, yaml_paths in hull_params.items():
            if param_name not in ranges:
                continue

            param_range = ranges[param_name]
            actual_value = _extract_first(content, yaml_paths)

            if actual_value is None:
                continue

            # Convert to float if string
            try:
                actual_value = float(actual_value)
            except (ValueError, TypeError):
                checks.append(PhysicalConsistencyCheck(
                    parameter=param_name,
                    actual_value=str(actual_value),
                    expected_range=f"[{param_range.min_value}, {param_range.max_value}]",
                    within_range=False,
                    difference_percent=None,
                    severity=Severity.WARNING,
                    message=f"Could not convert {param_name} to numeric value"
                ))
                continue

            # Check if within range
            within_range = is_within_range(
                actual_value,
                param_range.min_value,
                param_range.max_value
            )

            # Determine severity
            if within_range:
                severity = Severity.INFO
                message = f"{param_name} within acceptable range"
            else:
                severity = Severity.WARNING
                message = f"{param_name} outside generic range - verify design basis"

            checks.append(PhysicalConsistencyCheck(
                parameter=param_name,
                actual_value=f"{actual_value:.2f}",
                expected_range=f"[{param_range.min_value}, {param_range.max_value}]",
                within_range=within_range,
                difference_percent=None,
                severity=severity,
                message=message,
                reference_basis=param_range.reference_basis
            ))

        return checks

    def _validate_metocean(
        self,
        content: Dict,
        ranges: Dict[str, ParameterRange]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Validate metocean design parameters.

        Args:
            content: Merged YAML content
            ranges: Metocean parameter ranges

        Returns:
            List of physical checks
        """
        checks = []

        # Common metocean parameters.  In real OrcaFlex models Hs/Tp live
        # inside the Environment.WaveTrains list (spectral trains use
        # WaveHs/WaveTz — see modular_generator/builders/environment_builder.py
        # and docs/domains/orcaflex/examples/modular/C06/.../03_environment.yml)
        # and the surface current is 'RefCurrentSpeed' (see
        # format_converter/section_mapping.py INPUT_PARAMETERS).  Legacy flat
        # paths are kept as fallbacks for synthetic project files.
        metocean_params = {
            'operating_hs': [
                'Environment.WaveTrains[0].WaveHs',
                'Environment.WaveHs',
            ],
            'operating_tp': [
                'Environment.WaveTrains[0].WaveTp',
                'Environment.WaveTrains[0].WaveTz',
                'Environment.WaveTp',
            ],
            'operating_wind_speed': ['Environment.WindSpeed'],
            'operating_surface_current': [
                'Environment.RefCurrentSpeed',
                'Environment.SurfaceCurrentSpeed',
            ],
        }

        for param_name, yaml_paths in metocean_params.items():
            # Check if we have range data for this parameter
            matching_ranges = [k for k in ranges.keys() if param_name in k.lower()]

            if not matching_ranges:
                continue

            actual_value = _extract_first(content, yaml_paths)

            if actual_value is None:
                continue

            try:
                actual_value = float(actual_value)
            except (ValueError, TypeError):
                continue

            # Use first matching range
            param_range = ranges[matching_ranges[0]]

            within_range = is_within_range(
                actual_value,
                param_range.min_value,
                param_range.max_value
            )

            if within_range:
                severity = Severity.INFO
                message = f"{param_name} within design range"
            else:
                severity = Severity.WARNING
                message = f"{param_name} outside typical design range"

            checks.append(PhysicalConsistencyCheck(
                parameter=param_name,
                actual_value=f"{actual_value:.2f}",
                expected_range=f"[{param_range.min_value}, {param_range.max_value}]",
                within_range=within_range,
                difference_percent=None,
                severity=severity,
                message=message,
                reference_basis=param_range.reference_basis
            ))

        return checks

    def _validate_mooring_capacity(
        self,
        content: Dict,
        ranges: Dict[str, ParameterRange]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Validate mooring component capacities and safety factors.

        Args:
            content: Merged YAML content
            ranges: Mooring capacity ranges

        Returns:
            List of physical checks
        """
        checks = []

        # Common mooring parameters.  NOTE: OrcaFlex YAML line types do not
        # carry MBL/safety-factor properties (see the curated components in
        # docs/domains/orcaflex/library/line_types/, e.g. chain_84mm_r3.yml:
        # MBL appears only as a comment), so these paths only match
        # synthetic project files that embed them explicitly.
        mooring_params = {
            'chain_mbl': ['Line.MBL'],
            'chain_safety_factor': ['Line.SafetyFactor'],
            'connector_capacity': ['Connector.Capacity'],
        }

        for param_name, yaml_paths in mooring_params.items():
            matching_ranges = [k for k in ranges.keys() if any(word in k.lower() for word in param_name.split('_'))]

            if not matching_ranges:
                continue

            actual_value = _extract_first(content, yaml_paths)

            if actual_value is None:
                continue

            try:
                actual_value = float(actual_value)
            except (ValueError, TypeError):
                continue

            param_range = ranges[matching_ranges[0]]

            within_range = is_within_range(
                actual_value,
                param_range.min_value,
                param_range.max_value
            )

            # Safety factors outside range are critical
            if 'safety_factor' in param_name and not within_range:
                severity = Severity.CRITICAL
                message = f"{param_name} outside acceptable safety factor range"
            elif within_range:
                severity = Severity.INFO
                message = f"{param_name} within acceptable range"
            else:
                severity = Severity.WARNING
                message = f"{param_name} outside typical capacity range"

            checks.append(PhysicalConsistencyCheck(
                parameter=param_name,
                actual_value=f"{actual_value:.2f}",
                expected_range=f"[{param_range.min_value}, {param_range.max_value}]",
                within_range=within_range,
                difference_percent=None,
                severity=severity,
                message=message,
                reference_basis=param_range.reference_basis
            ))

        return checks

    def _compare_to_project_data(
        self,
        content: Dict,
        env_conditions: Dict[str, Dict],
        line_properties: Dict[str, Dict]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Compare parameters to project-specific data.

        Args:
            content: Merged YAML content
            env_conditions: Project environmental conditions
            line_properties: Project mooring line properties

        Returns:
            List of physical checks
        """
        checks = []

        # Compare environmental conditions if available
        if env_conditions:
            env_checks = self._compare_environmental_conditions(content, env_conditions)
            checks.extend(env_checks)

        # Compare mooring line properties if available
        if line_properties:
            line_checks = self._compare_line_properties(content, line_properties)
            checks.extend(line_checks)

        return checks

    def _compare_environmental_conditions(
        self,
        content: Dict,
        env_conditions: Dict[str, Dict]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Compare environmental conditions to project-specific data.

        Args:
            content: Merged YAML content
            env_conditions: Project environmental conditions

        Returns:
            List of physical checks
        """
        checks = []

        # Extract wave parameters (WaveTrains list first — real-model
        # layout — then legacy flat paths)
        actual_hs = _extract_first(content, [
            'Environment.WaveTrains[0].WaveHs',
            'Environment.WaveHs',
        ])
        actual_tp = _extract_first(content, [
            'Environment.WaveTrains[0].WaveTp',
            'Environment.WaveTrains[0].WaveTz',
            'Environment.WaveTp',
        ])

        if actual_hs is None or actual_tp is None:
            return checks

        try:
            actual_hs = float(actual_hs)
            actual_tp = float(actual_tp)
        except (ValueError, TypeError):
            return checks

        # Find matching sea state
        for sea_state_id, conditions in env_conditions.items():
            expected_hs = conditions.get('hs')
            expected_tp = conditions.get('tp')

            if expected_hs is None or expected_tp is None:
                continue

            # Check if within tolerance
            hs_within = is_within_tolerance(actual_hs, expected_hs, self.config.tolerance_percent)
            tp_within = is_within_tolerance(actual_tp, expected_tp, self.config.tolerance_percent)

            if hs_within and tp_within:
                # Found matching sea state
                hs_diff = calculate_percentage_difference(actual_hs, expected_hs)
                tp_diff = calculate_percentage_difference(actual_tp, expected_tp)

                checks.append(PhysicalConsistencyCheck(
                    parameter=f"wave_hs_{sea_state_id}",
                    actual_value=f"{actual_hs:.2f}",
                    expected_value=f"{expected_hs:.2f}",
                    within_range=True,
                    difference_percent=abs(hs_diff),
                    severity=Severity.INFO,
                    message=f"Hs matches {sea_state_id} within {self.config.tolerance_percent}% tolerance",
                    reference_basis="Project-specific data"
                ))

                checks.append(PhysicalConsistencyCheck(
                    parameter=f"wave_tp_{sea_state_id}",
                    actual_value=f"{actual_tp:.2f}",
                    expected_value=f"{expected_tp:.2f}",
                    within_range=True,
                    difference_percent=abs(tp_diff),
                    severity=Severity.INFO,
                    message=f"Tp matches {sea_state_id} within {self.config.tolerance_percent}% tolerance",
                    reference_basis="Project-specific data"
                ))

                break

        return checks

    def _compare_line_properties(
        self,
        content: Dict,
        line_properties: Dict[str, Dict]
    ) -> List[PhysicalConsistencyCheck]:
        """
        Compare mooring line properties to project-specific data.

        Args:
            content: Merged YAML content
            line_properties: Project line properties

        Returns:
            List of physical checks
        """
        checks = []

        # Extract line parameters
        actual_mbl = extract_parameter_from_yaml(content, 'Line.MBL')
        actual_length = extract_parameter_from_yaml(content, 'Line.Length')

        if actual_mbl is None:
            return checks

        try:
            actual_mbl = float(actual_mbl)
            if actual_length is not None:
                actual_length = float(actual_length)
        except (ValueError, TypeError):
            return checks

        # Compare to project-specific line properties
        for line_id, properties in line_properties.items():
            expected_mbl = properties.get('mbl')

            if expected_mbl is None:
                continue

            mbl_within = is_within_tolerance(actual_mbl, expected_mbl, self.config.tolerance_percent)

            if mbl_within:
                mbl_diff = calculate_percentage_difference(actual_mbl, expected_mbl)

                severity = Severity.INFO if abs(mbl_diff) < 5.0 else Severity.WARNING

                checks.append(PhysicalConsistencyCheck(
                    parameter=f"line_mbl_{line_id}",
                    actual_value=f"{actual_mbl:.2f}",
                    expected_value=f"{expected_mbl:.2f}",
                    within_range=True,
                    difference_percent=abs(mbl_diff),
                    severity=severity,
                    message=f"MBL matches {line_id} (diff: {mbl_diff:.1f}%)",
                    reference_basis="Project-specific mooring data"
                ))

        return checks
