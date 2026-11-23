#!/usr/bin/env python3
"""Template Validator for DigitalModel Input Templates.

This module provides comprehensive validation capabilities for all DigitalModel
input templates, ensuring data quality and engineering standard compliance.

Classes:
    TemplateValidator: Main validation engine
    ValidationResult: Validation result container
    ValidationRule: Individual validation rule
    EngineeringValidator: Engineering-specific validation logic

Functions:
    validate_template: Main validation entry point
    create_validation_report: Generate validation reports
    get_template_schema: Get validation schema for template type
"""

import os
import sys
import json
import yaml
import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional, Union, Tuple
from dataclasses import dataclass, field
from pathlib import Path
import logging
from datetime import datetime
import re
from abc import ABC, abstractmethod

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class ValidationResult:
    """Container for validation results.

    Attributes:
        is_valid: Overall validation status
        errors: Critical errors that must be fixed
        warnings: Warnings that should be addressed
        info: Informational messages
        summary: Validation summary
        template_type: Type of template validated
        file_path: Path to validated file
    """
    is_valid: bool = True
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    info: List[str] = field(default_factory=list)
    summary: Dict[str, Any] = field(default_factory=dict)
    template_type: Optional[str] = None
    file_path: Optional[str] = None

    def add_error(self, message: str, field: str = None):
        """Add an error message."""
        if field:
            message = f"[{field}] {message}"
        self.errors.append(message)
        self.is_valid = False

    def add_warning(self, message: str, field: str = None):
        """Add a warning message."""
        if field:
            message = f"[{field}] {message}"
        self.warnings.append(message)

    def add_info(self, message: str, field: str = None):
        """Add an info message."""
        if field:
            message = f"[{field}] {message}"
        self.info.append(message)


class ValidationRule(ABC):
    """Abstract base class for validation rules."""

    @abstractmethod
    def validate(self, data: Dict[str, Any], result: ValidationResult) -> None:
        """Validate data according to the rule."""
        pass


class RequiredFieldsRule(ValidationRule):
    """Validate that required fields are present."""

    def __init__(self, required_fields: List[str]):
        self.required_fields = required_fields

    def validate(self, data: Dict[str, Any], result: ValidationResult) -> None:
        """Check for required fields."""
        for field_path in self.required_fields:
            if not self._check_nested_field(data, field_path):
                result.add_error(f"Required field missing: {field_path}")

    def _check_nested_field(self, data: Dict[str, Any], field_path: str) -> bool:
        """Check if nested field exists."""
        parts = field_path.split('.')
        current = data

        for part in parts:
            # Handle array notation like "groups[].field"
            if '[]' in part:
                array_field = part.replace('[]', '')
                if array_field not in current:
                    return False
                if not isinstance(current[array_field], list):
                    return False
                if not current[array_field]:  # Empty array
                    return False
                # Check if field exists in first array element
                remaining_path = '.'.join(parts[parts.index(part) + 1:])
                if remaining_path:
                    return any(self._check_nested_field(item, remaining_path)
                             for item in current[array_field])
                return True
            else:
                if part not in current:
                    return False
                current = current[part]
        return True


class ValueRangeRule(ValidationRule):
    """Validate that numeric values are within acceptable ranges."""

    def __init__(self, field_ranges: Dict[str, Tuple[float, float]]):
        self.field_ranges = field_ranges

    def validate(self, data: Dict[str, Any], result: ValidationResult) -> None:
        """Check value ranges."""
        for field, (min_val, max_val) in self.field_ranges.items():
            value = self._get_nested_value(data, field)
            if value is not None:
                if isinstance(value, (int, float)):
                    if value < min_val or value > max_val:
                        result.add_error(
                            f"Value {value} outside acceptable range [{min_val}, {max_val}]",
                            field
                        )
                elif isinstance(value, list):
                    for i, val in enumerate(value):
                        if isinstance(val, (int, float)):
                            if val < min_val or val > max_val:
                                result.add_error(
                                    f"Array value {val} at index {i} outside range [{min_val}, {max_val}]",
                                    field
                                )

    def _get_nested_value(self, data: Dict[str, Any], field_path: str) -> Any:
        """Get nested field value."""
        try:
            parts = field_path.split('.')
            current = data
            for part in parts:
                if '[]' in part:
                    array_field = part.replace('[]', '')
                    if array_field in current and isinstance(current[array_field], list):
                        # Return list of values for array fields
                        remaining_path = '.'.join(parts[parts.index(part) + 1:])
                        if remaining_path:
                            return [self._get_nested_value(item, remaining_path)
                                   for item in current[array_field]]
                        return current[array_field]
                    return None
                current = current.get(part)
                if current is None:
                    return None
            return current
        except (KeyError, TypeError, AttributeError):
            return None


class EngineeringValidator:
    """Engineering-specific validation logic."""

    @staticmethod
    def validate_material_properties(data: Dict[str, Any], result: ValidationResult):
        """Validate material properties."""
        # Check for common engineering materials
        known_materials = {
            'steel', 'aluminum', 'titanium', 'composite', 'concrete',
            'API 5L X42', 'API 5L X52', 'API 5L X60', 'API 5L X65', 'API 5L X70', 'API 5L X80',
            'S235', 'S355', 'S420', 'S460'
        }

        # Validate material grades in different template types
        materials = []

        # API STD 2RD templates
        if 'Outer_Pipe' in data and 'Material' in data['Outer_Pipe']:
            material_grade = data['Outer_Pipe']['Material'].get('Material_Grade')
            if material_grade:
                materials.append(material_grade)

        # Plate buckling templates
        if 'groups' in data:
            for group in data['groups']:
                if 'material' in group and 'grade' in group['material']:
                    materials.append(group['material']['grade'])

        # Stress analysis templates
        if 'stress_components' in data:
            for component in data['stress_components']:
                if 'material' in component and 'grade' in component['material']:
                    materials.append(component['material']['grade'])

        for material in materials:
            if material not in known_materials:
                result.add_warning(f"Unknown material grade: {material}")

    @staticmethod
    def validate_pressure_consistency(data: Dict[str, Any], result: ValidationResult):
        """Validate pressure consistency with water depth."""
        water_depth = None
        internal_pressure = None

        # Get water depth
        if 'General' in data and 'Water_Depth' in data['General']:
            water_depth = data['General']['Water_Depth']
        elif 'environment' in data and 'water_depth' in data['environment']:
            water_depth = data['environment']['water_depth']

        # Get internal pressure
        if 'Design' in data and isinstance(data['Design'], list):
            for design_case in data['Design']:
                if 'InternalPressure' in design_case:
                    for pipe, pressure in design_case['InternalPressure'].items():
                        internal_pressure = pressure
                        break

        if water_depth and internal_pressure:
            # Hydrostatic pressure at depth (rough estimate: 0.45 psi/ft)
            hydrostatic_pressure = water_depth * 0.45
            if internal_pressure < hydrostatic_pressure * 0.5:
                result.add_warning(
                    f"Internal pressure ({internal_pressure} psi) seems low for water depth ({water_depth} ft)"
                )

    @staticmethod
    def validate_stress_limits(data: Dict[str, Any], result: ValidationResult):
        """Validate stress values against material limits."""
        if 'stress_components' in data:
            for component in data['stress_components']:
                material = component.get('material', {})
                yield_strength = material.get('yield_strength', 0)

                if 'stress_state' in component:
                    stress_state = component['stress_state']

                    # Check normal stresses
                    if 'normal_stress' in stress_state:
                        for direction, stress in stress_state['normal_stress'].items():
                            if abs(stress) > yield_strength:
                                result.add_error(
                                    f"Stress {stress} MPa exceeds yield strength {yield_strength} MPa",
                                    f"{component.get('component_id', 'Unknown')}.{direction}"
                                )

    @staticmethod
    def validate_fatigue_parameters(data: Dict[str, Any], result: ValidationResult):
        """Validate fatigue analysis parameters."""
        if 'inputs' in data and 'design' in data['inputs']:
            design = data['inputs']['design']

            design_life = design.get('design_life', 0)
            safety_factor = design.get('safety_factor', 0)

            if design_life < 1 or design_life > 100:
                result.add_warning(f"Design life {design_life} years seems unusual")

            if safety_factor < 1.5 or safety_factor > 50:
                result.add_warning(f"Safety factor {safety_factor} seems unusual")

    @staticmethod
    def validate_reservoir_properties(data: Dict[str, Any], result: ValidationResult):
        """Validate reservoir properties."""
        if 'volumetrics' in data and 'reservoir_parameters' in data['volumetrics']:
            params = data['volumetrics']['reservoir_parameters']

            porosity = params.get('porosity', 0)
            water_saturation = params.get('water_saturation', 0)
            net_to_gross = params.get('net_to_gross', 0)

            if porosity < 0 or porosity > 0.5:
                result.add_error(f"Porosity {porosity} outside realistic range [0, 0.5]")

            if water_saturation < 0 or water_saturation > 1:
                result.add_error(f"Water saturation {water_saturation} outside range [0, 1]")

            if net_to_gross < 0 or net_to_gross > 1:
                result.add_error(f"Net-to-gross ratio {net_to_gross} outside range [0, 1]")


class TemplateValidator:
    """Main template validation engine."""

    def __init__(self):
        self.engineering_validator = EngineeringValidator()
        self.template_schemas = self._load_template_schemas()

    def _load_template_schemas(self) -> Dict[str, Dict[str, Any]]:
        """Load validation schemas for different template types."""
        return {
            'api_std_2rd': {
                'required_fields': [
                    'General.Water_Depth',
                    'Outer_Pipe.Geometry.Nominal_OD',
                    'Outer_Pipe.Geometry.Design_WT',
                    'Outer_Pipe.Material.Material_Grade',
                    'Design'
                ],
                'value_ranges': {
                    'General.Water_Depth': (0, 15000),
                    'Outer_Pipe.Geometry.Nominal_OD': (2, 72),
                    'Outer_Pipe.Geometry.Design_WT': (0.1, 5.0)
                }
            },
            'plate_capacity': {
                'required_fields': [
                    'groups[].geometry.length',
                    'groups[].geometry.breadth',
                    'groups[].geometry.thickness',
                    'groups[].material.yield_strength',
                    'groups[].loading.longitudinal_stress'
                ],
                'value_ranges': {
                    'groups[].geometry.thickness': (0.005, 0.200),
                    'groups[].material.yield_strength': (200e6, 1000e6)
                }
            },
            'fatigue_analysis': {
                'required_fields': [
                    'fatigue_data.io',
                    'inputs.fatigue_curve',
                    'inputs.design.design_life',
                    'inputs.design.safety_factor'
                ],
                'value_ranges': {
                    'inputs.design.design_life': (1, 200),
                    'inputs.design.safety_factor': (1.0, 50.0)
                }
            },
            'stress_analysis': {
                'required_fields': [
                    'stress_components[].material.grade',
                    'stress_components[].geometry',
                    'stress_components[].loading'
                ],
                'value_ranges': {
                    'stress_components[].material.yield_strength': (100, 2000)
                }
            },
            'reservoir_analysis': {
                'required_fields': [
                    'wells[].well_name',
                    'wells[].coordinates',
                    'stratigraphy.formations',
                    'log_analysis.fluid_properties'
                ],
                'value_ranges': {
                    'volumetrics.reservoir_parameters.porosity': (0.0, 0.5),
                    'volumetrics.reservoir_parameters.water_saturation': (0.0, 1.0)
                }
            }
        }

    def detect_template_type(self, data: Dict[str, Any]) -> str:
        """Detect template type from data structure."""
        if 'Outer_Pipe' in data and 'Design' in data:
            return 'api_std_2rd'
        elif 'groups' in data and 'calculation_type' in data:
            if data.get('calculation_type') == 'DNV_rp_C201':
                return 'plate_capacity'
        elif 'fatigue_data' in data and 'inputs' in data:
            return 'fatigue_analysis'
        elif 'stress_components' in data and 'analysis_methods' in data:
            return 'stress_analysis'
        elif 'wells' in data and 'stratigraphy' in data:
            return 'reservoir_analysis'
        else:
            return 'unknown'

    def validate_file(self, file_path: str) -> ValidationResult:
        """Validate a template file."""
        result = ValidationResult()
        result.file_path = file_path

        try:
            # Load file
            with open(file_path, 'r', encoding='utf-8') as f:
                if file_path.endswith('.yml') or file_path.endswith('.yaml'):
                    data = yaml.safe_load(f)
                elif file_path.endswith('.json'):
                    data = json.load(f)
                else:
                    result.add_error(f"Unsupported file format: {file_path}")
                    return result

            if data is None:
                result.add_error("File is empty or contains no valid data")
                return result

            # Detect template type
            template_type = self.detect_template_type(data)
            result.template_type = template_type

            if template_type == 'unknown':
                result.add_warning("Could not determine template type")
                return result

            # Get validation schema
            schema = self.template_schemas.get(template_type, {})

            # Run validation rules
            self._validate_structure(data, schema, result)
            self._validate_engineering_constraints(data, template_type, result)

            # Generate summary
            result.summary = {
                'template_type': template_type,
                'file_size_kb': os.path.getsize(file_path) / 1024,
                'validation_date': datetime.now().isoformat(),
                'total_errors': len(result.errors),
                'total_warnings': len(result.warnings),
                'total_info': len(result.info)
            }

        except Exception as e:
            result.add_error(f"Failed to validate file: {str(e)}")

        return result

    def _validate_structure(self, data: Dict[str, Any], schema: Dict[str, Any],
                          result: ValidationResult):
        """Validate data structure against schema."""
        # Required fields
        if 'required_fields' in schema:
            rule = RequiredFieldsRule(schema['required_fields'])
            rule.validate(data, result)

        # Value ranges
        if 'value_ranges' in schema:
            rule = ValueRangeRule(schema['value_ranges'])
            rule.validate(data, result)

    def _validate_engineering_constraints(self, data: Dict[str, Any],
                                        template_type: str, result: ValidationResult):
        """Apply engineering-specific validation."""
        self.engineering_validator.validate_material_properties(data, result)

        if template_type == 'api_std_2rd':
            self.engineering_validator.validate_pressure_consistency(data, result)
        elif template_type == 'stress_analysis':
            self.engineering_validator.validate_stress_limits(data, result)
        elif template_type == 'fatigue_analysis':
            self.engineering_validator.validate_fatigue_parameters(data, result)
        elif template_type == 'reservoir_analysis':
            self.engineering_validator.validate_reservoir_properties(data, result)


def validate_template(file_path: str) -> ValidationResult:
    """Main validation entry point."""
    validator = TemplateValidator()
    return validator.validate_file(file_path)


def create_validation_report(result: ValidationResult, output_path: str = None) -> str:
    """Create a validation report."""
    report_lines = [
        f"Template Validation Report",
        f"=" * 50,
        f"File: {result.file_path}",
        f"Template Type: {result.template_type}",
        f"Validation Status: {'PASS' if result.is_valid else 'FAIL'}",
        f"Validation Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
        "",
    ]

    if result.errors:
        report_lines.append("ERRORS:")
        for error in result.errors:
            report_lines.append(f"  ❌ {error}")
        report_lines.append("")

    if result.warnings:
        report_lines.append("WARNINGS:")
        for warning in result.warnings:
            report_lines.append(f"  ⚠️  {warning}")
        report_lines.append("")

    if result.info:
        report_lines.append("INFORMATION:")
        for info in result.info:
            report_lines.append(f"  ℹ️  {info}")
        report_lines.append("")

    if result.summary:
        report_lines.append("SUMMARY:")
        for key, value in result.summary.items():
            report_lines.append(f"  {key}: {value}")
        report_lines.append("")

    report_text = "\n".join(report_lines)

    if output_path:
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(report_text)

    return report_text


def main():
    """Command line interface."""
    import argparse

    parser = argparse.ArgumentParser(description='Validate DigitalModel templates')
    parser.add_argument('file', help='Template file to validate')
    parser.add_argument('--report', help='Output validation report to file')
    parser.add_argument('--verbose', action='store_true', help='Verbose output')

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    # Validate file
    result = validate_template(args.file)

    # Generate report
    report = create_validation_report(result, args.report)

    # Print to console
    print(report)

    # Exit with appropriate code
    sys.exit(0 if result.is_valid else 1)


if __name__ == '__main__':
    main()