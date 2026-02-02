"""
Template validation integration tests.

This module validates that all input templates work correctly and
produce expected results for engineering scenarios.
"""

import pytest
import sys
import yaml
import json
import numpy as np
from pathlib import Path
from unittest.mock import patch, MagicMock
import tempfile
from typing import Dict, Any, List

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


@pytest.mark.integration
class TestInputTemplateValidation:
    """Test validation of input templates for various engineering scenarios."""

    @pytest.fixture
    def template_directory(self, temp_directory):
        """Create temporary directory with test templates."""
        template_dir = temp_directory / "templates"
        template_dir.mkdir()
        return template_dir

    @pytest.fixture
    def pipeline_template(self):
        """Standard pipeline analysis template."""
        return {
            "analysis_type": "pipeline",
            "metadata": {
                "project_name": "North Sea Pipeline",
                "analysis_date": "2025-01-15",
                "analyst": "John Engineer",
                "version": "1.0"
            },
            "environment": {
                "water_depth": 150.0,
                "seabed_type": "sandy_clay",
                "wave_height_significant": 8.5,
                "wave_period_peak": 12.0,
                "current_velocity_surface": 1.2,
                "current_velocity_bottom": 0.3,
                "temperature_seabed": 4.0,
                "temperature_surface": 15.0
            },
            "pipeline": {
                "geometry": {
                    "outer_diameter": 0.508,
                    "wall_thickness": 0.0254,
                    "length": 50000.0,
                    "burial_depth": 1.0
                },
                "material": {
                    "grade": "API_5L_X65",
                    "yield_strength": 450e6,
                    "ultimate_strength": 535e6,
                    "elastic_modulus": 210e9,
                    "poisson_ratio": 0.3,
                    "density": 7850
                },
                "coating": {
                    "type": "3LPE",
                    "thickness": 0.003,
                    "density": 1400
                },
                "contents": {
                    "fluid": "oil",
                    "density": 850,
                    "operating_pressure": 15e6,
                    "operating_temperature": 60.0
                }
            },
            "analysis_options": {
                "include_lateral_buckling": True,
                "include_upheaval_buckling": True,
                "include_fatigue": True,
                "include_free_spanning": True,
                "include_thermal_effects": True
            },
            "load_cases": [
                {
                    "name": "operating",
                    "pressure": 15e6,
                    "temperature": 60.0,
                    "current_factor": 1.0
                },
                {
                    "name": "hydrostatic_test",
                    "pressure": 22.5e6,
                    "temperature": 15.0,
                    "current_factor": 0.5
                },
                {
                    "name": "storm",
                    "pressure": 15e6,
                    "temperature": 60.0,
                    "current_factor": 1.5
                }
            ]
        }

    @pytest.fixture
    def plate_template(self):
        """Standard plate analysis template."""
        return {
            "analysis_type": "plate",
            "metadata": {
                "project_name": "FPSO Hull Plating",
                "analysis_date": "2025-01-15",
                "analyst": "Sarah Structural",
                "version": "2.1"
            },
            "plate_geometry": {
                "length": 4.0,
                "width": 3.0,
                "thickness": 0.02,
                "stiffener_spacing": 0.6,
                "stiffener_type": "T_bar"
            },
            "material": {
                "grade": "AH36",
                "yield_strength": 355e6,
                "ultimate_strength": 490e6,
                "elastic_modulus": 210e9,
                "poisson_ratio": 0.3,
                "density": 7850
            },
            "boundary_conditions": {
                "type": "simply_supported",
                "edge_supports": ["x_min", "x_max", "y_min", "y_max"]
            },
            "loading": {
                "type": "pressure",
                "magnitude": 50000,
                "distribution": "uniform",
                "safety_factor": 1.5
            },
            "analysis_options": {
                "include_buckling": True,
                "include_large_deflection": False,
                "mesh_refinement": "medium",
                "convergence_tolerance": 1e-6
            }
        }

    @pytest.fixture
    def fatigue_template(self):
        """Standard fatigue analysis template."""
        return {
            "analysis_type": "fatigue",
            "metadata": {
                "project_name": "Offshore Platform Joint",
                "analysis_date": "2025-01-15",
                "analyst": "Mike Fatigue",
                "version": "1.5"
            },
            "geometry": {
                "joint_type": "tubular_T_joint",
                "chord_diameter": 1.0,
                "chord_thickness": 0.05,
                "brace_diameter": 0.6,
                "brace_thickness": 0.032,
                "beta_ratio": 0.6,
                "gamma_ratio": 20.0
            },
            "material": {
                "grade": "S355",
                "yield_strength": 355e6,
                "ultimate_strength": 510e6,
                "fatigue_class": "G",
                "thickness_effect": True
            },
            "loading": {
                "stress_history": {
                    "type": "rainflow_spectrum",
                    "stress_ranges": [10, 20, 30, 40, 50, 60, 70, 80],
                    "cycle_counts": [1e7, 5e6, 2e6, 1e6, 5e5, 2e5, 1e5, 5e4]
                },
                "mean_stress": 50e6,
                "stress_ratio": 0.1
            },
            "environment": {
                "condition": "seawater_with_cathodic_protection",
                "temperature": 15.0,
                "salinity": 3.5
            },
            "analysis_options": {
                "design_life": 25,
                "confidence_level": 0.95,
                "inspection_strategy": "periodic",
                "safety_factor": 2.0
            }
        }

    def test_pipeline_template_validation(self, pipeline_template, template_directory):
        """Test pipeline template validation and processing."""
        # Save template to file
        template_file = template_directory / "pipeline_template.yaml"
        with open(template_file, 'w') as f:
            yaml.dump(pipeline_template, f)

        # Mock template validator and processor
        with patch('digitalmodel.infrastructure.validation.template_validator') as mock_validator, \
             patch('digitalmodel.analysis.pipeline_processor') as mock_processor:
            
            # Configure validator
            validation_result = {
                "valid": True,
                "errors": [],
                "warnings": [
                    "Large pipeline length may require segmented analysis"
                ],
                "schema_version": "2.1",
                "completeness": 0.95
            }
            mock_validator.validate_pipeline_template.return_value = validation_result
            
            # Configure processor
            processing_result = {
                "status": "success",
                "analysis_results": {
                    "max_stress": 380e6,
                    "max_strain": 0.0018,
                    "buckling_safety_factor": 2.1,
                    "fatigue_life": 45.6
                },
                "critical_locations": ["KP 12.5", "KP 28.3", "KP 41.7"],
                "recommendations": [
                    "Increase wall thickness at KP 12.5",
                    "Consider additional anchoring at KP 28.3"
                ]
            }
            mock_processor.process_pipeline_analysis.return_value = processing_result

            # Execute template validation
            validation = mock_validator.validate_pipeline_template(template_file)
            
            # Execute template processing if valid
            if validation["valid"]:
                results = mock_processor.process_pipeline_analysis(pipeline_template)

            # Validate results
            assert validation["valid"] is True
            assert len(validation["errors"]) == 0
            assert validation["completeness"] > 0.9
            
            assert results["status"] == "success"
            assert results["analysis_results"]["max_stress"] < pipeline_template["pipeline"]["material"]["yield_strength"]
            assert results["analysis_results"]["buckling_safety_factor"] > 1.5
            assert results["analysis_results"]["fatigue_life"] > 25.0  # Design life

    def test_plate_template_validation(self, plate_template, template_directory):
        """Test plate template validation and processing."""
        # Save template to file
        template_file = template_directory / "plate_template.yaml"
        with open(template_file, 'w') as f:
            yaml.dump(plate_template, f)

        # Mock template validator and processor
        with patch('digitalmodel.infrastructure.validation.template_validator') as mock_validator, \
             patch('digitalmodel.analysis.plate_processor') as mock_processor:
            
            # Configure validator
            validation_result = {
                "valid": True,
                "errors": [],
                "warnings": [],
                "schema_version": "1.8",
                "completeness": 1.0
            }
            mock_validator.validate_plate_template.return_value = validation_result
            
            # Configure processor
            processing_result = {
                "status": "success",
                "analysis_results": {
                    "ultimate_capacity": 750000,
                    "yield_capacity": 533000,
                    "buckling_capacity": 620000,
                    "governing_mode": "yield",
                    "utilization_ratio": 0.67,
                    "safety_margin": 1.49
                },
                "deflection": {
                    "maximum": 0.012,
                    "location": "center",
                    "allowable": 0.025
                }
            }
            mock_processor.process_plate_analysis.return_value = processing_result

            # Execute validation and processing
            validation = mock_validator.validate_plate_template(template_file)
            
            if validation["valid"]:
                results = mock_processor.process_plate_analysis(plate_template)

            # Validate results
            assert validation["valid"] is True
            assert validation["completeness"] == 1.0
            
            assert results["status"] == "success"
            assert results["analysis_results"]["utilization_ratio"] < 1.0
            assert results["analysis_results"]["safety_margin"] >= plate_template["loading"]["safety_factor"]
            assert results["deflection"]["maximum"] < results["deflection"]["allowable"]

    def test_fatigue_template_validation(self, fatigue_template, template_directory):
        """Test fatigue template validation and processing."""
        # Save template to file
        template_file = template_directory / "fatigue_template.yaml"
        with open(template_file, 'w') as f:
            yaml.dump(fatigue_template, f)

        # Mock template validator and processor
        with patch('digitalmodel.infrastructure.validation.template_validator') as mock_validator, \
             patch('digitalmodel.analysis.fatigue_processor') as mock_processor:
            
            # Configure validator
            validation_result = {
                "valid": True,
                "errors": [],
                "warnings": [
                    "High stress ranges detected - verify loading assumptions"
                ],
                "schema_version": "1.6",
                "completeness": 0.98
            }
            mock_validator.validate_fatigue_template.return_value = validation_result
            
            # Configure processor
            processing_result = {
                "status": "success",
                "fatigue_analysis": {
                    "total_damage": 0.72,
                    "fatigue_life": 34.7,
                    "safety_factor": 2.4,
                    "critical_detail": "chord_crown"
                },
                "damage_breakdown": {
                    "stress_range_10_MPa": 0.01,
                    "stress_range_20_MPa": 0.05,
                    "stress_range_30_MPa": 0.12,
                    "stress_range_40_MPa": 0.18,
                    "stress_range_50_MPa": 0.20,
                    "stress_range_60_MPa": 0.12,
                    "stress_range_70_MPa": 0.03,
                    "stress_range_80_MPa": 0.01
                },
                "hot_spot_stress_factor": 1.8
            }
            mock_processor.process_fatigue_analysis.return_value = processing_result

            # Execute validation and processing
            validation = mock_validator.validate_fatigue_template(template_file)
            
            if validation["valid"]:
                results = mock_processor.process_fatigue_analysis(fatigue_template)

            # Validate results
            assert validation["valid"] is True
            assert validation["completeness"] > 0.95
            
            assert results["status"] == "success"
            assert results["fatigue_analysis"]["total_damage"] < 1.0
            assert results["fatigue_analysis"]["fatigue_life"] >= fatigue_template["analysis_options"]["design_life"]
            assert results["fatigue_analysis"]["safety_factor"] >= fatigue_template["analysis_options"]["safety_factor"]
            
            # Validate damage breakdown sums to total
            damage_sum = sum(results["damage_breakdown"].values())
            assert abs(damage_sum - results["fatigue_analysis"]["total_damage"]) < 0.01

    def test_template_cross_validation(self, pipeline_template, plate_template, fatigue_template):
        """Test validation across multiple related templates."""
        templates = {
            "pipeline": pipeline_template,
            "plate": plate_template,
            "fatigue": fatigue_template
        }

        # Mock cross-validator
        with patch('digitalmodel.infrastructure.validation.cross_validator') as mock_cross_validator:
            
            cross_validation_result = {
                "consistent": True,
                "material_compatibility": True,
                "unit_consistency": True,
                "safety_factor_alignment": True,
                "inconsistencies": [],
                "recommendations": [
                    "Consider using same material grade across all analyses",
                    "Verify environmental conditions are consistent"
                ]
            }
            mock_cross_validator.validate_template_consistency.return_value = cross_validation_result

            # Execute cross-validation
            cross_validation = mock_cross_validator.validate_template_consistency(templates)

            # Validate cross-validation results
            assert cross_validation["consistent"] is True
            assert cross_validation["material_compatibility"] is True
            assert cross_validation["unit_consistency"] is True
            assert len(cross_validation["inconsistencies"]) == 0

    def test_template_error_handling(self, template_directory):
        """Test template validation error handling."""
        # Create invalid template
        invalid_template = {
            "analysis_type": "pipeline",
            "pipeline": {
                "geometry": {
                    "outer_diameter": -0.5,  # Invalid negative value
                    "wall_thickness": 0.0,   # Invalid zero thickness
                    "length": "invalid"      # Invalid type
                }
            }
        }
        
        template_file = template_directory / "invalid_template.yaml"
        with open(template_file, 'w') as f:
            yaml.dump(invalid_template, f)

        # Mock validator with error detection
        with patch('digitalmodel.infrastructure.validation.template_validator') as mock_validator:
            
            validation_result = {
                "valid": False,
                "errors": [
                    "outer_diameter must be positive",
                    "wall_thickness must be greater than zero",
                    "length must be numeric"
                ],
                "warnings": [],
                "schema_version": "2.1",
                "completeness": 0.3
            }
            mock_validator.validate_pipeline_template.return_value = validation_result

            # Execute validation
            validation = mock_validator.validate_pipeline_template(template_file)

            # Validate error detection
            assert validation["valid"] is False
            assert len(validation["errors"]) == 3
            assert "outer_diameter must be positive" in validation["errors"]
            assert "wall_thickness must be greater than zero" in validation["errors"]
            assert "length must be numeric" in validation["errors"]
            assert validation["completeness"] < 0.5

    def test_template_version_compatibility(self, pipeline_template, template_directory):
        """Test template version compatibility checking."""
        # Create template with different version
        old_version_template = pipeline_template.copy()
        old_version_template["template_version"] = "1.0"
        old_version_template["deprecated_field"] = "old_value"
        
        template_file = template_directory / "old_version_template.yaml"
        with open(template_file, 'w') as f:
            yaml.dump(old_version_template, f)

        # Mock version compatibility checker
        with patch('digitalmodel.infrastructure.validation.version_checker') as mock_version_checker:
            
            compatibility_result = {
                "compatible": True,
                "current_version": "2.1",
                "template_version": "1.0",
                "migration_required": True,
                "deprecated_fields": ["deprecated_field"],
                "missing_fields": ["analysis_options.include_thermal_effects"],
                "migration_warnings": [
                    "deprecated_field will be ignored",
                    "Missing thermal effects option - defaulting to True"
                ]
            }
            mock_version_checker.check_compatibility.return_value = compatibility_result

            # Execute version compatibility check
            compatibility = mock_version_checker.check_compatibility(template_file)

            # Validate compatibility results
            assert compatibility["compatible"] is True
            assert compatibility["migration_required"] is True
            assert "deprecated_field" in compatibility["deprecated_fields"]
            assert len(compatibility["migration_warnings"]) > 0

    def test_template_performance_validation(self, pipeline_template):
        """Test template validation performance characteristics."""
        import time
        
        # Mock performance validator
        with patch('digitalmodel.infrastructure.validation.performance_validator') as mock_perf_validator:
            
            def slow_validation(*args, **kwargs):
                time.sleep(0.01)  # Simulate validation work
                return {
                    "valid": True,
                    "validation_time": 0.01,
                    "complexity_score": 7.5,
                    "estimated_runtime": 120.0  # seconds
                }
            
            mock_perf_validator.validate_performance.side_effect = slow_validation

            # Measure validation performance
            start_time = time.time()
            validation = mock_perf_validator.validate_performance(pipeline_template)
            validation_time = time.time() - start_time

            # Validate performance characteristics
            assert validation["valid"] is True
            assert validation_time < 0.1  # Should be fast
            assert validation["complexity_score"] > 0
            assert validation["estimated_runtime"] > 0


@pytest.mark.integration
class TestTemplateIntegration:
    """Test integration of templates with analysis workflows."""

    def test_template_to_workflow_integration(self, pipeline_template):
        """Test integration of template with complete analysis workflow."""
        # Mock workflow executor
        with patch('digitalmodel.workflow.executor') as mock_executor:
            
            workflow_result = {
                "status": "completed",
                "execution_time": 245.6,
                "stages_completed": [
                    "input_validation",
                    "preprocessing",
                    "structural_analysis", 
                    "fatigue_analysis",
                    "postprocessing",
                    "report_generation"
                ],
                "results_summary": {
                    "overall_status": "pass",
                    "critical_issues": 0,
                    "warnings": 2,
                    "recommendations": 3
                }
            }
            mock_executor.execute_workflow.return_value = workflow_result

            # Execute template-driven workflow
            result = mock_executor.execute_workflow(template=pipeline_template)

            # Validate workflow integration
            assert result["status"] == "completed"
            assert len(result["stages_completed"]) == 6
            assert result["results_summary"]["overall_status"] == "pass"
            assert result["results_summary"]["critical_issues"] == 0

    def test_template_parameter_sensitivity(self, pipeline_template):
        """Test sensitivity of results to template parameter variations."""
        # Create parameter variations
        parameter_variations = [
            {"parameter": "wall_thickness", "factor": 0.9},
            {"parameter": "wall_thickness", "factor": 1.1},
            {"parameter": "operating_pressure", "factor": 0.8},
            {"parameter": "operating_pressure", "factor": 1.2}
        ]

        # Mock sensitivity analyzer
        with patch('digitalmodel.analysis.sensitivity_analyzer') as mock_sensitivity:
            
            sensitivity_results = {
                "base_case": {"max_stress": 250e6, "safety_factor": 1.8},
                "variations": {
                    "wall_thickness_0.9": {"max_stress": 278e6, "safety_factor": 1.62},
                    "wall_thickness_1.1": {"max_stress": 227e6, "safety_factor": 1.98},
                    "pressure_0.8": {"max_stress": 200e6, "safety_factor": 2.25},
                    "pressure_1.2": {"max_stress": 300e6, "safety_factor": 1.5}
                },
                "sensitivity_factors": {
                    "wall_thickness": 0.89,  # High sensitivity
                    "operating_pressure": 1.0   # Direct proportional
                }
            }
            mock_sensitivity.analyze_sensitivity.return_value = sensitivity_results

            # Execute sensitivity analysis
            sensitivity = mock_sensitivity.analyze_sensitivity(
                template=pipeline_template,
                variations=parameter_variations
            )

            # Validate sensitivity results
            assert "base_case" in sensitivity
            assert "variations" in sensitivity
            assert "sensitivity_factors" in sensitivity
            
            # Check that variations show expected trends
            base_stress = sensitivity["base_case"]["max_stress"]
            thin_wall_stress = sensitivity["variations"]["wall_thickness_0.9"]["max_stress"]
            thick_wall_stress = sensitivity["variations"]["wall_thickness_1.1"]["max_stress"]
            
            assert thin_wall_stress > base_stress  # Thinner wall = higher stress
            assert thick_wall_stress < base_stress  # Thicker wall = lower stress
