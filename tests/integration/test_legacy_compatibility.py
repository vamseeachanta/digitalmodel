"""
Legacy compatibility integration tests.

This module ensures that migrated modules maintain compatibility with
legacy data formats, APIs, and workflows.
"""

import pytest
import sys
import json
import pickle
import csv
import numpy as np
import pandas as pd
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open
import tempfile
from typing import Dict, Any, List

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


@pytest.mark.integration
class TestLegacyDataCompatibility:
    """Test compatibility with legacy data formats and structures."""

    @pytest.fixture
    def legacy_pipeline_data(self):
        """Legacy pipeline data format."""
        return {
            "version": "1.2",
            "created": "2020-03-15",
            "pipe_data": {
                "D": 0.508,  # Old naming convention
                "t": 0.0254,
                "L": 50000.0,
                "material_props": {
                    "Fy": 450e6,
                    "Fu": 535e6,
                    "E": 210e9,
                    "nu": 0.3
                }
            },
            "env_data": {
                "h_w": 150.0,  # Water depth
                "H_s": 8.5,    # Significant wave height
                "T_p": 12.0,   # Peak period
                "v_c": 1.2     # Current velocity
            },
            "analysis_flags": {
                "do_lateral_buckling": True,
                "do_upheaval_buckling": True,
                "do_fatigue": True
            }
        }

    @pytest.fixture
    def legacy_plate_data(self):
        """Legacy plate analysis data format."""
        return {
            "version": "0.9",
            "plate_props": {
                "a": 4.0,  # Length
                "b": 3.0,  # Width
                "t": 0.02, # Thickness
                "mat": "AH36"
            },
            "loads": {
                "p": 50000,  # Pressure
                "type": 1    # Load type code
            },
            "bc": 1,  # Boundary condition code
            "options": {
                "buckling": 1,
                "large_def": 0
            }
        }

    @pytest.fixture
    def legacy_fatigue_data(self):
        """Legacy fatigue analysis data format."""
        return {
            "version": "2.0",
            "joint_data": {
                "type": "T",
                "D": 1.0,    # Chord diameter
                "t": 0.05,   # Chord thickness
                "d": 0.6,    # Brace diameter
                "tb": 0.032, # Brace thickness
                "material": "S355"
            },
            "stress_data": {
                "ranges": [10, 20, 30, 40, 50, 60, 70, 80],
                "cycles": [1e7, 5e6, 2e6, 1e6, 5e5, 2e5, 1e5, 5e4],
                "mean": 50.0
            },
            "environment": "seawater",
            "design_life": 25
        }

    def test_legacy_pipeline_data_migration(self, legacy_pipeline_data, temp_directory):
        """Test migration of legacy pipeline data to new format."""
        # Save legacy data
        legacy_file = temp_directory / "legacy_pipeline.json"
        with open(legacy_file, 'w') as f:
            json.dump(legacy_pipeline_data, f)

        # Mock legacy data migrator
        with patch('digitalmodel.migration.legacy_migrator') as mock_migrator:
            
            # Configure migrator to convert legacy format
            migrated_data = {
                "analysis_type": "pipeline",
                "version": "3.0",
                "migrated_from": "1.2",
                "pipeline": {
                    "geometry": {
                        "outer_diameter": 0.508,  # Mapped from D
                        "wall_thickness": 0.0254,  # Mapped from t
                        "length": 50000.0          # Mapped from L
                    },
                    "material": {
                        "yield_strength": 450e6,    # Mapped from Fy
                        "ultimate_strength": 535e6,  # Mapped from Fu
                        "elastic_modulus": 210e9,    # Mapped from E
                        "poisson_ratio": 0.3         # Mapped from nu
                    }
                },
                "environment": {
                    "water_depth": 150.0,      # Mapped from h_w
                    "wave_height_significant": 8.5,  # Mapped from H_s
                    "wave_period_peak": 12.0,       # Mapped from T_p
                    "current_velocity_surface": 1.2  # Mapped from v_c
                },
                "analysis_options": {
                    "include_lateral_buckling": True,   # Mapped from do_lateral_buckling
                    "include_upheaval_buckling": True,  # Mapped from do_upheaval_buckling
                    "include_fatigue": True             # Mapped from do_fatigue
                },
                "migration_log": [
                    "Converted D to outer_diameter",
                    "Converted t to wall_thickness",
                    "Mapped material properties to new structure",
                    "Updated analysis flags to new naming"
                ]
            }
            mock_migrator.migrate_pipeline_data.return_value = migrated_data

            # Execute migration
            result = mock_migrator.migrate_pipeline_data(legacy_file)

            # Validate migration
            assert result["analysis_type"] == "pipeline"
            assert result["migrated_from"] == "1.2"
            assert result["pipeline"]["geometry"]["outer_diameter"] == legacy_pipeline_data["pipe_data"]["D"]
            assert result["pipeline"]["material"]["yield_strength"] == legacy_pipeline_data["pipe_data"]["material_props"]["Fy"]
            assert result["environment"]["water_depth"] == legacy_pipeline_data["env_data"]["h_w"]
            assert len(result["migration_log"]) > 0

    def test_legacy_plate_data_migration(self, legacy_plate_data, temp_directory):
        """Test migration of legacy plate data to new format."""
        # Save legacy data
        legacy_file = temp_directory / "legacy_plate.json"
        with open(legacy_file, 'w') as f:
            json.dump(legacy_plate_data, f)

        # Mock legacy data migrator
        with patch('digitalmodel.migration.legacy_migrator') as mock_migrator:
            
            # Configure migrator to convert legacy format
            migrated_data = {
                "analysis_type": "plate",
                "version": "2.5",
                "migrated_from": "0.9",
                "plate_geometry": {
                    "length": 4.0,     # Mapped from a
                    "width": 3.0,      # Mapped from b
                    "thickness": 0.02  # Mapped from t
                },
                "material": {
                    "grade": "AH36"  # Mapped from mat
                },
                "loading": {
                    "type": "pressure",      # Decoded from type code 1
                    "magnitude": 50000       # Mapped from p
                },
                "boundary_conditions": {
                    "type": "simply_supported"  # Decoded from bc code 1
                },
                "analysis_options": {
                    "include_buckling": True,         # Decoded from buckling flag 1
                    "include_large_deflection": False # Decoded from large_def flag 0
                },
                "migration_log": [
                    "Converted a,b,t to length,width,thickness",
                    "Decoded load type code 1 to pressure",
                    "Decoded boundary condition code 1 to simply_supported",
                    "Converted option flags to boolean values"
                ]
            }
            mock_migrator.migrate_plate_data.return_value = migrated_data

            # Execute migration
            result = mock_migrator.migrate_plate_data(legacy_file)

            # Validate migration
            assert result["analysis_type"] == "plate"
            assert result["migrated_from"] == "0.9"
            assert result["plate_geometry"]["length"] == legacy_plate_data["plate_props"]["a"]
            assert result["loading"]["type"] == "pressure"  # Decoded from code 1
            assert result["boundary_conditions"]["type"] == "simply_supported"  # Decoded from code 1
            assert result["analysis_options"]["include_buckling"] is True  # Converted from 1

    def test_legacy_fatigue_data_migration(self, legacy_fatigue_data, temp_directory):
        """Test migration of legacy fatigue data to new format."""
        # Save legacy data
        legacy_file = temp_directory / "legacy_fatigue.json"
        with open(legacy_file, 'w') as f:
            json.dump(legacy_fatigue_data, f)

        # Mock legacy data migrator
        with patch('digitalmodel.migration.legacy_migrator') as mock_migrator:
            
            # Configure migrator to convert legacy format
            migrated_data = {
                "analysis_type": "fatigue",
                "version": "2.8",
                "migrated_from": "2.0",
                "geometry": {
                    "joint_type": "tubular_T_joint",    # Expanded from "T"
                    "chord_diameter": 1.0,              # Mapped from D
                    "chord_thickness": 0.05,            # Mapped from t
                    "brace_diameter": 0.6,              # Mapped from d
                    "brace_thickness": 0.032,           # Mapped from tb
                    "beta_ratio": 0.6,                  # Calculated d/D
                    "gamma_ratio": 20.0                 # Calculated D/(2*t)
                },
                "material": {
                    "grade": "S355",
                    "fatigue_class": "G"  # Default for S355
                },
                "loading": {
                    "stress_history": {
                        "type": "rainflow_spectrum",
                        "stress_ranges": [10, 20, 30, 40, 50, 60, 70, 80],
                        "cycle_counts": [1e7, 5e6, 2e6, 1e6, 5e5, 2e5, 1e5, 5e4]
                    },
                    "mean_stress": 50.0
                },
                "environment": {
                    "condition": "seawater_with_cathodic_protection"  # Expanded from "seawater"
                },
                "analysis_options": {
                    "design_life": 25
                },
                "migration_log": [
                    "Expanded joint type T to tubular_T_joint",
                    "Calculated geometric ratios",
                    "Assigned default fatigue class for S355",
                    "Expanded environment to full description"
                ]
            }
            mock_migrator.migrate_fatigue_data.return_value = migrated_data

            # Execute migration
            result = mock_migrator.migrate_fatigue_data(legacy_file)

            # Validate migration
            assert result["analysis_type"] == "fatigue"
            assert result["migrated_from"] == "2.0"
            assert result["geometry"]["joint_type"] == "tubular_T_joint"  # Expanded from "T"
            assert result["geometry"]["beta_ratio"] == 0.6  # Calculated d/D
            assert result["geometry"]["gamma_ratio"] == 20.0  # Calculated D/(2*t)
            assert result["environment"]["condition"] == "seawater_with_cathodic_protection"

    def test_legacy_csv_data_import(self, temp_directory):
        """Test import of legacy CSV data files."""
        # Create legacy CSV data
        csv_data = [
            ["Time", "Stress_MPa", "Load_kN", "Displacement_mm"],
            [0.0, 50.0, 100.0, 0.0],
            [1.0, 75.0, 150.0, 2.5],
            [2.0, 60.0, 120.0, 1.8],
            [3.0, 85.0, 170.0, 3.2],
            [4.0, 45.0, 90.0, 0.5]
        ]
        
        csv_file = temp_directory / "legacy_data.csv"
        with open(csv_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerows(csv_data)

        # Mock CSV importer
        with patch('digitalmodel.import.csv_importer') as mock_importer:
            
            imported_data = {
                "format": "time_series",
                "source": "legacy_csv",
                "data": {
                    "time": [0.0, 1.0, 2.0, 3.0, 4.0],
                    "stress": [50e6, 75e6, 60e6, 85e6, 45e6],    # Converted to Pa
                    "load": [100e3, 150e3, 120e3, 170e3, 90e3],  # Converted to N
                    "displacement": [0.0, 0.0025, 0.0018, 0.0032, 0.0005]  # Converted to m
                },
                "units": {
                    "time": "s",
                    "stress": "Pa",
                    "load": "N",
                    "displacement": "m"
                },
                "conversion_log": [
                    "Converted stress from MPa to Pa",
                    "Converted load from kN to N",
                    "Converted displacement from mm to m"
                ]
            }
            mock_importer.import_csv_data.return_value = imported_data

            # Execute import
            result = mock_importer.import_csv_data(csv_file)

            # Validate import
            assert result["format"] == "time_series"
            assert result["source"] == "legacy_csv"
            assert len(result["data"]["time"]) == 5
            assert result["data"]["stress"][1] == 75e6  # 75 MPa converted to Pa
            assert result["data"]["load"][1] == 150e3   # 150 kN converted to N
            assert "conversion_log" in result

    def test_legacy_pickle_data_import(self, temp_directory):
        """Test import of legacy pickle data files."""
        # Create legacy pickle data
        legacy_analysis_results = {
            "max_stress": 250,  # MPa (old units)
            "deflection": 25,   # mm (old units)
            "safety_factor": 1.8,
            "analysis_date": "2019-06-12",
            "version": "legacy_v1.5"
        }
        
        pickle_file = temp_directory / "legacy_results.pkl"
        with open(pickle_file, 'wb') as f:
            pickle.dump(legacy_analysis_results, f)

        # Mock pickle importer
        with patch('digitalmodel.import.pickle_importer') as mock_importer:
            
            imported_data = {
                "format": "analysis_results",
                "source": "legacy_pickle",
                "original_version": "legacy_v1.5",
                "results": {
                    "max_stress": 250e6,  # Converted to Pa
                    "max_displacement": 0.025,  # Converted to m
                    "safety_factor": 1.8,
                    "analysis_date": "2019-06-12"
                },
                "conversion_applied": True,
                "conversion_log": [
                    "Converted max_stress from MPa to Pa",
                    "Converted deflection to max_displacement in meters"
                ]
            }
            mock_importer.import_pickle_data.return_value = imported_data

            # Execute import
            result = mock_importer.import_pickle_data(pickle_file)

            # Validate import
            assert result["format"] == "analysis_results"
            assert result["original_version"] == "legacy_v1.5"
            assert result["results"]["max_stress"] == 250e6  # Converted units
            assert result["results"]["max_displacement"] == 0.025  # Converted units
            assert result["conversion_applied"] is True

    def test_legacy_api_compatibility(self):
        """Test that legacy API calls still work with compatibility layer."""
        # Mock legacy API compatibility layer
        with patch('digitalmodel.legacy.api_compatibility') as mock_api:
            
            # Configure legacy API responses
            mock_api.calculate_pipe_stress.return_value = {
                "stress_MPa": 250.0,  # Legacy units
                "utilization": 0.56,
                "status": "OK"
            }
            
            mock_api.analyze_plate_capacity.return_value = {
                "capacity_kN": 750.0,  # Legacy units
                "deflection_mm": 12.5,  # Legacy units
                "mode": "yield"
            }

            # Test legacy API calls
            pipe_result = mock_api.calculate_pipe_stress(
                diameter=508,      # mm (legacy units)
                thickness=25.4,    # mm (legacy units)
                pressure=150       # bar (legacy units)
            )
            
            plate_result = mock_api.analyze_plate_capacity(
                length=4000,       # mm (legacy units)
                width=3000,        # mm (legacy units)
                thickness=20,      # mm (legacy units)
                load=50            # kN/m² (legacy units)
            )

            # Validate legacy API compatibility
            assert pipe_result["stress_MPa"] == 250.0
            assert pipe_result["status"] == "OK"
            assert plate_result["capacity_kN"] == 750.0
            assert plate_result["deflection_mm"] == 12.5

    def test_legacy_configuration_migration(self, temp_directory):
        """Test migration of legacy configuration files."""
        # Create legacy configuration
        legacy_config = """
[ANALYSIS]
type = pipeline
version = 1.0

[GEOMETRY]
diameter_mm = 508
thickness_mm = 25.4
length_m = 50000

[MATERIAL]
grade = X65
fy_mpa = 450
fu_mpa = 535

[OPTIONS]
lateral_buckling = yes
fatigue = yes
report_format = pdf
        """
        
        config_file = temp_directory / "legacy_config.ini"
        config_file.write_text(legacy_config)

        # Mock configuration migrator
        with patch('digitalmodel.migration.config_migrator') as mock_migrator:
            
            migrated_config = {
                "analysis_type": "pipeline",
                "version": "3.0",
                "migrated_from": "1.0",
                "pipeline": {
                    "geometry": {
                        "outer_diameter": 0.508,    # Converted from mm to m
                        "wall_thickness": 0.0254,   # Converted from mm to m
                        "length": 50000.0
                    },
                    "material": {
                        "grade": "API_5L_X65",       # Expanded grade name
                        "yield_strength": 450e6,     # Converted from MPa to Pa
                        "ultimate_strength": 535e6   # Converted from MPa to Pa
                    }
                },
                "analysis_options": {
                    "include_lateral_buckling": True,  # Converted yes to True
                    "include_fatigue": True           # Converted yes to True
                },
                "output": {
                    "report_format": "pdf"
                },
                "migration_notes": [
                    "Converted dimensions from mm to m",
                    "Expanded material grade name",
                    "Converted yes/no to boolean values",
                    "Updated configuration structure"
                ]
            }
            mock_migrator.migrate_config.return_value = migrated_config

            # Execute migration
            result = mock_migrator.migrate_config(config_file)

            # Validate migration
            assert result["analysis_type"] == "pipeline"
            assert result["migrated_from"] == "1.0"
            assert result["pipeline"]["geometry"]["outer_diameter"] == 0.508
            assert result["pipeline"]["material"]["grade"] == "API_5L_X65"
            assert result["analysis_options"]["include_lateral_buckling"] is True
            assert len(result["migration_notes"]) > 0

    def test_backward_compatibility_validation(self):
        """Test that migrated modules maintain backward compatibility."""
        # Mock backward compatibility checker
        with patch('digitalmodel.validation.compatibility_checker') as mock_checker:
            
            compatibility_report = {
                "overall_compatibility": 95.2,  # Percentage
                "api_compatibility": {
                    "legacy_v1": 100.0,
                    "legacy_v2": 98.5,
                    "legacy_v3": 92.1
                },
                "data_format_compatibility": {
                    "json_v1": 100.0,
                    "csv_legacy": 100.0,
                    "pickle_legacy": 95.0,
                    "ini_config": 98.0
                },
                "breaking_changes": [
                    "Removed deprecated function legacy_calculate_old()",
                    "Changed default units from imperial to SI"
                ],
                "migration_paths": {
                    "v1_to_v3": "automatic",
                    "v2_to_v3": "automatic",
                    "csv_to_json": "supported",
                    "ini_to_yaml": "supported"
                },
                "recommendations": [
                    "Update legacy API calls to use new interface",
                    "Migrate configuration files to YAML format",
                    "Update unit specifications in input files"
                ]
            }
            mock_checker.check_backward_compatibility.return_value = compatibility_report

            # Execute compatibility check
            report = mock_checker.check_backward_compatibility()

            # Validate compatibility report
            assert report["overall_compatibility"] > 90.0
            assert all(score >= 90.0 for score in report["api_compatibility"].values())
            assert all(score >= 90.0 for score in report["data_format_compatibility"].values())
            assert "migration_paths" in report
            assert "recommendations" in report


@pytest.mark.integration
class TestLegacyWorkflowCompatibility:
    """Test compatibility with legacy analysis workflows."""

    def test_legacy_batch_processing(self):
        """Test compatibility with legacy batch processing workflows."""
        # Mock legacy batch processor
        with patch('digitalmodel.legacy.batch_processor') as mock_processor:
            
            # Configure legacy batch processing
            batch_results = {
                "processed_files": 15,
                "successful": 14,
                "failed": 1,
                "results": [
                    {"file": "pipe_001.dat", "status": "success", "max_stress": 245e6},
                    {"file": "pipe_002.dat", "status": "success", "max_stress": 267e6},
                    {"file": "pipe_003.dat", "status": "failed", "error": "Invalid geometry"}
                ],
                "summary": {
                    "avg_stress": 256e6,
                    "max_stress": 289e6,
                    "success_rate": 0.933
                }
            }
            mock_processor.process_legacy_batch.return_value = batch_results

            # Execute legacy batch processing
            results = mock_processor.process_legacy_batch(
                input_directory="/legacy/data",
                file_pattern="*.dat",
                analysis_type="pipeline"
            )

            # Validate batch processing
            assert results["processed_files"] == 15
            assert results["successful"] == 14
            assert results["success_rate"] > 0.9
            assert len(results["results"]) == 3

    def test_legacy_report_generation(self):
        """Test compatibility with legacy report generation."""
        # Mock legacy report generator
        with patch('digitalmodel.legacy.report_generator') as mock_generator:
            
            report_result = {
                "report_generated": True,
                "format": "pdf",
                "pages": 25,
                "sections": [
                    "executive_summary",
                    "input_data",
                    "analysis_results",
                    "conclusions",
                    "appendices"
                ],
                "legacy_format_maintained": True,
                "file_path": "/output/legacy_report.pdf"
            }
            mock_generator.generate_legacy_report.return_value = report_result

            # Execute legacy report generation
            report = mock_generator.generate_legacy_report(
                analysis_results={"max_stress": 250e6},
                template="legacy_pipeline_template",
                output_format="pdf"
            )

            # Validate report generation
            assert report["report_generated"] is True
            assert report["legacy_format_maintained"] is True
            assert report["format"] == "pdf"
            assert "executive_summary" in report["sections"]

    def test_legacy_database_integration(self):
        """Test integration with legacy database systems."""
        # Mock legacy database interface
        with patch('digitalmodel.legacy.database_interface') as mock_db:
            
            # Configure legacy database operations
            mock_db.connect_legacy_db.return_value = True
            mock_db.query_legacy_data.return_value = [
                {"project_id": "PROJ_001", "analysis_date": "2020-01-15", "status": "completed"},
                {"project_id": "PROJ_002", "analysis_date": "2020-02-10", "status": "completed"},
                {"project_id": "PROJ_003", "analysis_date": "2020-03-05", "status": "pending"}
            ]
            mock_db.insert_legacy_results.return_value = {"inserted": True, "record_id": "REC_001"}

            # Execute legacy database operations
            connection = mock_db.connect_legacy_db("legacy_analysis_db")
            data = mock_db.query_legacy_data("SELECT * FROM projects WHERE year = 2020")
            insert_result = mock_db.insert_legacy_results(
                table="analysis_results",
                data={"project_id": "PROJ_004", "max_stress": 275e6}
            )

            # Validate database operations
            assert connection is True
            assert len(data) == 3
            assert data[0]["project_id"] == "PROJ_001"
            assert insert_result["inserted"] is True
            assert "record_id" in insert_result
