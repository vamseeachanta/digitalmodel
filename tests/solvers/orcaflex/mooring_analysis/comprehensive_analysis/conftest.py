"""Pytest fixtures for comprehensive mooring analysis tests."""

import pytest
from pathlib import Path
import pandas as pd
import numpy as np
from datetime import datetime
import tempfile
import shutil

from digitalmodel.solvers.orcaflex.mooring_analysis.comprehensive_analysis.config import (
    AnalysisConfig,
    ConvergenceCriteria,
    StiffnessConfig,
    FenderConfig,
    GroupingConfig,
    ReportConfig,
    ProcessingConfig,
    IndustryStandards
)


@pytest.fixture
def sample_pretension_dataframe():
    """Create sample pretension analysis DataFrame."""
    data = {
        'ObjectName': ['Line01', 'Line02', 'Line03', 'Line04'],
        'target_tension': [80.0, 80.0, 80.0, 30.0],
        'line_length': ["[11.0, 17.1133]", "[11.0, 15.0214]", "[11.0, 8.913]", "[11.0, 7.6341]"],
        'line_EA': ["[1170.0, 47700.0]"] * 4,
        'section_to_be_modified': [1, 1, 1, 1],
        'calculated_length': [28.0364, 25.9094, 19.7159, 19.5312],
        'tolerance': [50, 50, 1, 50],
        'current_tension': [156.5384, 152.4219, 237.7607, 0.3193],
        'new_line_length': ["[11.0, 17.8604]", "[11.0, 15.7251]", "[11.0, 8.913]", "[11.0, 7.3503]"],
        'end_Gx_force': [60.6323, 57.1712, 158.8691, -0.1577],
        'end_Gy_force': [-141.6564, -138.2602, -169.5102, -0.1841],
        'end_Gz_force': [27.5943, 29.1206, 50.5674, 0.2079],
        'tension_diff_percent': [95.673, 90.5274, 197.2009, -98.9356],
        'converged_line_length_total': [28.8604, 26.7251, 19.913, 18.3503],
        'length_percent_change': [2.939, 3.1481, 0.9999, -6.0463],
        'filename_stem': ['fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof'] * 4
    }
    return pd.DataFrame(data)


@pytest.fixture
def sample_stiffness_dataframe():
    """Create sample stiffness analysis DataFrame."""
    data = {
        'ObjectName': ['Line01', 'Line02', 'Line03', 'Line04'],
        'k_axial': [1500.0, 1450.0, 1600.0, 500.0],
        'k_x': [250.5, 245.3, 280.2, 85.6],
        'k_x_signed': [250.5, -245.3, 280.2, -85.6],
        'k_x_positive': [250.5, 0.0, 280.2, 0.0],
        'k_x_negative': [0.0, 245.3, 0.0, 85.6],
        'k_y': [180.2, 175.8, 195.4, 62.3],
        'k_z': [320.6, 315.2, 345.8, 95.4],
        'k_xy': [12.5, -10.3, 15.6, -5.2],
        'k_xz': [8.3, -7.1, 9.8, -3.1],
        'k_yz': [6.2, -5.4, 7.3, -2.5],
        'cos_x': [0.707, -0.707, 0.707, -0.707],
        'cos_y': [0.707, 0.707, -0.707, -0.707],
        'cos_z': [0.0, 0.0, 0.0, 0.0],
        'Fx': [60.6, 57.2, 158.9, -0.16],
        'Fy': [-141.7, -138.3, -169.5, -0.18],
        'Fz': [27.6, 29.1, 50.6, 0.21],
        'filename_stem': ['fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof'] * 4
    }
    return pd.DataFrame(data)


@pytest.fixture
def sample_fender_dataframe():
    """Create sample fender force analysis DataFrame."""
    data = {
        'FenderID': ['F1', 'F2', 'F3', 'F4'],
        'Location_X': [10.0, -10.0, 10.0, -10.0],
        'Location_Y': [5.0, 5.0, -5.0, -5.0],
        'Location_Z': [0.0, 0.0, 0.0, 0.0],
        'Max_Force': [2500.0, 2300.0, 1800.0, 0.0],
        'Mean_Force': [1200.0, 1100.0, 850.0, 0.0],
        'Contact': [True, True, True, False],
        'Contact_Duration': [45.5, 42.3, 35.2, 0.0],
        'Compression': [0.85, 0.78, 0.62, 0.0],
        'Design_Capacity': [5000.0, 5000.0, 5000.0, 5000.0],
        'filename_stem': ['fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof'] * 4
    }
    return pd.DataFrame(data)


@pytest.fixture
def sample_config():
    """Create sample analysis configuration."""
    config = AnalysisConfig(
        input_directory=Path('./test_data'),
        output_directory=Path('./test_output'),
        file_pattern="*.csv",
        recursive=True
    )
    config.convergence.tolerance = 0.05
    config.stiffness.vessel_mass_tonnes = 150000.0
    config.fender.design_capacity_kN = {'F1': 5000, 'F2': 5000, 'F3': 5000, 'F4': 5000}
    return config


@pytest.fixture
def temp_directory():
    """Create a temporary directory for test files."""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    shutil.rmtree(temp_dir)


@pytest.fixture
def sample_csv_files(temp_directory, sample_pretension_dataframe, 
                     sample_stiffness_dataframe, sample_fender_dataframe):
    """Create sample CSV files in temporary directory."""
    # Save pretension CSV
    pretension_file = temp_directory / "pretension_analysis.csv"
    sample_pretension_dataframe.to_csv(pretension_file, index=False)
    
    # Save stiffness CSV
    stiffness_file = temp_directory / "stiffness_analysis.csv"
    sample_stiffness_dataframe.to_csv(stiffness_file, index=False)
    
    # Save fender CSV
    fender_file = temp_directory / "fender_analysis.csv"
    sample_fender_dataframe.to_csv(fender_file, index=False)
    
    return {
        'pretension': pretension_file,
        'stiffness': stiffness_file,
        'fender': fender_file
    }


@pytest.fixture
def real_csv_paths():
    """Provide paths to real CSV files from example folders."""
    return {
        'pretension': Path('D:/github/digitalmodel/tests/domains/orcaflex/mooring-tension-iteration/'
                          'fsts-l015-test-cases/output/collate/csv/'
                          'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv'),
        'stiffness': Path('D:/github/digitalmodel/tests/domains/orcaflex/mooring-tension-iteration/'
                         'fsts-l015-test-cases/output/collate/csv/'
                         'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_mooring_stiffness_analysis.csv'),
        'fender': Path('D:/github/digitalmodel/tests/domains/orcaflex/mooring-tension-iteration/'
                      'fsts-l015-test-cases/output/collate/csv/'
                      'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_fender_force_analysis.csv')
    }


@pytest.fixture
def invalid_dataframe():
    """Create DataFrame with invalid/missing columns."""
    return pd.DataFrame({
        'wrong_column': [1, 2, 3],
        'another_wrong': ['a', 'b', 'c']
    })


@pytest.fixture
def malformed_csv_file(temp_directory):
    """Create a malformed CSV file for error testing."""
    malformed_file = temp_directory / "malformed.csv"
    with open(malformed_file, 'w') as f:
        f.write("This is not,a valid,CSV\n")
        f.write("Missing columns and bad data\n")
        f.write("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20\n")
    return malformed_file