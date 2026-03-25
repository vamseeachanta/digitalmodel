"""Pipeline module test fixtures."""
import pytest

@pytest.fixture
def pipeline_properties():
    """Sample pipeline properties."""
    return {
        'outer_diameter': 0.3239,  # m (12.75")
        'wall_thickness': 0.0159,  # m
        'material': 'API 5L X65',
        'youngs_modulus': 207e9,  # Pa
        'poissons_ratio': 0.3,
        'density': 7850,  # kg/m3
        'thermal_expansion': 1.17e-5,  # 1/K
    }

@pytest.fixture
def operating_conditions():
    """Sample operating conditions."""
    return {
        'pressure_internal': 10e6,  # Pa (10 MPa)
        'pressure_external': 2e6,   # Pa (2 MPa)
        'temperature_operating': 80,  # °C
        'temperature_installation': 10,  # °C
    }

@pytest.fixture
def soil_properties():
    """Sample soil properties for pipeline analysis."""
    return {
        'soil_type': 'clay',
        'undrained_shear_strength': 30e3,  # Pa
        'submerged_weight': 6000,  # N/m3
        'friction_coefficient': 0.5,
    }