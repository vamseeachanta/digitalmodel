"""DNV-RP-H103 code module test fixtures."""
import pytest

@pytest.fixture
def dnv_material_properties():
    """Material properties per DNV-RP-H103."""
    return {
        'steel_grade': 'X65',
        'yield_strength': 450e6,  # Pa
        'tensile_strength': 535e6,  # Pa
        'youngs_modulus': 207e9,  # Pa
        'poissons_ratio': 0.3,
        'design_factor': 0.72,
    }

@pytest.fixture
def dnv_load_cases():
    """Load cases for DNV-RP-H103 analysis."""
    return {
        'installation': {
            'safety_factor': 2.0,
            'load_factor': 1.3,
        },
        'operation': {
            'safety_factor': 1.5,
            'load_factor': 1.1,
        },
        'extreme': {
            'safety_factor': 1.1,
            'load_factor': 1.0,
        }
    }

@pytest.fixture
def circular_cross_section():
    """Circular cross-section properties."""
    return {
        'outer_diameter': 0.5,  # m
        'wall_thickness': 0.025,  # m
        'shape': 'circular'
    }

@pytest.fixture
def rectangular_cross_section():
    """Rectangular cross-section properties."""
    return {
        'width': 0.6,  # m
        'height': 0.4,  # m
        'thickness': 0.02,  # m
        'shape': 'rectangular'
    }