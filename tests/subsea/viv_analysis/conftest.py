"""VIV analysis module test fixtures."""
import pytest
import numpy as np

@pytest.fixture
def riser_properties():
    """Riser properties for VIV analysis."""
    return {
        'length': 1500,  # m
        'outer_diameter': 0.3556,  # m (14")
        'wall_thickness': 0.0254,  # m (1")
        'material_density': 7850,  # kg/m3
        'youngs_modulus': 200e9,  # Pa
        'structural_damping': 0.01,
        'strake_coverage': 0.0,  # No strakes initially
        'strake_pitch': 17.5,  # pitch/diameter ratio
        'strake_height': 0.25,  # height/diameter ratio
    }

@pytest.fixture
def current_profile():
    """Current profile for VIV analysis."""
    depths = np.array([0, 50, 100, 500, 1000, 1500])
    velocities = np.array([1.5, 1.3, 1.0, 0.5, 0.3, 0.1])
    return {
        'depths': depths,
        'velocities': velocities,
    }

@pytest.fixture
def viv_parameters():
    """VIV analysis parameters."""
    return {
        'strouhal_number': 0.2,
        'lift_coefficient': 0.3,
        'drag_coefficient': 1.2,
        'added_mass_coefficient': 1.0,
        'frequency_range': (0.1, 10),  # Hz
        'mode_shapes': 10,  # Number of modes to consider
        'reduced_velocity_range': (2, 12),
    }