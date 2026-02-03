"""Ship design module test fixtures."""
import pytest

@pytest.fixture
def ship_principal_dimensions():
    """Principal dimensions of a ship."""
    return {
        'length_overall': 320,  # m
        'length_between_perpendiculars': 310,  # m
        'beam': 58,  # m
        'depth': 30,  # m
        'design_draft': 20.8,  # m
        'scantling_draft': 21.5,  # m
        'displacement': 300000,  # tonnes
        'deadweight': 280000,  # tonnes
    }

@pytest.fixture
def ship_coefficients():
    """Ship form coefficients."""
    return {
        'block_coefficient': 0.82,
        'midship_coefficient': 0.98,
        'waterplane_coefficient': 0.88,
        'prismatic_coefficient': 0.84,
    }

@pytest.fixture
def structural_design():
    """Ship structural design parameters."""
    return {
        'frame_spacing': 0.85,  # m
        'double_bottom_height': 2.5,  # m
        'double_side_width': 3.0,  # m
        'deck_plating_thickness': 0.025,  # m
        'bottom_plating_thickness': 0.030,  # m
        'side_plating_thickness': 0.025,  # m
    }