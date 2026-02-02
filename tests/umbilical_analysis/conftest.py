"""Umbilical analysis module test fixtures."""
import pytest

@pytest.fixture
def umbilical_properties():
    """Umbilical cable properties."""
    return {
        'outer_diameter': 0.150,  # m
        'weight_in_air': 120,  # kg/m
        'weight_in_water': 60,  # kg/m
        'minimum_bend_radius': 2.25,  # m
        'axial_stiffness': 5e8,  # N
        'bending_stiffness': 5000,  # N.m2
        'torsional_stiffness': 2000,  # N.m2/rad
        'components': {
            'power_cores': 3,
            'signal_pairs': 12,
            'fiber_optics': 4,
            'hydraulic_lines': 2,
        }
    }

@pytest.fixture
def installation_parameters():
    """Umbilical installation parameters."""
    return {
        'water_depth': 1000,  # m
        'lay_angle': 85,  # degrees
        'vessel_speed': 0.5,  # m/s
        'top_tension': 50,  # kN
        'touchdown_point': 1200,  # m from surface position
    }

@pytest.fixture
def termination_design():
    """Umbilical termination design."""
    return {
        'type': 'UTA',  # Umbilical Termination Assembly
        'bend_stiffener_length': 5,  # m
        'bend_stiffener_k_factor': 15,
        'hang_off_angle': 5,  # degrees
    }