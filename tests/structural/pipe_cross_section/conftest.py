# ABOUTME: Pytest fixtures for pipe cross-section tests.
# ABOUTME: Provides reusable test configurations and pipe instances.

"""
Pytest Fixtures for Pipe Cross-Section Tests
=============================================

Provides standard test fixtures:
- pipe_config: Standard 24" coated pipe configuration
- pipe_instance: PipeCrossSection instance for testing
- expected_values: Dictionary of expected calculation results
"""

import pytest


@pytest.fixture
def pipe_config():
    """
    Standard 24-inch coated pipe configuration.

    Based on typical offshore pipeline specifications:
    - 24" x 0.5625" steel pipe (API 5L)
    - 3.5mm 3LPP anti-corrosion coating (ISO 21809)
    - 80mm concrete weight coating (DNV-ST-F101)
    """
    return {
        "steel_od_mm": 609.6,  # 24 inch
        "steel_wt_mm": 14.29,  # 0.5625 inch
        "steel_density": 7850,
        "lpp_thickness_mm": 3.5,
        "lpp_density": 1100,
        "concrete_thickness_mm": 80.0,
        "concrete_density": 3000,
        "seawater_density": 1025,
        "internal_contents": "air",
    }


@pytest.fixture
def pipe_config_no_coatings():
    """Bare steel pipe without coatings."""
    return {
        "steel_od_mm": 609.6,
        "steel_wt_mm": 14.29,
        "steel_density": 7850,
        "lpp_thickness_mm": 0,
        "lpp_density": 1100,
        "concrete_thickness_mm": 0,
        "concrete_density": 3000,
        "seawater_density": 1025,
        "internal_contents": "air",
    }


@pytest.fixture
def pipe_config_lpp_only():
    """Steel pipe with 3LPP coating only (no concrete)."""
    return {
        "steel_od_mm": 609.6,
        "steel_wt_mm": 14.29,
        "steel_density": 7850,
        "lpp_thickness_mm": 3.5,
        "lpp_density": 1100,
        "concrete_thickness_mm": 0,
        "concrete_density": 3000,
        "seawater_density": 1025,
        "internal_contents": "air",
    }


@pytest.fixture
def expected_values():
    """
    Expected calculation values for standard 24" coated pipe.

    These values are verified against manual calculations and
    industry standard software.
    """
    return {
        # Geometry
        "steel_id_mm": 581.0,
        "steel_od_mm": 609.6,
        "lpp_od_mm": 616.6,
        "concrete_od_mm": 776.6,
        "final_od_inch": 30.57,
        # Weights
        "steel_weight_kg_m": 209.8,
        "lpp_weight_kg_m": 7.4,
        "concrete_weight_kg_m": 525.2,
        "total_weight_kg_m": 742.4,
        # Buoyancy
        "displaced_volume_m3_m": 0.4737,
        "buoyancy_force_kg_m": 485.5,
        "submerged_weight_kg_m": 256.9,
    }


@pytest.fixture
def pipe_instance(pipe_config):
    """Create PipeCrossSection instance for testing."""
    from digitalmodel.pipe_cross_section import PipeCrossSection
    return PipeCrossSection(**pipe_config)


@pytest.fixture
def output_dir(tmp_path):
    """Temporary output directory for visualization tests."""
    output = tmp_path / "output"
    output.mkdir()
    return output
