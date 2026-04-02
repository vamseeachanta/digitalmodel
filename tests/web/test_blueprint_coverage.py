# ABOUTME: Blueprint coverage tests — verifies all service modules exist and follow patterns.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1584).
"""
Tests for web blueprint module coverage.

Verifies that all expected service blueprint directories exist under
digitaltwinfeed/ and each follows the standard pattern:
- __init__.py present
- Main module file present
- Blueprint declaration pattern
"""

import os
import pytest


def _dtf_root() -> str:
    import digitalmodel.web
    return os.path.join(os.path.dirname(digitalmodel.web.__file__), "digitaltwinfeed")


# Full list of expected service blueprints from app.py
EXPECTED_BLUEPRINTS = [
    "example_blueprint",
    "todorestful",
    "bseedata",
    "example_SPA",
    "BuoySALM",
    "BuoyCALM",
    "BuoyCoupled",
    "FixedWindTurbine",
    "FloatingWindTurbine",
    "OffshorePipeline",
    "OffshoreRiser",
    "Floaters",
    "Metocean",
    "OnshorePipeline",
    "FFS",
    "Pipings",
    "PressureVessels",
    "Foundations",
    "Fatigue",
    "StockAnalysis",
    "PortfolioAnalysis",
    "PlotlyPlot",
    "AppMenuDropdown",
    "AppMenuTextboxSubmit",
    "GoMFields",
]


class TestBlueprintDirectoriesExist:
    """Verify that each expected blueprint directory exists."""

    @pytest.mark.parametrize("bp_name", EXPECTED_BLUEPRINTS)
    def test_blueprint_directory_exists(self, bp_name):
        bp_dir = os.path.join(_dtf_root(), bp_name)
        assert os.path.isdir(bp_dir), f"Missing blueprint directory: {bp_name}"


class TestBlueprintInitFiles:
    """Verify each blueprint has an __init__.py."""

    @pytest.mark.parametrize("bp_name", EXPECTED_BLUEPRINTS)
    def test_init_file_exists(self, bp_name):
        init_path = os.path.join(_dtf_root(), bp_name, "__init__.py")
        assert os.path.isfile(init_path), f"Missing __init__.py in {bp_name}"


class TestBlueprintMainModule:
    """Spot-check that key blueprints have their main module file."""

    KEY_MODULES = [
        ("todorestful", "todorestful.py"),
        ("BuoySALM", "BuoySALM.py"),
        ("GoMFields", "GoMFields.py"),
        ("Metocean", "Metocean.py"),
        ("StockAnalysis", "StockAnalysis.py"),
    ]

    @pytest.mark.parametrize("bp_name,module_file", KEY_MODULES)
    def test_main_module_exists(self, bp_name, module_file):
        path = os.path.join(_dtf_root(), bp_name, module_file)
        assert os.path.isfile(path), f"Missing {module_file} in {bp_name}"


class TestBlueprintPatterns:
    """Verify key modules use Flask Blueprint pattern."""

    BLUEPRINT_MODULES = [
        "Metocean/Metocean.py",
        "Foundations/Foundations.py",
        "Fatigue/Fatigue.py",
        "OffshorePipeline/OffshorePipeline.py",
    ]

    @pytest.mark.parametrize("relpath", BLUEPRINT_MODULES)
    def test_module_declares_blueprint(self, relpath):
        path = os.path.join(_dtf_root(), relpath)
        with open(path) as f:
            src = f.read()
        assert "Blueprint" in src, f"{relpath} does not declare a Flask Blueprint"
