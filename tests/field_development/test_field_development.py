# ABOUTME: Tests for digitalmodel.field_development package — imports, config, schematic generation.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1589).
"""
Tests for digitalmodel.field_development

Covers:
- Package import tests
- generate_field_schematic config validation
- SOLVEIG_PHASE2_CONFIG template
- _infer_development_type routing logic
- PlatformSchematic, SubseaTiebackSchematic, FpsoSpreadSchematic instantiation
- Edge cases: missing keys, invalid values
"""

import os
import tempfile
import pytest

matplotlib = pytest.importorskip("matplotlib")
matplotlib.use("Agg")


class TestPackageImport:
    """Verify field_development package is importable."""

    def test_import_package(self):
        import digitalmodel.field_development
        assert hasattr(digitalmodel.field_development, "generate_field_schematic")
        assert hasattr(digitalmodel.field_development, "SOLVEIG_PHASE2_CONFIG")

    def test_import_schematics_subpackage(self):
        from digitalmodel.field_development.schematics import (
            SubseaTiebackSchematic,
            PlatformSchematic,
            FpsoSpreadSchematic,
            render_figure,
        )
        assert SubseaTiebackSchematic is not None
        assert PlatformSchematic is not None
        assert FpsoSpreadSchematic is not None
        assert callable(render_figure)

    def test_import_elements(self):
        from digitalmodel.field_development.schematics.elements import icons, annotations, seabed
        assert hasattr(icons, "make_well_symbol")
        assert hasattr(annotations, "add_scale_bar")
        assert hasattr(seabed, "draw_seabed_line")


class TestSolveigConfig:
    """Validate the SOLVEIG_PHASE2_CONFIG template dict."""

    def test_config_has_required_keys(self):
        from digitalmodel.field_development import SOLVEIG_PHASE2_CONFIG
        required = {"name", "water_depth_m", "n_templates", "n_wells", "host_type"}
        assert required.issubset(SOLVEIG_PHASE2_CONFIG.keys())

    def test_config_values_are_sensible(self):
        from digitalmodel.field_development import SOLVEIG_PHASE2_CONFIG
        assert SOLVEIG_PHASE2_CONFIG["water_depth_m"] > 0
        assert SOLVEIG_PHASE2_CONFIG["n_templates"] >= 1
        assert SOLVEIG_PHASE2_CONFIG["n_wells"] >= 1
        assert SOLVEIG_PHASE2_CONFIG["host_type"] in {"FPSO", "TLP", "SPAR", "fixed"}


class TestInferDevelopmentType:
    """Test the _infer_development_type routing function."""

    def test_fixed_platform_inferred(self):
        from digitalmodel.field_development.schematic_generator import _infer_development_type
        result = _infer_development_type(n_templates=0, host_type="fixed", flowline_length_km=0)
        assert result == "platform"

    def test_jacket_platform_inferred(self):
        from digitalmodel.field_development.schematic_generator import _infer_development_type
        result = _infer_development_type(n_templates=2, host_type="jacket", flowline_length_km=5)
        assert result == "platform"

    def test_subsea_tieback_inferred(self):
        from digitalmodel.field_development.schematic_generator import _infer_development_type
        result = _infer_development_type(n_templates=3, host_type="FPSO", flowline_length_km=12)
        assert result == "subsea_tieback"


class TestSchematicInstantiation:
    """Test that schematic classes can be instantiated with valid params."""

    def test_platform_schematic(self):
        from digitalmodel.field_development.schematics import PlatformSchematic
        ps = PlatformSchematic(water_depth_m=100, n_wells=4, host_type="fixed")
        assert ps.water_depth_m == 100.0
        assert ps.n_wells == 4
        assert ps.host_type == "fixed"

    def test_platform_schematic_caps_wells(self):
        from digitalmodel.field_development.schematics import PlatformSchematic
        ps = PlatformSchematic(water_depth_m=80, n_wells=20, host_type="fixed")
        assert ps.n_wells <= 8  # capped at _MAX_CONDUCTORS

    def test_subsea_tieback_schematic(self):
        from digitalmodel.field_development.schematics import SubseaTiebackSchematic
        st = SubseaTiebackSchematic(
            n_templates=3, water_depth_m=120, flowline_length_km=12.0, host_type="FPSO"
        )
        assert st.n_templates == 3
        assert st.water_depth_m == 120.0

    def test_fpso_spread_schematic(self):
        from digitalmodel.field_development.schematics import FpsoSpreadSchematic
        fs = FpsoSpreadSchematic(water_depth_m=500, n_mooring_lines=8, n_subsea_wells=12)
        assert fs.water_depth_m == 500.0
        assert fs.n_mooring_lines == 8

    def test_fpso_spread_mooring_angles(self):
        from digitalmodel.field_development.schematics import FpsoSpreadSchematic
        fs = FpsoSpreadSchematic(water_depth_m=500, n_mooring_lines=4, n_subsea_wells=4)
        angles = fs.compute_mooring_angles()
        assert len(angles) == 4
        assert angles[0] == 0.0
        assert angles[1] == pytest.approx(90.0)


class TestGenerateFieldSchematic:
    """Test the top-level generate_field_schematic API."""

    def test_missing_name_raises_key_error(self):
        from digitalmodel.field_development import generate_field_schematic
        with pytest.raises(KeyError):
            generate_field_schematic({"water_depth_m": 100, "output_path": "/tmp/x.png"})

    def test_missing_output_path_raises_key_error(self):
        from digitalmodel.field_development import generate_field_schematic
        with pytest.raises(KeyError):
            generate_field_schematic({"name": "Test", "water_depth_m": 100})

    def test_negative_water_depth_raises_value_error(self):
        from digitalmodel.field_development import generate_field_schematic
        with pytest.raises(ValueError, match="positive"):
            generate_field_schematic({
                "name": "Test",
                "water_depth_m": -10,
                "output_path": "/tmp/x.png",
            })

    def test_invalid_format_raises_value_error(self):
        from digitalmodel.field_development import generate_field_schematic
        with pytest.raises(ValueError, match="output_format"):
            generate_field_schematic({
                "name": "Test",
                "water_depth_m": 100,
                "output_path": "/tmp/x.png",
                "output_format": "bmp",
            })

    def test_successful_generation_svg(self):
        from digitalmodel.field_development import generate_field_schematic
        with tempfile.TemporaryDirectory() as tmpdir:
            out = os.path.join(tmpdir, "test_output.svg")
            result = generate_field_schematic({
                "name": "Unit Test Field",
                "water_depth_m": 120,
                "n_templates": 3,
                "n_wells": 4,
                "host_type": "FPSO",
                "flowline_length_km": 12.0,
                "output_format": "svg",
                "output_path": out,
            })
            assert os.path.isfile(result)

    def test_successful_generation_platform(self):
        from digitalmodel.field_development import generate_field_schematic
        with tempfile.TemporaryDirectory() as tmpdir:
            out = os.path.join(tmpdir, "platform.png")
            result = generate_field_schematic({
                "name": "Platform Test",
                "water_depth_m": 60,
                "n_templates": 0,
                "n_wells": 6,
                "host_type": "fixed",
                "output_format": "png",
                "output_path": out,
            })
            assert os.path.isfile(result)
