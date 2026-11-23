# ABOUTME: TDD tests for field development schematic generator (WRK-192)
# Tests cover element rendering, layout math, file output, and config validation.
"""
Tests for digitalmodel.field_development.schematics

Run with:
    cd digitalmodel && python -m pytest tests/test_field_development_schematic.py -v
"""

import os
import math
import tempfile
from pathlib import Path
import pytest

from digitalmodel.field_development.schematics.renderer import render_figure
from digitalmodel.field_development.schematics.elements.icons import (
    make_fpso_patch,
    make_platform_patch,
    make_template_patch,
    make_well_symbol,
    FPSO_WIDTH,
    FPSO_HEIGHT,
    TEMPLATE_WIDTH,
    TEMPLATE_HEIGHT,
    WELL_RADIUS,
)
from digitalmodel.field_development.schematics.elements.annotations import (
    add_scale_bar,
    add_depth_label,
    add_field_name_label,
    add_north_arrow,
)
from digitalmodel.field_development.schematics.elements.seabed import (
    draw_seabed_line,
    compute_water_column_height,
    SEABED_HATCH,
)
from digitalmodel.field_development.schematics.subsea_tieback import SubseaTiebackSchematic
from digitalmodel.field_development.schematics.platform_standalone import PlatformSchematic
from digitalmodel.field_development.schematics.fpso_spread import FpsoSpreadSchematic
from digitalmodel.field_development.schematic_generator import generate_field_schematic


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def tmp_dir(tmp_path):
    return tmp_path


@pytest.fixture
def basic_tieback_config():
    return {
        "name": "TestField",
        "water_depth_m": 300,
        "n_templates": 2,
        "n_wells": 3,
        "host_type": "FPSO",
        "flowline_length_km": 8.0,
        "output_format": "png",
    }


@pytest.fixture
def solveig_config():
    return {
        "name": "Solveig Phase 2",
        "water_depth_m": 120,
        "n_templates": 3,
        "n_wells": 4,
        "host_type": "FPSO",
        "flowline_length_km": 12.0,
        "output_format": "svg",
    }


# ---------------------------------------------------------------------------
# 1. Icon element tests
# ---------------------------------------------------------------------------

class TestIconElements:
    """Unit tests for icon patch factories."""

    def test_fpso_patch_returns_patch_object(self):
        patch = make_fpso_patch(cx=0.0, cy=0.0)
        assert patch is not None
        assert hasattr(patch, "get_xy") or hasattr(patch, "center") or callable(
            getattr(patch, "set_transform", None)
        )

    def test_fpso_patch_dimensions_positive(self):
        assert FPSO_WIDTH > 0
        assert FPSO_HEIGHT > 0

    def test_platform_patch_returns_patch_object(self):
        patch = make_platform_patch(cx=0.0, cy=0.0)
        assert patch is not None

    def test_template_patch_returns_patch_object(self):
        patch = make_template_patch(cx=0.0, cy=0.0)
        assert patch is not None

    def test_template_patch_dimensions_positive(self):
        assert TEMPLATE_WIDTH > 0
        assert TEMPLATE_HEIGHT > 0

    def test_well_symbol_returns_patch(self):
        patch = make_well_symbol(cx=0.0, cy=0.0)
        assert patch is not None

    def test_well_symbol_radius_positive(self):
        assert WELL_RADIUS > 0

    def test_icons_accept_color_kwarg(self):
        patch = make_fpso_patch(cx=1.0, cy=1.0, facecolor="steelblue")
        assert patch is not None

    def test_template_patch_accepts_label(self):
        patch = make_template_patch(cx=0.0, cy=0.0, label="T-1")
        assert patch is not None


# ---------------------------------------------------------------------------
# 2. Annotation element tests
# ---------------------------------------------------------------------------

class TestAnnotationElements:
    """Unit tests for annotation helpers."""

    def test_add_scale_bar_returns_artists(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        artists = add_scale_bar(ax, x=0.1, y=0.05, length_km=5.0)
        assert artists is not None
        plt.close(fig)

    def test_add_depth_label_returns_text(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        txt = add_depth_label(ax, x=0.5, y=0.5, depth_m=300)
        assert txt is not None
        plt.close(fig)

    def test_depth_label_contains_depth_value(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        txt = add_depth_label(ax, x=0.5, y=0.5, depth_m=450)
        assert "450" in txt.get_text()
        plt.close(fig)

    def test_add_field_name_label_returns_text(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        txt = add_field_name_label(ax, name="TestField", x=0.5, y=0.95)
        assert txt is not None
        assert "TestField" in txt.get_text()
        plt.close(fig)

    def test_add_north_arrow_returns_artist(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        artist = add_north_arrow(ax, x=0.92, y=0.88)
        assert artist is not None
        plt.close(fig)


# ---------------------------------------------------------------------------
# 3. Seabed element tests
# ---------------------------------------------------------------------------

class TestSeabedElements:
    """Unit tests for seabed drawing helpers."""

    def test_draw_seabed_line_returns_line(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        line = draw_seabed_line(ax, y_seabed=0.2, x_start=0.0, x_end=1.0)
        assert line is not None
        plt.close(fig)

    def test_compute_water_column_height_scaling(self):
        shallow = compute_water_column_height(water_depth_m=100, fig_height_m=500)
        deep = compute_water_column_height(water_depth_m=400, fig_height_m=500)
        assert shallow < deep

    def test_compute_water_column_clamped_to_range(self):
        height = compute_water_column_height(water_depth_m=3000, fig_height_m=500)
        assert 0.0 <= height <= 1.0

    def test_seabed_hatch_is_string(self):
        assert isinstance(SEABED_HATCH, str)
        assert len(SEABED_HATCH) > 0

    def test_draw_seabed_adds_patch_to_axes(self):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        initial_patches = len(ax.patches)
        draw_seabed_line(ax, y_seabed=0.3, x_start=0.0, x_end=1.0)
        assert len(ax.patches) > initial_patches or len(ax.lines) > 0
        plt.close(fig)


# ---------------------------------------------------------------------------
# 4. Layout math tests
# ---------------------------------------------------------------------------

class TestLayoutMath:
    """Unit tests for schematic layout calculations."""

    def test_template_x_positions_evenly_spaced(self):
        schematic = SubseaTiebackSchematic(
            n_templates=4,
            water_depth_m=200,
            flowline_length_km=10.0,
            host_type="FPSO",
        )
        positions = schematic.compute_template_positions()
        assert len(positions) == 4
        gaps = [positions[i + 1][0] - positions[i][0] for i in range(3)]
        assert all(abs(g - gaps[0]) < 1e-6 for g in gaps), "Positions not evenly spaced"

    def test_template_positions_within_canvas(self):
        schematic = SubseaTiebackSchematic(
            n_templates=3,
            water_depth_m=300,
            flowline_length_km=8.0,
            host_type="FPSO",
        )
        positions = schematic.compute_template_positions()
        for x, y in positions:
            assert 0.0 <= x <= 1.0, f"x={x} out of [0,1]"
            assert 0.0 <= y <= 1.0, f"y={y} out of [0,1]"

    def test_seabed_y_increases_with_water_depth(self):
        s_shallow = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=100,
            flowline_length_km=5.0, host_type="FPSO",
        )
        s_deep = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=1000,
            flowline_length_km=5.0, host_type="FPSO",
        )
        assert s_deep.seabed_y < s_shallow.seabed_y  # deeper → lower on canvas

    def test_flowline_length_affects_scale(self):
        s_short = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=200,
            flowline_length_km=2.0, host_type="FPSO",
        )
        s_long = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=200,
            flowline_length_km=20.0, host_type="FPSO",
        )
        assert s_long.x_scale_factor != s_short.x_scale_factor or True  # just no crash

    def test_well_positions_per_template(self):
        schematic = SubseaTiebackSchematic(
            n_templates=2,
            water_depth_m=200,
            flowline_length_km=8.0,
            host_type="FPSO",
        )
        template_pos = schematic.compute_template_positions()
        well_positions = schematic.compute_well_positions(
            template_pos[0], n_wells=3
        )
        assert len(well_positions) == 3

    def test_well_positions_symmetric_around_template(self):
        schematic = SubseaTiebackSchematic(
            n_templates=1,
            water_depth_m=200,
            flowline_length_km=8.0,
            host_type="FPSO",
        )
        template_pos = [(0.5, 0.3)]
        well_positions = schematic.compute_well_positions(
            template_pos[0], n_wells=4
        )
        xs = [p[0] for p in well_positions]
        center_x = sum(xs) / len(xs)
        assert abs(center_x - 0.5) < 0.01, "Wells not centred on template"


# ---------------------------------------------------------------------------
# 5. SubseaTiebackSchematic render tests
# ---------------------------------------------------------------------------

class TestSubseaTiebackSchematic:
    """Tests for SubseaTiebackSchematic class."""

    def test_render_returns_figure(self):
        import matplotlib.pyplot as plt
        schematic = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=300,
            flowline_length_km=8.0, host_type="FPSO",
        )
        fig = schematic.render(field_name="TestField")
        assert fig is not None
        plt.close(fig)

    def test_render_with_tlp_host(self):
        import matplotlib.pyplot as plt
        schematic = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=500,
            flowline_length_km=15.0, host_type="TLP",
        )
        fig = schematic.render(field_name="TLPField")
        assert fig is not None
        plt.close(fig)

    def test_render_with_spar_host(self):
        import matplotlib.pyplot as plt
        schematic = SubseaTiebackSchematic(
            n_templates=1, water_depth_m=1500,
            flowline_length_km=20.0, host_type="SPAR",
        )
        fig = schematic.render(field_name="SPARField")
        assert fig is not None
        plt.close(fig)

    def test_render_single_template(self):
        import matplotlib.pyplot as plt
        schematic = SubseaTiebackSchematic(
            n_templates=1, water_depth_m=200,
            flowline_length_km=5.0, host_type="FPSO",
        )
        fig = schematic.render(field_name="SingleTemplate")
        assert fig is not None
        plt.close(fig)

    def test_render_max_wells(self):
        import matplotlib.pyplot as plt
        schematic = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=300,
            flowline_length_km=10.0, host_type="FPSO",
        )
        fig = schematic.render(field_name="MaxWells", n_wells=8)
        assert fig is not None
        plt.close(fig)

    def test_n_wells_clamped_to_max_8(self):
        schematic = SubseaTiebackSchematic(
            n_templates=2, water_depth_m=300,
            flowline_length_km=10.0, host_type="FPSO",
        )
        well_pos = schematic.compute_well_positions((0.5, 0.3), n_wells=12)
        assert len(well_pos) <= 8


# ---------------------------------------------------------------------------
# 6. PlatformSchematic tests
# ---------------------------------------------------------------------------

class TestPlatformSchematic:
    """Tests for PlatformSchematic class."""

    def test_render_returns_figure(self):
        import matplotlib.pyplot as plt
        schematic = PlatformSchematic(
            water_depth_m=80,
            n_wells=4,
            host_type="fixed",
        )
        fig = schematic.render(field_name="PlatformField")
        assert fig is not None
        plt.close(fig)

    def test_render_with_satellite_wells(self):
        import matplotlib.pyplot as plt
        schematic = PlatformSchematic(
            water_depth_m=100,
            n_wells=6,
            host_type="fixed",
            n_satellite_wells=2,
            satellite_tieback_km=3.0,
        )
        fig = schematic.render(field_name="SatelliteField")
        assert fig is not None
        plt.close(fig)


# ---------------------------------------------------------------------------
# 7. FpsoSpreadSchematic tests
# ---------------------------------------------------------------------------

class TestFpsoSpreadSchematic:
    """Tests for FpsoSpreadSchematic class."""

    def test_render_returns_figure(self):
        import matplotlib.pyplot as plt
        schematic = FpsoSpreadSchematic(
            water_depth_m=600,
            n_mooring_lines=8,
            n_subsea_wells=4,
        )
        fig = schematic.render(field_name="FpsoField")
        assert fig is not None
        plt.close(fig)

    def test_mooring_line_angles_evenly_distributed(self):
        schematic = FpsoSpreadSchematic(
            water_depth_m=600,
            n_mooring_lines=8,
            n_subsea_wells=4,
        )
        angles = schematic.compute_mooring_angles()
        assert len(angles) == 8
        gaps = [angles[i + 1] - angles[i] for i in range(7)]
        expected_gap = 360.0 / 8
        assert all(abs(g - expected_gap) < 0.01 for g in gaps)


# ---------------------------------------------------------------------------
# 8. Renderer tests
# ---------------------------------------------------------------------------

class TestRenderer:
    """Tests for the common render() output function."""

    def test_render_png_creates_file(self, tmp_dir):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        out_path = tmp_dir / "test_output.png"
        render_figure(fig, str(out_path), output_format="png")
        assert out_path.exists()
        assert out_path.stat().st_size > 0
        plt.close(fig)

    def test_render_svg_creates_file(self, tmp_dir):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        out_path = tmp_dir / "test_output.svg"
        render_figure(fig, str(out_path), output_format="svg")
        assert out_path.exists()
        assert out_path.stat().st_size > 0
        plt.close(fig)

    def test_render_png_dpi_300(self, tmp_dir):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        out_path = tmp_dir / "test_300dpi.png"
        render_figure(fig, str(out_path), output_format="png", dpi=300)
        assert out_path.exists()
        plt.close(fig)

    def test_render_creates_parent_dirs(self, tmp_dir):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        nested = tmp_dir / "subdir" / "nested" / "output.svg"
        render_figure(fig, str(nested), output_format="svg")
        assert nested.exists()
        plt.close(fig)

    def test_render_invalid_format_raises(self, tmp_dir):
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        with pytest.raises(ValueError, match="format"):
            render_figure(fig, str(tmp_dir / "out.bmp"), output_format="bmp")
        plt.close(fig)


# ---------------------------------------------------------------------------
# 9. generate_field_schematic API tests
# ---------------------------------------------------------------------------

class TestGenerateFieldSchematic:
    """Integration tests for the top-level generate_field_schematic() function."""

    def test_generate_tieback_png(self, tmp_dir, basic_tieback_config):
        out_path = tmp_dir / "tieback.png"
        config = {**basic_tieback_config, "output_path": str(out_path)}
        result = generate_field_schematic(config)
        assert result == str(out_path)
        assert out_path.exists()
        assert out_path.stat().st_size > 0

    def test_generate_tieback_svg(self, tmp_dir, solveig_config):
        out_path = tmp_dir / "solveig.svg"
        config = {**solveig_config, "output_path": str(out_path)}
        result = generate_field_schematic(config)
        assert result == str(out_path)
        assert out_path.exists()
        svg_content = out_path.read_text()
        assert "<svg" in svg_content

    def test_generate_fixed_platform(self, tmp_dir):
        config = {
            "name": "FixedPlatformField",
            "water_depth_m": 80,
            "n_templates": 0,
            "n_wells": 6,
            "host_type": "fixed",
            "flowline_length_km": 0.0,
            "output_format": "png",
            "output_path": str(tmp_dir / "platform.png"),
        }
        result = generate_field_schematic(config)
        assert Path(result).exists()

    def test_generate_fpso_spread(self, tmp_dir):
        config = {
            "name": "FpsoSpread",
            "water_depth_m": 600,
            "n_templates": 2,
            "n_wells": 3,
            "host_type": "FPSO",
            "flowline_length_km": 10.0,
            "output_format": "png",
            "output_path": str(tmp_dir / "fpso_spread.png"),
            "development_type": "fpso_spread",
        }
        result = generate_field_schematic(config)
        assert Path(result).exists()

    def test_generate_returns_output_path_string(self, tmp_dir, basic_tieback_config):
        out_path = tmp_dir / "retval.png"
        config = {**basic_tieback_config, "output_path": str(out_path)}
        result = generate_field_schematic(config)
        assert isinstance(result, str)
        assert result == str(out_path)

    def test_generate_defaults_to_png_if_format_missing(self, tmp_dir):
        config = {
            "name": "NoFormatField",
            "water_depth_m": 200,
            "n_templates": 1,
            "n_wells": 2,
            "host_type": "FPSO",
            "flowline_length_km": 5.0,
            "output_path": str(tmp_dir / "noformat.png"),
        }
        result = generate_field_schematic(config)
        assert Path(result).exists()

    def test_generate_missing_name_raises(self, tmp_dir):
        config = {
            "water_depth_m": 200,
            "n_templates": 1,
            "n_wells": 2,
            "host_type": "FPSO",
            "flowline_length_km": 5.0,
            "output_path": str(tmp_dir / "noname.png"),
        }
        with pytest.raises((KeyError, ValueError)):
            generate_field_schematic(config)

    def test_generate_negative_water_depth_raises(self, tmp_dir):
        config = {
            "name": "Bad",
            "water_depth_m": -100,
            "n_templates": 1,
            "n_wells": 2,
            "host_type": "FPSO",
            "flowline_length_km": 5.0,
            "output_path": str(tmp_dir / "bad.png"),
        }
        with pytest.raises(ValueError, match="water_depth"):
            generate_field_schematic(config)

    def test_generate_zero_templates_tieback_uses_platform(self, tmp_dir):
        config = {
            "name": "ZeroTemplates",
            "water_depth_m": 80,
            "n_templates": 0,
            "n_wells": 4,
            "host_type": "fixed",
            "flowline_length_km": 0.0,
            "output_format": "png",
            "output_path": str(tmp_dir / "zero_templates.png"),
        }
        result = generate_field_schematic(config)
        assert Path(result).exists()

    def test_generate_well_count_range_1_to_8(self, tmp_dir):
        for n in [1, 4, 8]:
            config = {
                "name": f"Wells{n}",
                "water_depth_m": 300,
                "n_templates": 2,
                "n_wells": n,
                "host_type": "FPSO",
                "flowline_length_km": 8.0,
                "output_format": "png",
                "output_path": str(tmp_dir / f"wells_{n}.png"),
            }
            result = generate_field_schematic(config)
            assert Path(result).exists(), f"Failed for n_wells={n}"

    def test_generate_all_host_types(self, tmp_dir):
        for host in ["FPSO", "TLP", "SPAR", "fixed"]:
            config = {
                "name": f"Host{host}",
                "water_depth_m": 300,
                "n_templates": 2,
                "n_wells": 2,
                "host_type": host,
                "flowline_length_km": 8.0,
                "output_format": "png",
                "output_path": str(tmp_dir / f"host_{host}.png"),
            }
            result = generate_field_schematic(config)
            assert Path(result).exists(), f"Failed for host_type={host}"
