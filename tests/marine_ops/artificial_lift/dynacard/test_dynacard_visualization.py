# ABOUTME: Tests for the dynacard visualization package.
# ABOUTME: Validates SVG output for cards, gallery, schematic, and annotations.

import xml.etree.ElementTree as ET
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import ALL_GENERATORS
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.svg_primitives import (
    COLORS,
    MODE_LABELS,
    MODE_TIERS,
    MODES_BY_TIER,
    TIER_INFO,
    CoordMapper,
    draw_card_axes,
    svg_footer,
    svg_header,
    svg_line,
    svg_polyline,
    svg_rect,
    svg_text,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.card_renderer import (
    CardRenderer,
    generate_card,
    render_all_individual_cards,
    render_single_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.gallery_renderer import (
    GalleryRenderer,
    generate_gallery,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.rod_pump_schematic import (
    RodPumpSchematicGenerator,
    generate_rod_pump_schematic,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.diagnostic_annotator import (
    DiagnosticAnnotator,
    generate_sample_diagnostic,
)


# ---------------------------------------------------------------------------
# Helper to validate SVG XML
# ---------------------------------------------------------------------------


def _parse_svg(svg_str: str) -> ET.Element:
    """Parse SVG string and return root element."""
    return ET.fromstring(svg_str)


def _assert_valid_svg(svg_str: str) -> ET.Element:
    """Assert that the string is valid SVG XML and return root element."""
    root = _parse_svg(svg_str)
    assert root.tag == "{http://www.w3.org/2000/svg}svg" or root.tag == "svg"
    return root


# ---------------------------------------------------------------------------
# Tests: svg_primitives
# ---------------------------------------------------------------------------


class TestCoordMapper:
    """Tests for the CoordMapper coordinate mapping."""

    def test_x_mapping_identity(self):
        mapper = CoordMapper(0, 100, 0, 100, 0, 100, 0, 100)
        assert mapper.x(0) == pytest.approx(0.0)
        assert mapper.x(50) == pytest.approx(50.0)
        assert mapper.x(100) == pytest.approx(100.0)

    def test_y_mapping_inverted(self):
        mapper = CoordMapper(0, 100, 0, 100, 0, 100, 0, 100)
        # SVG y is inverted: world 0 -> svg 100, world 100 -> svg 0
        assert mapper.y(0) == pytest.approx(100.0)
        assert mapper.y(100) == pytest.approx(0.0)

    def test_x_mapping_with_offset(self):
        mapper = CoordMapper(0, 10, 0, 10, 50, 150, 20, 120)
        assert mapper.x(0) == pytest.approx(50.0)
        assert mapper.x(10) == pytest.approx(150.0)
        assert mapper.x(5) == pytest.approx(100.0)

    def test_y_mapping_with_offset(self):
        mapper = CoordMapper(0, 10, 0, 10, 50, 150, 20, 120)
        assert mapper.y(0) == pytest.approx(120.0)
        assert mapper.y(10) == pytest.approx(20.0)

    def test_zero_range_handled(self):
        mapper = CoordMapper(5, 5, 3, 3, 0, 100, 0, 100)
        # Should not crash, scale defaults to 1.0
        result = mapper.x(5)
        assert isinstance(result, float)


class TestSvgPrimitives:
    """Tests for SVG primitive functions."""

    def test_svg_header_contains_xmlns(self):
        header = svg_header(800, 600)
        assert 'xmlns="http://www.w3.org/2000/svg"' in header
        assert 'width="800"' in header
        assert 'height="600"' in header

    def test_svg_footer_closes_tag(self):
        assert svg_footer() == "</svg>"

    def test_svg_line_produces_valid_xml(self):
        line = svg_line(10, 20, 30, 40, "#ff0000", 2.0)
        assert "<line" in line
        assert 'stroke="#ff0000"' in line

    def test_svg_line_with_dash(self):
        line = svg_line(0, 0, 100, 100, "#000", dash="5,3")
        assert 'stroke-dasharray="5,3"' in line

    def test_svg_polyline_points(self):
        pts = [(10, 20), (30, 40), (50, 60)]
        result = svg_polyline(pts, "#000")
        assert "<polyline" in result
        assert "10.0,20.0" in result

    def test_svg_polygon_when_closed(self):
        pts = [(10, 20), (30, 40), (50, 60)]
        result = svg_polyline(pts, "#000", close=True)
        assert "<polygon" in result

    def test_svg_text_content(self):
        result = svg_text(100, 200, "Hello", size=14)
        assert "Hello" in result
        assert 'font-size="14"' in result
        assert 'font-family="sans-serif"' in result

    def test_svg_rect_attributes(self):
        result = svg_rect(10, 20, 100, 50, "#333", fill="#fff")
        assert "<rect" in result
        assert 'fill="#fff"' in result

    def test_svg_rect_with_rx(self):
        result = svg_rect(0, 0, 50, 50, "#000", rx=5)
        assert 'rx="5.0"' in result

    def test_draw_card_axes_produces_content(self):
        mapper = CoordMapper(0, 100, 0, 20000, 50, 250, 30, 180)
        result = draw_card_axes(mapper)
        assert "Position" in result
        assert "Load" in result
        assert "<line" in result


class TestColorScheme:
    """Tests for color constants."""

    def test_all_colors_are_hex(self):
        for name, color in COLORS.items():
            assert color.startswith("#"), f"Color {name} = {color} is not hex"
            assert len(color) == 7, f"Color {name} = {color} is not #RRGGBB"

    def test_tier_info_complete(self):
        assert set(TIER_INFO.keys()) == {1, 2, 3}
        for tier_num, info in TIER_INFO.items():
            assert "label" in info
            assert "color" in info

    def test_all_18_modes_have_tiers(self):
        assert len(MODE_TIERS) == 18
        for mode, tier in MODE_TIERS.items():
            assert tier in {1, 2, 3}

    def test_all_18_modes_have_labels(self):
        assert len(MODE_LABELS) == 18

    def test_modes_by_tier_sums_to_18(self):
        total = sum(len(modes) for modes in MODES_BY_TIER.values())
        assert total == 18

    def test_mode_tiers_match_modes_by_tier(self):
        for tier_num, modes in MODES_BY_TIER.items():
            for mode in modes:
                assert MODE_TIERS[mode] == tier_num


# ---------------------------------------------------------------------------
# Tests: card_renderer
# ---------------------------------------------------------------------------


class TestCardRenderer:
    """Tests for single card rendering."""

    def test_render_normal_card_is_valid_svg(self):
        card = generate_card("NORMAL")
        renderer = CardRenderer()
        svg = renderer.render(card, "NORMAL")
        _assert_valid_svg(svg)

    def test_render_contains_polyline(self):
        card = generate_card("NORMAL")
        renderer = CardRenderer()
        svg = renderer.render(card, "NORMAL")
        assert "<polygon" in svg or "<polyline" in svg

    @pytest.mark.parametrize("mode_name", list(ALL_GENERATORS.keys()))
    def test_render_all_18_modes_valid_svg(self, mode_name):
        card = generate_card(mode_name)
        renderer = CardRenderer()
        svg = renderer.render(card, mode_name)
        _assert_valid_svg(svg)

    @pytest.mark.parametrize("mode_name", list(ALL_GENERATORS.keys()))
    def test_render_compact_all_18_modes(self, mode_name):
        card = generate_card(mode_name)
        renderer = CardRenderer()
        frag = renderer.render_compact(card, mode_name)
        assert "<polygon" in frag or "<polyline" in frag
        assert MODE_LABELS[mode_name] in frag

    def test_render_single_card_writes_file(self, tmp_path):
        out = tmp_path / "test_card.svg"
        path = render_single_card("FLUID_POUND", out)
        assert path.exists()
        content = path.read_text()
        _assert_valid_svg(content)

    def test_render_all_individual_cards(self, tmp_path):
        out_dir = tmp_path / "cards"
        paths = render_all_individual_cards(out_dir)
        assert len(paths) == 18
        for p in paths:
            assert p.exists()
            _assert_valid_svg(p.read_text())

    def test_generate_card_unknown_mode_raises(self):
        with pytest.raises(ValueError, match="Unknown failure mode"):
            generate_card("NONEXISTENT_MODE")


# ---------------------------------------------------------------------------
# Tests: gallery_renderer
# ---------------------------------------------------------------------------


class TestGalleryRenderer:
    """Tests for the 18-card diagnostic gallery."""

    def test_gallery_is_valid_svg(self):
        gallery = GalleryRenderer()
        svg = gallery.render()
        _assert_valid_svg(svg)

    def test_gallery_contains_all_18_mode_labels(self):
        gallery = GalleryRenderer()
        svg = gallery.render()
        for mode_name, label in MODE_LABELS.items():
            assert label in svg, f"Missing label for {mode_name}: {label}"

    def test_gallery_contains_tier_headers(self):
        from xml.sax.saxutils import escape
        gallery = GalleryRenderer()
        svg = gallery.render()
        for tier_num in [1, 2, 3]:
            # Label may be XML-escaped (e.g. & -> &amp;)
            assert escape(TIER_INFO[tier_num]["label"]) in svg

    def test_gallery_has_18_card_traces(self):
        gallery = GalleryRenderer()
        svg = gallery.render()
        # Each card produces a polygon (closed polyline)
        polygon_count = svg.count("<polygon")
        assert polygon_count >= 18

    def test_generate_gallery_writes_file(self, tmp_path):
        out = tmp_path / "gallery.svg"
        path = generate_gallery(out)
        assert path.exists()
        _assert_valid_svg(path.read_text())


# ---------------------------------------------------------------------------
# Tests: rod_pump_schematic
# ---------------------------------------------------------------------------


class TestRodPumpSchematic:
    """Tests for the rod pump system schematic."""

    def test_schematic_is_valid_svg(self):
        gen = RodPumpSchematicGenerator()
        svg = gen.render()
        _assert_valid_svg(svg)

    def test_schematic_contains_component_labels(self):
        gen = RodPumpSchematicGenerator()
        svg = gen.render()
        expected_labels = [
            "Pumping Unit",
            "Polished",
            "Rod String",
            "Tubing",
            "Casing",
            "Traveling Valve",
            "Standing Valve",
            "Pump Barrel",
            "Reservoir",
            "Perforations",
        ]
        for label in expected_labels:
            assert label in svg, f"Missing label: {label}"

    def test_schematic_contains_card(self):
        gen = RodPumpSchematicGenerator()
        svg = gen.render()
        assert "Normal Dynamometer Card" in svg
        # Should contain a card trace polygon
        assert "<polygon" in svg or "<polyline" in svg

    def test_schematic_contains_data_flow(self):
        gen = RodPumpSchematicGenerator()
        svg = gen.render()
        assert "Dynamometer Data" in svg

    def test_generate_schematic_writes_file(self, tmp_path):
        out = tmp_path / "schematic.svg"
        path = generate_rod_pump_schematic(out)
        assert path.exists()
        _assert_valid_svg(path.read_text())


# ---------------------------------------------------------------------------
# Tests: diagnostic_annotator
# ---------------------------------------------------------------------------


class TestDiagnosticAnnotator:
    """Tests for annotated diagnostic card rendering."""

    def test_annotated_card_is_valid_svg(self):
        card = generate_card("FLUID_POUND")
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, "FLUID_POUND")
        _assert_valid_svg(svg)

    def test_annotated_card_contains_classification(self):
        card = generate_card("FLUID_POUND")
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, "FLUID_POUND")
        assert "Fluid Pound" in svg
        assert "Classification" in svg

    def test_annotated_card_contains_confidence(self):
        card = generate_card("FLUID_POUND")
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, "FLUID_POUND", confidence=0.95)
        assert "95.0%" in svg

    def test_annotated_card_contains_differential(self):
        card = generate_card("FLUID_POUND")
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, "FLUID_POUND")
        assert "Differential Diagnosis" in svg
        assert "Gas Interference" in svg

    def test_annotated_card_model_info(self):
        card = generate_card("FLUID_POUND")
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, "FLUID_POUND")
        assert "GradientBoosting" in svg
        assert "Bezerra" in svg

    @pytest.mark.parametrize("mode_name", ["NORMAL", "GAS_INTERFERENCE", "PUMP_TAGGING", "ROD_PARTING"])
    def test_annotated_card_multiple_modes(self, mode_name):
        card = generate_card(mode_name)
        annotator = DiagnosticAnnotator()
        svg = annotator.render(card, mode_name)
        _assert_valid_svg(svg)
        assert MODE_LABELS[mode_name] in svg

    def test_generate_sample_diagnostic_writes_file(self, tmp_path):
        out = tmp_path / "diagnostic.svg"
        path = generate_sample_diagnostic(out)
        assert path.exists()
        _assert_valid_svg(path.read_text())


# ---------------------------------------------------------------------------
# Tests: Integration (public API)
# ---------------------------------------------------------------------------


class TestVisualizationPublicAPI:
    """Tests for the visualization package public API."""

    def test_import_from_package(self):
        from digitalmodel.marine_ops.artificial_lift.dynacard.visualization import (
            COLORS,
            CardRenderer,
            DiagnosticAnnotator,
            GalleryRenderer,
            MODE_LABELS,
            RodPumpSchematicGenerator,
            generate_gallery,
            generate_rod_pump_schematic,
            generate_sample_diagnostic,
            render_all_individual_cards,
        )
        assert len(COLORS) > 0
        assert len(MODE_LABELS) == 18
