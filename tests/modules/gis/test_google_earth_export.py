"""Tests for GoogleEarthExporter — styled KML export for Google Earth."""

from __future__ import annotations

from pathlib import Path

from lxml import etree

from digitalmodel.modules.gis.integrations.google_earth_export import (
    GoogleEarthExporter,
)
from digitalmodel.modules.gis.layers.feature_layer import FeatureLayer
from digitalmodel.modules.gis.layers.well_layer import WellLayer

KML_NS = "http://www.opengis.net/kml/2.2"


def _tag(name: str) -> str:
    return f"{{{KML_NS}}}{name}"


# ---------------------------------------------------------------------------
# 1. export_wells — basic KML output
# ---------------------------------------------------------------------------


class TestExportWells:
    """Verify export_wells creates a valid KML with correct placemarks."""

    def test_creates_kml_file(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        result = GoogleEarthExporter.export_wells(layer, out)

        assert result.exists()
        assert result.suffix == ".kml"

    def test_correct_number_of_placemarks(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        GoogleEarthExporter.export_wells(layer, out)

        tree = etree.parse(str(out))
        placemarks = tree.getroot().iter(_tag("Placemark"))
        assert len(list(placemarks)) == len(layer)

    def test_placemarks_contain_descriptions(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        GoogleEarthExporter.export_wells(layer, out)

        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            desc = pm.find(_tag("description"))
            assert desc is not None
            assert "<table>" in desc.text

    def test_placemarks_have_point_geometry(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        GoogleEarthExporter.export_wells(layer, out)

        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            point = pm.find(_tag("Point"))
            assert point is not None
            coords = point.find(_tag("coordinates"))
            assert coords is not None
            assert "," in coords.text

    def test_works_with_well_layer(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        result = GoogleEarthExporter.export_wells(layer, out)

        assert result.exists()
        tree = etree.parse(str(out))
        placemarks = list(tree.getroot().iter(_tag("Placemark")))
        assert len(placemarks) == layer.well_count

    def test_returns_resolved_path(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells.kml"
        result = GoogleEarthExporter.export_wells(layer, out)

        assert result.is_absolute()


# ---------------------------------------------------------------------------
# 2. export_wells with color_by — styled placemarks
# ---------------------------------------------------------------------------


class TestExportWellsColorBy:
    """Verify color_by creates Style elements per unique value."""

    def test_creates_styles_for_unique_values(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells_colored.kml"
        GoogleEarthExporter.export_wells(layer, out, color_by="field")

        tree = etree.parse(str(out))
        styles = list(tree.getroot().iter(_tag("Style")))
        unique_fields = layer.data["field"].dropna().unique()
        assert len(styles) == len(unique_fields)

    def test_placemarks_reference_styles(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells_colored.kml"
        GoogleEarthExporter.export_wells(layer, out, color_by="field")

        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            style_url = pm.find(_tag("styleUrl"))
            assert style_url is not None
            assert style_url.text.startswith("#style-")

    def test_color_by_status(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        out = tmp_path / "wells_status.kml"
        GoogleEarthExporter.export_wells(layer, out, color_by="status")

        tree = etree.parse(str(out))
        styles = list(tree.getroot().iter(_tag("Style")))
        unique_statuses = layer.data["status"].dropna().unique()
        assert len(styles) == len(unique_statuses)


# ---------------------------------------------------------------------------
# 3. export_pipeline_routes — LineString geometry
# ---------------------------------------------------------------------------


class TestExportPipelineRoutes:
    """Verify export_pipeline_routes creates LineString placemarks."""

    def test_creates_kml_with_linestrings(self, tmp_path: Path) -> None:
        routes = [
            {
                "name": "Pipeline A",
                "coordinates": [(-90.5, 28.1), (-90.3, 28.2), (-90.1, 28.3)],
            },
            {
                "name": "Pipeline B",
                "coordinates": [(-89.8, 28.5), (-89.6, 28.6)],
                "properties": {"diameter_in": 12, "material": "steel"},
            },
        ]
        out = tmp_path / "pipelines.kml"
        result = GoogleEarthExporter.export_pipeline_routes(routes, out)

        assert result.exists()
        tree = etree.parse(str(out))
        linestrings = list(tree.getroot().iter(_tag("LineString")))
        assert len(linestrings) == 2

    def test_linestring_contains_coordinates(self, tmp_path: Path) -> None:
        routes = [
            {
                "name": "Test Route",
                "coordinates": [(-90.5, 28.1), (-90.3, 28.2)],
            },
        ]
        out = tmp_path / "pipelines.kml"
        GoogleEarthExporter.export_pipeline_routes(routes, out)

        tree = etree.parse(str(out))
        linestring = tree.getroot().find(f".//{_tag('LineString')}")
        coords = linestring.find(_tag("coordinates"))
        assert "-90.5,28.1" in coords.text
        assert "-90.3,28.2" in coords.text

    def test_pipeline_style_applied(self, tmp_path: Path) -> None:
        routes = [
            {
                "name": "Styled Route",
                "coordinates": [(-90.5, 28.1), (-90.3, 28.2)],
            },
        ]
        out = tmp_path / "pipelines.kml"
        GoogleEarthExporter.export_pipeline_routes(routes, out)

        tree = etree.parse(str(out))
        style = tree.getroot().find(f".//{_tag('Style')}")
        assert style is not None
        line_style = style.find(_tag("LineStyle"))
        assert line_style is not None
        width = line_style.find(_tag("width"))
        assert width.text == "3"


# ---------------------------------------------------------------------------
# 4. export_lease_blocks — Polygon with semi-transparent fill
# ---------------------------------------------------------------------------


class TestExportLeaseBlocks:
    """Verify export_lease_blocks creates Polygon placemarks."""

    def test_creates_kml_with_polygons(self, tmp_path: Path) -> None:
        blocks = [
            {
                "name": "Block A",
                "coordinates": [
                    (-90.5, 28.1),
                    (-90.3, 28.1),
                    (-90.3, 28.3),
                    (-90.5, 28.3),
                    (-90.5, 28.1),
                ],
            },
        ]
        out = tmp_path / "blocks.kml"
        result = GoogleEarthExporter.export_lease_blocks(blocks, out)

        assert result.exists()
        tree = etree.parse(str(out))
        polygons = list(tree.getroot().iter(_tag("Polygon")))
        assert len(polygons) == 1

    def test_polygon_has_outer_boundary(self, tmp_path: Path) -> None:
        blocks = [
            {
                "name": "Block B",
                "coordinates": [
                    (-90.0, 28.0),
                    (-89.8, 28.0),
                    (-89.8, 28.2),
                    (-90.0, 28.2),
                    (-90.0, 28.0),
                ],
            },
        ]
        out = tmp_path / "blocks.kml"
        GoogleEarthExporter.export_lease_blocks(blocks, out)

        tree = etree.parse(str(out))
        polygon = tree.getroot().find(f".//{_tag('Polygon')}")
        outer = polygon.find(_tag("outerBoundaryIs"))
        assert outer is not None
        ring = outer.find(_tag("LinearRing"))
        assert ring is not None

    def test_semi_transparent_fill_style(self, tmp_path: Path) -> None:
        blocks = [
            {
                "name": "Transparent Block",
                "coordinates": [
                    (-90.0, 28.0),
                    (-89.8, 28.0),
                    (-89.8, 28.2),
                    (-90.0, 28.2),
                    (-90.0, 28.0),
                ],
            },
        ]
        out = tmp_path / "blocks.kml"
        GoogleEarthExporter.export_lease_blocks(blocks, out)

        tree = etree.parse(str(out))
        style = tree.getroot().find(f".//{_tag('Style')}")
        poly_style = style.find(_tag("PolyStyle"))
        assert poly_style is not None
        color = poly_style.find(_tag("color"))
        # Alpha byte should indicate semi-transparency (not ff)
        assert color.text[:2] != "ff"


# ---------------------------------------------------------------------------
# 5. _build_description_html — HTML table generation
# ---------------------------------------------------------------------------


class TestBuildDescriptionHtml:
    """Verify _build_description_html generates valid HTML tables."""

    def test_generates_table(self) -> None:
        html = GoogleEarthExporter._build_description_html(
            {"name": "Well-A", "depth": 1500}
        )
        assert "<table>" in html
        assert "</table>" in html

    def test_contains_keys_and_values(self) -> None:
        props = {"field": "Green Canyon", "status": "active"}
        html = GoogleEarthExporter._build_description_html(props)

        assert "field" in html
        assert "Green Canyon" in html
        assert "status" in html
        assert "active" in html

    def test_each_property_has_row(self) -> None:
        props = {"a": 1, "b": 2, "c": 3}
        html = GoogleEarthExporter._build_description_html(props)

        assert html.count("<tr>") == 3

    def test_empty_dict_returns_empty_table(self) -> None:
        html = GoogleEarthExporter._build_description_html({})
        assert html == "<table></table>"
