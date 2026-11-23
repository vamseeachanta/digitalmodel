"""Google Earth KML export for styled offshore asset visualization.

Generates styled KML files optimized for Google Earth, supporting wells,
pipeline routes, and lease block polygons with configurable styling.
"""

from __future__ import annotations

import html
from pathlib import Path

from lxml import etree

KML_NS = "http://www.opengis.net/kml/2.2"
NSMAP = {None: KML_NS}

# Default color palette in KML's AABBGGRR format
_DEFAULT_COLORS = [
    "ff0000ff",  # red
    "ffff0000",  # blue
    "ff00ff00",  # green
    "ff00ffff",  # yellow
    "ffffff00",  # cyan
    "ffff00ff",  # magenta
    "ff0080ff",  # orange
    "ff800080",  # purple
]


def _tag(local_name: str) -> str:
    """Return a fully-qualified KML tag name."""
    return f"{{{KML_NS}}}{local_name}"


def _sanitise_style_id(value: str) -> str:
    """Sanitise a value for use as a KML style ID attribute."""
    return "".join(c if c.isalnum() or c in "-_." else "_" for c in str(value))


class GoogleEarthExporter:
    """Export offshore assets to styled KML files for Google Earth."""

    @classmethod
    def export_wells(
        cls,
        layer,
        filepath: str | Path,
        color_by: str | None = None,
        icon_url: str | None = None,
    ) -> Path:
        """Export a WellLayer or FeatureLayer to a styled KML file.

        Parameters
        ----------
        layer:
            A FeatureLayer or WellLayer instance.
        filepath:
            Destination KML file path.
        color_by:
            Optional column name to color-code placemarks by unique values.
        icon_url:
            Optional custom icon URL for placemarks.

        Returns
        -------
        Path
            The resolved output filepath.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        kml_root, document = cls._create_styled_document(layer.name)

        # Build color mapping if requested
        color_map: dict[str, str] = {}
        if color_by and color_by in layer.data.columns:
            unique_values = layer.data[color_by].dropna().unique()
            for i, val in enumerate(unique_values):
                color_map[str(val)] = _DEFAULT_COLORS[i % len(_DEFAULT_COLORS)]

            # Create Style elements for each unique value
            for val, color in color_map.items():
                style = etree.SubElement(document, _tag("Style"))
                style.set("id", f"style-{_sanitise_style_id(val)}")
                icon_style = etree.SubElement(style, _tag("IconStyle"))
                color_el = etree.SubElement(icon_style, _tag("color"))
                color_el.text = color
                if icon_url:
                    icon_el = etree.SubElement(icon_style, _tag("Icon"))
                    href = etree.SubElement(icon_el, _tag("href"))
                    href.text = icon_url

        elif icon_url:
            style = etree.SubElement(document, _tag("Style"))
            style.set("id", "default-style")
            icon_style = etree.SubElement(style, _tag("IconStyle"))
            icon_el = etree.SubElement(icon_style, _tag("Icon"))
            href = etree.SubElement(icon_el, _tag("href"))
            href.text = icon_url

        # Create placemarks
        df = layer.data
        for _, row in df.iterrows():
            placemark = etree.SubElement(document, _tag("Placemark"))

            name_val = row.get("name")
            if name_val is not None:
                name_el = etree.SubElement(placemark, _tag("name"))
                name_el.text = str(name_val)

            # Build HTML description from row properties
            props = {
                col: row[col]
                for col in df.columns
                if col not in (layer.lon_col, layer.lat_col)
            }
            desc_html = cls._build_description_html(props)
            desc_el = etree.SubElement(placemark, _tag("description"))
            desc_el.text = desc_html

            # Assign style reference
            if color_by and color_by in df.columns:
                val = row.get(color_by)
                if val is not None:
                    style_url = etree.SubElement(placemark, _tag("styleUrl"))
                    style_url.text = f"#style-{_sanitise_style_id(str(val))}"
            elif icon_url:
                style_url = etree.SubElement(placemark, _tag("styleUrl"))
                style_url.text = "#default-style"

            # Point geometry
            point = etree.SubElement(placemark, _tag("Point"))
            coords_el = etree.SubElement(point, _tag("coordinates"))
            lon = row[layer.lon_col]
            lat = row[layer.lat_col]
            coords_el.text = f"{lon},{lat}"

        tree = etree.ElementTree(kml_root)
        tree.write(
            str(filepath),
            xml_declaration=True,
            encoding="UTF-8",
            pretty_print=True,
        )
        return filepath.resolve()

    @classmethod
    def export_pipeline_routes(
        cls,
        routes: list[dict],
        filepath: str | Path,
    ) -> Path:
        """Export pipeline routes as LineString placemarks.

        Parameters
        ----------
        routes:
            List of dicts with 'name', 'coordinates' (list of (lon, lat)
            tuples), and optional 'properties'.
        filepath:
            Destination KML file path.

        Returns
        -------
        Path
            The resolved output filepath.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        kml_root, document = cls._create_styled_document("Pipeline Routes")

        # Line style: red, width 3
        style = etree.SubElement(document, _tag("Style"))
        style.set("id", "pipeline-style")
        line_style = etree.SubElement(style, _tag("LineStyle"))
        color_el = etree.SubElement(line_style, _tag("color"))
        color_el.text = "ff0000ff"
        width_el = etree.SubElement(line_style, _tag("width"))
        width_el.text = "3"

        for route in routes:
            placemark = etree.SubElement(document, _tag("Placemark"))

            name_el = etree.SubElement(placemark, _tag("name"))
            name_el.text = str(route["name"])

            props = route.get("properties")
            if props:
                desc_el = etree.SubElement(placemark, _tag("description"))
                desc_el.text = cls._build_description_html(props)

            style_url = etree.SubElement(placemark, _tag("styleUrl"))
            style_url.text = "#pipeline-style"

            linestring = etree.SubElement(placemark, _tag("LineString"))
            coords_el = etree.SubElement(linestring, _tag("coordinates"))
            coord_tokens = [f"{lon},{lat}" for lon, lat in route["coordinates"]]
            coords_el.text = " ".join(coord_tokens)

        tree = etree.ElementTree(kml_root)
        tree.write(
            str(filepath),
            xml_declaration=True,
            encoding="UTF-8",
            pretty_print=True,
        )
        return filepath.resolve()

    @classmethod
    def export_lease_blocks(
        cls,
        blocks: list[dict],
        filepath: str | Path,
    ) -> Path:
        """Export lease block polygons with semi-transparent fill.

        Parameters
        ----------
        blocks:
            List of dicts with 'name', 'coordinates' (list of (lon, lat)
            tuples forming a closed ring), and optional 'properties'.
        filepath:
            Destination KML file path.

        Returns
        -------
        Path
            The resolved output filepath.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        kml_root, document = cls._create_styled_document("Lease Blocks")

        # Polygon style: semi-transparent blue fill
        style = etree.SubElement(document, _tag("Style"))
        style.set("id", "lease-block-style")
        poly_style = etree.SubElement(style, _tag("PolyStyle"))
        color_el = etree.SubElement(poly_style, _tag("color"))
        color_el.text = "80ff0000"  # semi-transparent blue
        line_style = etree.SubElement(style, _tag("LineStyle"))
        line_color = etree.SubElement(line_style, _tag("color"))
        line_color.text = "ffff0000"
        line_width = etree.SubElement(line_style, _tag("width"))
        line_width.text = "2"

        for block in blocks:
            placemark = etree.SubElement(document, _tag("Placemark"))

            name_el = etree.SubElement(placemark, _tag("name"))
            name_el.text = str(block["name"])

            props = block.get("properties")
            if props:
                desc_el = etree.SubElement(placemark, _tag("description"))
                desc_el.text = cls._build_description_html(props)

            style_url = etree.SubElement(placemark, _tag("styleUrl"))
            style_url.text = "#lease-block-style"

            polygon = etree.SubElement(placemark, _tag("Polygon"))
            outer = etree.SubElement(polygon, _tag("outerBoundaryIs"))
            ring = etree.SubElement(outer, _tag("LinearRing"))
            coords_el = etree.SubElement(ring, _tag("coordinates"))
            coord_tokens = [f"{lon},{lat}" for lon, lat in block["coordinates"]]
            coords_el.text = " ".join(coord_tokens)

        tree = etree.ElementTree(kml_root)
        tree.write(
            str(filepath),
            xml_declaration=True,
            encoding="UTF-8",
            pretty_print=True,
        )
        return filepath.resolve()

    @classmethod
    def _create_styled_document(
        cls,
        name: str,
    ) -> tuple[etree._Element, etree._Element]:
        """Create a KML Document element with namespace.

        Parameters
        ----------
        name:
            Document name.

        Returns
        -------
        tuple[etree._Element, etree._Element]
            The (kml_root, document_element) pair.
        """
        kml = etree.Element(_tag("kml"), nsmap=NSMAP)
        document = etree.SubElement(kml, _tag("Document"))
        name_el = etree.SubElement(document, _tag("name"))
        name_el.text = name
        return kml, document

    @classmethod
    def _build_description_html(cls, properties: dict) -> str:
        """Convert a properties dict to an HTML table string.

        Parameters
        ----------
        properties:
            Key-value pairs to render.

        Returns
        -------
        str
            An HTML table suitable for KML Placemark descriptions.
        """
        rows = "".join(
            f"<tr><td><b>{html.escape(str(k))}</b></td>"
            f"<td>{html.escape(str(v))}</td></tr>"
            for k, v in properties.items()
        )
        return f"<table>{rows}</table>"
