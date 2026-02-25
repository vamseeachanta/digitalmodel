"""KML/KMZ file handler for reading and writing GIS features.

Replaces the legacy gis.py script with a structured class-based approach.
Supports reading/writing KML 2.2 and KMZ (ZIP-compressed KML) files,
as well as conversion between feature dictionaries and pandas DataFrames.
"""

from __future__ import annotations

import tempfile
import zipfile
from pathlib import Path

import pandas as pd
from lxml import etree

KML_NS = "http://www.opengis.net/kml/2.2"
NSMAP = {None: KML_NS}


def _tag(local_name: str) -> str:
    """Return a fully-qualified KML tag name."""
    return f"{{{KML_NS}}}{local_name}"


def _parse_coordinate_string(text: str) -> list[tuple[float, ...]]:
    """Parse a KML coordinate string into a list of tuples.

    KML coordinates are formatted as ``lon,lat[,alt]`` tuples separated by
    whitespace.  Each tuple is returned as ``(longitude, latitude)`` when no
    altitude is present, or ``(longitude, latitude, altitude)`` otherwise.
    """
    coordinates: list[tuple[float, ...]] = []
    for token in text.strip().split():
        parts = token.split(",")
        coordinates.append(tuple(float(p) for p in parts))
    return coordinates


def _format_coordinates(coords: list[tuple[float, ...]]) -> str:
    """Format coordinate tuples back into a KML coordinate string."""
    tokens: list[str] = []
    for coord in coords:
        tokens.append(",".join(str(c) for c in coord))
    return " ".join(tokens)


def _extract_placemark(placemark: etree._Element) -> dict:
    """Extract a feature dictionary from a single KML Placemark element."""
    name_el = placemark.find(_tag("name"))
    desc_el = placemark.find(_tag("description"))

    feature: dict = {
        "name": name_el.text if name_el is not None else None,
        "description": desc_el.text if desc_el is not None else None,
        "geometry_type": None,
        "coordinates": [],
        "properties": {},
    }

    # Extended data
    ext_data = placemark.find(_tag("ExtendedData"))
    if ext_data is not None:
        for data_el in ext_data.findall(_tag("Data")):
            key = data_el.get("name", "")
            value_el = data_el.find(_tag("value"))
            feature["properties"][key] = (
                value_el.text if value_el is not None else None
            )

    # Geometry extraction — try each supported type in order
    point = placemark.find(f".//{_tag('Point')}")
    linestring = placemark.find(f".//{_tag('LineString')}")
    polygon = placemark.find(f".//{_tag('Polygon')}")

    if point is not None:
        coords_el = point.find(_tag("coordinates"))
        if coords_el is not None and coords_el.text:
            feature["geometry_type"] = "Point"
            feature["coordinates"] = _parse_coordinate_string(coords_el.text)

    elif linestring is not None:
        coords_el = linestring.find(_tag("coordinates"))
        if coords_el is not None and coords_el.text:
            feature["geometry_type"] = "LineString"
            feature["coordinates"] = _parse_coordinate_string(coords_el.text)

    elif polygon is not None:
        outer = polygon.find(
            f"{_tag('outerBoundaryIs')}/{_tag('LinearRing')}/{_tag('coordinates')}"
        )
        if outer is not None and outer.text:
            feature["geometry_type"] = "Polygon"
            feature["coordinates"] = _parse_coordinate_string(outer.text)

    return feature


def _build_placemark(feature: dict) -> etree._Element:
    """Build a KML Placemark element from a feature dictionary."""
    placemark = etree.SubElement(etree.Element("dummy"), _tag("Placemark"))
    # Detach from dummy parent so it can be appended elsewhere
    placemark = etree.Element(_tag("Placemark"))

    if feature.get("name"):
        name_el = etree.SubElement(placemark, _tag("name"))
        name_el.text = str(feature["name"])

    if feature.get("description"):
        desc_el = etree.SubElement(placemark, _tag("description"))
        desc_el.text = str(feature["description"])

    geometry_type = feature.get("geometry_type", "Point")
    coordinates = feature.get("coordinates", [])

    if not coordinates:
        return placemark

    coord_text = _format_coordinates(coordinates)

    if geometry_type == "Point":
        point = etree.SubElement(placemark, _tag("Point"))
        coords_el = etree.SubElement(point, _tag("coordinates"))
        coords_el.text = coord_text

    elif geometry_type == "LineString":
        linestring = etree.SubElement(placemark, _tag("LineString"))
        coords_el = etree.SubElement(linestring, _tag("coordinates"))
        coords_el.text = coord_text

    elif geometry_type == "Polygon":
        polygon = etree.SubElement(placemark, _tag("Polygon"))
        outer = etree.SubElement(polygon, _tag("outerBoundaryIs"))
        ring = etree.SubElement(outer, _tag("LinearRing"))
        coords_el = etree.SubElement(ring, _tag("coordinates"))
        coords_el.text = coord_text

    return placemark


class KMLHandler:
    """Read and write KML/KMZ files and convert between features and DataFrames."""

    @staticmethod
    def read_kml(file_path: str | Path) -> list[dict]:
        """Parse a KML file and return a list of feature dictionaries.

        Each feature dictionary contains the keys:
        ``name``, ``description``, ``geometry_type``, ``coordinates``,
        and ``properties``.

        The parser disables external entity resolution and network access
        to prevent XXE attacks on untrusted KML input.
        """
        file_path = Path(file_path)
        parser = etree.XMLParser(
            resolve_entities=False,
            no_network=True,
        )
        tree = etree.parse(str(file_path), parser=parser)
        root = tree.getroot()

        features: list[dict] = []
        for placemark in root.iter(_tag("Placemark")):
            features.append(_extract_placemark(placemark))

        return features

    @staticmethod
    def read_kmz(file_path: str | Path) -> list[dict]:
        """Extract a KMZ archive and parse the KML file found inside.

        Uses safe extraction that rejects entries with absolute paths or
        ``..`` path segments to prevent zip-slip path traversal attacks.
        """
        file_path = Path(file_path)

        with tempfile.TemporaryDirectory() as tmp_dir:
            with zipfile.ZipFile(file_path, "r") as zf:
                for member in zf.infolist():
                    member_path = Path(member.filename)
                    if member_path.is_absolute() or ".." in member_path.parts:
                        raise ValueError(
                            f"Refusing to extract unsafe path: {member.filename}"
                        )
                    zf.extract(member, tmp_dir)

            kml_path: Path | None = None
            for path in Path(tmp_dir).rglob("*.kml"):
                kml_path = path
                break

            if kml_path is None:
                raise FileNotFoundError(
                    f"No KML file found inside the KMZ archive: {file_path}"
                )

            return KMLHandler.read_kml(kml_path)

    @staticmethod
    def read(file_path: str | Path) -> list[dict]:
        """Auto-detect file type by extension and read KML or KMZ."""
        file_path = Path(file_path)
        suffix = file_path.suffix.lower()

        if suffix == ".kml":
            return KMLHandler.read_kml(file_path)
        if suffix == ".kmz":
            return KMLHandler.read_kmz(file_path)

        raise ValueError(
            f"Unsupported file extension '{suffix}'. Expected '.kml' or '.kmz'."
        )

    @staticmethod
    def write_kml(
        features: list[dict],
        file_path: str | Path,
        document_name: str = "GIS Export",
    ) -> Path:
        """Write feature dictionaries to a KML 2.2 file.

        Returns the resolved output ``Path``.
        """
        file_path = Path(file_path)
        file_path.parent.mkdir(parents=True, exist_ok=True)

        kml = etree.Element(_tag("kml"), nsmap=NSMAP)
        document = etree.SubElement(kml, _tag("Document"))

        doc_name = etree.SubElement(document, _tag("name"))
        doc_name.text = document_name

        for feature in features:
            placemark = _build_placemark(feature)
            document.append(placemark)

        tree = etree.ElementTree(kml)
        tree.write(
            str(file_path),
            xml_declaration=True,
            encoding="UTF-8",
            pretty_print=True,
        )

        return file_path.resolve()

    @staticmethod
    def write_kmz(
        features: list[dict],
        file_path: str | Path,
        document_name: str = "GIS Export",
    ) -> Path:
        """Write feature dictionaries to a KMZ file (KML inside a ZIP).

        Returns the resolved output ``Path``.
        """
        file_path = Path(file_path)
        file_path.parent.mkdir(parents=True, exist_ok=True)

        with tempfile.TemporaryDirectory() as tmp_dir:
            kml_path = Path(tmp_dir) / "doc.kml"
            KMLHandler.write_kml(features, kml_path, document_name=document_name)

            with zipfile.ZipFile(file_path, "w", zipfile.ZIP_DEFLATED) as zf:
                zf.write(str(kml_path), arcname="doc.kml")

        return file_path.resolve()

    @staticmethod
    def features_to_dataframe(features: list[dict]) -> pd.DataFrame:
        """Convert a list of feature dictionaries to a pandas DataFrame.

        Coordinate tuples are unpacked so that the first coordinate's
        longitude and latitude appear in dedicated columns.  All entries
        from the ``properties`` dict are flattened into the DataFrame.
        """
        rows: list[dict] = []
        for feature in features:
            row: dict = {
                "name": feature.get("name"),
                "description": feature.get("description"),
                "geometry_type": feature.get("geometry_type"),
            }

            coords = feature.get("coordinates", [])
            if coords:
                row["longitude"] = coords[0][0]
                row["latitude"] = coords[0][1]
                if len(coords[0]) > 2:
                    row["altitude"] = coords[0][2]

            row["coordinates"] = coords
            row.update(feature.get("properties", {}))
            rows.append(row)

        return pd.DataFrame(rows)

    @staticmethod
    def dataframe_to_features(
        df: pd.DataFrame,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
        name_col: str | None = None,
    ) -> list[dict]:
        """Convert a DataFrame with longitude/latitude columns to feature dicts.

        Each row becomes a ``Point`` feature.  If *name_col* is provided its
        value is used as the feature name; otherwise the DataFrame index is
        used.
        """
        features: list[dict] = []
        for idx, row in df.iterrows():
            lon = row[lon_col]
            lat = row[lat_col]

            name = str(row[name_col]) if name_col and name_col in row.index else str(idx)

            feature: dict = {
                "name": name,
                "description": None,
                "geometry_type": "Point",
                "coordinates": [(float(lon), float(lat))],
                "properties": {
                    col: row[col]
                    for col in df.columns
                    if col not in {lon_col, lat_col, name_col}
                },
            }
            features.append(feature)

        return features
