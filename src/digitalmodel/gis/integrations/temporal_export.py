"""Temporal KML/GeoJSON/CSV export for animated Google Earth visualization."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd
from lxml import etree

KML_NS = "http://www.opengis.net/kml/2.2"
NSMAP = {None: KML_NS}


def _tag(local_name: str) -> str:
    """Return a fully-qualified KML tag name."""
    return f"{{{KML_NS}}}{local_name}"


class TemporalExporter:
    """Export temporal feature layers to KML, GeoJSON and CSV formats."""

    @classmethod
    def export_temporal_kml(
        cls, layer, filepath: str | Path,
        time_col: str = "timestamp", name_col: str = "name",
    ) -> Path:
        """Export layer to KML with ``<TimeStamp>`` per Placemark."""
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        kml, document = cls._create_document(layer.name)

        for _, row in layer.data.iterrows():
            pm = etree.SubElement(document, _tag("Placemark"))
            if name_col in layer.data.columns:
                etree.SubElement(pm, _tag("name")).text = str(row[name_col])
            if time_col in layer.data.columns:
                ts = etree.SubElement(pm, _tag("TimeStamp"))
                etree.SubElement(ts, _tag("when")).text = pd.Timestamp(row[time_col]).isoformat()
            point = etree.SubElement(pm, _tag("Point"))
            etree.SubElement(point, _tag("coordinates")).text = (
                f"{row[layer.lon_col]},{row[layer.lat_col]}"
            )

        cls._write_tree(kml, filepath)
        return filepath.resolve()

    @classmethod
    def export_timeline_kml(
        cls, layer, filepath: str | Path,
        start_col: str = "start_date", end_col: str = "end_date",
        name_col: str = "name",
    ) -> Path:
        """Export layer to KML with ``<TimeSpan>`` per Placemark.

        Features without an end date omit the ``<end>`` element.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        kml, document = cls._create_document(layer.name)

        for _, row in layer.data.iterrows():
            pm = etree.SubElement(document, _tag("Placemark"))
            if name_col in layer.data.columns:
                etree.SubElement(pm, _tag("name")).text = str(row[name_col])
            span = etree.SubElement(pm, _tag("TimeSpan"))
            if start_col in layer.data.columns:
                etree.SubElement(span, _tag("begin")).text = pd.Timestamp(row[start_col]).isoformat()
            if end_col in layer.data.columns and pd.notna(row[end_col]):
                etree.SubElement(span, _tag("end")).text = pd.Timestamp(row[end_col]).isoformat()
            point = etree.SubElement(pm, _tag("Point"))
            etree.SubElement(point, _tag("coordinates")).text = (
                f"{row[layer.lon_col]},{row[layer.lat_col]}"
            )

        cls._write_tree(kml, filepath)
        return filepath.resolve()

    @classmethod
    def export_animated_geojson(
        cls, layer, filepath: str | Path,
        time_col: str = "timestamp", freq: str = "YE",
    ) -> Path:
        """Export frames of GeoJSON FeatureCollections grouped by time period.

        Output: ``{"frames": [{"timestamp": "...", "features": {...}}, ...]}``
        Uses ``layer.animate_frames()`` if available, otherwise manual grouping.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        df = layer.data.copy()
        df[time_col] = pd.to_datetime(df[time_col])

        if hasattr(layer, "animate_frames"):
            raw_frames = layer.animate_frames(freq=freq)
        else:
            raw_frames = cls._group_frames(df, layer, time_col, freq)

        filepath.write_text(json.dumps({"frames": raw_frames}, default=str), encoding="utf-8")
        return filepath.resolve()

    @classmethod
    def export_temporal_csv(
        cls, layer, filepath: str | Path, time_col: str = "timestamp",
    ) -> Path:
        """Export temporal data as a time-sorted CSV with all columns."""
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        df = layer.data.copy().sort_values(by=time_col).reset_index(drop=True)
        df.to_csv(filepath, index=False)
        return filepath.resolve()

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @classmethod
    def _create_document(cls, name: str) -> tuple[etree._Element, etree._Element]:
        """Create a KML root with a Document element."""
        kml = etree.Element(_tag("kml"), nsmap=NSMAP)
        doc = etree.SubElement(kml, _tag("Document"))
        etree.SubElement(doc, _tag("name")).text = name
        return kml, doc

    @classmethod
    def _write_tree(cls, kml: etree._Element, filepath: Path) -> None:
        """Serialize the KML element tree to *filepath*."""
        etree.ElementTree(kml).write(
            str(filepath), xml_declaration=True, encoding="UTF-8", pretty_print=True,
        )

    @classmethod
    def _group_frames(cls, df: pd.DataFrame, layer, time_col: str, freq: str) -> list[dict]:
        """Group rows by time period and build GeoJSON FeatureCollections."""
        frames: list[dict] = []
        for period, group in df.groupby(pd.Grouper(key=time_col, freq=freq)):
            if group.empty:
                continue
            features = []
            for _, row in group.iterrows():
                props = {
                    col: (row[col].isoformat() if isinstance(row[col], pd.Timestamp) else row[col])
                    for col in df.columns if col not in (layer.lon_col, layer.lat_col)
                }
                features.append({
                    "type": "Feature",
                    "geometry": {
                        "type": "Point",
                        "coordinates": [float(row[layer.lon_col]), float(row[layer.lat_col])],
                    },
                    "properties": props,
                })
            frames.append({
                "timestamp": str(period),
                "features": {"type": "FeatureCollection", "features": features},
            })
        return frames
