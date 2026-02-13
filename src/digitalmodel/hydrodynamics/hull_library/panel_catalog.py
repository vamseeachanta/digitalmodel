"""Hull panel catalog data models.

ABOUTME: Defines PanelFormat, PanelCatalogEntry, and PanelCatalog — the data
models for inventorying hull panel meshes across the workspace. Supports YAML
and CSV serialization for machine-readable catalogs.
"""

from __future__ import annotations

import csv
from datetime import date
from enum import Enum
from pathlib import Path
from typing import Optional

import yaml
from pydantic import BaseModel, Field, model_validator

from .profile_schema import HullType


class PanelFormat(str, Enum):
    """File format of a hull panel mesh."""

    GDF = "gdf"
    AQWA_DAT = "aqwa_dat"
    OWD = "owd"
    OBJ = "obj"
    RHINO_3DM = "rhino_3dm"
    ORCAFLEX_YAML = "orcaflex_yaml"
    YAML_PROFILE = "yaml_profile"


class RaoReference(BaseModel):
    """Reference to an RAO dataset linked to a hull panel."""

    solver: str = Field(..., description="Solver name, e.g. orcawave, aqwa")
    draft_m: float = Field(..., description="Analysis draft in metres")
    loading_condition: Optional[str] = Field(
        None, description="Loading condition, e.g. ballast, full_load"
    )
    headings_deg: Optional[list[float]] = Field(
        None, description="Wave headings analysed (degrees)"
    )
    n_frequencies: Optional[int] = Field(
        None, description="Number of frequency bins"
    )
    date: Optional[str] = Field(None, description="ISO 8601 date string")
    file_path: str = Field(default="", description="Relative path to RAO data")
    benchmark_revision: Optional[str] = Field(
        None, description="Benchmark revision, e.g. r4, r1"
    )

    def to_dict(self) -> dict:
        """Serialize to dict, excluding None-valued optional fields."""
        return self.model_dump(exclude_none=True)


class PanelCatalogEntry(BaseModel):
    """A single hull panel entry in the catalog."""

    hull_id: str = Field(..., description="Unique identifier for this hull")
    hull_type: HullType = Field(..., description="Hull form classification")
    name: str = Field(..., description="Human-readable hull name")
    source: str = Field(..., description="Source directory or dataset ID")
    panel_format: PanelFormat = Field(..., description="Panel mesh file format")
    file_path: str = Field(..., description="Path relative to workspace root")

    panel_count: Optional[int] = Field(None, description="Number of panels")
    vertex_count: Optional[int] = Field(None, description="Number of vertices")
    symmetry: Optional[str] = Field(None, description="Symmetry plane (e.g. 'y')")
    length_m: Optional[float] = Field(None, description="Overall length in meters")
    beam_m: Optional[float] = Field(None, description="Beam/breadth in meters")
    draft_m: Optional[float] = Field(None, description="Draft in meters")
    displacement_t: Optional[float] = Field(None, description="Displacement in tonnes")
    description: Optional[str] = Field(None, description="Free-text description")
    loading_condition: Optional[str] = Field(None, description="Loading condition label")
    tags: list[str] = Field(default_factory=list, description="Searchable tags")
    raos: Optional[list[RaoReference]] = Field(
        None, description="Linked RAO datasets for this hull"
    )


class PanelCatalog(BaseModel):
    """Collection of hull panel catalog entries."""

    version: str = Field(default="1.0", description="Catalog schema version")
    updated: str = Field(
        default_factory=lambda: date.today().isoformat(),
        description="Last update date (ISO 8601)",
    )
    entries: list[PanelCatalogEntry] = Field(
        default_factory=list, description="Catalog entries"
    )

    @model_validator(mode="after")
    def validate_unique_hull_ids(self) -> "PanelCatalog":
        ids = [e.hull_id for e in self.entries]
        seen = set()
        for hull_id in ids:
            if hull_id in seen:
                raise ValueError(f"Duplicate hull_id: {hull_id}")
            seen.add(hull_id)
        return self

    def to_yaml(self, path: str | Path) -> Path:
        """Write catalog to YAML file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        data = {
            "version": self.version,
            "updated": self.updated,
            "entries": [
                _entry_to_yaml_dict(e) for e in self.entries
            ],
        }
        with open(path, "w") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)
        return path

    @classmethod
    def from_yaml(cls, path: str | Path) -> PanelCatalog:
        """Load catalog from YAML file."""
        path = Path(path)
        with open(path) as f:
            data = yaml.safe_load(f)
        entries = [
            PanelCatalogEntry(
                hull_id=e["hull_id"],
                hull_type=HullType(e["hull_type"]),
                name=e["name"],
                source=e["source"],
                panel_format=PanelFormat(e["panel_format"]),
                file_path=e["file_path"],
                panel_count=e.get("panel_count"),
                vertex_count=e.get("vertex_count"),
                symmetry=e.get("symmetry"),
                length_m=e.get("length_m"),
                beam_m=e.get("beam_m"),
                draft_m=e.get("draft_m"),
                displacement_t=e.get("displacement_t"),
                description=e.get("description"),
                loading_condition=e.get("loading_condition"),
                tags=e.get("tags", []),
                raos=[RaoReference(**r) for r in e["raos"]]
                if e.get("raos")
                else None,
            )
            for e in data.get("entries", [])
        ]
        return cls(
            version=data.get("version", "1.0"),
            updated=data.get("updated", date.today().isoformat()),
            entries=entries,
        )

    def to_csv(self, path: str | Path) -> Path:
        """Write catalog summary to CSV file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        fieldnames = [
            "hull_id", "hull_type", "name", "source", "panel_format",
            "file_path", "panel_count", "vertex_count", "symmetry",
            "length_m", "beam_m", "draft_m", "displacement_t",
            "description", "loading_condition", "tags", "rao_count",
        ]
        with open(path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for entry in self.entries:
                row = {}
                for field in fieldnames:
                    if field == "rao_count":
                        row[field] = len(entry.raos) if entry.raos else 0
                        continue
                    val = getattr(entry, field)
                    if val is None:
                        row[field] = ""
                    elif field == "tags":
                        row[field] = ";".join(val)
                    elif isinstance(val, Enum):
                        row[field] = val.value
                    else:
                        row[field] = val
                writer.writerow(row)
        return path


def _entry_to_yaml_dict(entry: PanelCatalogEntry) -> dict:
    """Convert entry to a clean YAML-serializable dict."""
    d: dict = {
        "hull_id": entry.hull_id,
        "hull_type": entry.hull_type.value,
        "name": entry.name,
        "source": entry.source,
        "panel_format": entry.panel_format.value,
        "file_path": entry.file_path,
    }
    for field in [
        "panel_count", "vertex_count", "symmetry", "length_m",
        "beam_m", "draft_m", "displacement_t", "description",
        "loading_condition",
    ]:
        val = getattr(entry, field)
        if val is not None:
            d[field] = val
    if entry.tags:
        d["tags"] = entry.tags
    if entry.raos:
        d["raos"] = [r.to_dict() for r in entry.raos]
    return d


__all__ = ["PanelFormat", "RaoReference", "PanelCatalogEntry", "PanelCatalog"]
