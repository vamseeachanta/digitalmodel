"""
ABOUTME: Hull line definition parser — Phase 1 of the hull panel generator.

Parses waterline, section, and profile curve data from CSV, JSON, and YAML
files into a validated HullLineDefinition model.  CSV column order: x, z, y.

Coordinate conventions:
  x: longitudinal position (m) from AP; z: keel-up (0=keel, draft=WL);
  y: half-breadth (always >= 0).
"""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import List

import yaml
from pydantic import BaseModel, Field, field_validator, model_validator

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------


class StationOffset(BaseModel):
    """Cross-section at longitudinal position *x*."""

    x: float = Field(..., description="Longitudinal position (m) from AP")
    offsets: List[tuple[float, float]] = Field(
        ..., description="List of (z, y) pairs: z keel-up (m), y half-breadth (m)"
    )

    @field_validator("offsets")
    @classmethod
    def _validate_offsets(
        cls, v: List[tuple[float, float]]
    ) -> List[tuple[float, float]]:
        if len(v) == 0:
            raise ValueError("offsets must contain at least one (z, y) pair")
        for i, (z, y) in enumerate(v):
            if z < 0:
                raise ValueError(
                    f"offsets[{i}]: z must be non-negative (keel-up), got z={z}"
                )
            if y < 0:
                raise ValueError(
                    f"offsets[{i}]: half-breadth y must be non-negative, got y={y}"
                )
        return v


class WaterlineCurve(BaseModel):
    """Waterline at constant z as (x, y) pairs."""

    z: float = Field(..., ge=0, description="Height above keel (m)")
    points: List[tuple[float, float]] = Field(default_factory=list)


class ProfileCurve(BaseModel):
    """Profile curve (sheer, keel, deck) in the x-z plane."""

    name: str = Field(default="profile")
    points: List[tuple[float, float]] = Field(default_factory=list)


class HullLineDefinition(BaseModel):
    """Complete hull line definition — primary input to the generator."""

    name: str
    hull_type: str
    length_bp: float = Field(..., gt=0)
    beam: float = Field(..., gt=0)
    draft: float = Field(..., gt=0)
    depth: float = Field(..., gt=0)
    source: str
    stations: List[StationOffset]
    waterlines: List[WaterlineCurve] = Field(default_factory=list)
    profile: List[ProfileCurve] = Field(default_factory=list)

    @field_validator("stations")
    @classmethod
    def _require_two_stations(cls, v: List[StationOffset]) -> List[StationOffset]:
        if len(v) < 2:
            raise ValueError(
                "HullLineDefinition requires at least two stations"
            )
        return v

    @model_validator(mode="after")
    def _sort_stations(self) -> "HullLineDefinition":
        self.stations = sorted(self.stations, key=lambda s: s.x)
        return self

    def to_dict(self) -> dict:
        """Convert to a plain dict for JSON/YAML round-trips."""
        return {
            "name": self.name,
            "hull_type": self.hull_type,
            "length_bp": self.length_bp,
            "beam": self.beam,
            "draft": self.draft,
            "depth": self.depth,
            "source": self.source,
            "stations": [
                {"x": s.x, "offsets": [list(pair) for pair in s.offsets]}
                for s in self.stations
            ],
            "waterlines": [
                {"z": wl.z, "points": [list(p) for p in wl.points]}
                for wl in self.waterlines
            ],
            "profile": [
                {"name": pf.name, "points": [list(p) for p in pf.points]}
                for pf in self.profile
            ],
        }

    def save_yaml(self, path: str | Path) -> Path:
        """Save definition to a YAML file."""
        path = Path(path)
        with open(path, "w") as f:
            yaml.dump(self.to_dict(), f, default_flow_style=False, sort_keys=False)
        return path

    def save_json(self, path: str | Path) -> Path:
        """Save definition to a JSON file."""
        path = Path(path)
        with open(path, "w") as f:
            json.dump(self.to_dict(), f, indent=2)
        return path

    def to_hull_profile(self) -> HullProfile:
        """Convert to ``HullProfile`` for use with existing mesh generators.

        Unrecognised *hull_type* strings map to ``HullType.CUSTOM``.
        """
        try:
            hull_type_enum = HullType(self.hull_type)
        except ValueError:
            hull_type_enum = HullType.CUSTOM
        hull_stations = [
            HullStation(
                x_position=s.x,
                waterline_offsets=[tuple(pair) for pair in s.offsets],
            )
            for s in self.stations
        ]
        return HullProfile(
            name=self.name,
            hull_type=hull_type_enum,
            stations=hull_stations,
            length_bp=self.length_bp,
            beam=self.beam,
            draft=self.draft,
            depth=self.depth,
            source=self.source,
        )


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------


class LineParser:
    """Parses hull line definitions from CSV, JSON, and YAML sources.

    CSV format: columns ``x``, ``z``, ``y`` (header required). Rows with the
    same ``x`` are grouped into one station.

    JSON / YAML: must follow the ``HullLineDefinition`` schema with stations
    as ``{"x": float, "offsets": [[z1,y1], ...]}``.
    """

    def parse_csv(
        self,
        path: str | Path,
        *,
        name: str,
        hull_type: str,
        length_bp: float,
        beam: float,
        draft: float,
        depth: float,
        source: str,
    ) -> HullLineDefinition:
        """Parse a CSV file of station offsets.

        Raises:
            FileNotFoundError: If *path* does not exist.
            ValueError: If CSV structure or values are invalid.
        """
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"CSV file not found: {path}")
        stations_map: dict[float, list[tuple[float, float]]] = {}
        with open(path, newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                try:
                    x_val = float(row["x"])
                    z_val = float(row["z"])
                    y_val = float(row["y"])
                except (KeyError, ValueError) as exc:
                    raise ValueError(
                        f"CSV row malformed (expected x, z, y columns): {row}"
                    ) from exc
                stations_map.setdefault(x_val, []).append((z_val, y_val))
        station_objects = [
            StationOffset(x=x, offsets=sorted(offs, key=lambda p: p[0]))
            for x, offs in stations_map.items()
        ]
        return HullLineDefinition(
            name=name,
            hull_type=hull_type,
            length_bp=length_bp,
            beam=beam,
            draft=draft,
            depth=depth,
            source=source,
            stations=station_objects,
        )

    def parse_json(self, path: str | Path) -> HullLineDefinition:
        """Load a HullLineDefinition from a JSON file."""
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"JSON file not found: {path}")
        with open(path) as f:
            data = json.load(f)
        return self.parse_json_dict(data)

    def parse_json_dict(self, data: dict) -> HullLineDefinition:
        """Build a HullLineDefinition from a plain dict."""
        return self._from_dict(data)

    def parse_yaml(self, path: str | Path) -> HullLineDefinition:
        """Load a HullLineDefinition from a YAML file."""
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"YAML file not found: {path}")
        with open(path) as f:
            data = yaml.safe_load(f)
        return self._from_dict(data)

    @staticmethod
    def _from_dict(data: dict) -> HullLineDefinition:
        """Convert a raw dict to HullLineDefinition."""
        station_objects = [
            StationOffset(
                x=float(s["x"]),
                offsets=[tuple(pair) for pair in s.get("offsets", [])],
            )
            for s in data.get("stations", [])
        ]
        waterlines = [
            WaterlineCurve(
                z=float(wl.get("z", 0.0)),
                points=[tuple(p) for p in wl.get("points", [])],
            )
            for wl in data.get("waterlines", [])
            if isinstance(wl, dict)
        ]
        profile = [
            ProfileCurve(
                name=pf.get("name", "profile"),
                points=[tuple(p) for p in pf.get("points", [])],
            )
            for pf in data.get("profile", [])
            if isinstance(pf, dict)
        ]
        return HullLineDefinition(
            name=data["name"],
            hull_type=data["hull_type"],
            length_bp=float(data["length_bp"]),
            beam=float(data["beam"]),
            draft=float(data["draft"]),
            depth=float(data["depth"]),
            source=data.get("source", "unknown"),
            stations=station_objects,
            waterlines=waterlines,
            profile=profile,
        )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "StationOffset",
    "WaterlineCurve",
    "ProfileCurve",
    "HullLineDefinition",
    "LineParser",
]
