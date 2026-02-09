"""
ABOUTME: Hull profile schema defining hull shapes as line profiles (waterlines,
sections, profiles) -- the naval architecture source of truth for hull geometry.

Uses Pydantic v2 BaseModel to define:
- HullType: enumeration of standard hull forms
- HullStation: a single cross-section defined by waterline offsets (z, y pairs)
- HullProfile: complete hull definition with principal dimensions, stations,
  and optional hydrostatic properties

Supports YAML serialization/deserialization for catalog storage.
"""

from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Optional

import yaml
from pydantic import BaseModel, Field, field_validator, model_validator


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------


class HullType(str, Enum):
    """Standard hull form types."""

    TANKER = "tanker"
    SEMI_PONTOON = "semi_pontoon"
    BARGE = "barge"
    SHIP = "ship"
    CUSTOM = "custom"


# ---------------------------------------------------------------------------
# Sub-models
# ---------------------------------------------------------------------------


class HullStation(BaseModel):
    """A single hull cross-section at a longitudinal position.

    Each station is defined by its x-position along the hull (measured from
    the aft perpendicular) and a list of waterline offsets. Each offset is a
    (z, y) pair where z is the vertical draft coordinate and y is the
    half-breadth at that draft.
    """

    x_position: float = Field(
        ...,
        description="Longitudinal position in meters (from aft perpendicular)",
    )
    waterline_offsets: list[tuple[float, float]] = Field(
        ...,
        description="List of (z, y) pairs: z = draft, y = half-breadth",
    )

    @field_validator("waterline_offsets")
    @classmethod
    def validate_offsets_not_empty(
        cls, v: list[tuple[float, float]]
    ) -> list[tuple[float, float]]:
        if len(v) == 0:
            raise ValueError(
                "waterline_offsets must contain at least one (z, y) pair"
            )
        return v


class HullProfile(BaseModel):
    """Complete hull profile definition.

    Captures the hull geometry as a set of cross-sectional stations, together
    with principal dimensions and optional hydrostatic properties.
    """

    name: str = Field(..., description="Hull profile identifier")
    hull_type: HullType = Field(..., description="Hull form classification")
    stations: list[HullStation] = Field(
        ...,
        description="Cross-sectional stations defining the hull shape",
    )
    length_bp: float = Field(
        ...,
        gt=0,
        description="Length between perpendiculars in meters",
    )
    beam: float = Field(
        ...,
        gt=0,
        description="Moulded beam (full breadth) in meters",
    )
    draft: float = Field(
        ...,
        gt=0,
        description="Design draft in meters",
    )
    depth: float = Field(
        ...,
        gt=0,
        description="Moulded depth in meters",
    )
    source: str = Field(
        ...,
        description="Data source or reference for this hull profile",
    )

    # Optional fields
    deck_profile: Optional[list[tuple[float, float]]] = Field(
        None,
        description="Deck edge profile as (x, y) pairs",
    )
    keel_profile: Optional[list[tuple[float, float]]] = Field(
        None,
        description="Keel profile as (x, z) pairs",
    )
    block_coefficient: Optional[float] = Field(
        None,
        description="Block coefficient Cb (0 < Cb <= 1)",
    )
    displacement: Optional[float] = Field(
        None,
        description="Displacement in tonnes",
    )

    @field_validator("stations")
    @classmethod
    def validate_minimum_stations(
        cls, v: list[HullStation]
    ) -> list[HullStation]:
        if len(v) < 2:
            raise ValueError(
                "At least two stations are required to define a hull profile"
            )
        return v

    @field_validator("block_coefficient")
    @classmethod
    def validate_block_coefficient(
        cls, v: Optional[float]
    ) -> Optional[float]:
        if v is not None and (v <= 0 or v > 1.0):
            raise ValueError(
                f"block_coefficient must be in range (0, 1], got {v}"
            )
        return v

    # ----- YAML serialization -----

    def to_yaml_dict(self) -> dict:
        """Serialize to a plain dict suitable for YAML output.

        Enum values are converted to strings and tuples to lists so that
        the result round-trips cleanly through ``yaml.safe_load``.
        """
        data: dict = {
            "name": self.name,
            "hull_type": self.hull_type.value,
            "stations": [
                {
                    "x_position": s.x_position,
                    "waterline_offsets": [
                        list(pair) for pair in s.waterline_offsets
                    ],
                }
                for s in self.stations
            ],
            "length_bp": self.length_bp,
            "beam": self.beam,
            "draft": self.draft,
            "depth": self.depth,
            "source": self.source,
        }
        if self.deck_profile is not None:
            data["deck_profile"] = [list(p) for p in self.deck_profile]
        if self.keel_profile is not None:
            data["keel_profile"] = [list(p) for p in self.keel_profile]
        if self.block_coefficient is not None:
            data["block_coefficient"] = self.block_coefficient
        if self.displacement is not None:
            data["displacement"] = self.displacement
        return data

    @classmethod
    def from_yaml_dict(cls, data: dict) -> HullProfile:
        """Deserialize from a plain dict (e.g. from ``yaml.safe_load``).

        Converts list pairs back to tuples for waterline offsets and
        string hull types back to enums.
        """
        stations = [
            HullStation(
                x_position=s["x_position"],
                waterline_offsets=[
                    tuple(pair) for pair in s["waterline_offsets"]
                ],
            )
            for s in data["stations"]
        ]
        kwargs: dict = {
            "name": data["name"],
            "hull_type": HullType(data["hull_type"]),
            "stations": stations,
            "length_bp": data["length_bp"],
            "beam": data["beam"],
            "draft": data["draft"],
            "depth": data["depth"],
            "source": data["source"],
        }
        if "deck_profile" in data:
            kwargs["deck_profile"] = [
                tuple(p) for p in data["deck_profile"]
            ]
        if "keel_profile" in data:
            kwargs["keel_profile"] = [
                tuple(p) for p in data["keel_profile"]
            ]
        if "block_coefficient" in data:
            kwargs["block_coefficient"] = data["block_coefficient"]
        if "displacement" in data:
            kwargs["displacement"] = data["displacement"]
        return cls(**kwargs)

    def save_yaml(self, path: str | Path) -> Path:
        """Save this hull profile to a YAML file."""
        path = Path(path)
        with open(path, "w") as f:
            yaml.dump(
                self.to_yaml_dict(),
                f,
                default_flow_style=False,
                sort_keys=False,
            )
        return path

    @classmethod
    def load_yaml(cls, path: str | Path) -> HullProfile:
        """Load a hull profile from a YAML file.

        Raises:
            FileNotFoundError: If the file does not exist.
        """
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"Hull profile file not found: {path}")
        with open(path) as f:
            data = yaml.safe_load(f)
        return cls.from_yaml_dict(data)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "HullType",
    "HullStation",
    "HullProfile",
]
