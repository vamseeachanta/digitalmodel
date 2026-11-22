"""Edition-keyed, citation-carrying screening inputs, following foam_system.

Bundled values are issue-supplied (#3821) NORSOK S-DP-001 Rev. 2 (1996), clauses
7.8/8.1 and 11.3 step 3: superseded regression inputs, not independently
source-verified design data. Current licensed values must be supplied by the
caller. Structural validation does not authenticate a standard or its contents.
"""

from __future__ import annotations

from pathlib import Path
from types import MappingProxyType
from typing import Annotated, Literal, Mapping

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    field_serializer,
    field_validator,
    model_validator,
)
import yaml

from digitalmodel.foam_system.sizing import Citation

LIBRARY_PATH = (
    Path(__file__).resolve().parent / "data" / "technical_safety_criteria.yml"
)
Positive = Annotated[float, Field(strict=True, gt=0, allow_inf_nan=False)]
NonNegative = Annotated[float, Field(strict=True, ge=0, allow_inf_nan=False)]
CriterionKey = Literal[
    "supply_vessel_collision_energy",
    "riser_collision_energy",
    "jet_peak",
    "jet_average",
    "jet_water_average",
    "pool_fuel_controlled_peak",
    "pool_fuel_controlled_average",
    "pool_fuel_controlled_water_average",
    "pool_ventilation_controlled_peak",
    "pool_ventilation_controlled_average",
    "pool_ventilation_controlled_water_average",
]


class _UniqueKeyLoader(yaml.SafeLoader):
    """Reject silent overwrites of edition names, limits and citation fields."""

    def construct_mapping(self, node, deep=False):
        self.flatten_mapping(node)
        mapping = {}
        for key_node, value_node in node.value:
            key = self.construct_object(key_node, deep=deep)
            try:
                if key in mapping:
                    raise ValueError(f"duplicate YAML key: {key!r}")
                mapping[key] = self.construct_object(value_node, deep=deep)
            except TypeError as exc:
                raise ValueError("YAML mapping keys must be scalar") from exc
        return mapping


class ScreeningModel(BaseModel):
    model_config = ConfigDict(extra="forbid", frozen=True)


def validate_citation(citation: Citation) -> Citation:
    """Strengthen the shared sidecar's nonblank checks without changing it."""
    for field in ("standard", "edition", "clause"):
        value = getattr(citation, field)
        if not isinstance(value, str) or not value.strip():
            raise ValueError(f"citation.{field} must be a non-empty string")
    return citation


class CitedValue(ScreeningModel):
    """A positive SI screening value and its mandatory Citation sidecar."""

    value: Positive
    unit: Literal["MJ", "kW/m2"]
    citation: Citation

    _citation_valid = field_validator("citation")(validate_citation)


class EditionCriteria(ScreeningModel):
    """One explicitly selected edition; results retain its status and values.

    ``project`` identifies caller-supplied criteria, not verified compliance
    with a current standard. ``superseded`` requires explicit test opt-in.
    """

    standard: str = Field(min_length=1)
    edition: str = Field(min_length=1)
    status: Literal["superseded", "project"]
    values: Mapping[CriterionKey, CitedValue]

    @model_validator(mode="after")
    def check_values(self) -> EditionCriteria:
        if not self.values or not self.standard.strip() or not self.edition.strip():
            raise ValueError("standard, edition and values must be non-empty")
        for key, value in self.values.items():
            expected = "MJ" if key.endswith("collision_energy") else "kW/m2"
            if value.unit != expected:
                raise ValueError(f"{key}: unit must be {expected}")
            if (value.citation.standard, value.citation.edition) != (
                self.standard,
                self.edition,
            ):
                raise ValueError(
                    f"{key}: citation standard and edition must match selected edition"
                )
        object.__setattr__(self, "values", MappingProxyType(dict(self.values)))
        return self

    @field_serializer("values")
    def serialize_values(self, values):
        return dict(values)

    def require(self, key: str, *, allow_test_criteria: bool = False) -> CitedValue:
        """Refuse historical use by default and refuse missing/uncited limits."""
        if type(allow_test_criteria) is not bool:
            raise ValueError("allow_test_criteria must be an explicit boolean")
        if self.status == "superseded" and not allow_test_criteria:
            raise ValueError(
                "superseded criteria are for tests only; explicit opt-in required"
            )
        if key not in self.values:
            raise ValueError(f"missing criterion {key!r} in edition {self.edition!r}")
        value = self.values[key]
        validate_citation(value.citation)
        return value


def get_criteria(key: str, path: Path | None = None) -> EditionCriteria:
    """Load an exact edition key; no latest-edition fallback or inherited values."""
    payload = yaml.load(
        (LIBRARY_PATH if path is None else Path(path)).read_text(encoding="utf-8"),
        Loader=_UniqueKeyLoader,
    )
    if (
        not isinstance(payload, dict)
        or type(payload.get("schema_version")) is not int
        or payload["schema_version"] != 1
    ):
        raise ValueError("criteria library schema_version must be 1")
    editions = payload.get("editions")
    if not isinstance(editions, dict) or not editions:
        raise ValueError("criteria library editions must be a non-empty mapping")
    # Validate every row, including unselected entries, to reject corrupt libraries.
    validated = {
        name: EditionCriteria.model_validate(row) for name, row in editions.items()
    }
    if key not in validated:
        raise ValueError(
            f"unknown criteria edition {key!r}; available: {sorted(validated)}"
        )
    return validated[key]
