"""Structured vendor equipment data, independent of standards citations.

Provider provenance records the source brochure without resolving wiki pages,
reading private documents, or invoking a solver. The companion provider library
owns the parameter YAML and the human-readable model-field mapping.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import ClassVar

import yaml


@dataclass(frozen=True)
class ProviderDataSource:
    """Vendor-document provenance; the PDF and its checksum are not fetched."""

    vendor: str
    vendor_slug: str
    document_name: str
    revision_date: str
    origin_url: str
    private_mount_path: str
    inventory_ref: str
    retrieved_date: str
    sha256: str = ""
    note: str = ""

    def __post_init__(self) -> None:
        for name in (
            "vendor",
            "vendor_slug",
            "document_name",
            "origin_url",
            "private_mount_path",
            "inventory_ref",
            "retrieved_date",
        ):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"ProviderDataSource.{name} must be non-empty string")


@dataclass(frozen=True)
class SpecValue:
    """A numeric specification with units or text with unit ``categorical``."""

    value: float | str
    unit: str
    note: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.unit, str) or not self.unit.strip():
            raise ValueError("SpecValue.unit must be non-empty string")
        if isinstance(self.value, bool) or not isinstance(
            self.value, (int, float, str)
        ):
            raise ValueError("SpecValue.value must be numeric or categorical text")
        if isinstance(self.value, str) != (self.unit == "categorical"):
            raise ValueError("SpecValue: string values require unit='categorical'")
        if isinstance(self.value, str) and not self.value.strip():
            raise ValueError("SpecValue categorical value must be non-empty")


@dataclass
class ProviderDataRecord:
    """One equipment-system variant with specifications and optional mappings.

    Numeric values must be finite and non-negative. Lists represent multiple
    specification variants and must contain at least one value. Validation
    checks data structure and ratings, without performing engineering sizing.
    """

    provider: ProviderDataSource
    system_name: str
    system_slug: str
    specifications: dict[str, SpecValue | list[SpecValue]]
    orcaflex_mapping: dict[str, str] = field(default_factory=dict)

    _REQUIRED_SPECS: ClassVar[frozenset[str]] = frozenset(
        {
            "mwd_m",
            "mwp_psi",
            "bore_production_id_m",
            "design_temp_min_degF",
            "design_temp_max_degF",
            "subsea_package_weight_kg",
        }
    )

    def validate(self) -> None:
        if not isinstance(self.provider, ProviderDataSource):
            raise ValueError("ProviderDataRecord.provider must be ProviderDataSource")
        for name in ("system_name", "system_slug"):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"ProviderDataRecord.{name} must be non-empty string")
        if not isinstance(self.specifications, dict):
            raise ValueError("ProviderDataRecord.specifications must be a mapping")
        missing = self._REQUIRED_SPECS - self.specifications.keys()
        if missing:
            raise ValueError(
                f"ProviderDataRecord missing required specs: {sorted(missing)}"
            )
        for key, spec in self.specifications.items():
            if not isinstance(key, str) or not key.strip():
                raise ValueError("Specification keys must be non-empty strings")
            items = spec if isinstance(spec, list) else [spec]
            if not items:
                raise ValueError(f"Spec {key!r} must contain at least one value")
            for item in items:
                if not isinstance(item, SpecValue):
                    raise ValueError(f"Spec {key!r} must contain SpecValue entries")
                if isinstance(item.value, (int, float)):
                    if not math.isfinite(item.value) or item.value < 0:
                        raise ValueError(
                            f"Spec {key!r} must be finite non-negative numeric value"
                        )
        if not isinstance(self.orcaflex_mapping, dict):
            raise ValueError("ProviderDataRecord.orcaflex_mapping must be a mapping")


def _mapping(value: object, name: str) -> dict:
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be a mapping")
    return value


def _spec_value(raw: object, key: str) -> SpecValue:
    try:
        return SpecValue(**_mapping(raw, f"Spec {key!r}"))
    except (TypeError, ValueError) as exc:
        raise ValueError(f"Spec {key!r}: {exc}") from exc


def load_provider_yaml(yaml_path: Path) -> ProviderDataRecord:
    """Read a provider YAML file and validate its structure and specifications.

    Invalid records raise ValueError. Malformed YAML raises yaml.YAMLError;
    filesystem errors propagate. Provenance dates should be quoted in YAML to
    retain their string representation.
    """
    raw = _mapping(yaml.safe_load(yaml_path.read_text(encoding="utf-8")), "Record")
    try:
        source_fields = _mapping(raw["source"], "source")
        system = _mapping(raw["system"], "system")
        raw_specs = _mapping(raw["specifications"], "specifications")
    except KeyError as exc:
        raise ValueError(f"Record missing required section: {exc.args[0]}") from exc

    try:
        source = ProviderDataSource(**source_fields)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"Invalid source: {exc}") from exc
    try:
        system_name, system_slug = system["name"], system["slug"]
    except KeyError as exc:
        raise ValueError(f"system missing required field: {exc.args[0]}") from exc

    specs = {}
    for key, value in raw_specs.items():
        specs[key] = (
            [_spec_value(item, key) for item in value]
            if isinstance(value, list)
            else _spec_value(value, key)
        )
    record = ProviderDataRecord(
        provider=source,
        system_name=system_name,
        system_slug=system_slug,
        specifications=specs,
        orcaflex_mapping=raw.get("orcaflex_mapping", {}),
    )
    record.validate()
    return record
