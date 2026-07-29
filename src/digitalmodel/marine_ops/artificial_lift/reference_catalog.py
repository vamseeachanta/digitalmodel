"""Strict lookups for packaged artificial-lift equipment reference data."""

from __future__ import annotations

import csv
from dataclasses import dataclass, fields
from decimal import Decimal, InvalidOperation
import hashlib
from functools import lru_cache
from importlib import resources
import re
from types import MappingProxyType
from typing import Mapping
import unicodedata

import yaml

from ._reference_catalog_schema import OUTPUT_DEFINITIONS, unit_for_field


class AmbiguousCatalogKeyError(LookupError):
    """Raised when a catalog query does not identify exactly one record."""


@dataclass(frozen=True)
class RodProperties:
    grade: str
    diameter_in: Decimal
    area_in2: Decimal
    unit_weight_lbf_ft: Decimal
    modulus_psi: Decimal
    catalog_sonic_velocity_ft_s: Decimal
    weight_derived_velocity_ft_s: Decimal
    tensile_strength_psi: Decimal
    raw_sonic_velocity_kft_s: Decimal
    source_rows: tuple[int, ...]


@dataclass(frozen=True)
class CouplingProperties:
    rod_diameter_in: Decimal
    manufacturer: str
    size: str
    coupling_type: str
    coupling_diameter_in: Decimal
    coupling_length_in: Decimal | None
    tensile_strength_psi: Decimal | None
    friction_coefficient: Decimal | None
    source_row: int


@dataclass(frozen=True)
class SurfaceUnitGeometry:
    source_catalog: str
    source_identifier: str | None
    source_row: int
    manufacturer_key: str
    model_key: str
    geometry_code: str
    gearbox_rating_raw: Decimal | None
    beam_rating_raw: Decimal | None
    max_stroke_length_raw: Decimal | None
    dimensional_a_raw: Decimal | None
    dimensional_c_raw: Decimal | None
    dimensional_i_raw: Decimal | None
    dimensional_k_raw: Decimal | None
    dimensional_p_raw: Decimal | None
    stroke_length_pin_1_raw: Decimal | None
    stroke_length_pin_2_raw: Decimal | None
    stroke_length_pin_3_raw: Decimal | None
    stroke_length_pin_4_raw: Decimal | None
    stroke_length_pin_5_raw: Decimal | None
    stroke_length_pin_6_raw: Decimal | None
    stroke_length_pin_7_raw: Decimal | None
    stroke_length_pin_8_raw: Decimal | None
    radius_pin_1_raw: Decimal | None
    radius_pin_2_raw: Decimal | None
    radius_pin_3_raw: Decimal | None
    radius_pin_4_raw: Decimal | None
    radius_pin_5_raw: Decimal | None
    radius_pin_6_raw: Decimal | None
    radius_pin_7_raw: Decimal | None
    radius_pin_8_raw: Decimal | None
    structural_imbalance_raw: Decimal | None
    phase_angle_raw: Decimal | None
    counterbalance_effect_raw: Decimal | None
    air_balance_raw: Decimal | None
    air_balance_dimensional_d_raw: Decimal | None
    air_balance_dimensional_f_raw: Decimal | None
    air_balance_dimensional_h_raw: Decimal | None


@dataclass(frozen=True)
class ReferenceCatalog:
    version: str
    rods: tuple[RodProperties, ...]
    couplings: tuple[CouplingProperties, ...]
    surface_units: tuple[SurfaceUnitGeometry, ...]
    manifest: Mapping
    _rod_index: Mapping

    def rod_properties(self, grade, diameter_in) -> RodProperties:
        key = (_grade_key(grade), _diameter_key(diameter_in))
        try:
            return self._rod_index[key]
        except KeyError:
            raise KeyError(f"unknown rod catalog key: {key[0]} / {key[1]}") from None

    def find_couplings(
        self, rod_diameter_in, manufacturer=None, size=None, type=None
    ) -> tuple[CouplingProperties, ...]:
        diameter = _diameter_key(rod_diameter_in, max_places=5)
        filters = tuple(
            _optional_text_key(value) for value in (manufacturer, size, type)
        )
        return tuple(
            row
            for row in self.couplings
            if row.rod_diameter_in == diameter
            and _matches_filters(row, filters)
        )

    def find_surface_units(
        self, manufacturer, model, source_catalog=None
    ) -> tuple[SurfaceUnitGeometry, ...]:
        manufacturer_key = _required_text_key(manufacturer)
        model_key = _required_text_key(model)
        source = _source_catalog_key(source_catalog)
        return tuple(
            row
            for row in self.surface_units
            if _row_text_key(row.manufacturer_key) == manufacturer_key
            and _row_text_key(row.model_key) == model_key
            and (source is None or row.source_catalog == source)
        )


def _matches_filters(row, filters) -> bool:
    values = (
        _required_text_key(row.manufacturer),
        _required_text_key(row.size),
        _required_text_key(row.coupling_type),
    )
    return all(expected is None or actual == expected for actual, expected in zip(
        values, filters
    ))


def _required_text_key(value) -> str:
    if not isinstance(value, str):
        raise TypeError("catalog text keys must be strings")
    key = " ".join(unicodedata.normalize("NFKC", value).split()).casefold()
    if not key:
        raise ValueError("catalog text keys must not be blank")
    return key


def _optional_text_key(value) -> str | None:
    return None if value is None else _required_text_key(value)


def _row_text_key(value) -> str:
    if not isinstance(value, str):
        return ""
    return " ".join(unicodedata.normalize("NFKC", value).split()).casefold()


def _grade_key(value) -> str:
    key = _required_text_key(value).upper()
    if re.fullmatch(r"\d+\s*-\s*(?:\d+(?:\.\d*)?|\.\d+)", key):
        raise ValueError("grade and diameter must be supplied separately")
    return re.sub(r"\s*-\s*", " - ", key)


def _diameter_key(value, max_places=3) -> Decimal:
    if isinstance(value, bool) or not isinstance(value, (str, int, float, Decimal)):
        raise TypeError("diameter must be a string or number")
    try:
        number = Decimal(str(value))
    except InvalidOperation as exc:
        raise ValueError(f"invalid diameter: {value!r}") from exc
    if not number.is_finite() or number <= 0:
        raise ValueError("diameter must be finite and positive")
    if number.as_tuple().exponent < -max_places:
        raise ValueError(f"diameter supports at most {max_places} fractional digits")
    return number.normalize()


def _source_catalog_key(value) -> str | None:
    if value is None:
        return None
    key = _required_text_key(value)
    allowed = {"surface_unit_catalog", "rodpump_units"}
    if key not in allowed:
        raise ValueError(f"unsupported surface catalog: {value!r}")
    return key


def _deep_freeze(value):
    if isinstance(value, dict):
        return MappingProxyType({
            key: _deep_freeze(item) for key, item in value.items()
        })
    if isinstance(value, list):
        return tuple(_deep_freeze(item) for item in value)
    return value


def _decimal(value: str) -> Decimal:
    return Decimal(value)


def _optional_decimal(value: str) -> Decimal | None:
    return Decimal(value) if value else None


def _read_csv(resource) -> list[dict[str, str]]:
    lines = (
        line
        for line in resource.read_text(encoding="utf-8").splitlines()
        if not line.startswith("#")
    )
    return list(csv.DictReader(lines))


def _csv_header(resource) -> list[str]:
    lines = (
        line
        for line in resource.read_text(encoding="utf-8").splitlines()
        if not line.startswith("#")
    )
    return next(csv.reader(lines))


def _verify_outputs(root, manifest) -> None:
    if set(manifest["outputs"]) != set(OUTPUT_DEFINITIONS):
        raise ValueError("catalog output set does not match schema")
    for filename, metadata in manifest["outputs"].items():
        resource = root.joinpath(filename)
        content = resource.read_bytes()
        digest = hashlib.sha256(content).hexdigest()
        if digest != metadata["sha256"]:
            raise ValueError(f"catalog digest mismatch: {filename}")
        rows = _read_csv(resource)
        if len(rows) != metadata["row_count"]:
            raise ValueError(f"catalog row-count mismatch: {filename}")
        expected_columns = OUTPUT_DEFINITIONS[filename]
        if metadata["columns"] != expected_columns:
            raise ValueError(f"catalog declared schema mismatch: {filename}")
        if _csv_header(resource) != expected_columns:
            raise ValueError(f"catalog CSV schema mismatch: {filename}")
        expected_units = {
            field: unit_for_field(filename, field) for field in expected_columns
        }
        if metadata["units"] != expected_units:
            raise ValueError(f"catalog units mismatch: {filename}")


def _load_rods(root) -> tuple[RodProperties, ...]:
    rows = _read_csv(root.joinpath("rod_details.csv"))
    return tuple(
        RodProperties(
            grade=row["grade"],
            diameter_in=_decimal(row["diameter_in"]),
            area_in2=_decimal(row["area_in2"]),
            unit_weight_lbf_ft=_decimal(row["unit_weight_lbf_ft"]),
            modulus_psi=_decimal(row["modulus_psi"]),
            catalog_sonic_velocity_ft_s=_decimal(
                row["catalog_sonic_velocity_ft_s"]
            ),
            weight_derived_velocity_ft_s=_decimal(
                row["weight_derived_velocity_ft_s"]
            ),
            tensile_strength_psi=_decimal(row["tensile_strength_psi"]),
            raw_sonic_velocity_kft_s=_decimal(
                row["raw_sonic_velocity_kft_s"]
            ),
            source_rows=tuple(int(value) for value in row["source_rows"].split(";")),
        )
        for row in rows
    )


def _load_couplings(root) -> tuple[CouplingProperties, ...]:
    rows = _read_csv(root.joinpath("couplings.csv"))
    return tuple(
        CouplingProperties(
            rod_diameter_in=_decimal(row["rod_diameter_in"]),
            manufacturer=row["manufacturer"],
            size=row["size"],
            coupling_type=row["coupling_type"],
            coupling_diameter_in=_decimal(row["coupling_diameter_in"]),
            coupling_length_in=_optional_decimal(row["coupling_length_in"]),
            tensile_strength_psi=_optional_decimal(row["tensile_strength_psi"]),
            friction_coefficient=_optional_decimal(row["friction_coefficient"]),
            source_row=int(row["source_row"]),
        )
        for row in rows
    )


def _load_surfaces(root) -> tuple[SurfaceUnitGeometry, ...]:
    numeric_fields = {
        field.name
        for field in fields(SurfaceUnitGeometry)
        if field.name.endswith("_raw")
    }
    surfaces = []
    for filename in ("surface_unit_catalog.csv", "rodpump_units.csv"):
        for row in _read_csv(root.joinpath(filename)):
            values = {
                name: _optional_decimal(value) if name in numeric_fields else value
                for name, value in row.items()
            }
            values["source_row"] = int(values["source_row"])
            values["source_identifier"] = values["source_identifier"] or None
            surfaces.append(SurfaceUnitGeometry(**values))
    return tuple(surfaces)


@lru_cache(maxsize=None)
def load_catalog(version: str = "v1") -> ReferenceCatalog:
    if version != "v1":
        raise KeyError(f"unknown catalog version: {version}")
    package = "digitalmodel.marine_ops.artificial_lift.reference_data"
    root = resources.files(package).joinpath(version)
    manifest = yaml.safe_load(root.joinpath("manifest.yml").read_text(encoding="utf-8"))
    if manifest.get("schema_version") != "1.0":
        raise ValueError("unsupported catalog schema version")
    if manifest.get("catalog_version") != version:
        raise ValueError("catalog version mismatch")
    _verify_outputs(root, manifest)
    rods = _load_rods(root)
    rod_index = MappingProxyType({
        (_grade_key(row.grade), row.diameter_in): row for row in rods
    })
    return ReferenceCatalog(
        version=version,
        rods=rods,
        couplings=_load_couplings(root),
        surface_units=_load_surfaces(root),
        manifest=_deep_freeze(manifest),
        _rod_index=rod_index,
    )


def rod_properties(grade, diameter_in) -> RodProperties:
    return load_catalog().rod_properties(grade, diameter_in)


def find_couplings(
    rod_diameter_in, manufacturer=None, size=None, type=None
) -> tuple[CouplingProperties, ...]:
    return load_catalog().find_couplings(
        rod_diameter_in, manufacturer=manufacturer, size=size, type=type
    )


def coupling_properties(
    rod_diameter_in, manufacturer=None, size=None, type=None
) -> CouplingProperties:
    matches = find_couplings(
        rod_diameter_in, manufacturer=manufacturer, size=size, type=type
    )
    if not matches:
        raise KeyError(f"unknown coupling catalog key: {rod_diameter_in!r}")
    if len(matches) != 1:
        raise AmbiguousCatalogKeyError(
            f"coupling key matched {len(matches)} catalog records"
        )
    return matches[0]


def find_surface_units(
    manufacturer, model, source_catalog=None
) -> tuple[SurfaceUnitGeometry, ...]:
    return load_catalog().find_surface_units(manufacturer, model, source_catalog)


def surface_unit_geometry(
    manufacturer, model, source_catalog=None
) -> SurfaceUnitGeometry:
    matches = find_surface_units(manufacturer, model, source_catalog)
    if not matches:
        raise KeyError(f"unknown surface-unit catalog key: {manufacturer!r} / {model!r}")
    if len(matches) != 1:
        raise AmbiguousCatalogKeyError(
            f"surface-unit key matched {len(matches)} catalog records"
        )
    return matches[0]
