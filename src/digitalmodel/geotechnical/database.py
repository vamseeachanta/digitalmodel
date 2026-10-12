"""Load the research-note transcription without inferring engineering inputs."""

from dataclasses import dataclass
from pathlib import Path
import re
from types import MappingProxyType
from typing import Mapping

import pint
import yaml

from digitalmodel.units import Q_, ureg
from .database_schema import ParameterRow, Quantity, Source, UnitValue, freeze

DEFAULT_PATH = Path(__file__).resolve().parents[3] / "data/geotechnical/geotech_database.yaml"
LOOKUP_FIELDS = ("soil_unit", "parameter", "site_or_region", "source_id", "confidence_grade")
NUMBER = r"[+-]?(?:\d+(?:,\d{3})*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?"
PLAIN = re.compile(rf"({NUMBER})(?:\s*(?:to|-)\s*({NUMBER}))?")
PREFIX = re.compile(r"^(around|typically around|typically about|typically|about|largely|"
                    r"generally|on the order of|approximately|<|>)\s*")
# Aliases only: all conversion factors are supplied by the existing Pint registry.
UNIT_TARGETS = {
    "kN/m3": ("kN/m**3", "N/m**3"), "%": ("percent", "dimensionless"),
    "-": ("dimensionless", "dimensionless"), "kPa": ("kPa", "Pa"),
    "Pa": ("Pa", "Pa"), "degrees": ("degree", "radian"), "m/s": ("m/s", "m/s"),
    "psf": ("force_pound/foot**2", "kPa"),
    "psf/ft": ("force_pound/foot**3", "kPa/m"), "pcf": ("pcf", "kN/m**3"),
}


@dataclass(frozen=True)
class Database:
    sources: Mapping[str, Source]
    rows: Mapping[str, ParameterRow]
    quantities: tuple[Quantity, ...]
    meta: Mapping
    gaps: tuple
    excluded: tuple
    indexes: Mapping

    def lookup(self, field: str, value: str) -> tuple[ParameterRow, ...]:
        if field not in self.indexes:
            raise ValueError(f"unsupported lookup field {field!r}")
        return self.indexes[field].get(value, ())

    def quantities_for(self, row_id: str) -> tuple[Quantity, ...]:
        return tuple(q for q in self.quantities if q.parent_row_id == row_id)


def _split(row: ParameterRow) -> tuple[tuple[str, str, str], ...]:
    """Split only explicit printed structures, retaining their row provenance."""
    name, value, units = row.parameter, row.value_or_range, row.units_as_printed
    if name == "Dry / wet unit weight":
        values = value.split(" / ")
        if len(values) == 2 and all(PLAIN.fullmatch(v) for v in values):
            return tuple(zip(("Dry unit weight", "wet unit weight"), values, (units, units)))
    if "; " in name:
        names, values, unit_names = name.split("; "), value.split("; "), units.split("; ")
        if len(names) == len(values) == len(unit_names) and all(names + values + unit_names):
            return tuple(zip(names, values, unit_names))
    if name == "Ratio of horizontal to vertical permeability":
        parts = [re.fullmatch(rf"({NUMBER} to {NUMBER}) (\(.+\))", v)
                 for v in value.split("; ")]
        if len(parts) > 1 and all(parts):
            return tuple((f"{name} {p[2]}", p[1], units) for p in parts)
    return ((name, value, units),)


def _printed_value(value: str, units: str) -> tuple[UnitValue | None, str]:
    """Read simple numbers/bounds; retain qualifications and leave formulas opaque."""
    text, qualifier = value, ""
    prefix = PREFIX.match(text)
    if prefix:
        qualifier, text = prefix[1], text[prefix.end():]
    suffix = re.search(r" (\([^()]+\))$", text)
    if suffix:
        qualifier = " ".join(filter(None, (qualifier, suffix[1])))
        text = text[:suffix.start()]
    match = PLAIN.fullmatch(text)
    if not match:
        return None, value
    first = float(match[1].replace(",", ""))
    second = float(match[2].replace(",", "")) if match[2] else None
    if second is not None:
        return UnitValue(units, minimum=first, maximum=second), qualifier
    if qualifier in ("<", ">"):
        bounds = {"maximum" if qualifier == "<" else "minimum": first}
        return UnitValue(units, **bounds), qualifier
    return UnitValue(units, value=first), qualifier


def _convert(value: UnitValue, printed_unit: str) -> UnitValue:
    original, target = UNIT_TARGETS[printed_unit]

    def convert(number):
        if number is None:
            return None
        quantity = Q_(number, original)
        # The shared registry defines pcf as mass density, while these rows give weight.
        if printed_unit == "pcf":
            quantity *= ureg.standard_gravity
        return quantity.to(target).magnitude

    return UnitValue(target, convert(value.value), convert(value.minimum), convert(value.maximum))


def _quantity(row: ParameterRow, name: str, value: str, units: str) -> Quantity:
    printed, qualifier = _printed_value(value, units)
    si, reason = None, ""
    if printed is None:
        reason = "printed value is a formula, alternatives or compound text"
    elif qualifier:
        reason = "printed value is qualified; qualification is retained"
    elif units not in UNIT_TARGETS:
        reason = "printed unit is not convertible with the repository unit facility"
    else:
        try:
            si = _convert(printed, units)
        except (pint.errors.PintError, ValueError) as error:
            reason = f"unit conversion unavailable: {error}"
    return Quantity(row, name, value, units, qualifier, printed, si, reason)


def _unique(entries, make, id_field):
    result = {}
    for entry in entries:
        record = make(entry)
        identifier = getattr(record, id_field)
        if identifier in result:
            raise ValueError(f"duplicate id {identifier!r}")
        result[identifier] = record
    return MappingProxyType(result)


def _read_yaml(path):
    """Reject duplicate mapping keys before a parser can silently discard cells."""
    class UniqueLoader(yaml.SafeLoader):
        pass

    def mapping(loader, node, deep=False):
        result = {}
        for key_node, value_node in node.value:
            key = loader.construct_object(key_node, deep=deep)
            if key in result:
                raise ValueError(f"duplicate YAML key {key!r}")
            result[key] = loader.construct_object(value_node, deep=deep)
        return result

    UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, mapping)
    with Path(path).open(encoding="utf-8") as stream:
        return yaml.load(stream, Loader=UniqueLoader)


def load_database(data=DEFAULT_PATH) -> Database:
    """Load a YAML path or validate an in-memory transcription, without defaults."""
    if not isinstance(data, Mapping):
        data = _read_yaml(data)
    for section in ("meta", "sources", "parameters", "gaps", "excluded"):
        if not isinstance(data, Mapping) or section not in data:
            raise ValueError(f"required section {section!r} is missing")
    sources = _unique(data["sources"], Source.from_entry, "source_id")
    rows = _unique(data["parameters"], lambda e: ParameterRow.from_entry(e, sources), "row_id")
    quantities = tuple(_quantity(row, *part) for row in rows.values() for part in _split(row))
    indexes = {}
    for field in LOOKUP_FIELDS:
        index = {}
        for row in rows.values():
            index.setdefault(getattr(row, field), []).append(row)
        indexes[field] = freeze(index)
    return Database(sources, rows, quantities, freeze(data["meta"]), freeze(data["gaps"]),
                    freeze(data["excluded"]), freeze(indexes))
