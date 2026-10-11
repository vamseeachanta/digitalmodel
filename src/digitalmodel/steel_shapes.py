"""AISC nominal section lookup with explicit imperial and metric aliases.

Geometry defaults to mm; metadata retains source inches and in². Names ignore
case and whitespace and accept either ``x`` or ``×``. Fractions/decimals are
accepted only when explicitly catalogued (no arithmetic designation guessing).
See data_systems/data/steel_shapes_provenance.yaml for model limitations.
"""

from __future__ import annotations

import copy
import json
import math
import warnings
from importlib.resources import files
from pathlib import Path

_DATA_PACKAGE = "digitalmodel.data_systems.data"
_IN_TO_MM = 25.4  # Existing scripts/integrations/aisc_section_lookup.py.
_FIELDS = {
    "W": ("d", "bf", "tf", "tw", "kdes"),
    "HP": ("d", "bf", "tf", "tw", "kdes"),
    "S": ("d", "bf", "tf", "tw", "kdes"),
    "C": ("d", "bf", "tf", "tw", "kdes"),
    "L": ("d", "b", "t", "kdes"),
    "HSS_RECT": ("d", "b", "t", "tnom"),
    "HSS_ROUND": ("d", "t", "tnom"),
    "PIPE": ("d", "t", "tnom"),
}


def _normalize(name: str) -> str:
    if not isinstance(name, str):
        raise TypeError("Steel section name must be a string")
    key = "".join(name.upper().replace("×", "X").split())
    if not key:
        raise ValueError("Steel section name must be non-empty")
    return key


def _check_dimensions(item: dict) -> None:
    dims = item["dimensions"]
    for key in _FIELDS[item["family"]]:
        value = dims.get(key)
        if (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(value)
            or value <= 0
        ):
            raise ValueError(f"Invalid dimension {key} for {item['name']}")
    family = item["family"]
    if family in {"W", "S", "HP", "C"}:
        r = dims["kdes"] - dims["tf"]
        outstand = dims["bf"] - dims["tw"]
        if family != "C":
            outstand /= 2
        invalid = (
            r < 0 or 2 * dims["kdes"] >= dims["d"] or outstand <= 0 or r > outstand
        )
    elif family == "L":
        r = dims["kdes"] - dims["t"]
        invalid = r < 0 or dims["kdes"] >= min(dims["d"], dims["b"])
    else:
        size = min(dims["d"], dims.get("b", dims["d"]))
        invalid = 2 * dims["t"] >= size or dims["t"] > dims["tnom"]
        if family == "HSS_RECT":
            invalid |= 4 * dims["t"] > size
    if invalid:
        raise ValueError(f"Impossible geometry dimensions for {item['name']}")


def validate_catalog(data: dict) -> dict[tuple[str, str], dict]:
    """Validate schema, finite dimensions, geometry fit and unique aliases.

    Returns an alias index. Input records are never modified. Schema validation
    and geometry checks also apply to user-imported catalogues.
    """
    from jsonschema import Draft202012Validator, FormatChecker

    schema = json.loads(
        files(_DATA_PACKAGE).joinpath("steel_shapes.schema.json").read_text()
    )
    errors = list(
        Draft202012Validator(schema, format_checker=FormatChecker()).iter_errors(data)
    )
    if errors:
        error = errors[0]
        raise ValueError(
            f"Invalid catalogue dimension/metadata at {list(error.path)}: {error.message}"
        )
    index = {}
    for item in data["sections"]:
        if not math.isfinite(item["area"]):
            raise ValueError(f"Invalid area for {item['name']}")
        _check_dimensions(item)
        names_by_system = {
            "imperial": [item["name"], *item["aliases"]],
            "metric": [item["metric_name"], *item.get("metric_aliases", [])],
        }
        for system, names in names_by_system.items():
            for name in names:
                key = (system, _normalize(name))
                if key in index and index[key] is not item:
                    raise ValueError(f"Duplicate {system} section alias: {name}")
                index[key] = item
    return index


def _load_catalog(catalog_path: str | Path | None) -> dict[tuple[str, str], dict]:
    path = (
        files(_DATA_PACKAGE).joinpath("steel_shapes_sample.json")
        if catalog_path is None
        else Path(catalog_path)
    )
    return validate_catalog(json.loads(path.read_text(encoding="utf-8")))


def get_section_data(
    name: str, *, name_system: str | None = None, catalog_path: str | Path | None = None
) -> dict:
    """Return an independent copy of raw inch dimensions, area and provenance.

    ``catalog_path`` selects an explicit full local catalogue; absence of that
    file fails rather than silently falling back to the packaged sample.
    """
    key = _normalize(name)
    if name_system not in {None, "imperial", "metric"}:
        raise ValueError("name_system must be None, 'imperial' or 'metric'")
    index = _load_catalog(catalog_path)
    systems = ("imperial", "metric") if name_system is None else (name_system,)
    matches = [index[(system, key)] for system in systems if (system, key) in index]
    if not matches:
        raise KeyError(
            f"Unknown steel section {name!r}; use list_sections() for available names"
        )
    if len(matches) > 1 and matches[0] is not matches[1]:
        raise KeyError(
            f"Ambiguous steel section {name!r}; specify name_system='imperial' or 'metric'"
        )
    return copy.deepcopy(matches[0])


def list_sections(
    family: str | None = None, *, catalog_path: str | Path | None = None
) -> list[str]:
    """List canonical imperial names; HSS subfamilies are RECT and ROUND."""
    records = _load_catalog(catalog_path).values()
    return sorted(
        {
            item["name"]
            for item in records
            if family is None or item["family"] == family.upper()
        }
    )


def _tapered_geometry(library, family: str, dims: dict):
    # Approximate S/C flange slope: 2:12, cited in provenance. The published
    # nominal tf is interpreted as mid-outstand thickness. Toe radii are omitted.
    slope = 2 / 12
    outstand = dims["bf"] - dims["tw"]
    if family == "S":
        outstand /= 2
    root_thickness = dims["tf"] + slope * outstand / 2
    # Circle tangent to x=tw and y=tf_root-slope*(x-tw):
    # kdes = tf_root + r*(sqrt(1+slope²)-slope).
    radius = (dims["kdes"] - root_thickness) / (math.sqrt(1 + slope**2) - slope)
    if radius < 0 or radius > outstand or dims["tf"] - slope * outstand / 2 <= 0:
        raise ValueError("Tapered geometry basis does not fit the published dimensions")
    warnings.warn(
        "S/C geometry is approximate: nominal mid-flange thickness and sharp toes; "
        "original rolled-profile validation remains required",
        UserWarning,
        stacklevel=3,
    )
    constructor = (
        library.tapered_flange_i_section
        if family == "S"
        else library.tapered_flange_channel
    )
    return constructor(
        d=dims["d"],
        b=dims["bf"],
        t_f=dims["tf"],
        t_w=dims["tw"],
        r_r=radius,
        r_f=0,
        alpha=math.degrees(math.atan(slope)),
        n_r=16,
    )


def _construct_geometry(library, family: str, dims: dict):
    """Construct a validated nominal profile using sourced dimensions."""
    if family in {"W", "HP"}:
        return library.i_section(
            d=dims["d"],
            b=dims["bf"],
            t_f=dims["tf"],
            t_w=dims["tw"],
            r=dims["kdes"] - dims["tf"],
            n_r=16,
        )
    if family in {"S", "C"}:
        return _tapered_geometry(library, family, dims)
    if family == "L":
        return library.angle_section(
            d=dims["d"],
            b=dims["b"],
            t=dims["t"],
            r_r=dims["kdes"] - dims["t"],
            r_t=0,
            n_r=16,
        )
    if family == "HSS_RECT":
        return library.rectangular_hollow_section(
            d=dims["d"], b=dims["b"], t=dims["t"], r_out=2 * dims["t"], n_r=16
        )
    return library.circular_hollow_section(d=dims["d"], t=dims["t"], n=64)


def get_section(
    name: str,
    unit: str = "metric",
    *,
    name_system: str | None = None,
    catalog_path: str | Path | None = None,
):
    """Return an unmeshed sectionproperties Geometry in mm or inches.

    Metric *names* are aliases; ``unit`` controls geometry coordinates regardless
    of the name used. W/HP root radius is kdes-tf; L root radius is kdes-t with
    sharp toes; HSS outer radius is idealised as 2*tdes. Pipe and round HSS use
    design wall thickness. S/C issue an approximation warning. No material or
    strength is inferred from designation. Resolution reuses existing sections.
    """
    if unit not in {"metric", "imperial"}:
        raise ValueError("unit must be 'metric' (mm) or 'imperial' (in)")
    data = get_section_data(name, name_system=name_system, catalog_path=catalog_path)
    try:
        from sectionproperties.pre import library
    except ImportError as exc:
        raise ImportError(
            "Geometry construction requires the declared sectionproperties dependency"
        ) from exc
    scale = _IN_TO_MM if unit == "metric" else 1
    dims = {key: value * scale for key, value in data["dimensions"].items()}
    return _construct_geometry(library, data["family"], dims)
