# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free source metadata and catalogue page rendering."""

from html import escape
import math
import re
from typing import Any, Iterable

import yaml

MISSING = "not stated"
SAFE_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
OBJECTS = (
    "Lines",
    "Vessels",
    "6DBuoys",
    "3DBuoys",
    "LineTypes",
    "VesselTypes",
    "BuoyTypes",
    "Shapes",
    "Constraints",
    "Winches",
    "Links",
    "ClumpTypes",
    "WingTypes",
    "FlexJointTypes",
    "FlexJoints",
    "DragChainTypes",
    "DragChains",
    "StiffenerTypes",
    "SupportTypes",
    "MorisonElementTypes",
    "Turbines",
    "AttachedBuoys",
    "VariableData",
    "ExpansionTables",
    "PyModels",
    "WakeModels",
    "MultibodyGroups",
    "BrowserGroups",
)
SPEC_OBJECTS = {re.sub(r"(?<!^)(?=[A-Z])", "_", key).lower(): key for key in OBJECTS}
SPEC_OBJECTS.update(
    buoys_6d="6DBuoys", buoys_3d="3DBuoys", variable_data_sources="VariableData"
)
ENV_KEYS = (
    "WaterDepth",
    "WaterSurfaceZ",
    "Density",
    "WaveHeight",
    "WavePeriod",
    "WaveDirection",
    "WaveHs",
    "WaveTp",
    "WaveTz",
    "RefCurrentSpeed",
    "RefCurrentDirection",
    "WindSpeed",
    "WindDirection",
    "depth",
    "density",
    "height",
    "period",
    "direction",
    "speed",
    "hs",
    "tp",
)
ENV_TYPES = "airy|Airy|dean_stream|Dean stream|JONSWAP|jonswap|Flat".split("|")
COUNT_RULE = (
    "Counting rule: object lists count entries; named object mappings count "
    "definitions (New/Deleted are directives). Include-only objects count under "
    "IncludeFile; BaseFile/IncludeFile references are counted, never expanded. "
    "Specification objects are labelled generic.section; other top-level sections "
    "are not object counts."
)


def mapping(value: Any) -> dict:
    """Return a mapping without inventing absent values."""
    return value if isinstance(value, dict) else {}


def number(value: Any) -> bool:
    """Recognise finite numerical values, excluding YAML booleans."""
    return type(value) in (int, float) and math.isfinite(value)


def fmt(value: Any) -> str:
    """Format stated numbers consistently."""
    return f"{value:.3f}" if number(value) else MISSING


def records(value: Any) -> list[dict]:
    """Read list and named-mapping object representations."""
    if isinstance(value, list):
        return [v for v in value if isinstance(v, dict)]
    return [
        v
        for k, v in mapping(value).items()
        if k not in ("New", "Deleted") and isinstance(v, dict)
    ]


def object_sections(data: dict) -> list[tuple[str, list[dict]]]:
    """Enumerate collections from the generic schema's section registry."""
    result = [(key, records(data[key])) for key in OBJECTS if key in data]
    generic = mapping(data.get("generic"))
    result += [
        ("generic." + key, records(generic[key]))
        for key in SPEC_OBJECTS
        if key in generic
    ]
    return result


def object_counts(data: dict) -> str:
    """Count definitions in each source without expanding include references."""
    counts = []
    includes = 0
    for key, rows in object_sections(data):
        only_include = sum(
            set(row) <= {"Name", "name", "IncludeFile"} and "IncludeFile" in row
            for row in rows
        )
        includes += only_include
        counts.append(f"{key}: {len(rows) - only_include}")
    for key in ("BaseFile", "IncludeFile"):
        value = data.get(key)
        count = len(value) if isinstance(value, list) else int(value is not None)
        if key == "IncludeFile":
            count += includes
        if count:
            counts.append(f"{key}: {count}")
    return "; ".join(counts) or MISSING


def environment_summary(env: dict, prefix: str = "", depth: int = 0) -> list[str]:
    """Read bounded environment metadata without echoing names or free text."""
    result: list[str] = []
    if depth > 3:
        return result
    for key, value in sorted(env.items(), key=lambda item: str(item[0])):
        if key in ENV_KEYS and number(value):
            result.append(f"{prefix}{key}={fmt(value)}")
        elif key in ("WaveType", "type", "SeabedType") and value in ENV_TYPES:
            result.append(f"{prefix}{key}={value}")
        elif key in ("water", "waves", "current", "wind"):
            result += environment_summary(mapping(value), f"{key}.", depth + 1)
        elif key in ("WaveTrains", "trains") and isinstance(value, list):
            for index, train in enumerate(value):
                result += environment_summary(
                    mapping(train), f"wave {index + 1}.", depth + 1
                )
    return result


def position(row: dict, prefix: str) -> tuple | None:
    """Require all three coordinates; never substitute zero."""
    values = row.get(prefix + "Position")
    if values is None:
        values = [row.get(prefix + axis) for axis in "XYZ"]
    if not isinstance(values, (list, tuple)) or len(values) != 3:
        return None
    return tuple(values) if all(map(number, values)) else None


def line_ends(props: dict) -> tuple[list, list]:
    """Read explicit scalar ends or tabular connection rows."""
    for key, value in props.items():
        if isinstance(key, str) and key.startswith("Connection, ConnectionX,"):
            if isinstance(value, list) and len(value) == 2:
                fields = [field.strip() for field in key.split(",")]
                rows = [
                    dict(zip(fields, row)) for row in value if isinstance(row, list)
                ]
                if len(rows) == 2:
                    return [position(r, "Connection") for r in rows], [
                        r.get("Connection") for r in rows
                    ]
    return (
        [position(props, "End" + end) for end in "AB"],
        [props.get("End" + end + "Connection") for end in "AB"],
    )


def geometry(data: dict) -> tuple[list, list, int]:
    """Extract explicit global frames; report other frames as unresolved."""
    chords, points, unresolved = [], [], 0
    for section, rows in object_sections(data):
        kind = SPEC_OBJECTS.get(section.removeprefix("generic."), section)
        if kind not in ("Lines", "Vessels", "6DBuoys", "3DBuoys"):
            continue
        for row in rows:
            props = mapping(row.get("properties")) | row
            if kind == "Lines":
                ends, frames = line_ends(props)
                if all(ends) and all(frame in ("Free", "Fixed") for frame in frames):
                    chords.append(ends)
                else:
                    unresolved += 1
            else:
                props = dict(props)
                if "initial_position" in props:
                    props["InitialPosition"] = props["initial_position"]
                point = position(props, "Initial")
                frame = props.get("Connection", props.get("connection"))
                if point and frame in ("Free", "Fixed"):
                    points.append((kind, point))
                else:
                    unresolved += 1
    return chords, points, unresolved


def sketch(chords: list, points: list, axis: int, depth: Any, surface: Any) -> str:
    """Project actual coordinates, preserving the aspect ratio and vertical sign."""
    coords = [point for chord in chords for point in chord] + [p for _, p in points]
    xs, ys = [p[0] for p in coords], [p[axis] for p in coords]
    references = []
    if axis == 2 and number(surface):
        references.append(("still water", surface))
        if number(depth):
            references.append(("seabed", surface - depth))
        ys += [value for _, value in references]
    span = max(max(xs) - min(xs), max(ys) - min(ys), 1)
    pad = span * 0.08
    x, y = min(xs) - pad, -max(ys) - pad
    width, height = max(xs) - min(xs) + 2 * pad, max(ys) - min(ys) + 2 * pad
    label = "Plan (X/Y)" if axis == 1 else "Elevation (X/Z)"
    out = (
        f'<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="{label}"'
        f' viewBox="{fmt(x)} {fmt(y)} {fmt(width)} {fmt(height)}">'
        f"<title>{label}; chords, not solved shapes; coordinates in file length units"
        '</title><g transform="scale(1,-1)">'
    )
    for first, last in chords:
        out += (
            f'<line class="chord" x1="{fmt(first[0])}" y1="{fmt(first[axis])}"'
            f' x2="{fmt(last[0])}" y2="{fmt(last[axis])}"/>'
        )
    for kind, point in points:
        out += (
            f'<circle cx="{fmt(point[0])}" cy="{fmt(point[axis])}"'
            f' r="{fmt(span * 0.012)}"><title>{kind} position</title></circle>'
        )
    for kind, value in references:
        out += (
            f'<line class="{kind.replace(" ", "-")}" x1="{fmt(x)}" y1="{fmt(value)}"'
            f' x2="{fmt(x + width)}" y2="{fmt(value)}">'
            f"<title>{kind}: {fmt(value)}</title></line>"
        )
    return out + "</g></svg>"


def parse_model(text: str) -> tuple[dict, bool]:
    """Read source-local fields and expose unreadable documents explicitly."""
    try:
        if "\ufffd" in text:
            raise ValueError("invalid source encoding")
        data = yaml.load(text, Loader=SAFE_LOADER)
        if not isinstance(data, dict):
            raise ValueError("not a model mapping")
        return data, False
    except (yaml.YAMLError, ValueError, RecursionError):
        return {}, True


def card_metadata(data: dict, provenance: str, claim: str, usage: Iterable[str]) -> str:
    """Render selected fields without object identities or free text."""
    env = mapping(data.get("Environment", data.get("environment")))
    depth = env.get("WaterDepth", mapping(env.get("water")).get("depth"))
    system = mapping(data.get("metadata")).get(
        "structure", mapping(data.get("model")).get("type")
    )
    if not isinstance(system, str) or not re.fullmatch(r"[a-z_ -]{1,60}", system):
        system = MISSING
    rows = (
        ("System type", system),
        ("Object counts", object_counts(data)),
        ("Water depth", fmt(depth)),
        ("Environment", "; ".join(environment_summary(env)) or MISSING),
        ("Provenance", provenance),
        ("Claim", claim),
        (
            "Tests and workflows (text references, not proof)",
            "; ".join(sorted(set(usage))) or MISSING,
        ),
    )
    return "".join(f"<p>{label}: {escape(value)}</p>" for label, value in rows)


def card_geometry(data: dict) -> str:
    """Draw only explicitly resolvable global coordinates."""
    env = mapping(data.get("Environment", data.get("environment")))
    depth = env.get("WaterDepth", mapping(env.get("water")).get("depth"))
    surface = env.get("WaterSurfaceZ")
    chords, points, unresolved = geometry(data)
    out = []
    if chords or points:
        for axis, title in ((1, "Plan"), (2, "Elevation")):
            out += [f"<h4>{title}</h4>", sketch(chords, points, axis, depth, surface)]
        out.append(
            "<p>Lines are straight chords, not solved shapes. Blue: chords; dots: "
            "body positions; cyan: still water; brown: seabed depth reference, "
            "not bathymetry. Axes: file length units; still water elevation: "
            f"{fmt(surface)}.</p>"
        )
    if unresolved or not (chords or points):
        out.append(
            "<p>Unresolved geometry cannot be drawn without the solver or additional "
            f"coordinate-frame data; unresolved objects: {unresolved}.</p>"
        )
    return "".join(out)


def render_card(
    path: str,
    text: str,
    provenance: str = MISSING,
    claim: str = "no claim",
    usage: Iterable[str] = (),
    extracted: bool = False,
) -> str:
    """Render metadata and caution without embedding source identities or raw YAML."""
    data, failed = parse_model(text)
    out = [f"<article><h3>{escape(path)}</h3>"]
    if failed:
        out.append(
            "<p>YAML parse failure, invalid UTF-8 or non-mapping document; "
            "no fields inferred.</p>"
        )
    out.append(card_metadata(data, provenance, claim, usage))
    descriptions = [data.get("description")] + [
        mapping(data.get(key)).get("description") for key in ("metadata", "model")
    ]
    extracted |= any(
        isinstance(value, str) and value.startswith("Extracted from")
        for value in descriptions
    )
    if provenance in (MISSING, "vendor example") or extracted:
        prefix = "vendor example models; " if provenance == "vendor example" else ""
        out.append(
            f"<p>{prefix}metadata and schematic only; redistribution terms to be "
            "checked before any model file is shared outside</p>"
        )
    return "".join(out) + card_geometry(data) + "</article>"


def page_template(body: str, listing: str) -> str:
    """Emit a self-contained page with generation and staleness instructions."""
    masters = body.count("/master.yml</h3><p>YAML parse failure")
    return (
        '<!doctype html><html lang="en"><meta charset="utf-8">'
        '<meta name="viewport" content="width=device-width,initial-scale=1">\n'
        "<title>OrcaFlex model library catalogue</title><style>"
        "body{font:16px system-ui;margin:2em auto;max-width:1000px;"
        "padding:0 1em;color:#172b3a}article{border:1px solid #ccd;padding:1em;"
        "margin:1em 0;break-inside:avoid}h3,p{overflow-wrap:anywhere}"
        "svg{width:46%;height:240px;background:#f5f9fb}"
        "svg line{stroke:#1570aa;stroke-width:2;vector-effect:non-scaling-stroke}"
        "svg circle{fill:#172b3a}svg .still-water{stroke:#159aa0}"
        "svg .seabed{stroke:#946338}</style>"
        "<h1>OrcaFlex model library catalogue</h1>"
        "<p>Generated by <code>python src/digitalmodel/solvers/orcaflex/"
        "model_catalogue.py --output docs/domains/orcaflex/library/"
        "model_catalogue.html</code>. Inputs: library/model_library, templates, "
        "library/templates and claim registry/inventory under docs/domains/orcaflex; "
        "path references in tests, examples/workflows and "
        "docs/registry/workflows.yaml. "
        "Staleness is detected by test_committed_page_is_fresh in "
        "tests/solvers/orcaflex/test_model_catalogue.py, comparing committed bytes "
        "with two consecutive builds.</p>"
        "<p>Plain-reader failures in the repository's master-file form: "
        f"{masters}; all master.yml files are counted; includes are not assembled.</p>"
        f"<p>{COUNT_RULE}</p>"
        "<p>One card per YAML source variant; includes are not assembled. No "
        "loadability or solved-shape claim is implied. Selected environment fields "
        "only; values retain file units. Missing fields are not stated.</p>\n"
        f"{body}<h2>Files without cards</h2><ul>{listing}</ul></html>\n"
    )
