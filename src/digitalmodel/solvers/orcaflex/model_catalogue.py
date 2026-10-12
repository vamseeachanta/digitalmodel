"""YAML-only catalogue; run this file directly with --output page.html."""

import argparse
from html import escape
import math
from pathlib import Path
import re
from typing import Any, Iterable

import yaml

ROOTS = ("library/model_library", "templates", "library/templates")
MISSING = "not stated"
SAFE_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
OBJECTS = """Lines Vessels 6DBuoys 3DBuoys LineTypes VesselTypes BuoyTypes Shapes Constraints Winches Links ClumpTypes WingTypes FlexJointTypes FlexJoints DragChainTypes DragChains StiffenerTypes SupportTypes MorisonElementTypes Turbines AttachedBuoys VariableData ExpansionTables PyModels WakeModels MultibodyGroups BrowserGroups""".split()
SPEC_OBJECTS = {re.sub(r"(?<!^)(?=[A-Z])", "_", key).lower(): key for key in OBJECTS}
SPEC_OBJECTS.update(
    buoys_6d="6DBuoys", buoys_3d="3DBuoys", variable_data_sources="VariableData"
)
ENV_KEYS = """WaterDepth WaterSurfaceZ Density WaveHeight WavePeriod WaveDirection WaveHs WaveTp WaveTz RefCurrentSpeed RefCurrentDirection WindSpeed WindDirection depth density height period direction speed hs tp""".split()
ENV_TYPES = "airy|Airy|dean_stream|Dean stream|JONSWAP|jonswap|Flat".split("|")
COUNT_RULE = """Counting rule: object lists count entries; named object mappings count definitions (New/Deleted are directives). Include-only objects count under IncludeFile; BaseFile/IncludeFile references are counted, never expanded. Specification objects are labelled generic.section; other top-level sections are not object counts."""


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


def classify_provenance(name: str, readme: str, header: str) -> str:
    """Use affirmative provenance text, never a system-type 'generic' value."""
    text = (readme + "\n" + header).lower()
    pattern = r"project[- ]derived|sanitis|sanitiz|private (?:data )?repository"
    if re.search(pattern, text):
        return "project-derived sanitised"
    repositories = re.findall(r"github\.com/[^/\s)]+/([\w-]+)", text)
    if any(repo != "digitalmodel" for repo in repositories):
        return "project-derived sanitised"
    if re.search(r"provenance:\s*generic|origin class:\s*generic", text):
        return "generic"
    if name.startswith("mj") and "reconstructed from published literature" in text:
        return "generic (literature-derived)"
    if (
        re.match(r"[a-e]\d", name)
        and "orcina example lettering" in text
        or re.search(r"source:.*orcaflex examples|provenance:\s*vendor example", text)
    ):
        return "vendor example"
    return MISSING


def claim_levels(registry: str, inventory: str) -> dict[str, str]:
    """Keep pending inventory entries separate from attested registry levels."""
    pending = mapping(yaml.load(inventory, Loader=SAFE_LOADER)).get("inventory", [])
    claims = {row["name"]: "no claim (pending inventory)" for row in pending}
    for row in mapping(yaml.load(registry, Loader=SAFE_LOADER)).get("models", []):
        level = row.get("highest_validated_level")
        if level in ("L1", "L2", "L3"):
            claims[row["name"]] = f"{level} (registry)"
    return claims


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
    out = f'<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="{label}" viewBox="{fmt(x)} {fmt(y)} {fmt(width)} {fmt(height)}"><title>{label}; chords, not solved shapes; coordinates in file length units</title><g transform="scale(1,-1)">'
    for first, last in chords:
        out += f'<line class="chord" x1="{fmt(first[0])}" y1="{fmt(first[axis])}" x2="{fmt(last[0])}" y2="{fmt(last[axis])}"/>'
    for kind, point in points:
        out += f'<circle cx="{fmt(point[0])}" cy="{fmt(point[axis])}" r="{fmt(span * 0.012)}"><title>{kind} position</title></circle>'
    for kind, value in references:
        out += f'<line class="{kind.replace(" ", "-")}" x1="{fmt(x)}" y1="{fmt(value)}" x2="{fmt(x + width)}" y2="{fmt(value)}"><title>{kind}: {fmt(value)}</title></line>'
    return out + "</g></svg>"


def render_card(
    path: str,
    text: str,
    provenance: str = MISSING,
    claim: str = "no claim",
    usage: Iterable[str] = (),
) -> str:
    """Render metadata and geometry without embedding source names or raw YAML."""
    out = [f"<article><h3>{escape(path)}</h3>"]
    try:
        if "\ufffd" in text:
            raise ValueError("invalid source encoding")
        data = yaml.load(text, Loader=SAFE_LOADER)
        if not isinstance(data, dict):
            raise ValueError("not a model mapping")
    except (yaml.YAMLError, ValueError, RecursionError):
        data = {}
        out.append(
            "<p>YAML parse failure, invalid UTF-8 or non-mapping document; no fields inferred.</p>"
        )
    env = mapping(data.get("Environment", data.get("environment")))
    depth = env.get("WaterDepth", mapping(env.get("water")).get("depth"))
    surface = env.get("WaterSurfaceZ")
    system = mapping(data.get("metadata")).get(
        "structure", mapping(data.get("model")).get("type")
    )
    if not isinstance(system, str) or not re.fullmatch(r"[a-z_ -]{1,60}", system):
        system = MISSING
    summary = "; ".join(environment_summary(env)) or MISSING
    references = "; ".join(sorted(set(usage))) or MISSING
    out.append(f"""<p>System type: {escape(system)}</p><p>Object counts: {escape(object_counts(data))}</p>
<p>Water depth: {fmt(depth)}</p><p>Environment: {escape(summary)}</p><p>Provenance: {escape(provenance)}</p><p>Claim: {escape(claim)}</p><p>Tests and workflows (text references, not proof): {escape(references)}</p>""")
    if provenance == "vendor example":
        out.append(
            "<p>vendor example models; metadata and schematic only; redistribution terms to be checked before any model file is shared outside</p>"
        )
    chords, points, unresolved = geometry(data)
    if chords or points:
        for axis, title in ((1, "Plan"), (2, "Elevation")):
            out += [f"<h4>{title}</h4>", sketch(chords, points, axis, depth, surface)]
        out.append(
            f"<p>Lines are straight chords, not solved shapes. Blue: chords; dots: body positions; cyan: still water; brown: seabed depth reference, not bathymetry. Axes: file length units; still water elevation: {fmt(surface)}.</p>"
        )
    if unresolved or not (chords or points):
        out.append(
            f"<p>Unresolved geometry cannot be drawn without the solver or additional coordinate-frame data; unresolved objects: {unresolved}.</p>"
        )
    return "".join(out) + "</article>"


def render_catalogue(
    entries: Iterable[tuple[str, str, str]],
    claims: dict | None = None,
    usage: dict | None = None,
    omissions: Iterable[str] = (),
) -> str:
    """Consume one source at a time, then sort rendered cards deterministically."""
    cards = []
    omitted = list(omissions)
    for path, text, provenance in entries:
        if (
            provenance.startswith("project-derived")
            or "project-derived" in Path(path).parts
        ):
            omitted.append(path + ": excluded: project-derived")
            continue
        name = model_name(path)
        claim, refs = (claims or {}).get(name, "no claim"), (usage or {}).get(name, [])
        cards.append((path, render_card(path, text, provenance, claim, refs)))
    body = "".join(card for _, card in sorted(cards))
    listing = "".join("<li>" + escape(item) + "</li>" for item in sorted(omitted))
    return f"""<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>OrcaFlex model library catalogue</title><style>body{{font:16px system-ui;margin:2em auto;max-width:1000px;padding:0 1em;color:#172b3a}}article{{border:1px solid #ccd;padding:1em;margin:1em 0;break-inside:avoid}}
h3,p{{overflow-wrap:anywhere}}svg{{width:46%;height:240px;background:#f5f9fb}}svg line{{stroke:#1570aa;stroke-width:2;vector-effect:non-scaling-stroke}}
svg circle{{fill:#172b3a}}svg .still-water{{stroke:#159aa0}}svg .seabed{{stroke:#946338}}</style><h1>OrcaFlex model library catalogue</h1><p>{COUNT_RULE}</p>
<p>One card per YAML source variant; includes are not assembled. No loadability or solved-shape claim is implied. Selected environment fields only; values retain file units. Missing fields are not stated.</p>
{body}<h2>Files without cards</h2><ul>{listing}</ul></html>
"""


def model_name(path: str) -> str:
    """Identify the owning model for registry matching and reference searches."""
    parts = Path(path).parts
    if "model_library" in parts:
        return parts[parts.index("model_library") + 1]
    parent = Path(path).parent
    if parent.name in ("base", "cases", "variations"):
        parent = parent.parent
    return parent.name


def read_text(path: Path) -> str:
    """Expose invalid encoding to card rendering as an explicit read failure."""
    return path.read_text(encoding="utf-8-sig", errors="replace")


def header(path: Path) -> str:
    """Read only the initial comment block for provenance classification."""
    lines = []
    with path.open(encoding="utf-8-sig", errors="replace") as stream:
        for line in stream:
            if line.strip() and not line.startswith(("#", "%", "---")):
                break
            lines.append(line)
    return "".join(lines)


def source_groups(domain: Path) -> list[tuple[Path, list[Path]]]:
    """Walk the three catalogue roots in a stable order."""
    groups: dict[Path, list[Path]] = {}
    for folder in ROOTS:
        root = domain / folder
        for path in sorted(root.rglob("*")):
            if path.is_file() and path.suffix.lower() in (".yml", ".yaml"):
                relative = path.relative_to(root)
                group = root / relative.parts[0] if len(relative.parts) > 1 else root
                deep = folder == "templates" and len(relative.parts) > 2
                if deep and relative.parts[0] != "include_structure":
                    group /= relative.parts[1]
                groups.setdefault(group, []).append(path)
        for child in sorted(root.iterdir()):
            populated = any(child == g or child in g.parents for g in groups)
            if child.is_dir() and not populated:
                groups.setdefault(child, [])
    return sorted(groups.items())


def catalogue_entries(root: Path, groups: list, omissions: list) -> Iterable:
    """Check group provenance before emitting any member's metadata."""
    for group, paths in groups:
        readmes = [group / "README.md", group.parent / "README.md"]
        readme = "\n".join(read_text(p) for p in readmes if p.is_file())
        headers = [header(p) for p in paths]
        provenance = classify_provenance(group.name, readme, "\n".join(headers))
        if provenance.startswith("project-derived") or not paths:
            reason = "excluded: project-derived" if paths else "no YAML files"
            omissions.append(group.relative_to(root).as_posix() + ": " + reason)
            continue
        for path, source_header in zip(paths, headers):
            relative = path.relative_to(root).as_posix()
            if path.name in ("catalog.yaml", "audit_results.yaml"):
                omissions.append(relative + ": catalogue/audit metadata, not a model")
                continue
            origin = classify_provenance(model_name(relative), readme, source_header)
            yield relative, read_text(path), origin


def build(root: Path) -> str:
    """Build from repository metadata and file text, without resolving includes."""
    domain = root / "docs/domains/orcaflex"
    claims = claim_levels(
        read_text(domain / "MODEL_CLAIM_REGISTRY.yaml"),
        read_text(domain / "MODEL_CLAIM_INVENTORY.yaml"),
    )
    groups = source_groups(domain)
    names = {
        model_name(p.relative_to(root).as_posix()) for _, paths in groups for p in paths
    }
    usage: dict[str, list[str]] = {name: [] for name in names}
    candidates = [root / "docs/registry/workflows.yaml"]
    for folder in ("tests", "examples/workflows"):
        candidates += sorted((root / folder).rglob("*"))
    for path in candidates:
        if path.is_file() and path.suffix in (".py", ".yaml", ".yml", ".md", ".json"):
            text = read_text(path)
            for name in names:
                if re.search(r"(?<![\w-])" + re.escape(name) + r"(?![\w-])", text):
                    usage[name].append(path.relative_to(root).as_posix())
    omissions: list[str] = []

    page = render_catalogue(catalogue_entries(root, groups, omissions), claims, usage)
    listing = "".join("<li>" + escape(item) + "</li>" for item in sorted(omissions))
    return page.replace("</ul></html>", listing + "</ul></html>")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    page = build(Path(__file__).resolve().parents[4])
    args.output.write_text(page, encoding="utf-8", newline="\n")
    print(f"Catalogue: {len(page.encode('utf-8'))} bytes")
