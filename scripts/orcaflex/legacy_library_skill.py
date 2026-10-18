# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Render optional source-bound skill drafts from inspected specifications."""

import yaml


def names(objects):
    """Describe only names actually declared by the specification."""
    return ", ".join(str(item.get("name", "unnamed")) for item in objects)


def group_record(item, group, root):
    """Extract declared quantities and objects without inferring geometry."""
    relative = item["path"]
    spec = yaml.load((root / relative).read_bytes(), Loader=getattr(yaml, "CSafeLoader", yaml.SafeLoader))
    env = spec.get("environment", {})
    raw = env.get("raw_properties", {})
    waves = env.get("waves", {})
    generic = spec.get("generic", {})
    lines = generic.get("lines", [])
    line_types = generic.get("line_types", [])
    if lines:
        configuration = (f"Specification with {len(lines)} line objects and {len(line_types)} line types. "
                         f"Declared lines: {names(lines)}. Declared line types: {names(line_types)}.")
    elif generic:
        configuration = (f"Specification with {len(line_types)} line types and no line objects; "
                         f"declared line types: {names(line_types)}. "
                         "Jumper geometry is not established by this specification.")
    else:
        configuration = "Parameter override with no object geometry; a standalone jumper configuration is not established."
    return {"group": group, "source": relative, "source_sha256": item["sha256"],
                   "source_url": item["source_url"], "retrieval_date": item["retrieval_date"],
                   "record_count": 1, "configuration": configuration,
                   "water_depth_m": env.get("water", {}).get("depth"),
                   "wave_height_m": waves.get("height", raw.get("WaveHs")),
                   "wave_height_kind": "Hs" if "WaveHs" in raw and not waves else "height",
                   "wave_type": waves.get("type", raw.get("WaveType")),
                   "wave_period_s": waves.get("period", raw.get("WaveTp")),
                   "wave_direction_deg": waves.get("direction", raw.get("WaveDirection")),
                   "current_speed_m_s": env.get("current", {}).get("speed"),
                   "wind_speed_m_s": env.get("wind", {}).get("speed"),
                   "stages_s": spec.get("simulation", {}).get("stages"),
                   "time_step_s": spec.get("simulation", {}).get("time_step")}


def shown(value, unit=""):
    """Preserve parsed values and omit units for absent quantities."""
    if value is None:
        return "not recorded"
    text = ", ".join(shown(item) for item in value) if isinstance(value, list) else str(value)
    return text + (" " + unit if unit else "")


def document_header(revision):
    """Render the revision and scope of the draft."""
    return ["---", "name: orcaflex-jumper-analysis",
            "description: Prepare inspected jumper-library inputs with the existing format converter; native execution requires separate qualification.",
            "---", "# Jumper library preparation", "",
            "Prepare solver-free inputs from the recorded specifications and retain validator failures and absent comparators. "
            "The specifications do not establish an M-shape, lazy-S or steep-S classification or a verified operational envelope.",
            "", "## Revision history", "", "| Revision | Date | Change and reason |", "| --- | --- | --- |",
            f"| {revision['revision']} | {revision['date']} | {revision['change']} |", "",
            f"Declared revision: {revision['revision']}.", "", "## Specification groups", "",
            "Values below are declared in each specification, not demonstrated response limits or ranges across every override. "
            "Units follow the repository environment schema. Unrecorded values remain unrecorded.", ""]


def document_rows(groups):
    """Render declared quantities and their recorded units."""
    text = []
    for row in groups:
        text.extend([f"### {row['group']}", "", row["configuration"], "",
                     f"Source: `{row['source']}`; digest in `jumper-groups.json`.", "",
                     f"Water depth {shown(row['water_depth_m'], 'm')}; wave {row['wave_height_kind']} "
                     f"{shown(row['wave_height_m'], 'm')}, wave type {shown(row['wave_type'])}, "
                     f"period {shown(row['wave_period_s'], 's')}, "
                     f"direction {shown(row['wave_direction_deg'], 'degrees')}. "
                     f"Current {shown(row['current_speed_m_s'], 'm/s')}; wind {shown(row['wind_speed_m_s'], 'm/s')}. "
                     f"Simulation stages {shown(row['stages_s'], 's')}; time step {shown(row['time_step_s'], 's')}.", ""])
    return text


def document_footer():
    """Retain preparation instructions and unqualified execution limits."""
    return ["## Worked preparation", "",
                 "Set `OUTPUT_DIR` to a writable directory outside the repository. This converts one named model without a solver:",
                 "", "```sh",
                 "PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter single2modular docs/domains/orcaflex/jumper/manifold_to_plet/monolithic/DZ_AHCoff.yml -o \"$OUTPUT_DIR/modular\"",
                 "PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter modular2single \"$OUTPUT_DIR/modular/master.yml\" -o \"$OUTPUT_DIR/roundtrip.yml\"",
                 "```", "",
                 "For parametric preparation, edit a copy of a group's `spec.yml`, retain its source digest, "
                 "validate it against `ProjectInputSpec`, then use the same CLI's `spec2modular` command. "
                 "The two specifications without jumper line geometry require that missing basis before a jumper model can be generated.", "",
                 "Use `validators.json` to identify structural failures and `comparators.json` for stored-result availability. "
                 "A conversion exit code does not establish semantic fidelity: the existing semantic-diff taxonomy and converter "
                 "remain the basis for the outstanding conversion campaign (#2014).", "",
                 "## Resonance and execution limits", "",
                 "The `sut_mm/resonance` group records a separate simulation specification. "
                 "The group name alone does not establish a resonant frequency, dynamic amplification or response criterion. "
                 "Those results require qualified native dynamics and read-back against an explicit comparator (#2014). "
                 "No native result is claimed here. The native manifest template keeps load, statics and comparison refused "
                 "until a qualified execution adapter and an approved manifest digest exist.", "",
                 "The document is retained here as a skill draft; installation at the requested skill location and a complete "
                 "group-wide operating envelope remain open (#2015).", ""]


def build_skill(inventory, root, encode):
    """Build optional draft artifacts only for published specifications."""
    sources = {item["path"]: item for item in inventory["inputs"]}
    qualified = {item["path"] for item in inventory["models"]}
    groups = []
    for group in ("manifold_to_plet", "plet_to_plem", "sut_mm", "sut_mm/resonance"):
        relative = f"docs/domains/orcaflex/jumper/{group}/spec.yml"
        if relative not in qualified:
            raise ValueError("jumper specification is not in the inspected model set")
        groups.append(group_record(sources[relative], group, root))
    revision = {"revision": "1", "date": groups[0]["retrieval_date"],
                "change": "Initial source-bound library preparation document and execution limits."}
    record = {"source_ref": inventory["source_ref"], "groups": groups, "revisions": [revision]}
    text = document_header(revision) + document_rows(groups) + document_footer()
    return {"jumper-groups.json": encode(record),
            "orcaflex-jumper-analysis/SKILL.md": "\n".join(text).encode()}
