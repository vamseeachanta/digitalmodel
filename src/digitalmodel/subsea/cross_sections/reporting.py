"""Deterministic report model and renderers for subsea cross-section fixtures."""

from __future__ import annotations

from dataclasses import dataclass
from html import escape
from importlib import resources
from pathlib import Path

from digitalmodel.subsea.cross_sections.io import load_cross_section_fixture
from digitalmodel.subsea.cross_sections.schema import CrossSectionDefinition, Provenance, UnitValue
from digitalmodel.subsea.cross_sections.visualization import render_cross_section_svg

REPORT_MD = "offshore_cross_section_report.md"
REPORT_HTML = "offshore_cross_section_report.html"
CAVEATS = [
    "cable-duty",
    "umbilical-schematic",
    "pipeline-route-coating",
    "flexible-pipe-deferred",
]


@dataclass(frozen=True)
class ReportFixture:
    definition: CrossSectionDefinition
    visual_svg: str
    caveat_tags: tuple[str, ...]


@dataclass(frozen=True)
class CrossSectionReport:
    fixtures: tuple[ReportFixture, ...]
    provenance: tuple[Provenance, ...]
    caveats: list[str]


def fixture_paths() -> list[Path]:
    """Return bundled fixture paths in deterministic filename order."""

    fixture_root = resources.files("digitalmodel.subsea.cross_sections") / "fixtures"
    return sorted(Path(str(path)) for path in fixture_root.iterdir() if path.name.endswith(".yml"))


def build_report_model(paths: list[Path] | None = None) -> CrossSectionReport:
    """Load fixtures, validate them through the schema package, and build the report model."""

    definitions = [load_cross_section_fixture(path) for path in (paths or fixture_paths())]
    definitions = sorted(definitions, key=lambda item: (_family_group(item.family), item.id))
    fixtures = tuple(
        ReportFixture(
            definition=definition,
            visual_svg=render_cross_section_svg(definition),
            caveat_tags=tuple(_caveat_tags(definition)),
        )
        for definition in definitions
    )
    provenance = _dedupe_provenance(definitions)
    return CrossSectionReport(fixtures=fixtures, provenance=provenance, caveats=list(CAVEATS))


def render_markdown_report(report: CrossSectionReport) -> str:
    rows = [
        "# Offshore Cable, Umbilical, and Pipeline Cross-Section Report",
        "",
        "Generated deterministically from the bundled `digitalmodel.subsea.cross_sections` YAML fixtures. No timestamps, host paths, or derived mechanical capacities are emitted.",
        "",
        "## Regeneration",
        "",
        "```bash",
        "PYTHONPATH=src uv run python -m digitalmodel.subsea.cross_sections.cli --output-dir docs/subsea/cross_sections --format all",
        "```",
        "",
        "## Fixture Inventory",
        "",
        "| Fixture ID | Name | Family | Duty |",
        "|---|---|---|---|",
    ]
    for item in report.fixtures:
        d = item.definition
        rows.append(f"| `{d.id}` | {d.name} | `{d.family}` | `{d.duty}` |")
    rows.extend([
        "",
        "## Cross-Family Comparison",
        "",
        "| Fixture ID | Name | Family | Duty | Overall OD / envelope | Radial layer count | Packed component count | Key ratings | Primary source IDs | Caveat tags |",
        "|---|---|---|---|---|---:|---:|---|---|---|",
    ])
    for item in report.fixtures:
        rows.append(_comparison_row(item))
    rows.extend(["", "## Cross-Section Visuals", ""])
    for item in report.fixtures:
        rows.extend([
            f"### {item.definition.name} (`{item.definition.id}`)",
            "",
            item.visual_svg,
            "",
        ])
    rows.extend([
        "## Provenance",
        "",
        "| Source ID | Type | Citation | URL/path | Note | Derived from |",
        "|---|---|---|---|---|---|",
    ])
    for source in report.provenance:
        rows.append(_provenance_row(source))
    rows.extend([
        "",
        "## Engineering Caveats",
        "",
        "- `cable-duty`: static/export/inter-array cable fixtures are examples only; dynamic cable duty and fatigue assessment require project-specific mechanics.",
        "- `umbilical-schematic`: packed umbilical visuals are not-to-scale schematic arrangements, not optimized manufacturing layouts.",
        "- `pipeline-route-coating`: concrete-coated pipeline dimensions are route- and project-specific; no stability, collapse, or code capacity is derived here.",
        "- `flexible-pipe-deferred`: flexible pipe/riser mechanics are deferred to #2516; this report does not model carcass, pressure armor, tensile armor, bending, fatigue, or dynamic riser behavior.",
        "",
        "## Deferred Scope and Follow-Ups",
        "",
        "- #2516 owns flexible pipe/riser mechanics and dynamic behavior.",
        "- Vendor dimensions, source catalogue completeness, mechanical capacities, thermal ratings, weights, fatigue, and OrcaFlex export remain out of scope for this deterministic reporting pass.",
        "",
    ])
    return "\n".join(rows)


def render_html_report(report: CrossSectionReport) -> str:
    body = [
        "<!doctype html>",
        '<html lang="en">',
        "<head>",
        '<meta charset="utf-8">',
        "<title>Offshore Cable, Umbilical, and Pipeline Cross-Section Report</title>",
        "<style>body{font-family:Arial,sans-serif;line-height:1.4;margin:2rem;}table{border-collapse:collapse;width:100%;margin:1rem 0;}th,td{border:1px solid #ccc;padding:.35rem;text-align:left;}code{background:#f3f3f3;padding:.1rem .2rem;}svg{max-width:100%;height:auto;border:1px solid #ddd;margin:.5rem 0;}</style>",
        "</head><body>",
        "<h1>Offshore Cable, Umbilical, and Pipeline Cross-Section Report</h1>",
        "<p>Generated deterministically from bundled YAML fixtures. No timestamps, host paths, or derived mechanical capacities are emitted.</p>",
        "<h2>Regeneration</h2>",
        "<pre><code>PYTHONPATH=src uv run python -m digitalmodel.subsea.cross_sections.cli --output-dir docs/subsea/cross_sections --format all</code></pre>",
        "<h2>Fixture Inventory</h2>",
        "<table><thead><tr><th>Fixture ID</th><th>Name</th><th>Family</th><th>Duty</th></tr></thead><tbody>",
    ]
    for item in report.fixtures:
        d = item.definition
        body.append(f"<tr><td><code>{escape(d.id)}</code></td><td>{escape(d.name)}</td><td><code>{escape(d.family)}</code></td><td><code>{escape(d.duty)}</code></td></tr>")
    body.extend([
        "</tbody></table>",
        "<h2>Cross-Family Comparison</h2>",
        "<table><thead><tr><th>Fixture ID</th><th>Name</th><th>Family</th><th>Duty</th><th>Overall OD / envelope</th><th>Radial layer count</th><th>Packed component count</th><th>Key ratings</th><th>Primary source IDs</th><th>Caveat tags</th></tr></thead><tbody>",
    ])
    for item in report.fixtures:
        body.append(_comparison_html_row(item))
    body.extend(["</tbody></table>", "<h2>Cross-Section Visuals</h2>"])
    for item in report.fixtures:
        body.append(f"<h3>{escape(item.definition.name)} (<code>{escape(item.definition.id)}</code>)</h3>")
        body.append(item.visual_svg)
    body.extend([
        "<h2>Provenance</h2>",
        "<table><thead><tr><th>Source ID</th><th>Type</th><th>Citation</th><th>URL/path</th><th>Note</th><th>Derived from</th></tr></thead><tbody>",
    ])
    for source in report.provenance:
        body.append(_provenance_html_row(source))
    body.extend([
        "</tbody></table>",
        "<h2>Engineering Caveats</h2>",
        "<ul>",
        "<li><code>cable-duty</code>: static/export/inter-array cable fixtures are examples only; dynamic cable duty and fatigue assessment require project-specific mechanics.</li>",
        "<li><code>umbilical-schematic</code>: packed umbilical visuals are not-to-scale schematic arrangements, not optimized manufacturing layouts.</li>",
        "<li><code>pipeline-route-coating</code>: concrete-coated pipeline dimensions are route- and project-specific; no stability, collapse, or code capacity is derived here.</li>",
        "<li><code>flexible-pipe-deferred</code>: flexible pipe/riser mechanics are deferred to #2516; this report does not model carcass, pressure armor, tensile armor, bending, fatigue, or dynamic riser behavior.</li>",
        "</ul>",
        "<h2>Deferred Scope and Follow-Ups</h2>",
        "<ul><li>#2516 owns flexible pipe/riser mechanics and dynamic behavior.</li><li>Vendor dimensions, source catalogue completeness, mechanical capacities, thermal ratings, weights, fatigue, and OrcaFlex export remain out of scope.</li></ul>",
        "</body></html>",
    ])
    return "\n".join(body) + "\n"


def write_report_files(output_dir: Path, formats: set[str]) -> list[Path]:
    report = build_report_model()
    output_dir.mkdir(parents=True, exist_ok=True)
    generated: list[Path] = []
    if "md" in formats:
        generated.append(_atomic_write(output_dir / REPORT_MD, render_markdown_report(report)))
    if "html" in formats:
        generated.append(_atomic_write(output_dir / REPORT_HTML, render_html_report(report)))
    return generated


def _atomic_write(path: Path, text: str) -> Path:
    tmp = path.with_suffix(path.suffix + ".tmp")
    try:
        tmp.write_text(text, encoding="utf-8")
        tmp.replace(path)
    finally:
        if tmp.exists():
            tmp.unlink()
    return path


def _family_group(family: str) -> int:
    order = {
        "offshore_wind_inter_array_cable": 0,
        "offshore_wind_hvac_export_cable": 1,
        "offshore_wind_hvdc_export_cable": 2,
        "power_optical_hybrid_umbilical": 3,
        "rigid_pipeline_flowline": 4,
        "steel_tube_electro_hydraulic_umbilical": 5,
        "thermoplastic_electro_hydraulic_umbilical": 6,
    }
    return order.get(family, 99)


def _caveat_tags(definition: CrossSectionDefinition) -> list[str]:
    tags = []
    if definition.family.startswith("offshore_wind"):
        tags.append("cable-duty")
    if "umbilical" in definition.family:
        tags.append("umbilical-schematic")
    if "pipeline" in definition.family:
        tags.append("pipeline-route-coating")
    tags.append("flexible-pipe-deferred")
    return tags


def _dedupe_provenance(definitions: list[CrossSectionDefinition]) -> tuple[Provenance, ...]:
    seen: dict[tuple, Provenance] = {}
    for definition in definitions:
        for source in definition.provenance:
            key = (
                source.source_id,
                source.source_type,
                source.citation or "",
                source.url_or_path or "",
                source.note or "",
                tuple(source.derived_from),
            )
            seen[key] = source
    return tuple(seen[key] for key in sorted(seen))


def _comparison_row(item: ReportFixture) -> str:
    d = item.definition
    return "| " + " | ".join([
        f"`{d.id}`",
        d.name,
        f"`{d.family}`",
        f"`{d.duty}`",
        _overall_envelope(d),
        str(len(d.radial_layers)),
        str(len(d.packed_components)),
        _key_ratings(d),
        ", ".join(f"`{source.source_id}`" for source in d.provenance),
        ", ".join(f"`{tag}`" for tag in item.caveat_tags),
    ]) + " |"


def _comparison_html_row(item: ReportFixture) -> str:
    d = item.definition
    cells = [
        f"<code>{escape(d.id)}</code>",
        escape(d.name),
        f"<code>{escape(d.family)}</code>",
        f"<code>{escape(d.duty)}</code>",
        escape(_overall_envelope(d)),
        str(len(d.radial_layers)),
        str(len(d.packed_components)),
        escape(_key_ratings(d)),
        ", ".join(f"<code>{escape(source.source_id)}</code>" for source in d.provenance),
        ", ".join(f"<code>{escape(tag)}</code>" for tag in item.caveat_tags),
    ]
    return "<tr>" + "".join(f"<td>{cell}</td>" for cell in cells) + "</tr>"


def _overall_envelope(definition: CrossSectionDefinition) -> str:
    if definition.radial_layers:
        outer = definition.radial_layers[-1].derived_outer_diameter
        return _format_unit(outer) if outer else "not specified"
    diameters = [component.diameter for component in definition.packed_components if component.diameter]
    if diameters:
        max_diameter = max(diameters, key=lambda value: value.value)
        return f"component max {_format_unit(max_diameter)} (bundle envelope not specified)"
    return "not specified"


def _key_ratings(definition: CrossSectionDefinition) -> str:
    values = []
    dm = definition.design_metadata
    for label, value in [
        ("voltage", dm.voltage_class),
        ("pressure", dm.design_pressure),
        ("temperature", dm.design_temperature),
    ]:
        if value is not None:
            values.append(f"{label}: {_format_unit(value)}")
    for component in definition.packed_components:
        if component.voltage_rating is not None:
            values.append(f"{component.name} voltage: {_format_unit(component.voltage_rating)}")
        if component.pressure_rating is not None:
            values.append(f"{component.name} pressure: {_format_unit(component.pressure_rating)}")
    return "; ".join(values) if values else "not specified"


def _format_unit(value: UnitValue | None) -> str:
    if value is None:
        return "not specified"
    rendered = f"{value.value:.3f}".rstrip("0").rstrip(".")
    return f"{rendered} {value.unit}"


def _provenance_row(source: Provenance) -> str:
    return "| " + " | ".join([
        f"`{source.source_id}`",
        f"`{source.source_type}`",
        source.citation or "not specified",
        source.url_or_path or "not specified",
        source.note or "not specified",
        ", ".join(source.derived_from) if source.derived_from else "not applicable",
    ]) + " |"


def _provenance_html_row(source: Provenance) -> str:
    cells = [
        f"<code>{escape(source.source_id)}</code>",
        f"<code>{escape(source.source_type)}</code>",
        escape(source.citation or "not specified"),
        escape(source.url_or_path or "not specified"),
        escape(source.note or "not specified"),
        escape(", ".join(source.derived_from) if source.derived_from else "not applicable"),
    ]
    return "<tr>" + "".join(f"<td>{cell}</td>" for cell in cells) + "</tr>"
