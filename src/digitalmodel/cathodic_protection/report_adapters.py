"""Cathodic-protection consumers of the standard report engine (#2212 part 2).

ABOUTME: Two ``@report_adapter`` registrations turn engine-adapter results
(``cathodic_protection.anode_design``) or a ``CPAssessmentReport``
(``cathodic_protection.assessment``) into a :class:`ReportSpec`; the engine
renders HTML/PDF. No HTML is written here.

``anode_design`` reads the routed cfg produced by
:func:`~digitalmodel.cathodic_protection.engine_adapter.run_cathodic_protection`
(``cfg["inputs"]`` for the input echo, ``cfg["results"]`` for the numbers,
``cfg["report"]["document"]`` for document control) and lays out, per route:

* DNV-RP-B401 offshore: Design basis, Current demand (table + ``I(t)`` line
  figure), Anode requirements (table + count bar figure), Adequacy (status +
  fresh/depleted ``R``/``I`` table), References.
* DNV-RP-F103 bracelet: Design basis, Coating breakdown and current demand,
  Anode requirements (count/spacing/protected length + bar figure), Adequacy
  (route status + spacing <= 2 x protected length status), References.
* ABS ships / offshore: the tables the legacy route exposes, plus the status.

Every Adequacy section opens with the route's use status
(``results["status"]["use_status"]``, owner decision 2026-09-27, epic #2206)
and the route's StatusBlock detail repeats it, so each HTML/PDF deliverable
states whether it may go to a client and on what condition.

``assessment`` takes a :class:`CPAssessmentReport` (plus optional CIS survey
points/analysis and a :class:`DepletionProfile`) and lays out compliance,
potential-vs-distance, remaining-mass-vs-years and recommendations.

Determinism: nothing here reads the clock; the provenance digests are sha256
of the input file (when routed from a file) and of the results mapping.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import asdict
from pathlib import Path
from typing import Any, Literal, Mapping, Sequence

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import normalize_edition
from digitalmodel.cathodic_protection.anode_depletion import DepletionProfile
from digitalmodel.cathodic_protection.b401_tables import B401_WIKI_PATH, table_label
from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceStatus,
    CPAssessmentReport,
)
from digitalmodel.cathodic_protection.cp_survey import CISAnalysisResult, CISSurveyPoint
from digitalmodel.cathodic_protection.engine_adapter import (
    KEY_ABS_OFFSHORE,
    KEY_ABS_SHIPS,
    KEY_B401,
    KEY_F103,
    KEY_F103_2010,
    STATUS_PASS,
    USE_STATUS_CLIENT_EOR,
    USE_STATUS_EXPERIMENTAL,
    USE_STATUS_LEGACY_UNCITED,
)
from digitalmodel.cathodic_protection.f103_tables import F103_WIKI_PATH
from digitalmodel.citations.schema import Citation
from digitalmodel.reporting.adapters import report_adapter
from digitalmodel.reporting.figures import figure_from_columns
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.spec import (
    Block,
    DocumentMeta,
    FigureBlock,
    ReportSpec,
    Section,
    StandardLabel,
    StatusBlock,
    TableBlock,
    TextBlock,
)

#: Used when the input carries no ``report.document``; passes ``DOC_NUMBER_RE``.
#: The revision is ``"00"`` (the pattern needs two digits) and the input echo
#: says the number is a placeholder.
PLACEHOLDER_DOCUMENT_NUMBER = "X0000-CP-000-00"
PLACEHOLDER_REVISION = "00"

#: Figure ids (unique per report; asserted by the tests).
FIG_DEMAND_VS_TIME = "fig-cp-demand-vs-time"
FIG_ANODE_COUNTS = "fig-cp-anode-counts"
FIG_POTENTIAL_VS_DISTANCE = "fig-cp-potential-vs-distance"
FIG_REMAINING_MASS = "fig-cp-remaining-mass"

#: Number of time samples for the ``I(t)`` curve (0 .. design life inclusive).
DEMAND_CURVE_SAMPLES = 11

_WIKI_BY_CODE: dict[str, str] = {
    "dnv-rp-b401": B401_WIKI_PATH,
    "dnv-rp-f103": F103_WIKI_PATH,
}
_PUBLISHER_BY_CODE: dict[str, str] = {"dnv-rp-b401": "DNV", "dnv-rp-f103": "DNV"}
_STANDARD_RE = re.compile(r"^(?P<code>.+?)\s*\((?P<edition>[^()]+)\)\s*$")

#: Report-facing labels for the legacy ABS routes (no cited tables).
_ABS_STANDARDS: dict[str, tuple[str, str]] = {
    KEY_ABS_SHIPS: ("ABS GN Cathodic Protection of Ships", "December 2017"),
    KEY_ABS_OFFSHORE: ("ABS GN Cathodic Protection of Offshore Structures", "December 2018"),
}
_ABS_PROVENANCE = "legacy solver; table values not cited"

Row = list[Any]


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------


def _mapping(node: Any) -> Mapping[str, Any]:
    return node if isinstance(node, Mapping) else {}


def _sha256(payload: bytes) -> str:
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def _results_digest(results: Mapping[str, Any]) -> str:
    return _sha256(json.dumps(results, sort_keys=True, default=str).encode("utf-8"))


def _provenance(cfg: Mapping[str, Any], results: Mapping[str, Any], what: str) -> Provenance:
    """Input YAML (relative to the config dir, sha256) plus the results digest."""
    prov = Provenance()
    raw_path = cfg.get("_config_file_path")
    if raw_path:
        path = Path(str(raw_path))
        if path.is_file():
            base = Path(str(cfg.get("_config_dir_path") or path.parent))
            try:
                identifier = path.resolve().relative_to(base.resolve()).as_posix()
            except ValueError:
                identifier = path.name
            prov.add(
                "file",
                identifier,
                digest=_sha256(path.read_bytes()),
                description="routed input YAML (cathodic_protection)",
            )
    prov.add(
        "results",
        f"cfg[{what}]",
        digest=_results_digest(results),
        description="engine-adapter results mapping this report is a view over",
    )
    return prov


def _document(cfg: Mapping[str, Any], default_title: str) -> tuple[DocumentMeta, str | None]:
    """``report.document`` from the input, else a placeholder (and a note)."""
    supplied = _mapping(_mapping(cfg.get("report")).get("document"))
    if supplied:
        return DocumentMeta.model_validate(dict(supplied)), None
    note = (
        f"placeholder {PLACEHOLDER_DOCUMENT_NUMBER} rev {PLACEHOLDER_REVISION}: "
        "no report.document in the input"
    )
    return (
        DocumentMeta(
            number=PLACEHOLDER_DOCUMENT_NUMBER,
            revision=PLACEHOLDER_REVISION,
            title=default_title,
            project="unassigned",
            client="unassigned",
        ),
        note,
    )


def _parse_label(label: str) -> tuple[str, str, str] | None:
    """``"code_id revision section"`` (``b401_tables.citation_label``) split."""
    parts = label.split(" ", 2)
    if len(parts) != 3 or not all(p.strip() for p in parts):
        return None
    return parts[0].strip().lower(), parts[1].strip(), parts[2].strip()


def _citations_from_labels(labels: Sequence[Any], note: str) -> list[Citation]:
    """Citation records for the labels whose code has a known wiki page."""
    records: list[Citation] = []
    seen: set[str] = set()
    for raw in labels:
        if isinstance(raw, Citation):
            candidate = raw
        else:
            parsed = _parse_label(str(raw))
            if parsed is None or parsed[0] not in _WIKI_BY_CODE:
                continue
            code, revision, section = parsed
            candidate = Citation(
                code_id=code,
                publisher=_PUBLISHER_BY_CODE[code],
                revision=revision,
                section=section,
                wiki_path=_WIKI_BY_CODE[code],
                note=note,
            )
        key = f"{candidate.code_id} {candidate.revision} {candidate.section}"
        if key not in seen:
            seen.add(key)
            records.append(candidate)
    return records


def _standards(results: Mapping[str, Any], calc_type: str) -> list[StandardLabel]:
    """Route standard/edition/provenance, plus any other code cited."""
    labels: list[StandardLabel] = []
    standard = str(results.get("standard") or "").strip()
    if standard:
        match = _STANDARD_RE.match(standard)
        code = match.group("code") if match else standard
        edition = match.group("edition") if match else str(results.get("edition") or "n/a")
        labels.append(
            StandardLabel(
                code_id=code,
                edition=str(edition),
                provenance=str(results.get("provenance") or "not stated"),
            )
        )
    elif calc_type in _ABS_STANDARDS:
        code, edition = _ABS_STANDARDS[calc_type]
        labels.append(StandardLabel(code_id=code, edition=edition, provenance=_ABS_PROVENANCE))
    # "DNVGL-RP-F103" (2019) and "DNVGL-RP-B401" (2017) are cited as
    # "dnv-rp-..."; fold the prefix so the route standard is not listed twice.
    main = {re.sub(r"^dnvgl-", "dnv-", lbl.code_id.lower()) for lbl in labels}
    extra: dict[str, str] = {}
    for raw in results.get("citations") or []:
        parsed = _parse_label(str(raw))
        if parsed and parsed[0] in _WIKI_BY_CODE and parsed[0] not in main:
            extra.setdefault(parsed[0], parsed[1])
    for code, revision in sorted(extra.items()):
        labels.append(
            StandardLabel(
                code_id=code.upper(),
                edition=revision,
                provenance=f"tables cited through the {calc_type} route",
            )
        )
    return labels


def _kv_table(title: str, rows: Sequence[Row], source: str | None = None) -> TableBlock:
    return TableBlock(
        title=title,
        columns=["Item", "Value", "Unit"],
        rows=[list(r) for r in rows],
        source=source,
    )


#: Report wording per ``results["status"]["use_status"]`` token.
_USE_STATUS_TEXT: dict[str, str] = {
    USE_STATUS_CLIENT_EOR: (
        "Use status: approved for client use subject to an engineer-of-record "
        "check of this deliverable."
    ),
    USE_STATUS_EXPERIMENTAL: (
        "Use status: experimental-known-understatement; not for design use. "
        "Understates mean demand by about a third and final demand by about half "
        "per the 2026-09-27 benchmark (#2259, #1852)."
    ),
    USE_STATUS_LEGACY_UNCITED: (
        "Use status: legacy solver with uncited tables; not for client use "
        "without an independent check."
    ),
}
_USE_STATUS_MISSING = (
    "Use status: not recorded; not for client use without an independent check."
)


def _use_status_text(status: Mapping[str, Any]) -> str:
    token = str(status.get("use_status") or "")
    return _USE_STATUS_TEXT.get(token, _USE_STATUS_MISSING)


def _use_status_block(status: Mapping[str, Any]) -> TextBlock:
    return TextBlock(markdown=_use_status_text(status))


def _status_block(status: Mapping[str, Any], label: str) -> StatusBlock:
    result: Literal["PASS", "FAIL"] = (
        "PASS" if str(status.get("result", "")).upper() == STATUS_PASS else "FAIL"
    )
    governing = str(status.get("governing_case") or "").strip() or None
    if result == "FAIL" and not governing:
        governing = "not stated"
    reason = str(status.get("reason") or "").strip()
    return StatusBlock(
        label=label,
        status=result,
        governing_case=governing,
        detail="; ".join(part for part in (reason, _use_status_text(status)) if part),
    )


def _checks_table(status: Mapping[str, Any]) -> TableBlock:
    rows: list[Row] = []
    for name, ok in sorted(_mapping(status.get("checks")).items()):
        verdict = "not run" if ok is None else ("PASS" if ok else "FAIL")
        rows.append([name.replace("_", " "), verdict])
    if not rows:
        rows.append(["(no checks recorded)", ""])
    return TableBlock(title="Adequacy checks", columns=["Check", "Result"], rows=rows)


def _counts_figure(names: Sequence[str], counts: Sequence[Any], title: str) -> FigureBlock:
    return FigureBlock(
        title=title,
        caption="Required anode count per sizing case; the largest governs.",
        figure_id=FIG_ANODE_COUNTS,
        plotly=figure_from_columns(
            "bar",
            list(names),
            {"Anodes": [int(c) for c in counts]},
            title=title,
            x_label="sizing case",
            y_label="anodes",
        ),
    )


def _references_section(results: Mapping[str, Any], usage: Sequence[tuple[str, Sequence[Any]]]) -> Section:
    rows: list[Row] = []
    for item, labels in usage:
        text = ", ".join(sorted({str(x) for x in labels})) or "none"
        rows.append([item, text])
    blocks: list[Block] = []
    if results.get("citations"):
        blocks.append(
            TextBlock(
                markdown=(
                    "Every tabulated constant carries the standard, revision and "
                    "table it was read from; the *References cited* table at the "
                    "end of this report lists the resolved citation records."
                )
            )
        )
    else:
        blocks.append(
            TextBlock(
                markdown=(
                    "This route runs the legacy solver, which carries no cited "
                    "table records; the standard label above is the report-facing "
                    "identification only."
                )
            )
        )
    if rows:
        blocks.append(
            TableBlock(title="Where each cited table was used", columns=["Item", "Citations"], rows=rows)
        )
    return Section(key="references", title="References", blocks=blocks)


# ---------------------------------------------------------------------------
# DNV-RP-B401 offshore structure
# ---------------------------------------------------------------------------


def _b401_sections(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Section]:
    if results.get("anode_families"):
        from digitalmodel.cathodic_protection.b401_family_report import (
            build_b401_family_sections,
        )

        return build_b401_family_sections(inputs, results)
    areas = _mapping(results.get("surface_areas_m2"))
    breakdown = _mapping(results.get("coating_breakdown"))
    densities = _mapping(results.get("current_densities_A_m2"))
    demand = _mapping(results.get("current_demand_A"))
    req = _mapping(results.get("anode_requirements"))
    ver = _mapping(results.get("current_output_verification"))
    status = _mapping(results.get("status"))
    design_life = float(results.get("design_life_years") or 0.0)
    edition = normalize_edition(str(results.get("edition")))
    coating_table = table_label(edition, 4)
    resistance_table = table_label(edition, 7)
    zones = [z for z in areas if z != "total_m2"]
    environment = _mapping(inputs.get("environment"))
    anode = _mapping(inputs.get("anode"))

    # Design basis --------------------------------------------------------
    design_rows: list[Row] = [
        ["Standard", results.get("standard", ""), "-"],
        ["Edition provenance", results.get("provenance", ""), "-"],
        ["Design life", design_life, "yr"],
        ["Seawater temperature", environment.get("seawater_temperature_C", ""), "degC"],
        ["Seawater resistivity", environment.get("seawater_resistivity_ohm_m", ""), "ohm.m"],
        ["Total surface area", areas.get("total_m2", ""), "m2"],
        ["Anode material", req.get("anode_material", anode.get("material", "")), "-"],
        ["Anode type", req.get("anode_type", anode.get("type", "")), "-"],
        ["Anode net mass", req.get("individual_mass_kg", ""), "kg"],
        ["Anode length", anode.get("length_m", ""), "m"],
        ["Anode equivalent radius (fresh)", ver.get("equivalent_radius_initial_m", ""), "m"],
        ["Utilisation factor", req.get("utilization_factor", ""), "-"],
        ["Utilisation factor source", req.get("utilization_factor_source", ""), "-"],
        ["Electrochemical capacity", req.get("electrochemical_capacity_Ah_kg", ""), "Ah/kg"],
    ]
    has_area_basis = any("area_basis" in _mapping(demand.get(z)) for z in zones)
    zone_rows: list[Row] = []
    for z in zones:
        fc = _mapping(breakdown.get(z))
        dz = _mapping(densities.get(z))
        zone_rows.append(
            [
                z,
                dz.get("exposure_zone", dz.get("base_zone", "")),
                areas.get(z, ""),
                *([_mapping(demand.get(z)).get("area_basis", "steel_surface")] if has_area_basis else []),
                fc.get("coating_category", ""),
                dz.get("depth_band", ""),
                dz.get("climate", ""),
                fc.get("a", ""),
                fc.get("b_per_yr", ""),
            ]
        )
    design_basis = Section(
        key="design-basis",
        title="Design basis",
        subtitle="Input echo: structure zones, environment and anode data",
        blocks=[
            _kv_table("Design data", design_rows, source="cfg[inputs], cfg[results]"),
            TableBlock(
                title=f"Zones and coating breakdown constants ({coating_table})",
                columns=["Zone", "Exposure", "Area"] + (["Area basis"] if has_area_basis else []) + ["Coating", "Depth band", "Climate", "a", "b"],
                units=["-", "-", "m2"] + (["-"] if has_area_basis else []) + ["category", "m", "-", "-", "1/yr"],
                rows=zone_rows,
                source="cfg[results][coating_breakdown], cfg[results][current_densities_A_m2]",
            ),
        ],
    )

    # Current demand ------------------------------------------------------
    demand_rows: list[Row] = []
    for z in zones:
        d = _mapping(demand.get(z))
        demand_rows.append(
            [
                z,
                d.get("area_m2", ""),
                *([d.get("area_basis", "steel_surface")] if has_area_basis else []),
                d.get("i_initial_A_m2", ""),
                d.get("i_mean_A_m2", ""),
                d.get("i_final_A_m2", ""),
                d.get("f_ci", ""),
                d.get("f_cm", ""),
                d.get("f_cf", ""),
                d.get("I_initial_A", ""),
                d.get("I_mean_A", ""),
                d.get("I_final_A", ""),
            ]
        )
    demand_rows.append(
        [
            "Total",
            areas.get("total_m2", ""),
            *([""] if has_area_basis else []),
            "", "", "", "", "", "",
            round(float(demand.get("total_initial_A", 0.0)), 4),
            round(float(demand.get("total_mean_A", 0.0)), 4),
            round(float(demand.get("total_final_A", 0.0)), 4),
        ]
    )
    steps = max(DEMAND_CURVE_SAMPLES - 1, 1)
    t_years = [round(design_life * k / steps, 6) for k in range(steps + 1)]
    series: dict[str, list[float]] = {}
    for z in zones:
        fc = _mapping(breakdown.get(z))
        a = float(fc.get("a", 1.0))
        b = float(fc.get("b_per_yr", 0.0))
        area = float(areas.get(z, 0.0))
        i_mean = float(_mapping(densities.get(z)).get("i_mean_A_m2", 0.0))
        series[z] = [
            round(kernel.current_demand(area, i_mean, kernel.coating_breakdown_linear(a, b, t)), 6)
            for t in t_years
        ]
    current_demand = Section(
        key="current-demand",
        title="Current demand",
        subtitle="DNV-RP-B401 Sec. 7.4: I = A x i x f_c per zone and design phase",
        blocks=[
            TableBlock(
                title="Current density, coating breakdown and demand by zone",
                columns=[
                    "Zone", "Area", *(["Area basis"] if has_area_basis else []), "i initial", "i mean", "i final",
                    "f_ci", "f_cm", "f_cf", "I initial", "I mean", "I final",
                ],
                units=["-", "m2"] + (["-"] if has_area_basis else []) + ["A/m2", "A/m2", "A/m2", "-", "-", "-", "A", "A", "A"],
                rows=demand_rows,
                source="cfg[results][current_demand_A]",
            ),
            FigureBlock(
                title="Maintenance current demand over the design life",
                caption=(
                    f"I(t) = A x i_mean x min(1, a + b t) per zone with {coating_table} "
                    "breakdown constants; the initial and final design cases use the "
                    "phase densities tabulated above (i_initial with f_ci, i_final with f_cf)."
                ),
                figure_id=FIG_DEMAND_VS_TIME,
                plotly=figure_from_columns(
                    "line",
                    t_years,
                    series,
                    title="Current demand vs time",
                    x_label="years in service",
                    y_label="current demand (A)",
                ),
            ),
        ],
    )

    # Anode requirements --------------------------------------------------
    n_mass = ver.get("count_by_mass", req.get("anode_count", 0))
    n_initial = ver.get("count_by_initial_current", 0)
    n_final = ver.get("count_by_final_current", 0)
    recommended = ver.get("recommended_anode_count", max(int(n_mass), int(n_initial), int(n_final)))
    req_rows: list[Row] = [
        ["Total net anode mass required (Sec. 7.7)", req.get("total_mass_kg", ""), "kg"],
        ["Individual anode net mass", req.get("individual_mass_kg", ""), "kg"],
        ["Design life", req.get("design_life_hours", ""), "h"],
        ["Anodes by mass (N_mass)", n_mass, "-"],
        ["Anodes by initial current output (N_initial)", n_initial, "-"],
        ["Anodes by final current output (N_final)", n_final, "-"],
        ["Recommended anode count", recommended, "-"],
        ["Count verified", ver.get("anode_count", ""), "-"],
        ["Count source", ver.get("anode_count_source", ""), "-"],
    ]
    anode_requirements = Section(
        key="anode-requirements",
        title="Anode requirements",
        subtitle="N = max(N_mass, N_initial, N_final), DNV-RP-B401 Sec. 7.7-7.8",
        blocks=[
            _kv_table(
                "Anode mass and count",
                req_rows,
                source="cfg[results][anode_requirements], cfg[results][current_output_verification]",
            ),
            _counts_figure(
                ["By mass", "By initial current", "By final current", "Recommended"],
                [n_mass, n_initial, n_final, recommended],
                "Anode count by sizing case",
            ),
        ],
    )

    # Adequacy ------------------------------------------------------------
    n_verified = ver.get("anode_count", "")
    ri_rows: list[Row] = [
        [
            "Initial (fresh anode)",
            ver.get("equivalent_radius_initial_m", ""),
            ver.get("anode_resistance_initial_ohm", ""),
            ver.get("anode_current_output_initial_A", ""),
            ver.get("total_anode_current_output_initial_A", ""),
            ver.get("initial_current_demand_A", ""),
            ver.get("initial_meets_demand", ""),
        ],
        [
            "Final (depleted anode)",
            ver.get("equivalent_radius_final_m", ""),
            ver.get("anode_resistance_final_ohm", ""),
            ver.get("anode_current_output_final_A", ""),
            ver.get("total_anode_current_output_final_A", ""),
            ver.get("final_current_demand_A", ""),
            ver.get("final_meets_demand", ""),
        ],
    ]
    adequacy = Section(
        key="adequacy",
        title="Adequacy",
        subtitle=f"DNV-RP-B401 Sec. 7.8 current-output verification ({resistance_table} resistance)",
        blocks=[
            _use_status_block(status),
            _status_block(status, "Anode design adequacy"),
            TableBlock(
                title=f"Anode resistance and current output, {n_verified} anodes, fresh vs depleted",
                columns=[
                    "Case", "Equivalent radius", "Resistance R_a", "Output per anode",
                    "Total output", "Demand", "Meets demand",
                ],
                units=["-", "m", "ohm", "A", "A", "A", "-"],
                rows=ri_rows,
                source="cfg[results][current_output_verification]",
            ),
            _checks_table(status),
        ],
    )

    usage: list[tuple[str, Sequence[Any]]] = []
    for z in zones:
        usage.append((f"{z}: design current density", _mapping(densities.get(z)).get("citations") or []))
        usage.append((f"{z}: coating breakdown", _mapping(breakdown.get(z)).get("citations") or []))
    usage.append(("Anode capacity and utilisation", req.get("citations") or []))
    usage.append(("Driving voltage", ver.get("citations") or []))
    return [design_basis, current_demand, anode_requirements, adequacy, _references_section(results, usage)]


# ---------------------------------------------------------------------------
# DNV-RP-F103 submarine pipeline (bracelets)
# ---------------------------------------------------------------------------


def _f103_sections(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Section]:
    geom = _mapping(results.get("pipeline_geometry_m"))
    fc = _mapping(results.get("coating_breakdown_factors"))
    dens = _mapping(results.get("current_densities_A_m2"))
    demand = _mapping(results.get("current_demand_A"))
    req = _mapping(results.get("anode_requirements"))
    spacing = _mapping(results.get("anode_spacing_m"))
    atten = _mapping(results.get("attenuation_analysis"))
    status = _mapping(results.get("status"))
    environment = _mapping(inputs.get("environment"))

    design_rows: list[Row] = [
        ["Standard", results.get("standard", ""), "-"],
        ["Edition provenance", results.get("provenance", ""), "-"],
        ["Design life", results.get("design_life_years", ""), "yr"],
        ["Outer diameter", geom.get("outer_diameter_m", ""), "m"],
        ["Wall thickness", geom.get("wall_thickness_m", ""), "m"],
        ["Length", geom.get("length_m", ""), "m"],
        ["Outer surface area", geom.get("outer_surface_area_m2", ""), "m2"],
        ["Linepipe coating", fc.get("linepipe_coating", ""), "-"],
        ["Field joint coating (system id)", fc.get("field_joint_coating", ""), "-"],
        ["Field joint infill", fc.get("field_joint_infill", ""), "-"],
        ["Field joint area", geom.get("field_joint_area_m2", ""), "m2"],
        ["Burial condition", dens.get("burial_condition", ""), "-"],
        ["Internal fluid temperature", dens.get("internal_fluid_temperature_C", ""), "degC"],
        ["Temperature band", dens.get("temperature_band", ""), "degC"],
        ["Steel resistivity", geom.get("steel_resistivity_ohm_m", ""), "ohm.m"],
        ["Seawater resistivity", environment.get("seawater_resistivity_ohm_m", "default"), "ohm.m"],
        ["Anode material", req.get("anode_material", ""), "-"],
        ["Bracelet net mass", req.get("individual_anode_mass_kg", ""), "kg"],
        ["Bracelet exposed area", req.get("bracelet_exposed_area_m2", ""), "m2"],
        ["Utilisation factor", req.get("utilization_factor", ""), "-"],
        ["Utilisation factor source", req.get("utilization_factor_source", ""), "-"],
        ["Metallic voltage drop allowance", atten.get("metallic_voltage_drop_V", ""), "V"],
    ]
    design_basis = Section(
        key="design-basis",
        title="Design basis",
        subtitle="Input echo: pipeline geometry, coating, exposure and anode data",
        blocks=[_kv_table("Design data", design_rows, source="cfg[inputs], cfg[results]")],
    )

    current_demand = Section(
        key="current-demand",
        title="Coating breakdown and current demand",
        subtitle="DNV-RP-F103 Table 5-1 density, Table A.1 breakdown, field joints included",
        blocks=[
            TableBlock(
                title="Coating breakdown factors",
                columns=["Surface", "Coating", "f_cm", "f_cf"],
                units=["-", "-", "-", "-"],
                rows=[
                    ["Linepipe", fc.get("linepipe_coating", ""), fc.get("mean_factor", ""), fc.get("final_factor", "")],
                    [
                        "Field joints",
                        fc.get("field_joint_coating", ""),
                        fc.get("mean_factor_field_joint", ""),
                        fc.get("final_factor_field_joint", ""),
                    ],
                ],
                source="cfg[results][coating_breakdown_factors]",
            ),
            _kv_table(
                "Current demand",
                [
                    ["Mean design current density", dens.get("mean_current_density_A_m2", ""), "A/m2"],
                    ["Linepipe area", geom.get("linepipe_area_m2", ""), "m2"],
                    ["Field joint area", geom.get("field_joint_area_m2", ""), "m2"],
                    ["Mean current demand", demand.get("mean_current_demand_A", ""), "A"],
                    ["Final current demand", demand.get("final_current_demand_A", ""), "A"],
                ],
                source="cfg[results][current_demand_A]",
            ),
        ],
    )

    n_mass = req.get("anode_count_by_mass", 0)
    n_final = req.get("anode_count_by_final_current", 0)
    n_total = req.get("anode_count", max(int(n_mass), int(n_final)))
    anode_requirements = Section(
        key="anode-requirements",
        title="Anode requirements",
        subtitle="Bracelet count, spacing and protected length",
        blocks=[
            _kv_table(
                "Anode mass, count and spacing",
                [
                    ["Total net anode mass required", req.get("total_anode_mass_kg", ""), "kg"],
                    ["Anode capacity", req.get("anode_capacity_Ah_kg", ""), "Ah/kg"],
                    ["Bracelets by mass (N_mass)", n_mass, "-"],
                    ["Bracelets by final current output (N_final)", n_final, "-"],
                    ["Bracelets installed (N)", n_total, "-"],
                    ["Anode spacing", spacing.get("spacing_m", ""), "m"],
                    ["Protected length (attenuation)", atten.get("protected_length_m", ""), "m"],
                    ["Maximum spacing (2 x protected length)", spacing.get("max_spacing_m", ""), "m"],
                ],
                source="cfg[results][anode_requirements], cfg[results][anode_spacing_m]",
            ),
            _counts_figure(
                ["By mass", "By final current", "Installed"],
                [n_mass, n_final, n_total],
                "Bracelet count by sizing case",
            ),
        ],
    )

    spacing_ok = bool(spacing.get("spacing_ok"))
    adequacy = Section(
        key="adequacy",
        title="Adequacy",
        subtitle="Route status, spacing check and current output",
        blocks=[
            _use_status_block(status),
            _status_block(status, "Bracelet CP design adequacy"),
            StatusBlock(
                label="Anode spacing <= 2 x protected length",
                status="PASS" if spacing_ok else "FAIL",
                governing_case=None if spacing_ok else "spacing",
                detail=(
                    f"spacing {spacing.get('spacing_m', '')} m vs "
                    f"{spacing.get('max_spacing_m', '')} m allowed"
                ),
            ),
            TableBlock(
                title="Anode resistance and current output (fresh bracelet)",
                columns=["Item", "Value", "Unit"],
                rows=[
                    ["Driving voltage", req.get("driving_voltage_V", ""), "V"],
                    ["Anode resistance R_a", req.get("anode_resistance_ohm", ""), "ohm"],
                    ["Output per anode", req.get("anode_current_output_A", ""), "A"],
                    [
                        "Total output (N anodes)",
                        round(float(req.get("anode_current_output_A", 0.0)) * float(n_total), 4),
                        "A",
                    ],
                    ["Final current demand", demand.get("final_current_demand_A", ""), "A"],
                    ["Current output adequate", atten.get("current_output_ok", ""), "-"],
                ],
                source="cfg[results][anode_requirements], cfg[results][attenuation_analysis]",
            ),
            _checks_table(status),
        ],
    )
    usage: list[tuple[str, Sequence[Any]]] = [("Route citations", results.get("citations") or [])]
    return [design_basis, current_demand, anode_requirements, adequacy, _references_section(results, usage)]


# ---------------------------------------------------------------------------
# ABS legacy routes
# ---------------------------------------------------------------------------


def _phase_rows(block: Mapping[str, Any], phases: Sequence[str]) -> list[Row]:
    rows: list[Row] = []
    for name, values in block.items():
        if isinstance(values, Mapping):
            rows.append([name, *[values.get(p, "") for p in phases]])
    return rows


def _abs_ships_sections(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Section]:
    demand = _mapping(results.get("current_demand_A"))
    dens = _mapping(results.get("current_densities_mA_m2"))
    fc = _mapping(results.get("coating_breakdown_factors"))
    req = _mapping(results.get("anode_requirements"))
    perf = _mapping(results.get("anode_performance"))
    status = _mapping(results.get("status"))
    phases = ("initial", "mean", "final")
    areas = _mapping(demand.get("areas_m2"))
    design_rows: list[Row] = [
        ["Design life", results.get("design_life", ""), "yr"],
        ["Seawater temperature", results.get("temperature", ""), "degC"],
        ["Anode current capacity", results.get("anode_current_capacity", ""), "Ah/kg"],
        ["Coated area", areas.get("coated", ""), "m2"],
        ["Uncoated area", areas.get("uncoated", ""), "m2"],
        ["Total area", areas.get("total", ""), "m2"],
    ]
    anode_in = _mapping(inputs.get("anode"))
    for key in ("material", "anode_Utilisation_factor"):
        if key in anode_in:
            design_rows.append([f"Anode {key}", anode_in.get(key), "-"])
    sections = [
        Section(
            key="design-basis",
            title="Design basis",
            subtitle="Input echo: hull areas, environment and anode data",
            blocks=[
                _kv_table("Design data", design_rows, source="cfg[inputs], cfg[results]"),
                TableBlock(
                    title="Coating breakdown factors",
                    columns=["Factor", "Value"],
                    rows=[[k, v] for k, v in sorted(fc.items())],
                    source="cfg[results][coating_breakdown_factors]",
                ),
            ],
        ),
        Section(
            key="current-demand",
            title="Current demand",
            blocks=[
                TableBlock(
                    title="Design current densities",
                    columns=["Surface / phase", "Density"],
                    units=["-", "mA/m2"],
                    rows=[[k, v] for k, v in sorted(dens.items())],
                    source="cfg[results][current_densities_mA_m2]",
                ),
                TableBlock(
                    title="Current demand by surface and phase",
                    columns=["Surface", "Initial", "Mean", "Final"],
                    units=["-", "A", "A", "A"],
                    rows=_phase_rows({k: v for k, v in demand.items() if k != "areas_m2"}, phases),
                    source="cfg[results][current_demand_A]",
                ),
            ],
        ),
        Section(
            key="anode-requirements",
            title="Anode requirements",
            blocks=[
                _kv_table(
                    "Anode mass and count",
                    [
                        ["Mean current", req.get("mean_current_A", ""), "A"],
                        ["Total net anode mass required", req.get("total_mass_kg", ""), "kg"],
                        ["Anode count (rounded up)", req.get("anode_count", ""), "-"],
                        ["Anode count (unrounded)", req.get("anode_count_raw", ""), "-"],
                    ],
                    source="cfg[results][anode_requirements]",
                )
            ],
        ),
    ]
    adequacy_blocks: list[Block] = [
        _use_status_block(status),
        _status_block(status, "Hull CP design adequacy"),
    ]
    if perf:
        resist = _mapping(perf.get("resistance_ohm"))
        out = _mapping(perf.get("current_output_A"))
        checks = _mapping(perf.get("checks"))
        totals = _mapping(demand.get("totals"))
        adequacy_blocks.append(
            TableBlock(
                title="Anode resistance and current output, fresh vs depleted",
                columns=["Case", "Resistance R_a", "Output per anode", "Total output", "Demand", "Meets demand"],
                units=["-", "ohm", "A", "A", "A", "-"],
                rows=[
                    [
                        "Initial (fresh anode)",
                        resist.get("initial", ""),
                        out.get("initial_per_anode", ""),
                        out.get("initial_total", ""),
                        totals.get("initial", ""),
                        checks.get("initial_meets_demand", ""),
                    ],
                    [
                        "Final (depleted anode)",
                        resist.get("final", ""),
                        out.get("final_per_anode", ""),
                        out.get("final_total", ""),
                        totals.get("final", ""),
                        checks.get("final_meets_demand", ""),
                    ],
                ],
                source="cfg[results][anode_performance]",
            )
        )
    adequacy_blocks.append(_checks_table(status))
    sections.append(Section(key="adequacy", title="Adequacy", blocks=adequacy_blocks))
    sections.append(_references_section(results, []))
    return sections


def _abs_offshore_sections(inputs: Mapping[str, Any], results: Mapping[str, Any]) -> list[Section]:
    fc = _mapping(results.get("coating_breakdown_factors"))
    dens = _mapping(results.get("current_densities_mA_m2"))
    demand = _mapping(results.get("current_demand_A"))
    req = _mapping(results.get("anode_requirements"))
    status = _mapping(results.get("status"))
    phases = ("initial", "mean", "final")
    design_rows: list[Row] = [
        ["Zone", results.get("zone", ""), "-"],
        ["Water depth", results.get("water_depth_m", ""), "m"],
        ["Depth zone", fc.get("depth_zone", ""), "-"],
        ["Climatic region", results.get("climatic_region", ""), "-"],
        ["Surface area", results.get("surface_area_m2", ""), "m2"],
        ["Design life", results.get("design_life_years", ""), "yr"],
        ["Anode current capacity", results.get("anode_current_capacity_Ah_kg", ""), "Ah/kg"],
        ["Anode utilisation factor", results.get("anode_utilisation_factor", ""), "-"],
    ]
    sections = [
        Section(
            key="design-basis",
            title="Design basis",
            subtitle="Input echo: zone, environment and anode data",
            blocks=[
                _kv_table("Design data", design_rows, source="cfg[inputs], cfg[results]"),
                _kv_table(
                    "Coating breakdown",
                    [
                        ["alpha (initial breakdown)", fc.get("alpha", ""), "-"],
                        ["beta (mean)", fc.get("beta_mean", ""), "1/yr"],
                        ["beta (final)", fc.get("beta_final", ""), "1/yr"],
                        ["f_c initial", fc.get("initial", ""), "-"],
                        ["f_c mean", fc.get("mean", ""), "-"],
                        ["f_c final", fc.get("final", ""), "-"],
                    ],
                    source="cfg[results][coating_breakdown_factors]",
                ),
            ],
        ),
        Section(
            key="current-demand",
            title="Current demand",
            blocks=[
                TableBlock(
                    title="Design current density and demand by phase",
                    columns=["Phase", "Current density", "Current demand"],
                    units=["-", "mA/m2", "A"],
                    rows=[[p, dens.get(p, ""), demand.get(p, "")] for p in phases],
                    source="cfg[results][current_densities_mA_m2], cfg[results][current_demand_A]",
                )
            ],
        ),
        Section(
            key="anode-requirements",
            title="Anode requirements",
            blocks=[
                _kv_table(
                    "Anode mass and count",
                    [
                        ["Total net anode mass required", req.get("total_mass_kg", results.get("anode_mass_kg", "")), "kg"],
                        ["Individual anode net mass", req.get("individual_mass_kg", "not given"), "kg"],
                        ["Anode count", req.get("anode_count", "not derived"), "-"],
                    ],
                    source="cfg[results][anode_requirements]",
                )
            ],
        ),
        Section(
            key="adequacy",
            title="Adequacy",
            blocks=[
                _use_status_block(status),
                _status_block(status, "Offshore structure CP design adequacy"),
                _checks_table(status),
            ],
        ),
        _references_section(results, []),
    ]
    return sections


# ---------------------------------------------------------------------------
# Registered adapters
# ---------------------------------------------------------------------------

_SECTION_BUILDERS = {
    KEY_B401: _b401_sections,
    KEY_F103: _f103_sections,
    KEY_F103_2010: _f103_sections,
    KEY_ABS_SHIPS: _abs_ships_sections,
    KEY_ABS_OFFSHORE: _abs_offshore_sections,
}


@report_adapter("cathodic_protection.anode_design")
def anode_design_report(cfg: Mapping[str, Any]) -> ReportSpec:
    """Anode-design report from a routed ``cathodic_protection`` cfg.

    Works for every engine-adapter route (B401 offshore, F103 bracelet, ABS
    ships / offshore). Raises ``ValueError`` when ``cfg["results"]`` is
    missing or the route is not one the adapter lays out.
    """
    inputs = _mapping(cfg.get("inputs"))
    results = _mapping(cfg.get("results"))
    if not results:
        raise ValueError(
            "cathodic_protection.anode_design needs cfg['results'] from "
            "run_cathodic_protection; nothing to report"
        )
    calc_type = str(inputs.get("calculation_type") or "")
    builder = _SECTION_BUILDERS.get(calc_type)
    if builder is None:
        raise ValueError(
            f"cathodic_protection.anode_design has no layout for calculation_type "
            f"{calc_type!r}; supported: {sorted(_SECTION_BUILDERS)}"
        )
    standard = str(results.get("standard") or _ABS_STANDARDS.get(calc_type, ("", ""))[0])
    document, doc_note = _document(cfg, f"Cathodic protection anode design - {standard}".strip(" -"))
    note = f"{standard}: {results.get('provenance', 'legacy solver')}"
    echo: dict[str, Any] = {"inputs": dict(inputs)}
    if doc_note:
        echo["report.document"] = doc_note
    return ReportSpec(
        document=document,
        standards=_standards(results, calc_type),
        citations=[asdict(c) for c in _citations_from_labels(results.get("citations") or [], note)],
        sections=builder(inputs, results),
        provenance=_provenance(cfg, results, "results"),
        input_echo=echo,
    )


# ---------------------------------------------------------------------------
# Assessment (CPAssessmentReport + survey + depletion)
# ---------------------------------------------------------------------------


def _compliance_status_block(report: CPAssessmentReport) -> StatusBlock:
    failing = [
        c.check_id
        for c in report.compliance_checks
        if c.status in (ComplianceStatus.NON_COMPLIANT, ComplianceStatus.MARGINAL)
    ]
    passed = report.overall_status == ComplianceStatus.COMPLIANT
    counts = {
        s.value: sum(1 for c in report.compliance_checks if c.status == s) for s in ComplianceStatus
    }
    detail = "; ".join(f"{k.replace('_', ' ')}: {v}" for k, v in counts.items() if v)
    return StatusBlock(
        label=f"Overall compliance ({report.system_id})",
        status="PASS" if passed else "FAIL",
        governing_case=None if passed else (", ".join(failing) or report.overall_status.value),
        detail=detail or "no compliance checks recorded",
    )


def build_assessment_spec(
    report: CPAssessmentReport,
    *,
    cis_points: Sequence[CISSurveyPoint] | None = None,
    cis_result: CISAnalysisResult | None = None,
    depletion: DepletionProfile | None = None,
    document: DocumentMeta | None = None,
    provenance: Provenance | None = None,
) -> ReportSpec:
    """Typed builder behind ``cathodic_protection.assessment``.

    ``document`` defaults to the placeholder number; ``provenance`` defaults to
    a sha256 of the report model (plus the survey / depletion models when
    given), so the spec always passes the engine's provenance rule.
    """
    doc_note: str | None = None
    if document is None:
        document = DocumentMeta(
            number=PLACEHOLDER_DOCUMENT_NUMBER,
            revision=PLACEHOLDER_REVISION,
            title=report.report_title,
            project="unassigned",
            client="unassigned",
        )
        doc_note = (
            f"placeholder {PLACEHOLDER_DOCUMENT_NUMBER} rev {PLACEHOLDER_REVISION}: "
            "no document control supplied"
        )
    if provenance is None:
        provenance = Provenance().add(
            "results",
            "CPAssessmentReport",
            digest=_sha256(report.model_dump_json().encode("utf-8")),
            description=f"cp_reporting assessment for {report.system_id}",
        )
        for label, model in (("CISAnalysisResult", cis_result), ("DepletionProfile", depletion)):
            if model is not None:
                provenance.add("results", label, digest=_sha256(model.model_dump_json().encode("utf-8")))
        if cis_points:
            payload = json.dumps([p.model_dump() for p in cis_points], sort_keys=True)
            provenance.add("results", "CISSurveyPoint[]", digest=_sha256(payload.encode("utf-8")))

    summary_md = (
        f"**System:** {report.system_id}\n\n**Asset:** {report.asset_description}\n\n"
        f"**Report date:** {report.report_date}"
    )
    if report.summary_text:
        summary_md += f"\n\n{report.summary_text}"
    sections: list[Section] = [
        Section(
            key="summary",
            title="Summary",
            blocks=[TextBlock(markdown=summary_md), _compliance_status_block(report)],
        )
    ]

    check_rows: list[Row] = [
        [
            c.check_id, c.description, c.standard_reference, c.criterion_value,
            c.measured_value, c.unit, c.status.value.replace("_", " "), c.notes,
        ]
        for c in report.compliance_checks
    ]
    compliance_blocks: list[Block] = []
    if check_rows:
        compliance_blocks.append(
            TableBlock(
                title="Compliance checks",
                columns=["Check", "Description", "Standard reference", "Criterion", "Measured", "Unit", "Status", "Notes"],
                rows=check_rows,
                source="CPAssessmentReport.compliance_checks",
            )
        )
    else:
        compliance_blocks.append(TextBlock(markdown="No compliance checks were recorded."))
    sections.append(Section(key="compliance", title="Compliance", blocks=compliance_blocks))

    if cis_points or cis_result is not None:
        survey_blocks: list[Block] = []
        if cis_points:
            distances = [p.distance_m for p in cis_points]
            series: dict[str, list[float]] = {"ON potential": [p.on_potential_V for p in cis_points]}
            if all(p.off_potential_V is not None for p in cis_points):
                series["OFF potential"] = [float(p.off_potential_V or 0.0) for p in cis_points]
            survey_blocks.append(
                FigureBlock(
                    title="Structure-to-electrolyte potential vs distance",
                    caption="Close-interval survey; potentials vs CSE.",
                    figure_id=FIG_POTENTIAL_VS_DISTANCE,
                    plotly=figure_from_columns(
                        "line",
                        distances,
                        series,
                        title="Potential vs distance",
                        x_label="distance (m)",
                        y_label="potential (V vs CSE)",
                    ),
                )
            )
        if cis_result is not None:
            survey_blocks.append(
                _kv_table(
                    "CIS analysis",
                    [
                        ["Survey points", cis_result.total_points, "-"],
                        ["Protected points", cis_result.protected_points, "-"],
                        ["Under-protected points", cis_result.underprotected_points, "-"],
                        ["Over-protected points", cis_result.overprotected_points, "-"],
                        ["Protection percentage", cis_result.protection_percentage, "%"],
                        ["Most negative potential", cis_result.min_potential_V, "V"],
                        ["Least negative potential", cis_result.max_potential_V, "V"],
                        ["Mean potential", cis_result.mean_potential_V, "V"],
                        [
                            "Deficiency locations",
                            ", ".join(f"{d:g}" for d in cis_result.deficiency_locations) or "none",
                            "m",
                        ],
                    ],
                    source="CISAnalysisResult",
                )
            )
        sections.append(Section(key="survey", title="Survey", blocks=survey_blocks))

    if report.remaining_life is not None or depletion is not None:
        life_blocks: list[Block] = []
        if report.remaining_life is not None:
            rl = report.remaining_life
            life_blocks.append(
                _kv_table(
                    "Remaining life",
                    [
                        ["Installation date", rl.installation_date or "not given", "-"],
                        ["Design life", rl.design_life_years, "yr"],
                        ["Elapsed", rl.elapsed_years, "yr"],
                        ["Remaining anode life", rl.remaining_anode_life_years, "yr"],
                        ["Remaining design life", rl.remaining_design_life_years, "yr"],
                        ["Life extension feasible", rl.life_extension_feasible, "-"],
                        ["Limiting factor", rl.limiting_factor, "-"],
                    ],
                    source="CPAssessmentReport.remaining_life",
                )
            )
        if depletion is not None:
            series = {"Remaining gross mass": list(depletion.remaining_mass_kg)}
            if depletion.usable_mass_kg and len(depletion.usable_mass_kg) == len(depletion.years):
                series["Remaining usable mass"] = list(depletion.usable_mass_kg)
            life_blocks.append(
                FigureBlock(
                    title="Remaining anode mass vs years in service",
                    caption=f"Usable mass exhausted at {depletion.end_of_life_year:g} years.",
                    figure_id=FIG_REMAINING_MASS,
                    plotly=figure_from_columns(
                        "line",
                        list(depletion.years),
                        series,
                        title="Remaining anode mass",
                        x_label="years in service",
                        y_label="mass (kg)",
                    ),
                )
            )
        sections.append(Section(key="remaining-life", title="Remaining life", blocks=life_blocks))

    rec_rows: list[Row] = [
        [r.recommendation_id, r.priority.value, r.description, r.action_required, r.estimated_cost_category, r.timeframe]
        for r in report.recommendations
    ]
    rec_blocks: list[Block] = (
        [
            TableBlock(
                title="Recommendations",
                columns=["ID", "Priority", "Description", "Action", "Cost category", "Timeframe"],
                rows=rec_rows,
                source="CPAssessmentReport.recommendations",
            )
        ]
        if rec_rows
        else [TextBlock(markdown="No recommendations.")]
    )
    sections.append(Section(key="recommendations", title="Recommendations", blocks=rec_blocks))

    echo: dict[str, Any] = {
        "system_id": report.system_id,
        "asset_description": report.asset_description,
        "compliance_checks": len(report.compliance_checks),
        "cis_points": len(cis_points or []),
        "depletion_profile": depletion is not None,
    }
    if doc_note:
        echo["report.document"] = doc_note
    return ReportSpec(document=document, sections=sections, provenance=provenance, input_echo=echo)


def _model(model_type: type[Any], value: Any) -> Any:
    if value is None or isinstance(value, model_type):
        return value
    return model_type.model_validate(value)


@report_adapter("cathodic_protection.assessment")
def assessment_report(cfg: Mapping[str, Any]) -> ReportSpec:
    """Assessment report from ``cfg["assessment"]`` (or ``cfg["results"]``).

    The block carries ``report`` (a :class:`CPAssessmentReport` or its
    mapping) and optionally ``cis_points``, ``cis_result`` and
    ``depletion_profile``; ``report.document`` in the cfg supplies document
    control as for the anode-design adapter.
    """
    block = _mapping(cfg.get("assessment")) or _mapping(cfg.get("results"))
    if "report" not in block:
        raise ValueError(
            "cathodic_protection.assessment needs cfg['assessment']['report'] "
            "(a CPAssessmentReport or its mapping)"
        )
    report = _model(CPAssessmentReport, block["report"])
    points_raw = block.get("cis_points") or []
    cis_points = [_model(CISSurveyPoint, p) for p in points_raw]
    supplied = _mapping(_mapping(cfg.get("report")).get("document"))
    document = DocumentMeta.model_validate(dict(supplied)) if supplied else None
    prov = _provenance(cfg, {"report": report.model_dump(), "cis_points": len(cis_points)}, "assessment")
    return build_assessment_spec(
        report,
        cis_points=cis_points or None,
        cis_result=_model(CISAnalysisResult, block.get("cis_result")),
        depletion=_model(DepletionProfile, block.get("depletion_profile")),
        document=document,
        provenance=prov,
    )


__all__ = [
    "DEMAND_CURVE_SAMPLES",
    "FIG_ANODE_COUNTS",
    "FIG_DEMAND_VS_TIME",
    "FIG_POTENTIAL_VS_DISTANCE",
    "FIG_REMAINING_MASS",
    "PLACEHOLDER_DOCUMENT_NUMBER",
    "PLACEHOLDER_REVISION",
    "anode_design_report",
    "assessment_report",
    "build_assessment_spec",
]
