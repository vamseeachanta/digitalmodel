# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate a fixed-platform air-gap capability page from an invented example.

Run with PYTHONPATH=src python scripts/capabilities/build_air_gap_capability.py.
The deterministic page uses the structural API page stylesheet. Wall time is
printed separately so a benchmark never changes the generated content.
"""

from __future__ import annotations

import argparse
from datetime import date
from html import escape
from pathlib import Path
from time import perf_counter

from digitalmodel.structural.jacket_topside.air_gap import (
    AssessmentBasis,
    CrestCase,
    DesignBasis,
    Silhouette,
    SubsidenceHistory,
    Survey,
    assess_air_gap,
    screen_remediation,
    screen_wave_in_deck,
)

PAGE = Path(__file__).resolve().parents[2] / "docs/api/structural/air-gap-analysis.html"


def build_example() -> dict:
    """Calculate every displayed result from the committed screening engine."""
    basis = DesignBasis(
        deck_underside_elevation_m=16.0,
        hat_elevation_m=2.0,
        surge_m=1.0,
        vertical_datum="invented fixed datum",
        assessment_basis=AssessmentBasis(
            "API RP 2SIM", "illustrative only", "invented design basis; not code-qualified",
        ),
        crest_cases=(
            CrestCase("operating", 10.0, 8.0, 0.5),
            CrestCase("design", 100.0, 10.0, 1.5),
        ),
    )
    history = SubsidenceHistory(
        (Survey(date(2016, 1, 1), 0.0), Survey(date(2020, 1, 1), 1.0)), 0.25,
    )
    as_of, target = date(2020, 1, 1), date(2032, 1, 1)
    assessment = assess_air_gap(
        basis, history, as_of, (date(2024, 1, 1), date(2028, 1, 1), target),
    )
    silhouette = Silhouette(20.0, 3.0, 0.5, 4.0, 2.0, 1025.0,
                            "invented input: uniform empty light framing, area ratio 0.5; API C17.6.2(a)")
    return {
        "label": "invented example platform",
        "basis": basis, "history": history, "assessment": assessment,
        "silhouette": silhouette,
        "loads": tuple(screen_wave_in_deck(s, silhouette) for s in assessment.snapshots),
        "remediation": screen_remediation(basis, history, as_of, target, 0.2),
    }


def _table(headers: tuple[str, ...], rows: list[tuple]) -> str:
    heading = "".join(f"<th scope=\"col\">{escape(h)}</th>" for h in headers)
    body = "\n".join("<tr>" + "".join(f"<td>{escape(str(v))}</td>" for v in row)
                     + "</tr>" for row in rows)
    return f"<table><thead><tr>{heading}</tr></thead><tbody>\n{body}\n</tbody></table>"


def render_page(example: dict) -> str:
    """Render escaped example inputs, engine results, options and limits."""
    basis, history = example["basis"], example["history"]
    assessment, remediation = example["assessment"], example["remediation"]
    silhouette = example["silhouette"]
    surveys = _table(("Survey date", "Cumulative subsidence (m)"), [
        (s.on.isoformat(), f"{s.cumulative_m:.1f}") for s in history.surveys
    ])
    cases = _table(("Case", "Return period (years)", "Crest above still water (m)",
                    "Required clearance above crest (m)"), [
        (c.name, f"{c.return_period_years:.1f}", f"{c.crest_above_still_water_m:.1f}",
         f"{c.required_clearance_m:.1f}") for c in basis.crest_cases
    ])
    results = _table(("Date", "Case", "Deck elevation (m)", "Still-water air gap (m)",
                      "Crest clearance (m)", "Margin (m)", "Meets clearance", "Wave contact",
                      "Horizontal drag (kN)"), [
        (s.on.isoformat(), s.case_name, f"{s.deck_elevation_m:.1f}",
         f"{s.still_water_air_gap_m:.1f}", f"{s.crest_clearance_m:.1f}",
         f"{s.clearance_margin_m:.1f}", str(s.meets_required_clearance).lower(),
         str(s.wave_contact).lower(), f"{load.horizontal_force_kn:.1f}")
        for s, load in zip(assessment.snapshots, example["loads"])
    ])
    breaches = _table(("Case", "First daily breach", "Breach year", "Horizon"), [
        (b.case_name, b.first_breach_date.isoformat() if b.first_breach_date else "none in horizon",
         b.first_breach_year if b.first_breach_year else "none", b.through_date.isoformat())
        for b in assessment.breaches
    ])
    options = []
    for option in remediation.options:
        lift = (f"<p>Geometric lift including allowance: {option.required_lift_m:.1f} m.</p>"
                if option.required_lift_m is not None else "")
        steps = "".join(f"<li>{escape(step)}</li>" for step in option.checklist)
        options.append(f"<h3>{escape(option.name)}</h3>{lift}<ol>{steps}</ol>")
    needed = "".join(f"<li>{escape(item)}</li>" for item in remediation.engineering_inputs_needed)
    limits = "".join(f"<li>{escape(item)}</li>" for item in example["loads"][0].limits)
    load = example["loads"][-1]
    citation = load.method_basis
    coefficients = _table(("Factor", "Value (-)", "Source", "Where used"), [
        (item.name, str(item.value), item.source, item.used_in)
        for item in load.coefficients
    ])
    return f"""<!DOCTYPE html>
<html lang="en" data-theme="light">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fixed-platform air-gap analysis</title>
<link rel="stylesheet" href="../_assets/brand.css">
<style>
body{{font-family:system-ui,sans-serif;max-width:1100px;margin:auto;padding:24px;
color:var(--ink,#203040);background:var(--bg,#fff);line-height:1.5}}
section{{margin:24px 0}}table{{border-collapse:collapse;display:block;overflow-x:auto}}
th,td{{border:1px solid var(--line,#ccd);padding:8px;text-align:left}}
th{{background:var(--panel,#eef)}}code{{overflow-wrap:anywhere}}
</style></head><body>
<h1>Fixed-platform air-gap analysis</h1>
<section><h2>Summary</h2>
<p>Subsidence reduces the underside clearance against a supplied design basis.
The engine reports dated clearances, first daily breaches, horizontal wave-contact drag
and three remediation routes. Results below are for an {escape(example['label'])};
they do not establish structural acceptance or lifting feasibility.</p></section>
<section><h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-23</td><td>Initial clearance and remediation example to expose subsidence screening.</td></tr></tbody></table></section>
<section><h2>Design basis</h2>
<p>Example geometry, density and kinematics are invented inputs, labelled assumed, based on previous experience.
Method coefficients are attributed separately below; the example is not site data.
SI units and a common {escape(basis.vertical_datum)} apply. Original deck underside:
{basis.deck_underside_elevation_m:.1f} m; HAT: {basis.hat_elevation_m:.1f} m;
coincident surge: {basis.surge_m:.1f} m.
Assessment route: {escape(basis.assessment_basis.code)},
{escape(basis.assessment_basis.edition)}; {escape(basis.assessment_basis.reference)}.</p>
{cases}
<p>Crests are heights above HAT plus surge. Required clearance is above the crest.
A current deck survey must be converted to the original zero-subsidence elevation
before cumulative loss is deducted. Return periods, code edition and reference,
clearance and coincident combinations must be supplied for an actual assessment.</p>
{surveys}
<p>Survey values are linearly interpolated by days. Forecast rate after the last survey:
{history.forecast_rate_m_per_year:.2f} m/year. A year is 365.25 days.
Forecast water levels stay fixed; uplift, differential settlement and sea-level change
are outside this model.</p></section>
<section><h2>Clearance and wave contact</h2>
<p>Deck elevation = original underside minus cumulative subsidence.
Still-water air gap = deck elevation minus HAT minus surge.
Crest clearance = still-water air gap minus crest height.
Margin = crest clearance minus required clearance. Equality meets clearance;
crest clearance at or below zero flags wave contact. Comparisons allow 1e-9 m
numerical round-off, which is not an engineering clearance tolerance.</p>
{results}
{breaches}
<p>Breaches are the first calendar day below the requirement beyond numerical round-off, searched
from the first survey to the reported horizon, including historical crossings.
An already-breached first survey bounds the onset; it cannot identify an earlier date.
No breach in the horizon is not an unlimited-life result.</p></section>
<section><h2>Wave-in-deck screening</h2>
<p>Status: <code>{escape(load.status)}</code>. Method:
<a href="{escape(citation.source)}">{escape(citation.standard)},
{escape(citation.edition)}, {escape(citation.clause)}</a>.
The historical edition is identified; applicability to an actual assessment needs confirmation.</p>
<p>For this rectangular screen, A = immersion times weighted area ratio times
(width cos heading + orthogonal width sin heading). Immersion is capped at the
silhouette depth. Nonuniform framing requires separate area integration.
F = 0.5 rho Cd A (aw V + ab U)^2; division by 1000 converts N to kN.
V is the unmodified maximum horizontal wave speed at the lower of crest and
silhouette top. U is in-line current; opposing/oblique currents are unsupported.
Force acts halfway up the immersed envelope. Original underside must coincide
with the lowest silhouette elevation; depth includes the solid equipment envelope.</p>
<p>Invented input geometry: width {silhouette.width_m:.1f} m;
envelope depth {silhouette.deck_depth_m:.1f} m;
orthogonal width {silhouette.orthogonal_width_m:.1f} m;
heading {silhouette.heading_deg:.1f} degrees; weighted area ratio {silhouette.solidity:.1f}.
Invented input wave speed {silhouette.velocity_m_s:.1f} m/s;
current {silhouette.current_m_s:.1f} m/s; density {silhouette.density_kg_m3:.1f} kg/m3.
Deck classification: {escape(silhouette.deck_type)}; storm: {escape(silhouette.storm_type)}.
The empty light-framing portion is represented by the uniform half-area ratio;
the deck classification is an independent invented assumption for the overall deck.</p>
{coefficients}
<p>At 2032-01-01 the design crest immerses 1.0 m:
area = {load.wetted_silhouette_m2:.1f} m2;
effective speed = {load.effective_velocity_m_s:.2f} m/s;
horizontal force = {load.horizontal_force_kn:.1f} kN;
force elevation = {load.force_elevation_m:.1f} m in the common datum.
At first touch the force is zero but contact still requires a full assessment.</p>
<ul>{limits}</ul></section>
<section><h2>Remediation options</h2>
<p>Target service end: {remediation.target_date.isoformat()}; governing case:
{escape(remediation.governing_case)}. Minimum lift restores all supplied target clearances;
this example adds 0.2 m allowance. It is not a jack stroke or approval to lift.</p>
{''.join(options)}
<h3>Engineering inputs for the business case</h3><ul>{needed}</ul>
<p>Costs and NPV are outside the engine. Manning and shut-in limits need risk assessment;
the checklist supplies no operating permission or numerical lift procedure.</p></section>
<section><h2>Reproduction and limits</h2>
<p>Engine: <code>src/digitalmodel/structural/jacket_topside/air_gap.py</code>.
Builder: <code>scripts/capabilities/build_air_gap_capability.py</code>.
Regenerate with <code>PYTHONPATH=src python scripts/capabilities/build_air_gap_capability.py</code>.
The builder prints wall time; tests compare regenerated bytes and hand-worked literals.</p>
<p>Left: Code-specific API RP 2A / API RP 2SIM / ISO 19902 qualification requires
the applicable edition and site basis. The cited public API text supplies the
silhouette method; no site validation or structural acceptance is established.
Full wave-in-deck, structural response and temporary-works design remain required.</p></section>
</body></html>
"""


def main(argv: list[str] | None = None) -> None:
    """Write the deterministic page and print the measured wall time."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=PAGE)
    args = parser.parse_args(argv)
    start = perf_counter()
    page = render_page(build_example())
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(page, encoding="utf-8", newline="\n")
    print(f"Generated air-gap capability page; wall time: {perf_counter() - start:.6f} s")


if __name__ == "__main__":
    main()
