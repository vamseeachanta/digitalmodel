# ABOUTME: Generate client-facing 1-page capability PDFs (light theme, branded).
# ABOUTME: One PDF per left-nav section and per live work; rendered via headless Chrome.
"""Build the **capability one-pagers** — a single-page, client-facing PDF for every
left-nav section and every live work surfaced on ``/capabilities/``, plus a
self-contained workflow-API artifact (a typed ResultEnvelope + request snippet +
an ``<iframe>`` embedding the live report) for every work.

Each one-pager is a self-contained, light-themed, A4 page (digitalmodel logo,
what-it-is, key figures, what-you-get, and the live link) rendered to PDF with
headless Chrome. Outputs (committed frozen artifacts; mkdocs serves them as static
files under ``docs/api``):

    docs/api/capabilities/pdf/<id>.pdf        # section + work one-pagers
    docs/api/capabilities/api/<id>.{html,json}  # one workflow-API call per work

Run (stdlib only; needs google-chrome on PATH, override with CHROME=/path):
    python3 scripts/capabilities/build_onepagers.py
"""

from __future__ import annotations

import html
import json as _json
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_CAP = _REPO / "docs" / "api" / "capabilities"
_OUT = _CAP / "pdf"
_API = _CAP / "api"
_SITE = "https://vamseeachanta.github.io/digitalmodel"
_CHROME = os.environ.get("CHROME") or shutil.which("google-chrome") or shutil.which(
    "google-chrome-stable"
) or shutil.which("chromium")

# digitalmodel brand logo (native colors), inlined from the repo asset with any
# XML prolog stripped so it drops cleanly into both the one-pager and API pages.
_LOGO = re.sub(
    r"<\?xml[^>]*\?>", "", (_REPO / "assets" / "logo" / "digitalmodel_logo_compact.svg").read_text(
        encoding="utf-8"
    )
).strip()

# Each spec: id, kind (section|work), title, std (basis line), path (live page —
# relative under the site, or a full http(s) URL for an external deliverable),
# blurb (what it is), figures [(value,label)], bullets (what you get).
SPECS: list[dict] = [
    # ---- sections (one per left-nav menu item) ----
    dict(id="sec-ffs", kind="section", title="Fitness-for-service",
         std="API 579-1 · ASME B31G · DNV-RP-F101", path="capabilities/#ffs",
         blurb="Remaining-strength assessment of corroded and metal-loss components with an "
               "inspector decision layer — benchmarked to standard worked examples and published "
               "ratings across the three governing FFS codes. Built to slot behind inspection, "
               "mechanical-integrity and RBI service programs: thickness and defect data in, "
               "code-cited verdict and report out.",
         figures=[("3", "governing FFS codes"), ("Part 4/5", "API 579-1 metal loss"),
                  ("510/570/653", "interval planning")],
         bullets=["Remaining-strength and inspector-verdict walkthrough across API 579-1, ASME B31G and DNV-RP-F101",
                  "Accept / re-rate / take-more-measurements / escalate driven by measurement sufficiency",
                  "UT / phased-array thickness-grid intake; remaining life and next-inspection interval per API 510 / 570 / 653",
                  "RSTRENG effective-area and B31G screens validated to published worked examples",
                  "API 579-1 Part 6 pitting (auto-routed from UT grids), Part 12 dent / dent-gouge, and BS 7910 Option-1 FAD weld-flaw envelopes"]),
    dict(id="sec-structural", kind="section", title="Ship structural strength",
         std="DNV-RP-C201 · class rules", path="capabilities/#structural",
         blurb="Plate and stiffened-panel buckling for ship structure — plate-field, column and "
               "tripping interaction explored against the governing class code.",
         figures=[("215.61 MPa", "panel tripping f_T"), ("DNV-RP-C201", "governing code")],
         bullets=["Plate capacity vs geometry and combined in-plane loading (Johnson-Ostenfeld correction)",
                  "Plate-field / column / torsional-tripping interaction for stiffened panels",
                  "Tripping stress reproduces the DNV-RP-C201 worked example (f_T = 215.61 MPa)"]),
    dict(id="sec-hydro", kind="section", title="Hydrodynamics & diffraction",
         std="WAMIT · AQWA · OrcaWave · OCIMF · Wang (1975)", path="capabilities/#hydro",
         blurb="Panel-method diffraction / radiation RAOs cross-checked between solvers, a 3-way "
               "unit-box benchmark, environmental load coefficients and a closed-form passing-ship "
               "interaction.",
         figures=[("3", "diffraction solvers"), ("6-DOF", "RAO overlay")],
         bullets=["Cross-solver 6-DOF RAO overlay between two diffraction solvers on one vessel",
                  "WAMIT / AQWA / OrcaWave unit-box added-mass, damping and RAO benchmark",
                  "OCIMF wind & current coefficients and a Wang (1975) passing-ship interaction check"]),
    dict(id="sec-risers", kind="section", title="Risers & pipelines",
         std="OrcaFlex · DNV-OS-F201", path="capabilities/#risers",
         blurb="OrcaFlex riser global-analysis verification — a validation report against reference "
               "cases and a mesh / segmentation convergence study.",
         figures=[],
         bullets=["Riser global-analysis results verified against the tier-2 fast model library",
                  "DNV-OS-F201 code-check on the riser response",
                  "Segmentation / mesh-density convergence study quantifying discretisation effect"]),
    dict(id="sec-subsea", kind="section", title="Subsea",
         std="cables · umbilicals · pipelines", path="capabilities/#subsea",
         blurb="To-scale cross-section schematics for offshore cables, umbilicals and pipelines, "
               "generated deterministically from a layer specification.",
         figures=[],
         bullets=["Layer-by-layer cross-section schematics for cable, umbilical and pipeline bundles",
                  "Envelope geometry computed from the layer specification",
                  "Self-contained, deterministic static output"]),
    dict(id="sec-installation", kind="section", title="Installation",
         std="DNV-RP-H103 · DNV-ST-N001 · IMCA · HSE RR444", path="capabilities/#installation",
         blurb="Marine-installation suitability — a crane-vessel pamphlet with lift envelopes, "
               "splash-zone operability and weather windows, a provenance-audited installation "
               "fleet database with confidence-weighted suitability ranking, and a DNV 2.7-1 "
               "offshore-container screen.",
         figures=[],
         bullets=["Crane lift suitability against the vessel's load chart",
                  "Operational and per-structure splash-zone Hs/Tp envelopes",
                  "Confidence-weighted vessel suitability ranking over the installation fleet",
                  "DNV 2.7-1 (DNVGL-ST-E271) offshore-container utilization screen"]),
    dict(id="sec-wind", kind="section", title="Floating wind",
         std="semi-sub · spar · TLP · barge · LCOE", path="capabilities/#wind",
         blurb="Floating-wind concept screening and economics — parametric floater archetypes "
               "screened across site load cases as the licence-free first tier, and an LCOE / "
               "TOTEX trade space with reliability, qualification and industrialisation levers.",
         figures=[("4", "floater archetypes"), ("IEA 15 MW", "reference turbine")],
         bullets=["Stability, motion, modal-separation and mooring-offset screens per variant",
                  "LCOE / TOTEX engine with capex breakdown and reliability scenarios",
                  "Qualification levers and turbine- / farm-scale economies of scale"]),
    dict(id="sec-validation", kind="section", title="Validated against published references",
         std="governing-code provenance · CI-guarded", path="capabilities/#validation",
         blurb="Every strength / FFS result reproduces a standard's worked example, a published "
               "rating, or the governing closed form, and carries a code_reference naming its "
               "governing code — kept in sync with the codes register by a CI test.",
         figures=[("16", "validated cases"), ("100%", "code-referenced"), ("CI", "drift-guarded")],
         bullets=["Published-validated vs derivation-anchored clearly labelled on every figure",
                  "Tubular, structural-detail and manoeuvring golden values frozen as references",
                  "A published number can never silently cite a code the register doesn't list"]),

    # ---- works (one per live card) ----
    dict(id="ffs-showcase", kind="work", title="FFS decision-layer showcase",
         std="API 579-1 · ASME B31G · DNV-RP-F101", path="ffs/ffs-showcase.html",
         blurb="Interactive remaining-strength and inspector-verdict walkthrough across the three "
               "governing FFS codes.",
         figures=[], bullets=["Remaining-strength across API 579-1, ASME B31G and DNV-RP-F101",
                              "Inspector decision layer over the computed margins"]),
    dict(id="ffs-field-dashboard", kind="work", title="FFS inspector field dashboard",
         std="measurement sufficiency", path="ffs/ffs-field-dashboard.html",
         blurb="Accept / re-rate / take-more-measurements / escalate, driven by measurement "
               "sufficiency on real defect data.",
         figures=[], bullets=["Decision verdict per defect from measurement sufficiency",
                              "Runs on real metal-loss defect data"]),
    dict(id="riser-joint-acceptance", kind="work", title="Riser-joint flaw-acceptance explorer",
         std="API 579-1 · Modified B31G · API RP 1111", path="ffs/riser-joint-acceptance-explorer.html",
         blurb="Level-1 acceptable flaw length-vs-depth envelopes for a drilling-riser joint with "
               "campaign-end growth allowance, collapse-limited water depth and string-zone "
               "placement — demonstrated on real anonymized inspection data.",
         figures=[], bullets=["Acceptance envelopes split base-metal / weld and campaign start / end",
                              "Collapse-qualified water depth from the measured minimum wall (API RP 1111)",
                              "String-zone placement verdict on the real 26-joint inspected fleet"]),
    dict(id="buckling-plate", kind="work", title="Ship plate buckling explorer",
         std="DNV-RP-C201", path="buckling/ship-plate-buckling.html",
         blurb="Plate capacity vs geometry and combined in-plane loading, with the Johnson-Ostenfeld "
               "inelastic correction.",
         figures=[], bullets=["Plate capacity swept over geometry and combined loading",
                              "Johnson-Ostenfeld inelastic correction applied"]),
    dict(id="buckling-panel", kind="work", title="Stiffened-panel buckling explorer",
         std="DNV-RP-C201", path="buckling/ship-panel-buckling.html",
         blurb="Plate-field / column / torsional-tripping interaction for a stiffened panel, swept "
               "over scantlings and load.",
         figures=[("215.61 MPa", "tripping f_T")],
         bullets=["Plate-field / column / tripping interaction over scantlings and load",
                  "Tripping stress reproduces the DNV-RP-C201 worked example"]),
    dict(id="rao-comparison", kind="work", title="AQWA vs OrcaWave RAO comparison",
         std="cross-solver diffraction", path="hydro/rao-comparison/",
         blurb="Interactive 6-DOF RAO overlay between two diffraction solvers on the same vessel model.",
         figures=[], bullets=["6-DOF RAO overlay between two diffraction solvers",
                              "Same vessel model on both solvers"]),
    dict(id="unitbox-report", kind="work", title="Unit-box 3-way diffraction benchmark",
         std="WAMIT · AQWA · OrcaWave", path="hydro/unit-box-benchmark/benchmark_report.html",
         blurb="Added-mass, damping and RAO amplitude & phase compared across three diffraction solvers.",
         figures=[("3", "solvers")], bullets=["Added-mass, damping and RAO amplitude & phase",
                              "Compared across WAMIT, AQWA and OrcaWave"]),
    dict(id="unitbox-amplitude", kind="work", title="RAO amplitude overlay",
         std="WAMIT · AQWA · OrcaWave", path="hydro/unit-box-benchmark/benchmark_amplitude.html",
         blurb="Per-DOF RAO amplitude overlay for the unit-box benchmark across the three solvers.",
         figures=[], bullets=["Per-DOF RAO amplitude overlay", "Three-solver unit-box benchmark"]),
    dict(id="unitbox-phase", kind="work", title="RAO phase overlay",
         std="WAMIT · AQWA · OrcaWave", path="hydro/unit-box-benchmark/benchmark_phase.html",
         blurb="Per-DOF RAO phase overlay for the unit-box benchmark across the three solvers.",
         figures=[], bullets=["Per-DOF RAO phase overlay", "Three-solver unit-box benchmark"]),
    dict(id="unitbox-combined", kind="work", title="Combined solver comparison",
         std="WAMIT · AQWA · OrcaWave", path="hydro/unit-box-benchmark/benchmark_combined.html",
         blurb="Combined amplitude-and-phase view of the three-solver unit-box diffraction comparison.",
         figures=[], bullets=["Combined amplitude-and-phase view", "Three-solver unit-box comparison"]),
    dict(id="unitbox-heatmap", kind="work", title="Solver correlation heatmap",
         std="WAMIT · AQWA · OrcaWave", path="hydro/unit-box-benchmark/benchmark_heatmap.html",
         blurb="Heatmap of cross-solver agreement across DOFs and frequencies for the unit-box benchmark.",
         figures=[], bullets=["Cross-solver agreement heatmap", "Across DOFs and frequencies"]),
    dict(id="ocimf", kind="work", title="OCIMF coefficient explorer",
         std="OCIMF MEG3 / MEG4", path="hydro/ocimf-coefficient-explorer.html",
         blurb="Wind & current force / moment coefficients interpolated by heading and loading condition.",
         figures=[], bullets=["Wind & current force / moment coefficients",
                              "Interpolated by heading and loading condition"]),
    dict(id="passing-ship", kind="work", title="Passing-ship interaction benchmark",
         std="Wang (1975)", path="hydro/passing-ship-benchmark.html",
         blurb="Sway-force and yaw-moment interaction validated against the published closed-form solution.",
         figures=[], bullets=["Sway-force and yaw-moment interaction",
                              "Validated against the Wang (1975) closed form"]),
    dict(id="drilling-riser-operability", kind="work",
         title="Drilling-riser operability envelope explorer",
         std="API-RP-16Q · API-STD-2RD",
         path="drilling/drilling-riser-operability-explorer.html",
         blurb="Governing utilisation across vessel offset and surface current for a generic "
               "drilling-riser stack-up per operating mode; operable when utilisation is at or "
               "below 1.0, with near-boundary points escalating to the exact analytical screen.",
         figures=[], bullets=["Offset x current operability envelope per operating mode",
                              "Governing utilisation vs cited code limits (API RP 16Q / API STD 2RD)",
                              "Near-boundary points escalate to the exact analytical run"]),
    dict(id="drilling-riser-operability-monitor", kind="work",
         title="Operability & integrity monitor",
         std="API-RP-16Q · API-STD-2RD",
         path="drilling/operability-monitor.html",
         blurb="Live-style playback of a generic drilling-riser scenario: telemetry and metocean drive "
               "the operability margin, the flex-joint response (physics plus a measured-tracking "
               "correction), a wellhead bending-moment indicator, and the drift-off time-to-limit — every "
               "value pre-computed by the analytical seams and played back (the page does no physics).",
         figures=[], bullets=["Watch-circle, operability margin, flex-joint, wellhead moment and drift-off at a glance",
                              "Twin-B correction only tightens the verdict; limits stay cited code values",
                              "Screening tier — operability and drift-off escalate states shown separately"]),
    dict(id="riser-validation", kind="work", title="Riser validation report",
         std="OrcaFlex · tier-2 fast library", path="orcaflex/riser-validation-report.html",
         blurb="Riser global-analysis results verified against reference cases from the tier-2 fast "
               "model library.",
         figures=[], bullets=["Global-analysis results vs reference cases",
                              "DNV-OS-F201 code-check on the response"]),
    dict(id="riser-mesh", kind="work", title="Riser mesh sensitivity report",
         std="OrcaFlex · convergence study", path="orcaflex/riser-mesh-sensitivity.html",
         blurb="Segmentation / mesh-density convergence study quantifying discretisation effect on "
               "riser response.",
         figures=[], bullets=["Segmentation / mesh-density convergence study",
                              "Discretisation effect on riser response"]),
    dict(id="subsea-xsection", kind="work", title="Offshore cross-section report",
         std="cables · umbilicals · pipelines", path="subsea/offshore-cross-section-report.html",
         blurb="Layer-by-layer cross-section schematics for cable, umbilical and pipeline bundles "
               "with envelope geometry.",
         figures=[], bullets=["Layer-by-layer cross-section schematics",
                              "Envelope geometry from a layer specification"]),
    dict(id="installation-bokalift", kind="work", title="Installation vessel pamphlet — BokaLift 2",
         std="DNV-RP-H103 · DNV-ST-N001 · IMCA · HSE RR444",
         path="https://vamseeachanta.github.io/deckhand-sandbox/domains/floating-marine/"
              "deckhand-deliverables/2026/2026-06-28/installation-vessel-pamphlet-bokalift2/report.html",
         blurb="Crane lift suitability, operational + per-structure splash-zone Hs/Tp envelopes, "
               "weather-window operability and DP/mooring failure-risk, published as a Deckhand "
               "deliverable.",
         figures=[], bullets=["Crane lift suitability against the load chart",
                              "Splash-zone Hs/Tp envelopes and weather-window operability",
                              "DP / mooring failure-risk screen"]),
    dict(id="sec-manoeuvring", kind="section", title="Manoeuvring & station-keeping",
         std="IMO MSC.137(76) · Clarke 1983 · OCIMF", path="capabilities/#manoeuvring",
         blurb="Low-speed manoeuvring and station-keeping screens — turning circle against the IMO "
               "criteria, minimum steerage speed by loading condition, the rudder angle needed "
               "to hold heading against a current, built on a researched rudder-type database, "
               "and a mooring-system resilience traffic-light composed from the pre-computed "
               "mooring atlases.",
         figures=[],
         bullets=["Turning circle vs the 5·L IMO advance/tactical-diameter limit",
                  "Steerage-threshold speed vs wind (laden / ballast / kick-ahead)",
                  "Engine-on rudder angle to balance the current yaw moment",
                  "Mooring resilience traffic-light (intact / damaged / foundation / fatigue)"]),
    dict(id="rudder-explorer", kind="work", title="Rudder & low-speed manoeuvring explorer",
         std="IMO MSC.137(76) · Clarke 1983 · OCIMF", path="hydro/rudder-maneuvering-explorer.html",
         blurb="Interactive turning circle vs the 5·L limit, steerage-threshold speed vs wind "
               "(laden / ballast / kick-ahead), and the engine-on rudder angle to balance the "
               "current yaw moment.",
         figures=[], bullets=["Turning circle vs the IMO 5·L limit",
                              "Steerage-threshold speed vs wind by loading condition",
                              "Engine-on rudder angle to balance the current"]),
    dict(id="rudder-database", kind="work", title="Rudder-type database",
         std="Molland & Turnock · Brix · DNV/ABS",
         path="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/"
              "naval_architecture/data/rudder_database.yml",
         blurb="Nine rudder types (spade, semi-balanced horn, flap/Becker, Schilling, fishtail, "
               "Kort nozzle, twisted, gate) with area ratios, aspect ratios, lift coefficients and "
               "a class-rule area sizing check.",
         figures=[], bullets=["Nine rudder types with area / aspect ratios and lift coefficients",
                              "Class-rule (DNV/ABS) rudder-area sizing check"]),
    dict(id="diffraction-deck-prep", kind="work",
         title="Diffraction deck generation — spec to solver input",
         std="AQWA · OrcaWave · canonical spec",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/workflows/"
              "aqwa-diffraction-deck-prep",
         blurb="One canonical vessel spec (YAML) deterministically prepared into a solver-ready "
               "AQWA diffraction deck offline; the OrcaWave solve runs on the licensed lane with "
               "provenance carried through.",
         figures=[], bullets=["Solver-ready AQWA .dat deck prepared offline from one spec",
                              "OrcaWave solve on the licensed lane, same canonical spec",
                              "Deterministic registry workflow, durable-workflow tested"]),
    dict(id="vessel-suitability", kind="work",
         title="Installation vessel database & suitability ranking",
         std="DNV-RP-H103 · flag-don't-fake provenance",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/data/vessels",
         blurb="Curated + worldenergydata installation crane fleet (crane curves, deck, DP) with "
               "cited / estimated / gap provenance on every field; ranks vessels for a lift by "
               "capability margin weighted by data confidence.",
         figures=[], bullets=["Every field cited, flagged estimated, or an explicit gap — never faked",
                              "Confidence-weighted capability score per vessel for a lift requirement",
                              "Defensibility flag on each ranking"]),
    dict(id="container-utilization", kind="work",
         title="Offshore container utilization — DNV 2.7-1",
         std="DNVGL-ST-E271 (2017)",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/structural/"
              "offshore_container_utilization",
         blurb="Clause-traced offshore-lift screening for CCUs — 2.5·R·g primary load, 3·R·g "
               "pad-eye set, 0.85·Re allowable — swept into utilization curves across rating, "
               "aspect ratio and sling angle.",
         figures=[], bullets=["Clause-traced design loads per DNVGL-ST-E271 §4.2.3.1 / §4.2.1",
                              "Utilization curves vs rating, aspect ratio and sling angle",
                              "Screening tier ahead of the certified FEA + prototype tests"]),
    dict(id="wind-sizing", kind="work", title="Floating-wind sizing & concept screening",
         std="semi-sub · spar · TLP · barge · IEA 15 MW",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/workflows/"
              "floating-wind-sizing",
         blurb="Parametric floater archetypes swept across site load cases and screened on "
               "stability, motion, modal separation and mooring offset — the closed-form first "
               "tier ahead of the licensed OrcaWave/OrcaFlex pass.",
         figures=[("4", "floater archetypes")],
         bullets=["Stability / motion / modal / mooring screen per design variant",
                  "IEA 15 MW-class lumped turbine topside carried explicitly",
                  "Licence-free closed-form tier; shortlist goes to OrcaWave/OrcaFlex"]),
    dict(id="wind-economics", kind="work", title="Floating-wind LCOE / TOTEX tradespace",
         std="LCOE · reliability · economies of scale",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/workflows/"
              "floating-wind-economics",
         blurb="Levelised-cost engine with capex breakdown, availability / reliability scenarios, "
               "qualification levers and turbine- / farm-scale economies swept over the concept "
               "trade space.",
         figures=[], bullets=["LCOE / TOTEX with an explicit capex breakdown",
                              "Availability and reliability scenarios applied to the cost of energy",
                              "Turbine- and farm-scale economies-of-scale sweeps"]),
    dict(id="mooring-resilience", kind="work", title="Mooring resilience screen",
         std="API RP 2SK · DNV-OS-E301",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/marine_ops/"
              "mooring_resilience",
         blurb="Intact / one-line-damaged tension, anchor-foundation and chain-fatigue checks "
               "composed from the pre-computed mooring and anchor atlases into a single "
               "traffic-light — out-of-range cases are escalated, never extrapolated.",
         figures=[], bullets=["Intact 1.67 / damaged 1.25 ASD factors per API RP 2SK",
                              "Anchor-foundation check at 1.5 per DNV-OS-E301 practice",
                              "GREEN / AMBER / RED / ESCALATE traffic-light per case"]),
    dict(id="short-horizon-motion-forecast", kind="work",
         title="Short-horizon motion forecast",
         std="MMS / MRU feed · DNV-ST-N001 · DNV-ST-0358 / Walk2Work · CAP 437",
         path="hydro/short-horizon-motion-forecast.html",
         blurb="Live vessel & structure motions from an onboard MMS / MRU, plus a phase-resolved "
               "wave forecast transferred through the asset RAO to predict the next few motions — "
               "a rolling go / no-go with lead time for crane lifts, walk-to-work gangways and "
               "helideck landings, bounded honestly by the predictable zone.",
         figures=[], bullets=["Two data modes: measured (MMS/MRU) 'now' + predicted forecast",
                              "6-DOF vessel & structure-interface motion via AQWA / OrcaFlex RAOs",
                              "Rolling GO / CAUTION / NO-GO with lead-time to first breach"]),
    dict(id="sec-artificial-lift", kind="section", title="Artificial lift — rod-pump diagnostics",
         std="Gibbs wave equation · Bezerra projections", path="capabilities/#artificial-lift",
         blurb="Sucker-rod-pump dynamometer-card (dynacard) diagnostics — surface-to-downhole "
               "wave-equation solvers, an 18-mode failure-signature library plus a 336-card "
               "example library, ML card classification with a CI drift-guard on every "
               "published diagnosis, recommended controller setpoints & alarms, and a "
               "field-wide health screen.",
         figures=[("336", "example cards"), ("28", "guide entries"), ("2", "registered workflows")],
         bullets=["Troubleshooting use cases: card signature → ML diagnosis → field response",
                  "Example-card library: synthetic-verified, measured field wells, digitized archive",
                  "POC setpoints & alarms recommended per well from its own card",
                  "Field-wide per-well health ranking with a fail-closed screening verdict"]),
    dict(id="dynacard-troubleshooting", kind="work", title="Dynacard troubleshooting explorer",
         std="Gibbs wave equation · Bezerra projections · 18 failure modes",
         path="artificial-lift/dynacard-troubleshooting.html",
         blurb="Seven curated rod-pump troubleshooting cases, a 336-card example library behind "
               "phenomenon dropdowns (multiple cards per phenomenon), a 28-entry troubleshooting "
               "guide, and recommended controller setpoints & alarms computed per well.",
         figures=[("336", "example cards"), ("28", "phenomena in guide"), ("7/7", "CI-verified diagnoses")],
         bullets=["Example-card library: synthetic-verified, measured field wells, digitized archive",
                  "ML classification with confidence and top-3 differential",
                  "Two-tier load / span / card-area alarm recipes computed per well",
                  "Every published diagnosis re-verified against the live classifier in CI"]),
    dict(id="sec-well", kind="section", title="Well construction — casing & tubulars",
         std="API 5C3 · API 5CT · NACE MR0175", path="capabilities/#well",
         blurb="Production-casing design checks on the API 5C3 / API 5CT product catalog — burst "
               "(tubing leak + frac screen-out), full-evacuation collapse, buoyed-weight tension "
               "and Von Mises triaxial against published operator design-factor minimums, plus the "
               "maximum allowable frac surface pressure, the NACE MR0175 sour-service screen and "
               "the connection-class tension efficiencies.",
         figures=[("4", "design-check modes"), ("14,520 psi", "golden Barlow burst"),
                  ("MR0175", "sour-service screen")],
         bullets=["Burst / collapse / tension / triaxial design factors with governing depth per product",
                  "API 5C3 ratings (4-regime collapse) computed from the API 5CT catalog, API-rounded",
                  "Max allowable frac surface pressure at the burst design factor",
                  "NACE MR0175 sour screen (0.05 psia H2S; 65 / 265 psia) with grade temperature windows"]),
    dict(id="casing-design", kind="work", title="Casing design explorer",
         std="API 5C3 · API 5CT · NACE MR0175",
         path="well/casing-design-explorer.html",
         blurb="Eight candidate products checked against a reference frac'd well — per-mode design "
               "factors with governing depth and pass/fail, max allowable frac surface pressure, "
               "the golden Barlow worked example (5-1/2\" 23# P110 → 14,520 psi), the NACE "
               "sour-service screen and the connection-class table.",
         figures=[("8", "candidate products"), ("14,520 psi", "golden Barlow burst")],
         bullets=["Per-mode design factor, governing depth and pass/fail for every product",
                  "Depth profiles for the tubing-leak and frac screen-out load cases vs the burst rating",
                  "Max allowable frac surface pressure at DF 1.25 per product",
                  "Every published margin re-verified against the live checks in CI"]),
    dict(id="dynacard-field-health", kind="work", title="Field-wide dynacard health rollup",
         std="per-well diagnosis · fail-closed screening",
         path="https://github.com/vamseeachanta/digitalmodel/tree/main/examples/workflows/"
              "artificial-lift-field-health",
         blurb="Every well's dynamometer card classified and ranked into normal / warning / "
               "critical / failure, with the worst wells surfaced and a fail-closed field "
               "screening verdict as JSON + CSV.",
         figures=[], bullets=["Per-well ML diagnosis rolled up to field status counts",
                              "Worst-wells ranking by severity and fillage",
                              "Fail-closed screening verdict (JSON summary + per-well CSV)"]),
    # ---- Corrosion & production chemistry ----
    dict(id="sec-corrosion-production", kind="section", title="Corrosion & production chemistry",
         std="MIL-STD-889 · Oddo–Tomson (SPE 21710)", path="capabilities/#corrosion-production",
         blurb="Screening-level materials-compatibility and produced-water scaling checks that sit "
               "alongside the integrity workflow — dissimilar-metal galvanic risk on the published "
               "anodic-index method, and mineral-scale saturation tendency in the Oddo–Tomson "
               "conditional-solubility framework. Both run from public constants with every "
               "coefficient documented and overridable; parameterized for calibration before "
               "quantitative use.",
         figures=[("3", "galvanic environment classes"), ("7", "scale families"),
                  ("public", "constant basis")],
         bullets=["Galvanic couple screen (OK / marginal / protect) vs per-environment allowable ΔV, MIL-STD-889 anodic index",
                  "Mineral-scale saturation indices (calcite / sulfate / halite) with bottomhole→wellhead trending",
                  "Formation-water × seawater mixing sweeps for waterflood compatibility",
                  "Published anodic-index and thermodynamic constants only; no operator-proprietary data"]),
    dict(id="galvanic-explorer", kind="work", title="Galvanic compatibility explorer",
         std="MIL-STD-889 anodic index", path="corrosion/galvanic-compatibility-explorer.html",
         blurb="Dissimilar-metal couple screening (OK / marginal / protect) against the "
               "environment-dependent allowable potential difference, identifying the anodic "
               "(sacrificial) member with area-ratio guidance — a per-environment compatibility "
               "heatmap over the common construction metals.",
         figures=[("0.15–0.50 V", "allowable ΔV by environment")],
         bullets=["Couple verdict and anodic member from the published anodic-index table",
                  "Per-environment compatibility matrix (harsh-marine / industrial / controlled-indoor)",
                  "Area-ratio and coat-the-cathode guidance on flagged couples"]),
    dict(id="scale-si-explorer", kind="work", title="Mineral-scale saturation explorer",
         std="Oddo–Tomson · SPE 21710", path="production/scale-si-explorer.html",
         blurb="Saturation index per scale family (calcite, sulfates, halite) swept over temperature "
               "and pressure, bottomhole→wellhead trending, and a formation-water × seawater mixing "
               "sweep for waterflood compatibility — the Oddo–Tomson conditional-solubility framework "
               "with documented, overridable coefficients.",
         figures=[("7", "scale families")],
         bullets=["SI = log10(IAP / Ksp_cond) per family, supersaturation flagged at SI > 0",
                  "T/P sweep and bottomhole→wellhead SI trending along the production string",
                  "Waterflood mixing sweep (barium-rich formation water × sulfate-rich seawater)"]),
    dict(id="wall-thickness-explorer", kind="work", title="Wall-thickness multi-code explorer",
         std="DNV-ST-F101 · API RP 1111 · ASME B31.4/8 · PD 8010-2 · ISO 13623",
         path="structural/wall-thickness-explorer.html",
         blurb="One 12.75-inch X65 deepwater line sized under all nine registered design codes at "
               "once — a wall-thickness slider drives each code's governing utilization against the "
               "1.0 limit, with the minimum wall to pass per code. Every point is a live "
               "WallThicknessAnalyzer evaluation (369 in total), not a mock.",
         figures=[("9", "design codes"), ("369", "engine evaluations"), ("~19.5 mm", "min wall, pipeline codes")],
         bullets=["Burst / pressure-containment, collapse (solved from the cubic), propagation-buckling and combined-loading utilizations per code",
                  "Edition-aware factors (DNV-OS-F101 2007 vs DNV-ST-F101 2021; API RP 1111 3rd vs 4th edition)",
                  "Propagation buckling governs the pipeline codes (~19.5 mm needed) while DNV-ST-F201 riser LRFD passes at 8 mm"]),
    dict(id="sloshing-explorer", kind="work", title="Tank sloshing natural-period & resonance explorer",
         std="Faltinsen linear potential theory · McIver · API 650 Annex E",
         path="structural/sloshing-explorer.html",
         blurb="Fundamental sloshing natural period across tank shapes — rectangular, upright "
               "cylinder, horizontal cylinder (road tanker) and elliptical/obround fuel tanks — as a "
               "normalized master curve that collapses every size onto one dimensionless family, a "
               "shape-selector lookup for real periods, and a resonance screen showing partial-fill "
               "coupling with vessel roll. Backed by a worldwide relationship survey.",
         figures=[("4", "tank shapes"), ("Ω₁=ω₁√(Lc/g)", "normalized master curve"),
                  ("<0.5%", "API 650 ↔ potential-flow")],
         bullets=["Normalized dimensionless master curve (Ω₁ vs fill/slenderness ratio) — any size reads off one family",
                  "Shape dropdown → real natural periods for rectangular, upright/horizontal cylinder and oval cross-sections (equivalent-rectangle for the tanker shapes)",
                  "Resonance screen: partial fill sweeps the sloshing period into the vessel roll band — the coupling external diffraction (AQWA/OrcaWave) misses, and the analytical→CFD tiering behind the ballast-tank study",
                  "Relationship basis surveyed worldwide: Abramson, Ibrahim, Faltinsen & Timokha, McIver, Housner/API 650, plus road/automotive-tanker (UMTRI, Rajagounder, Micheli)"]),
    dict(id="cathodic-protection-explorer", kind="work", title="Cathodic-protection anode explorer",
         std="DNV-RP-B401",
         path="structural/cathodic-protection-explorer.html",
         blurb="DNV-RP-B401 sacrificial-anode design for a jacket or monopile — structure and "
               "seawater-climate selectors and a design-life slider drive total anode mass by "
               "external-coating quality. Every value is a live marine_structure_current_demand "
               "run (current demand → anode mass → anode count).",
         figures=[("31.7 → 18.6 t", "bare vs premium coating"), ("159 → 93", "anode count")],
         bullets=["Current demand with coating breakdown → anode mass → anode count, per DNV-RP-B401",
                  "Coating quality (bare / aged / good / premium) swept against a 10–40 yr design life and seawater climate",
                  "Premium external coating cuts a 4-leg jacket's 25-yr anode mass from ~31.7 t (159 anodes) to ~18.6 t (93)"]),

    # ---- sections added by #1456 (closes the #1444 pdf_gaps set; every
    # ---- standard in a std line is grep-grounded in a file the section
    # ---- links — enforced by tests/capabilities test_specs_standards_grounded) ----
    dict(id="sec-fatigue", kind="section",
         title="Fatigue & fracture — S-N life and crack growth",
         std="DNV-RP-C203 · API RP 2A-WSD · BS 7608 · EN 1993-1-9 · NORSOK N-004 · BS 7910",
         path="capabilities/#fatigue",
         blurb="A code-referenced fatigue toolkit: a 221-curve S-N library spanning DNV-RP-C203, "
               "API RP 2A-WSD, BS 7608, Eurocode 3 and NORSOK N-004; Efthymiou stress-concentration "
               "factors for tubular joints; rainflow counting into Miner's-rule damage; "
               "hotspot-stress extrapolation; and Paris'-law crack growth. Every curve, SCF "
               "equation and read-out formula carries its clause reference in-code, with numeric "
               "checks pinned to published tables and hand calculations.",
         figures=[("221", "S-N curves, 6 standards"), ("17", "standard curve sets"),
                  ("3", "environments (air / seawater+CP / free corrosion)")],
         bullets=["Single S-N lookup across 17 standard sets keyed by curve id (e.g. DNV-RP-C203:D:air) with slope, knee and thickness-correction metadata",
                  "Efthymiou (OTC 4829) SCFs for T/Y and K tubular joints under axial, in-plane and out-of-plane bending, plus DNV plate / weld SCFs",
                  "Weld-fatigue quick-check: stress-range histogram + weld class → thickness correction → Palmgren-Miner damage, pass/fail and life factor",
                  "Two- and three-point hotspot-stress extrapolation to the weld toe (DNV-RP-C203 §4.3 · IIW)",
                  "Paris'-law crack-growth integration (BS 7910) from initial to critical crack size — the fracture-mechanics complement to the S-N route"]),
    dict(id="sec-cfd", kind="section",
         title="Computational fluid dynamics (CFD)",
         std="OpenFOAM v2312 · known-answer verification · VOF / RANS / overset",
         path="capabilities/#cfd",
         blurb="Open-source CFD (OpenFOAM) verified against classical analytical and benchmark "
               "solutions — known-answer verification across steady-laminar, transient-laminar, "
               "turbulent (RANS), free-surface (VOF), wave-generation, 6-DOF floating-body and "
               "wave-body interaction (overset) regimes, computed on real OpenFOAM v2312, not "
               "mocked. Reports are interactive: live charts, scrubbable animations and a member "
               "VIV lock-in screening calculator.",
         figures=[("9", "verification cases + RAO spot-check"), ("73", "real 2D sloshing cases"),
                  ("1.7%", "heave RAO vs long-wave limit")],
         bullets=["Free-surface (VOF) canon: dam break (Martin & Moyce), numerical wave tank, green-water impact (Kleefsman / MARIN) and forced-roll tank sloshing within 0.29% of the analytical tanh dispersion",
                  "Overset wave-body interaction: floating-box heave RAO within 1.7% of the long-wave limit, frequency-swept against a frozen potential-flow (BEM) reference",
                  "Cylinder wave loading vs the exact MacCamy-Fuchs diffraction closed form (3.4% at ka = 0.96) — a case a Morison estimate fails",
                  "Laminar / turbulent benchmarks: Blasius flat plate, Re=100 Karman street (Strouhal within 0.9%), k-omega SST law-of-the-wall, NACA0012 polar",
                  "Ballast-tank sloshing study: master curve pinned by measured VOF fields, passive roll-damper reduction, and a run-time estimator calibrated to the machine's own measured runs"]),
    dict(id="sec-wall-thickness", kind="section",
         title="Wall thickness — sizing & code checks",
         std="DNV-ST-F101 · API RP 1111 · ASME B31.4 · ASME B31.8 · PD 8010-2 · ISO 13623 · DNV-ST-F201 · API STD 2RD",
         path="capabilities/#wall-thickness",
         blurb="Pipeline and riser wall-thickness sizing and pressure-integrity code checks — one "
               "geometry / material / loads spec dispatched across ten registered design codes "
               "(edition-aware: DNV-OS-F101 2007 vs DNV-ST-F101 2021, API RP 1111 3rd vs 4th "
               "edition), with burst, collapse (solved from the cubic, not tabulated), "
               "propagation-buckling and combined-loading utilizations, minimum-thickness solvers "
               "and cross-code comparison reports. Every result carries a code_reference kept in "
               "sync with the codes register.",
         figures=[("10", "registered design codes"), ("369", "live engine evaluations"),
                  ("~19.5 mm", "min wall, pipeline codes")],
         bullets=["Strategy-registry engine: submarine pipeline, riser LRFD and onshore / transmission codes behind one PipeGeometry / PipeMaterial / DesignLoads spec",
                  "Edition-aware safety factors with a machine-readable clause / equation manifest per code",
                  "Cross-code comparison live: the same 12.75″ X65 deepwater line under all registered codes, min wall to pass per code",
                  "API RP 1111 installation screens — propagating-buckle pressure, transition water depth, buckle-arrestor verdicts",
                  "Screening-level quick check from a minimal input deck, plus 30 CFR Part 250 regulatory burst / minimum-thickness cards"]),
    dict(id="sec-viv", kind="section",
         title="Vortex-induced vibration — screening, frequency & fatigue",
         std="DNV-RP-C205 · Strouhal · DNV-RP-C203",
         path="capabilities/#viv",
         blurb="A first-pass VIV toolkit for tubular members and risers: natural-frequency "
               "calculation (Euler-Bernoulli beam + tension-controlled string, five boundary "
               "conditions, fluid added mass), vortex-shedding frequency from the Strouhal "
               "relation, reduced-velocity lock-in screening with a safety factor, and VIV-induced "
               "fatigue on DNV-RP-C203 S-N curves — the full-member generalization of the lock-in "
               "check the CFD Re=100 cylinder card exposes. Backed by unit, CLI and integration "
               "tests and a standalone CI workflow.",
         figures=[("4–8", "cross-flow lock-in V_r band"), ("13", "DNV-RP-C203 S-N curves"),
                  ("5", "beam boundary conditions")],
         bullets=["Reduced-velocity lock-in screening live: V_r = V/(f_n·D) against the 4–8 cross-flow band with safe / marginal / lock-in status and safety factor",
                  "Natural frequency from Euler-Bernoulli beam + tensioned-string models, five boundary conditions, fluid added mass",
                  "Strouhal shedding frequency with Reynolds- and surface-condition-based selection (smooth / rough / straked / helical), plus current-profile sweeps",
                  "VIV amplitude (A/D) to bending stress range to Palmgren-Miner damage and fatigue life across 13 tabulated S-N curves",
                  "Config-driven tubular-member workflow emitting CSV result tables, runnable end-to-end from the example input.yml"]),
    dict(id="sec-field-development", kind="section",
         title="Field development — concept screening, cost & economics",
         std="GoM 10-field CAPEX benchmark · DCF NPV / IRR · Arps decline",
         path="capabilities/#field-development",
         blurb="The oil-and-gas concept-select twin of the floating-wind LCOE work: screen an "
               "offshore prospect across four decision axes (schedule, rig-days, driver-level "
               "CAPEX and lifecycle intervention), rank host concepts (TLP / spar / semi / FPSO / "
               "subsea tieback), and run pre-tax discounted-cash-flow economics on the survivors. "
               "All parameters are generic Gulf-of-Mexico defaults from a reviewable YAML; every "
               "field is an INPUT — no reservoir, flow-assurance or structural simulation is "
               "solved. Rig demand is reported in days, never $/day.",
         figures=[("4", "decision axes"), ("10", "GoM benchmark fields"),
                  ("~$55/bbl", "reference break-even")],
         bullets=["Four-axis concept screen (schedule weeks, development rig-days, driver-level CAPEX, intervention index) over TLP / spar / semi / FPSO / subsea-tieback hosts",
                  "CAPEX ranges anchored to 10 GoM reference fields, scaled by a 0.6-power capacity law and a depth factor, with distance-banded subsea-tieback logic",
                  "Pre-tax DCF economics live: CAPEX selector and oil-price slider drive NPV across break-even, with hyperbolic / harmonic Arps decline and five fiscal regimes in the same engine",
                  "Field-development layout schematic generator plus a structured catalog of worked reference developments",
                  "Honesty contract: rule / parameter screening only — rate and well count are inputs, rig demand in days, never $/day"]),
    dict(id="sec-naval-architecture", kind="section",
         title="Naval architecture — stability, resistance & hull strength",
         std="Holtrop-Mennen · IMO IS Code 2008 · IACS UR S11 · DNV-OS-C101",
         path="capabilities/#naval-architecture",
         blurb="A ship-design suite beyond the rudder / manoeuvring tools: Holtrop-Mennen "
               "resistance and powering, IMO intact and damage stability with GZ-curve area "
               "criteria, IACS hull-girder longitudinal strength and class-rule scantlings, and "
               "floating-platform stability for FPSO, semi-sub, spar, TLP and barge hulls. Every "
               "method carries its standard in the code and is pinned to published worked examples "
               "in dedicated tests.",
         figures=[("3", "hull forms, live explorer"), ("6", "IMO intact-stability criteria"),
                  ("5", "floating-platform types")],
         bullets=["Calm-water resistance and effective power (Holtrop-Mennen) with frictional, wave-making, form and transom components, live over a 6–18 kn sweep",
                  "GZ righting-arm curves from cross-curves (GZ = KN − KG·sinφ), lost-buoyancy damage method, all six IMO intact-stability criteria — pinned to EN400 worked conditions",
                  "IACS UR S11 vertical wave bending moment, permissible bending stress 175/k, and the S11A partial-factor ultimate check",
                  "Prescriptive plate-thickness and lateral-pressure scantling checks to class rules",
                  "Seakeeping natural periods, simplified RAOs and spectral RMS response; intact / damaged platform stability with wind-heel and area criteria"]),
    dict(id="sec-geotechnical", kind="section",
         title="Geotechnical — pile, anchor & foundation capacity",
         std="API RP 2GEO · DNV-RP-E302 / E303 · DNV-RP-C212 · DNV-RP-F107",
         path="capabilities/#geotechnical",
         blurb="Standards-referenced calculators for the seabed side of an offshore mooring or "
               "foundation problem — the engines that size and check the anchors the "
               "mooring-resilience atlases place. Pile axial capacity, drag- and suction-anchor "
               "holding, mudmat bearing and sliding, plus liquefaction triggering and scour "
               "screening. Each function is a dataclass-returning engine with input validation "
               "and its governing standard stamped on the result.",
         figures=[("α / β", "clay / sand skin friction"), ("3", "seabed soil classes, live"),
                  ("4", "engine families")],
         bullets=["Driven-pile axial capacity per API RP 2GEO: α method in clay, β method in sand, end bearing, summed layer-by-layer",
                  "Drag-anchor holding from empirical efficiency factors (DNV-RP-E302, API RP 2SK) live across soft clay / stiff clay / sand; suction-caisson capacity (DNV-RP-E303) in the same engine",
                  "Mudmat bearing capacity (DNV-RP-C212 / Brinch Hansen) with shape, depth and effective-area corrections, plus base sliding",
                  "Seed-Idriss / NCEER per-layer liquefaction factor of safety with magnitude scaling",
                  "DNV-RP-F107 equilibrium scour for pipelines and monopiles and rock-armour thickness"]),
    dict(id="sec-production-engineering", kind="section",
         title="Production engineering — nodal analysis & well deliverability",
         std="Vogel · Fetkovich · Hagedorn-Brown · Brent-method operating point",
         path="capabilities/#production-engineering",
         blurb="A node-based well-performance engine: intersect an inflow (IPR) curve with a "
               "vertical-lift (VLP) pressure traverse to find the operating point, then bound it "
               "with a confidence band derived from the underlying test-quality score. The "
               "correlations are textbook and each is unit-tested against its published equation; "
               "the distinctive layer is physics-based data reconciliation — GIGO divergence and "
               "nonlinearity flags that catch a bad test record before it corrupts the nodal "
               "model. Distinct from the rod-pump dynacard diagnostics under Artificial lift.",
         figures=[("±5 / ±15 / ±30%", "confidence bands (G/A/R)"),
                  ("≥20%", "GIGO divergence flag")],
         bullets=["Operating point by solving IPR(q) = VLP(q) with Brent's method, bracketed from zero to absolute open flow",
                  "Vogel / Fetkovich / Klins-Clark inflow and Hagedorn-Brown / Beggs-Brill outflow correlations, each unit-tested against its published equation",
                  "Green / Amber / Red confidence band keyed to the test-quality score",
                  "ESP sizing from total dynamic head (net lift + Hazen-Williams tubing friction + discharge head) with stage-count, motor and operating-range pass/fail",
                  "Test-to-model reconciliation: QC score, nonlinearity detection (transient / slug / gas-lift instability / high watercut), GIGO divergence diagnosis and recommendations"]),
    dict(id="sec-drilling-engineering", kind="section",
         title="Drilling engineering — pore pressure, hydraulics & well control",
         std="Eaton · Bowers · Burkhardt · API RP 13D",
         path="capabilities/#drilling-engineering",
         blurb="Closed-form, license-free drilling-geomechanics and hydraulics engines: "
               "multi-method pore-pressure prediction with explicit P10/P50/P90 uncertainty and a "
               "communicated safe mud-weight window, swab/surge tripping limits, annular "
               "hydraulics and ECD, ROP models, and a real-time dysfunction / kick detector. "
               "Every method is a published closed form with unit tests pinned to hand calcs and "
               "standard limiting cases — distinct from the casing-design explorer under Well "
               "construction, which covers tubular strength.",
         figures=[("P10/P50/P90", "pore-pressure uncertainty"), ("8", "Bourgoyne-Young ROP parameters")],
         bullets=["Multi-method pore pressure live: Eaton and Bowers gradients diverging down an overpressured well, governing minimum mud weight and the kick/loss mud-weight window",
                  "Swab / surge from Burkhardt's clinging constant through Bingham annular friction, solved for the max trip speed that keeps BHP inside the window",
                  "Generalised annular hydraulics per API RP 13D (velocity, frictional loss, ECD, cuttings transport) for arbitrary pipe-in-hole geometry",
                  "Bourgoyne-Young 8-parameter and Warren power-law ROP models",
                  "Real-time dysfunction & kick detector over surface-parameter windows: stick-slip, washout, bit-balling, instability and kick with severity and mitigation"]),
    dict(id="sec-cathodic", kind="section",
         title="Cathodic protection",
         std="DNV-RP-B401 · DNV-RP-F103 · ISO 15589-1 / -2 · API RP 1632 · NACE SP0169",
         path="capabilities/#cathodic",
         blurb="Sacrificial-anode and impressed-current CP design across structure types — "
               "current demand with coating breakdown, anode mass / resistance / output and "
               "spacing, protected length, depletion & retrofit remaining life, survey "
               "interpretation (CIS / DCVG) and stray-current screens. Edition-aware DNV-RP-B401 "
               "(2017 / 2021) backed by doc-verified test vectors — 500+ tests across the module.",
         figures=[("18", "CP engine modules"), ("500+", "doc-verified tests"),
                  ("31.7 → 18.6 t", "bare vs premium coating, 25-yr jacket")],
         bullets=["Current demand with coating breakdown → anode mass → anode count on the McCoy / Dwight resistance formulas, edition-aware DNV-RP-B401",
                  "Structure-type demo set: jacket, pipeline, manifold, monopile and FPSO hull as durable registry workflows with pinned inputs and CI tests",
                  "Pipeline CP: DNV-RP-F103 protected length, ISO 15589-1 / -2 and bracelet anodes; onshore galvanic + ICCP rectifier and ground-bed sizing per API RP 1632",
                  "Depletion & retrofit remaining life, CIS / DCVG survey interpretation, AC / DC stray-current interference screens",
                  "Factory-applied coating selection (DNV-RP-F106) with per-system breakdown factors feeding the current demand"]),
]


# Capabilities whose output is produced by a deterministic registry workflow
# (docs/registry/workflows.yaml) — these get a real workflow-API call. Everything
# else is a published report surface, whose API is an HTTP GET of the report.
# Only ids verified to exist in docs/registry/workflows.yaml are cited here.
WORKFLOW_MAP: dict[str, str] = {
    "buckling-plate": "elastic-buckling",
    "buckling-panel": "elastic-buckling",
    "rao-comparison": "rao-tabulation",
    "unitbox-report": "rao-tabulation",
    "riser-validation": "dnv-os-f201-riser",
    "diffraction-deck-prep": "aqwa-diffraction-deck-prep",
    "dynacard-troubleshooting": "dynacard-diagnostics",
    "dynacard-field-health": "artificial-lift-field-health",
    "drilling-riser-operability": "drilling-riser-operability-query",
    "drilling-riser-operability-monitor": "drilling-riser-operability-query",
}
_API_INVOKE = "uv run python -m digitalmodel {input}"
_BOT = "https://t.me/the_deckhand_bot"

# A ready-to-send natural-language "starting prompt" per live work — paste it to
# @the_deckhand_bot (hermes routes NL to the workflow) or pass it as the body of
# a workflow-API run. Keyed by spec id; works without a prompt fall back to a
# generic ask built from the title.
PROMPTS: dict[str, str] = {
    "ffs-showcase": "Run an FFS remaining-strength assessment (API 579-1 / ASME B31G / DNV-RP-F101) on a corroded pipe and give me the inspector verdict.",
    "riser-joint-acceptance": "Run the drilling-riser joint FFS screen: Level-1 flaw-acceptance envelope, collapse-limited water depth and string-zone placement for a corroded joint.",
    "ffs-field-dashboard": "Given my metal-loss measurements, tell me whether to accept, re-rate, take more measurements, or escalate.",
    "buckling-plate": "Check ship plate buckling per DNV-RP-C201 for an 800x2400x14 mm panel under in-plane compression.",
    "buckling-panel": "Check stiffened-panel buckling (plate / column / tripping) per DNV-RP-C201 for my scantlings.",
    "rao-comparison": "Compare AQWA vs OrcaWave 6-DOF displacement RAOs for my vessel.",
    "unitbox-report": "Run the unit-box 3-way diffraction benchmark across WAMIT, AQWA and OrcaWave.",
    "unitbox-amplitude": "Show the RAO amplitude overlay for the unit-box 3-solver diffraction benchmark.",
    "unitbox-phase": "Show the RAO phase overlay for the unit-box 3-solver diffraction benchmark.",
    "unitbox-combined": "Show the combined amplitude-and-phase solver comparison for the unit-box benchmark.",
    "unitbox-heatmap": "Show the cross-solver correlation heatmap for the unit-box diffraction benchmark.",
    "ocimf": "Give me OCIMF MEG3/MEG4 wind and current coefficients for my heading and loading condition.",
    "passing-ship": "Run the Wang (1975) passing-ship interaction benchmark - sway force and yaw moment.",
    "drilling-riser-operability": "Screen my drilling-riser operability: governing utilisation across vessel offset and surface current per operating mode, against the API RP 16Q / API STD 2RD limits, and flag near-boundary points that need the exact analytical run.",
    "drilling-riser-operability-monitor": "Monitor my drilling-riser operability and integrity from a telemetry track: show the operability margin, the flex-joint response with the measured-tracking correction, a wellhead bending-moment indicator, and the drift-off time-to-limit if thrusters were lost now.",
    "riser-validation": "Run the riser global-analysis validation against the tier-2 reference cases (DNV-OS-F201 code check).",
    "riser-mesh": "Run a riser mesh-density convergence study and quantify the discretisation effect on response.",
    "subsea-xsection": "Generate a layer-by-layer offshore cable / umbilical / pipeline cross-section with envelope geometry.",
    "installation-bokalift": "Assess crane-lift installation suitability and splash-zone weather windows for BokaLift 2.",
    "rudder-explorer": "Check turning circle vs the IMO 5.L limit and the minimum steerage speed for my vessel.",
    "rudder-database": "Show the rudder-type database with area ratios, aspect ratios and lift coefficients.",
    "diffraction-deck-prep": "Prepare a solver-ready AQWA diffraction deck from my canonical vessel spec.",
    "vessel-suitability": "Rank the installation fleet for my lift (weight, reach, water depth) with confidence-weighted suitability.",
    "container-utilization": "Screen my offshore container (CCU) utilization per DNV 2.7-1 for rating, aspect ratio and sling angle.",
    "wind-sizing": "Screen floating-wind floater variants (semi-sub / spar / TLP / barge) for my site load cases.",
    "wind-economics": "Run the floating-wind LCOE / TOTEX tradespace with reliability and economies-of-scale levers.",
    "mooring-resilience": "Run the mooring resilience screen (intact / damaged / foundation / fatigue) for my spread-moored unit.",
    "dynacard-troubleshooting": "Diagnose this rod-pump dynamometer card - failure mode, confidence and what to do about it.",
    "dynacard-field-health": "Screen my field's rod-pump dynacards and rank the worst wells by health status.",
    "casing-design": "Check my production casing design (burst / collapse / tension / triaxial) per API 5C3 and give me the max frac surface pressure.",
    "galvanic-explorer": "Screen this dissimilar-metal couple for galvanic compatibility in a marine environment (MIL-STD-889 anodic index) and tell me if it needs protection.",
    "scale-si-explorer": "Compute produced-water mineral-scale saturation indices (Oddo-Tomson) over my T/P path and check formation-water vs seawater mixing for waterflood compatibility.",
}


def _prompt_for(spec: dict) -> str:
    return PROMPTS.get(spec["id"], f"Run the digitalmodel {spec['title'].lower()} and send me the report.")


def _report_url(spec: dict) -> str:
    p = spec["path"]
    return p if p.startswith("http") else f"{_SITE}/{p}"


def _api_envelope(spec: dict) -> dict:
    """A typed, self-contained workflow-API ResultEnvelope for one capability —
    honest about whether it is a registry workflow or a published report surface."""
    report_url = _report_url(spec)
    wf = WORKFLOW_MAP.get(spec["id"])
    env: dict = {
        "schema": "deckhand.workflow_api.ResultEnvelope/2",
        "status": "ok",
        "repo": "digitalmodel",
        "report_url": report_url,
        "provenance": {"basis": spec["std"], "deterministic": True,
                       "source": "validated digitalmodel solver"},
    }
    if wf:
        env["request"] = {
            "method": "POST", "path": "/api/run",
            "body": {"repo": "digitalmodel", "workflow": wf},
        }
        env["invocation"] = _API_INVOKE.format(input=f"examples/workflows/{wf}/input.yml")
        env["workflow"] = f"digitalmodel:{wf}"
        env["outputs"] = [{"kind": "report", "url": report_url}]
        env["note"] = ("Deterministic registry workflow (docs/registry/workflows.yaml). "
                       "POST to the Deckhand workflow-API, or run the invocation locally; "
                       "the embedded page is a representative live output of the workflow.")
    else:
        env["request"] = {"method": "GET", "url": report_url}
        env["surface"] = f"digitalmodel:{spec['id']}"
        env["outputs"] = [{"kind": "report", "url": report_url}]
        env["note"] = ("Published report surface — the deterministic output is served as a "
                       "self-contained page; GET the report_url to consume it.")
    # Natural-language starting prompt + how-to-run, so users can fire it via the
    # Deckhand bot (hermes routes NL → workflow) or the HTTP / CLI paths.
    prompt = _prompt_for(spec)
    env["prompt"] = prompt
    how: list[dict] = [
        {"via": "telegram", "bot": "@the_deckhand_bot", "deep_link": _BOT,
         "step": f"Open @the_deckhand_bot and send: {prompt}"}
    ]
    if wf:
        how.append({"via": "http", "step": "POST /api/run with a scoped bearer token",
                    "body": env["request"]["body"]})
        how.append({"via": "cli", "step": env["invocation"]})
    else:
        how.append({"via": "http", "step": f"GET {report_url}"})
    env["how_to_run"] = how
    return env


_TEMPLATE = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<style>
  @page{{size:A4;margin:0}}
  :root{{--navy:#0B3D91;--teal:#0f8a7e;--ink:#13233f;--muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc}}
  *{{box-sizing:border-box;margin:0;padding:0}}
  body{{font-family:Arial,Helvetica,sans-serif;color:var(--ink);background:#fff;
       width:210mm;min-height:297mm;padding:18mm 18mm 14mm}}
  .top{{display:flex;align-items:center;justify-content:space-between;
       border-bottom:2px solid var(--navy);padding-bottom:12px}}
  .top svg{{height:30px;width:auto}}
  .kind{{font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--teal);font-weight:700}}
  h1{{font-size:30px;color:var(--navy);margin:26px 0 4px;letter-spacing:-.4px;line-height:1.15}}
  .std{{color:var(--teal);font-weight:700;font-size:13px}}
  .blurb{{color:var(--ink);font-size:14.5px;line-height:1.55;margin:18px 0 4px;max-width:165mm}}
  .figs{{display:flex;gap:14px;margin:22px 0 6px}}
  .fig{{flex:1;background:var(--soft);border:1px solid var(--line);border-radius:12px;padding:14px 16px}}
  .fig .v{{font-size:25px;font-weight:800;color:var(--navy);letter-spacing:-.5px}}
  .fig .l{{font-size:11.5px;color:var(--muted);margin-top:3px}}
  h2{{font-size:13px;text-transform:uppercase;letter-spacing:1px;color:var(--muted);
     margin:26px 0 10px;border-left:4px solid var(--teal);padding-left:10px}}
  ul{{list-style:none}}
  li{{font-size:14px;line-height:1.5;padding:6px 0 6px 24px;position:relative;border-bottom:1px solid var(--line)}}
  li:before{{content:"";position:absolute;left:4px;top:13px;width:8px;height:8px;border-radius:50%;
            background:var(--teal)}}
  .foot{{position:absolute;left:18mm;right:18mm;bottom:14mm;border-top:1px solid var(--line);
        padding-top:10px;display:flex;justify-content:space-between;align-items:flex-end;
        font-size:11.5px;color:var(--muted)}}
  .foot .live{{color:var(--navy);font-weight:700}}
  .cta{{background:linear-gradient(135deg,#0f8a7e,#0B3D91);color:#fff;font-weight:700;
       font-size:12px;padding:8px 14px;border-radius:9px;display:inline-block}}
</style></head>
<body>
  <div class="top">{logo}<div class="kind">Capability one-pager</div></div>
  <div class="std">{std}</div>
  <h1>{title}</h1>
  <p class="blurb">{blurb}</p>
  {figs}
  <h2>What you get</h2>
  <ul>{bullets}</ul>
  {ask}
  <div class="foot">
    <div>digitalmodel · validated engineering solvers · governing-code provenance<br>
      <span class="live">Explore live: {live}</span></div>
    <span class="cta">View interactive &rarr;</span>
  </div>
</body></html>"""


_API_TEMPLATE = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title} — API call</title>
<style>
  :root{{--navy:#0B3D91;--teal:#0f8a7e;--ink:#13233f;--muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc}}
  *{{box-sizing:border-box;margin:0;padding:0}}
  body{{font-family:Arial,Helvetica,sans-serif;color:var(--ink);background:#eef3fa;line-height:1.5}}
  .wrap{{max-width:1000px;margin:0 auto;padding:22px}}
  .top{{display:flex;align-items:center;justify-content:space-between;border-bottom:2px solid var(--navy);padding-bottom:12px}}
  .top svg{{height:28px}} .kind{{font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--teal);font-weight:700}}
  h1{{font-size:23px;color:var(--navy);margin:18px 0 2px}}
  .std{{color:var(--teal);font-weight:700;font-size:12.5px}}
  .row{{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-top:16px}}
  .panel{{background:#fff;border:1px solid var(--line);border-radius:12px;padding:14px 16px}}
  .panel h2{{font-size:11.5px;text-transform:uppercase;letter-spacing:1px;color:var(--muted);margin-bottom:8px}}
  pre{{background:#0e1726;color:#d6e2f5;border-radius:9px;padding:12px;font:12px/1.5 ui-monospace,Menlo,monospace;overflow:auto;white-space:pre-wrap;word-break:break-word}}
  .verb{{display:inline-block;font:700 11px ui-monospace,monospace;background:var(--teal);color:#fff;padding:2px 8px;border-radius:6px;margin-bottom:8px}}
  a.btn{{display:inline-block;margin-top:10px;font-size:12.5px;font-weight:700;color:#fff;
        background:linear-gradient(135deg,#0f8a7e,#0B3D91);padding:8px 14px;border-radius:9px;text-decoration:none}}
  .reportwrap{{margin-top:16px;background:#fff;border:1px solid var(--line);border-radius:12px;overflow:hidden}}
  .reportwrap .bar{{font-size:12px;color:var(--muted);padding:9px 14px;border-bottom:1px solid var(--line);display:flex;justify-content:space-between}}
  iframe{{width:100%;height:560px;border:0;display:block;background:#fff}}
  .note{{color:var(--muted);font-size:12.5px;margin-top:6px}}
  a{{color:var(--navy)}}
  .bigpanel{{background:#fff;border:1px solid var(--line);border-radius:12px;padding:14px 16px;margin-top:16px}}
  .bigpanel h2{{font-size:11.5px;text-transform:uppercase;letter-spacing:1px;color:var(--muted);margin-bottom:8px}}
  .prompt{{display:flex;gap:10px;align-items:flex-start}}
  .prompt .q{{flex:1;background:var(--soft);border:1px solid var(--line);border-radius:9px;padding:11px 13px;font-size:14px;color:var(--ink)}}
  .copy{{font:700 12px Arial;color:#fff;background:linear-gradient(135deg,#0f8a7e,#0B3D91);border:0;border-radius:9px;padding:10px 13px;cursor:pointer;white-space:nowrap}}
  .how ol{{margin:8px 0 0 18px}} .how li{{font-size:13.5px;margin:7px 0;color:var(--ink)}}
  .how code{{background:#0e1726;color:#d6e2f5;padding:1px 6px;border-radius:5px;font-size:12px}}
</style></head>
<body><div class="wrap">
  <div class="top">{logo}<div class="kind">Workflow-API · self-contained call</div></div>
  <h1>{title}</h1><div class="std">{std}</div>

  <div class="bigpanel">
    <h2>Starting prompt — fire it at Deckhand</h2>
    <div class="prompt">
      <div class="q" id="prompt">{prompt}</div>
      <button class="copy" onclick="navigator.clipboard&amp;&amp;navigator.clipboard.writeText(document.getElementById('prompt').innerText)">Copy</button>
      <a class="copy" href="{bot}" style="text-decoration:none">Run on Deckhand &rarr;</a>
    </div>
    <div class="how"><h2 style="margin-top:14px">How to run the API</h2><ol>{howsteps}</ol></div>
  </div>

  <div class="row">
    <div class="panel"><h2>Request — input</h2><span class="verb">{verb}</span>{reqsnippet}</div>
    <div class="panel"><h2>Response — ResultEnvelope</h2><pre>{envelope}</pre>
      <a class="btn" href="{id}.json" download>Download envelope JSON &darr;</a></div>
  </div>
  <p class="note">{note}</p>
  <div class="reportwrap">
    <div class="bar"><span>Comprehensive report (live output)</span><a href="{report_url}">open full report &rarr;</a></div>
    <iframe src="{report_url}" loading="lazy" title="{title} report"></iframe>
  </div>
</div></body></html>"""


def _render_api_html(spec: dict, env: dict) -> str:
    report_url = env["report_url"]
    if env["request"]["method"] == "POST":
        verb = "POST /api/run"
        reqsnippet = (
            f"<pre>curl -X POST https://api.deckhand/run \\\n"
            f"  -H 'content-type: application/json' \\\n"
            f"  -d '{html.escape(_json.dumps(env['request']['body']))}'\n\n"
            f"# or run locally (deterministic):\n# {html.escape(env['invocation'])}</pre>"
        )
    else:
        verb = "GET"
        reqsnippet = f"<pre>curl -L {html.escape(report_url)}</pre>"
    howsteps = ""
    for step in env["how_to_run"]:
        label = {"telegram": "Telegram", "http": "HTTP", "cli": "CLI"}.get(step["via"], step["via"])
        howsteps += f"<li><b>{label}.</b> {html.escape(step['step'])}</li>"
    return _API_TEMPLATE.format(
        logo=_LOGO, title=html.escape(spec["title"]), std=html.escape(spec["std"]),
        verb=verb, reqsnippet=reqsnippet,
        envelope=html.escape(_json.dumps(env, indent=2)),
        id=spec["id"], note=html.escape(env["note"]), report_url=html.escape(report_url),
        prompt=html.escape(env["prompt"]), bot=_BOT, howsteps=howsteps,
    )


def _render_html(spec: dict) -> str:
    figs = ""
    if spec["figures"]:
        cells = "".join(
            f'<div class="fig"><div class="v">{html.escape(value)}</div>'
            f'<div class="l">{html.escape(label)}</div></div>'
            for value, label in spec["figures"]
        )
        figs = f'<div class="figs">{cells}</div>'
    bullets = "".join(f"<li>{html.escape(b)}</li>" for b in spec["bullets"])
    live = _report_url(spec)
    ask = ""
    if spec["kind"] == "work":
        ask = ('<div style="margin-top:18px;padding:11px 14px;background:var(--soft);'
               'border:1px solid var(--line);border-radius:10px;font-size:12.5px">'
               f'<b>Ask Deckhand:</b> &ldquo;{html.escape(_prompt_for(spec))}&rdquo; '
               '&mdash; send to @the_deckhand_bot to run it live.</div>')
    return _TEMPLATE.format(
        logo=_LOGO, std=html.escape(spec["std"]), title=html.escape(spec["title"]),
        blurb=html.escape(spec["blurb"]), figs=figs, bullets=bullets, live=html.escape(live),
        ask=ask,
    )


def _to_pdf(html_text: str, out_pdf: Path) -> None:
    with tempfile.NamedTemporaryFile("w", suffix=".html", delete=False, encoding="utf-8") as f:
        f.write(html_text)
        src = f.name
    try:
        subprocess.run(
            [_CHROME, "--headless", "--no-sandbox", "--disable-gpu",
             "--no-pdf-header-footer", f"--print-to-pdf={out_pdf}", src],
            check=True, capture_output=True, timeout=90,
        )
    finally:
        os.unlink(src)


def main() -> None:
    """Build all one-pagers, or only the spec ids passed as CLI args
    (keeps the committed-PDF diff scoped when adding one capability)."""
    import sys

    only = set(sys.argv[1:])
    unknown = only - {spec["id"] for spec in SPECS}
    if unknown:
        raise SystemExit(f"Unknown spec id(s): {sorted(unknown)}")
    if not _CHROME:
        raise SystemExit("No Chrome found; set CHROME=/path/to/google-chrome")
    _OUT.mkdir(parents=True, exist_ok=True)
    _API.mkdir(parents=True, exist_ok=True)
    ids: set[str] = set()
    n_pdf = 0
    n_api = 0
    for spec in SPECS:
        assert spec["id"] not in ids, f"duplicate id {spec['id']}"
        ids.add(spec["id"])
        if only and spec["id"] not in only:
            continue
        _to_pdf(_render_html(spec), _OUT / f"{spec['id']}.pdf")
        n_pdf += 1
        # API artifacts: one self-contained workflow-API call per live work.
        if spec["kind"] == "work":
            env = _api_envelope(spec)
            (_API / f"{spec['id']}.json").write_text(
                _json.dumps(env, indent=2) + "\n", encoding="utf-8")
            (_API / f"{spec['id']}.html").write_text(
                _render_api_html(spec, env), encoding="utf-8")
            n_api += 1
    print(f"Wrote {n_pdf} one-pager PDFs into {_OUT.relative_to(_REPO)}/ "
          f"and {n_api} API artifacts (.html+.json) into {_API.relative_to(_REPO)}/")


if __name__ == "__main__":
    main()
