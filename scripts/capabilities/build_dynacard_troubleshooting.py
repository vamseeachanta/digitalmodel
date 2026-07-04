# ABOUTME: Build the dynacard troubleshooting explorer (capabilities page).
# ABOUTME: Use cases + 336-card example library browser + POC settings & alarms catalog.
"""Build the **dynacard troubleshooting explorer** — a self-contained,
light-themed page published on the capabilities page under "Artificial lift":

1. Seven curated troubleshooting use cases (card signature -> ML diagnosis ->
   field response), generated deterministically from the module's synthetic
   card library and verified against the shipped classifier.
2. An **example-card library browser**: every card in the module's
   example-card library (synthetic-verified variants, measured field wells,
   digitized field-archive shapes) behind phenomenon + card dropdowns, with
   the troubleshooting guide entry for the selected phenomenon.
3. A **controller settings & alarms** section: the field-practice setpoint
   rules from ``dynacard.poc_settings`` with recommended setpoints computed
   live for the measured wells and the training-deck worked example.

Outputs (committed frozen artifacts; served as static files under docs/api):

    docs/api/artificial-lift/dynacard-troubleshooting.html   # explorer page
    docs/api/artificial-lift/dynacard-troubleshooting.json   # frozen case data

The JSON is the drift guard: tests/marine_ops/artificial_lift/dynacard/
test_troubleshooting_usecases.py re-runs every case against the live
classifier and fails if a published diagnosis no longer matches.

Run:
    uv run python scripts/capabilities/build_dynacard_troubleshooting.py
"""

from __future__ import annotations

import html as html_mod
import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    run_p1_calculations,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.example_cards import (
    load_example_cards,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.poc_settings import (
    AlarmSetpoints,
    automatic_idle_time,
    estimate_production_bpd,
    recommend_setpoints,
    separator_capacity_bpd,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.troubleshooting import (
    TROUBLESHOOTING_GUIDE,
)
from digitalmodel.marine_ops.artificial_lift.field_health import (
    CRITICAL_CLASSIFICATIONS,
    FAILURE_CLASSIFICATIONS,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "artificial-lift"
_SEED = 711  # same seed as examples/workflows/dynacard-diagnostics/input.yml

# Well configuration mirrors the registered dynacard-diagnostics workflow input.
_WELL = dict(
    rod=dict(diameter=1.0, length=5000.0),
    pump=dict(diameter=1.75, depth=5000.0),
    surface_unit=dict(
        manufacturer="Sim",
        unit_type="Test",
        stroke_length=144.0,
        gear_box_rating=640000.0,
    ),
    spm=10.0,
)

# The curated troubleshooting use cases. Symptom = what the operator sees at
# the wellsite; actions = standard rod-pump dynamometer-analysis responses.
# Every mode here must classify back to itself (the drift test enforces it).
USE_CASES: list[dict] = [
    dict(
        mode="NORMAL",
        title="Healthy baseline",
        symptom=(
            "Full, smooth parallelogram-shaped card. Steady production, loads "
            "well inside rod and gearbox ratings. This is the reference shape "
            "every other case is compared against."
        ),
        actions=[
            "No intervention — record the card as the well's healthy reference",
            "Trend fillage and peak/minimum load for early drift detection",
        ],
    ),
    dict(
        mode="GAS_INTERFERENCE",
        title="Gas interference",
        symptom=(
            "Rounded lower-right corner: the traveling valve opens late on the "
            "downstroke because free gas in the barrel must compress before the "
            "load can transfer. Production is down while the pump card looks "
            "“spongy”."
        ),
        actions=[
            "Lower the pump intake below the perforations if the completion allows",
            "Install or service a downhole gas separator (gas anchor)",
            "Reduce SPM to give the annulus time to separate gas",
        ],
    ),
    dict(
        mode="FLUID_POUND",
        title="Fluid pound (pumped-off well)",
        symptom=(
            "Abrupt load drop partway down the downstroke: the plunger strikes "
            "the fluid level in a partially filled barrel. Audible pounding, "
            "shock loading on rods, tubing and gearbox."
        ),
        actions=[
            "Slow the unit or shorten the stroke to match pump displacement to inflow",
            "Install a pump-off controller or variable-speed drive",
            "Verify fluid level and inflow before resizing the pump",
        ],
    ),
    dict(
        mode="VALVE_LEAK_TV",
        title="Traveling valve leak (worn pump)",
        symptom=(
            "Load picks up slowly and decays on the upstroke: fluid slips back "
            "past the traveling valve or plunger fit, so the rods never carry "
            "the full fluid load. Gradual production decline."
        ),
        actions=[
            "Confirm with a standing-valve / traveling-valve dynamometer check",
            "Pull the pump; repair or replace the traveling valve, seat and plunger",
            "Review produced-solids and corrosion history before re-running",
        ],
    ),
    dict(
        mode="PUMP_TAGGING",
        title="Pump tagging (spacing fault)",
        symptom=(
            "Sharp load spike at the stroke end: the plunger strikes the "
            "standing valve or the top of the pump. Audible metallic tag; "
            "risk of valve, plunger and rod damage every stroke."
        ),
        actions=[
            "Stop and re-space the plunger off bottom",
            "Re-check spacing after the well reaches operating temperature",
            "Verify the surface stroke setting matches the pump design",
        ],
    ),
    dict(
        mode="ROD_PARTING",
        title="Parted rod string",
        symptom=(
            "Card collapses to a thin, low-load band with near-zero area: the "
            "pump no longer carries fluid load because the rod string is "
            "parted. Production stops; polished-rod load drops sharply."
        ),
        actions=[
            "Stop the unit immediately to avoid compounding downhole damage",
            "Fish the parted rod string and inspect the break face",
            "Review rod loading, corrosion program and service factor before re-running",
        ],
    ),
    dict(
        mode="GAS_LOCK",
        title="Gas-locked pump",
        symptom=(
            "Near-zero card area with a gas-cushion signature: gas trapped "
            "between the valves compresses and re-expands each stroke, so "
            "neither valve cycles and the pump stops delivering."
        ),
        actions=[
            "Re-space the pump to raise the compression ratio and break the lock",
            "Install a gas separator or relocate the intake below the perforations",
            "Consider a variable slippage or gas-lock-resistant pump design",
        ],
    ),
]

# Field-practice alarm/setpoint rule catalog rendered in the settings section.
RULES = [
    ("High load — alarm", "PPRL × 1.10–1.12",
     "Stuck pump on upstroke, paraffin/scale friction, flowline blockage, falling casing fluid level"),
    ("High load — shutdown", "PPRL × 1.20–1.25, capped at the unit structure rating",
     "Same causes, escalated before rods or structure are damaged"),
    ("Low load — alarm", "MPRL − 1,000…1,500 lb (deep) or MPRL × 0.9 (shallow)",
     "Rod float, developing rod part"),
    ("Low load — shutdown", "MPRL − 2,000…2,500 lb (deep) or MPRL × 0.8 (shallow)",
     "Parted rods, unseated pump — protect the string"),
    ("Load-span malfunction", "span < 0.6–0.75 × normal span, debounced over consecutive strokes",
     "Deep rod parts, dead pump valves, gas-locked pump, flumping (flowing) wells"),
    ("Card area — low", "area < 0.5 × reference card area",
     "Deep rod part, gas lock, inoperative pump (work-done proxy)"),
    ("Card area — high", "area > 1.5 × reference card area",
     "Paraffin / scale / emulsion friction — condition-based chemical-treatment trigger"),
    ("Pump-off strokes", "2–5 consecutive low-fillage strokes before idling; 10–15 for gassy wells",
     "Separates true pump-off from gas interference (gas: lower setpoint + more strokes)"),
    ("Fillage setpoint + VSD deadband", "setpoint ~90 % fillage, deadband ±5 % (2 % tight)",
     "Speed-by-fillage control without hunting; never operate at ~50 % fillage"),
    ("VSD speed limits", "min ≥ 4 SPM without low-speed gearbox lube; max from stroke × SPM < 1,440 in/min (~1,200 preferred)",
     "Gearbox lubrication at low speed; polished-rod velocity / failure frequency at high speed"),
    ("Automatic idle time", "next idle = target cycle − average run time (floor at min idle); halve cycle when run < 50 %",
     "Cycle-time stabilization — self-adjusts for outages, pump wear and inflow changes"),
    ("Runtime / cycle trending", "daily runtime and cycle-consistency KPIs vs baseline",
     "Rising = pump wear or tubing leak; falling = reduced inflow or restricted intake; erratic = slugs/thief zones"),
]


def _health_status(mode: str) -> str:
    if mode == "NORMAL":
        return "normal"
    if mode in FAILURE_CLASSIFICATIONS:
        return "failure"
    if mode in CRITICAL_CLASSIFICATIONS:
        return "critical"
    return "warning"


def _shoelace_area(position: list[float], load: list[float]) -> float:
    x = np.asarray(position)
    y = np.asarray(load)
    return float(abs(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1))) / 2.0)


def _build_context(mode: str, card) -> DynacardAnalysisContext:
    return DynacardAnalysisContext(
        api14=f"SIM-{mode}-{_SEED}",
        surface_card=card,
        rod_string=[RodSection(**_WELL["rod"])],
        pump=PumpProperties(**_WELL["pump"]),
        surface_unit=SurfaceUnit(**_WELL["surface_unit"]),
        spm=_WELL["spm"],
    )


def build_cases() -> list[dict]:
    """Run every use case through the classifier and P1 calculations.

    Published metrics are the load-based quantities that genuinely
    discriminate between these card signatures (fluid load, peak/min load,
    card area vs the healthy reference). The P1 corner-detection fillage is
    not a reliable discriminator on the synthetic library shapes, so it is
    deliberately not published.
    """
    diagnostics = PumpDiagnostics()
    healthy_area = _shoelace_area(
        ALL_GENERATORS["NORMAL"](seed=_SEED).position,
        ALL_GENERATORS["NORMAL"](seed=_SEED).load,
    )
    cases = []
    for spec in USE_CASES:
        mode = spec["mode"]
        card = ALL_GENERATORS[mode](seed=_SEED)
        ctx = _build_context(mode, card)
        results = AnalysisResults(ctx=ctx, downhole_card=card)
        diag = diagnostics.classify_with_context(results)
        p1 = run_p1_calculations(ctx, card)
        area = _shoelace_area(card.position, card.load)
        cases.append(
            dict(
                mode=mode,
                seed=_SEED,
                title=spec["title"],
                severity=_health_status(mode),
                symptom=spec["symptom"],
                actions=spec["actions"],
                classification=diag.classification,
                confidence=round(diag.confidence, 4),
                differential=diag.differential,
                model_version=diag.model_version,
                metrics=dict(
                    fluid_load_lbs=round(p1["fluid_load"].fluid_load, 0),
                    peak_load_lbs=round(max(card.load), 0),
                    min_load_lbs=round(min(card.load), 0),
                    card_area_inlb=round(area, 0),
                    area_vs_healthy_pct=round(area / healthy_area * 100.0, 1),
                ),
                card=dict(
                    position=[round(p, 2) for p in card.position],
                    load=[round(v, 1) for v in card.load],
                ),
            )
        )
    return cases


def library_payload() -> dict:
    """The example-card library + troubleshooting guide for the browser."""
    cards = [
        dict(
            id=c.id, phenomenon=c.phenomenon, source=c.source, units=c.units,
            render=c.render, position=c.position, load=c.load,
            meta={k: v for k, v in c.meta.items() if v is not None},
        )
        for c in load_example_cards()
    ]
    guide = {
        key: dict(
            title=e.title, symptom=e.symptom, mechanism=e.mechanism,
            actions=e.actions, severity=e.severity,
        )
        for key, e in TROUBLESHOOTING_GUIDE.items()
    }
    return dict(cards=cards, guide=guide)


def _sp_row(label: str, sp: AlarmSetpoints, extra: str = "") -> str:
    e = html_mod.escape
    cap = " (capped)" if sp.structure_rating_capped else ""
    return (
        f"<tr><td>{e(label)}</td>"
        f"<td class=\"num\">{sp.peak_load_reference_lbs:,.0f} / {sp.min_load_reference_lbs:,.0f}</td>"
        f"<td class=\"num\">{sp.high_load_alarm_lbs:,.0f}</td>"
        f"<td class=\"num\">{sp.high_load_shutdown_lbs:,.0f}{cap}</td>"
        f"<td class=\"num\">{sp.low_load_alarm_lbs:,.0f}</td>"
        f"<td class=\"num\">{sp.low_load_shutdown_lbs:,.0f}</td>"
        f"<td class=\"num\">{sp.load_span_malfunction_lbs:,.0f}</td>"
        f"<td class=\"num\">{'' if sp.vsd_max_spm is None else f'{sp.vsd_max_spm:g}'}</td>"
        f"<td>{e(extra)}</td></tr>"
    )


def settings_section_html() -> str:
    """Static settings & alarms section: rule catalog + computed setpoints."""
    e = html_mod.escape
    rules_rows = "".join(
        f"<tr><td><b>{e(n)}</b></td><td>{e(r)}</td><td>{e(c)}</td></tr>"
        for n, r, c in RULES
    )
    # Computed setpoints: training-deck worked example + measured wells +
    # the synthetic baseline well (single source of truth: poc_settings).
    rows = [_sp_row("Training-deck worked example", recommend_setpoints(20000.0, 10000.0))]
    for c in load_example_cards(source="field-measured"):
        sp = recommend_setpoints(
            max(c.load), min(c.load), stroke_length_in=c.meta.get("stroke_in"),
        )
        extra = f"{c.meta.get('spm')} SPM · {c.meta.get('stroke_in'):g}\" stroke"
        rows.append(_sp_row(f"{c.id} (measured)", sp, extra))
    normal = ALL_GENERATORS["NORMAL"](seed=_SEED)
    rows.append(_sp_row(
        "Synthetic baseline well",
        recommend_setpoints(
            max(normal.load), min(normal.load),
            structure_rating_lbs=None,
            stroke_length_in=_WELL["surface_unit"]["stroke_length"],
        ),
        f"{_WELL['spm']:g} SPM · {_WELL['surface_unit']['stroke_length']:g}\" stroke",
    ))
    ait = automatic_idle_time(target_cycle_min=20.0, average_run_min=15.0)
    ait_over = automatic_idle_time(target_cycle_min=20.0, average_run_min=8.0)
    prod = estimate_production_bpd(1.75, 10.0, 130.0)
    sep = separator_capacity_bpd(2.3)
    calcs = f"""
      <div class="grid" style="margin-top:14px">
        <div class="card"><h3>Automatic idle time</h3><div class="std">cycle-time stabilization</div>
          <p>Hold total cycle time constant: next idle = target cycle − average run time.
          20-min cycle with 15-min average run → <b>{ait['next_idle_min']:g} min idle</b>.
          Over-pumped well (8-min run) → cycle halves to {ait_over['target_cycle_min']:g} min,
          idle floors at {ait_over['next_idle_min']:g} min.</p></div>
        <div class="card"><h3>Displacement estimate</h3><div class="std">BFPD = 0.1166 · d² · SPM · SL · η</div>
          <p>1.75″ plunger, 10 SPM, 130″ downhole stroke, η 0.85 →
          <b>{prod:,.0f} BPD</b>. The 0.1166 constant reproduces the published
          pump-constant table (1-1/16″ → 0.132, 2-1/4″ → 0.590).</p></div>
        <div class="card"><h3>Gas-separator capacity</h3><div class="std">6 in/s bubble-rise limit</div>
          <p>Separation holds while quiet-zone downward velocity stays below the
          ~6 in/s rise velocity of a ¼″ bubble ≈ <b>50 BPD per in²</b> of annular
          area (2.3 in² → {sep:,.0f} BPD). Length adds no capacity — pump
          displacement above separator capacity ⇒ gas interference.</p></div>
        <div class="card"><h3>Rod-part screen</h3><div class="std">static load vs buoyant weight</div>
          <p>Rods are parted when static PPRL &lt; buoyant rod-string weight
          (steel ≈ 0.87 × air weight). Together with the low-load and
          card-area-low alarms this catches both shallow and deep parts.</p></div>
      </div>"""
    return f"""
  <section class="panel" style="margin-top:26px" id="settings">
    <h2>Controller settings &amp; alarms — recommended practice</h2>
    <p class="sub" style="margin:6px 0 12px">Field-practice rules encoded in
    <code>dynacard.poc_settings</code> (<code>recommend_setpoints</code> /
    <code>evaluate_alarms</code>), cross-checked between operator automation
    training and pump-off-controller vendor guidance. The setpoint table below
    is computed live by the same code that ships in the module.</p>
    <div class="tablewrap"><table>
      <thead><tr><th>Rule</th><th>Recipe</th><th>What it catches</th></tr></thead>
      <tbody>{rules_rows}</tbody>
    </table></div>
    <h2 style="margin-top:20px">Recommended setpoints — computed per well</h2>
    <div class="tablewrap"><table>
      <thead><tr><th>Well / example</th><th>PPRL / MPRL (lb)</th><th>High alarm</th>
      <th>High shutdown</th><th>Low alarm</th><th>Low shutdown</th>
      <th>Span malfunction</th><th>VSD max SPM</th><th>Context</th></tr></thead>
      <tbody>{''.join(rows)}</tbody>
    </table></div>
    {calcs}
  </section>"""


_PAGE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Dynacard troubleshooting explorer — digitalmodel</title>
<style>
  :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#ffffff;--ink:#13233f;
        --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;
        --series-card:#2563C4;--series-ref:#0B8F80;
        --st-normal:#1f9d57;--st-warning:#b7791f;--st-critical:#c2410c;--st-failure:#b91c1c}
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
       color:var(--ink);line-height:1.5;padding:26px 22px 60px}
  main{max-width:1160px;margin:0 auto}
  a{color:var(--navy)}
  h1{font-size:26px;color:var(--navy);letter-spacing:-.3px}
  .sub{color:var(--muted);font-size:14.5px;margin:8px 0 4px;max-width:880px}
  .crumb{font-size:12.5px;margin-bottom:14px}
  .tabs{display:flex;gap:8px;flex-wrap:wrap;margin:20px 0 16px}
  .tab{display:flex;align-items:center;gap:8px;font-size:13px;font-weight:700;color:var(--ink);
       background:var(--panel);border:1px solid var(--line);border-radius:10px;
       padding:8px 13px;cursor:pointer}
  .tab:hover{border-color:var(--teal)}
  .tab.active{color:#fff;background:linear-gradient(135deg,var(--teal),var(--navy));border-color:transparent}
  .dot{width:9px;height:9px;border-radius:50%;flex:none}
  .tab.active .dot{outline:2px solid rgba(255,255,255,.7)}
  .layout{display:grid;grid-template-columns:minmax(0,7fr) minmax(0,5fr);gap:18px}
  @media(max-width:900px){.layout{grid-template-columns:1fr}}
  .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;padding:16px 18px}
  .panel h2{font-size:12px;text-transform:uppercase;letter-spacing:1px;color:var(--muted);margin-bottom:10px}
  .chartwrap{position:relative}
  svg{display:block;width:100%;height:auto}
  .legend{display:flex;gap:18px;margin-top:8px;font-size:12.5px;color:var(--muted)}
  .legend .sw{display:inline-block;width:16px;height:3px;border-radius:2px;vertical-align:middle;margin-right:6px}
  .tooltip{position:absolute;pointer-events:none;background:#0e1726;color:#e8eef9;font-size:11.5px;
           padding:6px 9px;border-radius:7px;white-space:nowrap;display:none;z-index:5}
  .verdict{display:flex;align-items:center;gap:10px;flex-wrap:wrap}
  .chip{display:inline-flex;align-items:center;gap:7px;font-size:12.5px;font-weight:700;color:#fff;
        border-radius:9px;padding:5px 11px}
  .conf{font-size:12.5px;color:var(--muted)}
  .confbar{height:6px;background:var(--soft);border:1px solid var(--line);border-radius:4px;
           overflow:hidden;margin:8px 0 2px}
  .confbar span{display:block;height:100%;background:linear-gradient(90deg,var(--teal),var(--navy))}
  .diff{margin-top:10px;font-size:12.5px;color:var(--muted)}
  .diff b{color:var(--ink)}
  .sym{font-size:13.5px;color:var(--ink);margin-top:12px}
  .mech{font-size:12.5px;color:var(--muted);margin-top:8px;font-style:italic}
  .metrics{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin-top:14px}
  @media(max-width:520px){.metrics{grid-template-columns:repeat(2,1fr)}}
  .m{background:var(--soft);border:1px solid var(--line);border-radius:10px;padding:9px 11px}
  .m .v{font-size:17px;font-weight:800;color:var(--navy);font-variant-numeric:tabular-nums}
  .m .l{font-size:10.5px;color:var(--muted);margin-top:2px;text-transform:uppercase;letter-spacing:.4px}
  .actions{margin-top:14px}
  .actions li{font-size:13.5px;list-style:none;position:relative;padding:5px 0 5px 22px;
              border-bottom:1px solid var(--line)}
  .actions li:last-child{border-bottom:none}
  .actions li:before{content:"";position:absolute;left:3px;top:12px;width:8px;height:8px;
                     border-radius:50%;background:var(--teal)}
  .tablewrap{overflow-x:auto;margin-top:12px}
  table{width:100%;border-collapse:collapse;font-size:13px;background:var(--panel);
        border:1px solid var(--line);border-radius:12px;overflow:hidden}
  th,td{padding:8px 11px;text-align:left;border-bottom:1px solid var(--line)}
  th{background:var(--soft);color:var(--muted);font-weight:700;font-size:11px;
     text-transform:uppercase;letter-spacing:.4px;white-space:nowrap}
  tr:last-child td{border-bottom:none}
  td.num{font-variant-numeric:tabular-nums;text-align:right;white-space:nowrap}
  .stlabel{display:inline-flex;align-items:center;gap:6px;font-weight:700}
  .controls{display:flex;gap:10px;flex-wrap:wrap;align-items:center;margin:16px 0 14px}
  .controls select{font-size:13px;padding:8px 10px;border:1px solid var(--line);border-radius:9px;
                   background:#fff;color:var(--ink);max-width:340px}
  .controls button{font-size:13px;font-weight:700;padding:8px 12px;border:1px solid var(--line);
                   border-radius:9px;background:#fff;color:var(--navy);cursor:pointer}
  .controls button:hover{border-color:var(--teal)}
  .badge{display:inline-block;font-size:11px;font-weight:700;padding:3px 9px;border-radius:7px;
         border:1px solid var(--line);color:var(--muted);background:var(--soft)}
  .badge.synthetic-verified{color:#0B3D91;border-color:#b9cbe6}
  .badge.field-measured{color:#0f6a30;border-color:#bfe3cd}
  .badge.field-archive-digitized{color:#7c4a03;border-color:#eeddc0}
  .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(250px,1fr));gap:14px}
  .card{background:var(--soft);border:1px solid var(--line);border-radius:12px;padding:13px 15px}
  .card h3{font-size:14px;color:var(--ink)}
  .card .std{color:var(--teal);font-size:11.5px;font-weight:700;margin:3px 0 7px}
  .card p{color:var(--muted);font-size:12.5px}
  .prov{margin-top:26px;background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:15px 18px;font-size:12.5px;color:var(--muted)}
  .prov b{color:var(--ink)}
  code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;background:var(--soft);
       border:1px solid var(--line);padding:1px 5px;border-radius:5px;font-size:11.5px}
  section>h2:first-child{font-size:19px;color:var(--navy);border-left:4px solid var(--teal);
        padding-left:11px;text-transform:none;letter-spacing:0}
  .panel>h2:first-child{font-size:19px;color:var(--navy);border-left:4px solid var(--teal);
        padding-left:11px;text-transform:none;letter-spacing:0}
</style>
</head>
<body>
<main>
  <div class="crumb"><a href="../capabilities/">&larr; digitalmodel capabilities</a></div>
  <h1>Dynacard troubleshooting explorer</h1>
  <p class="sub">Rod-pump dynamometer-card diagnostics in three layers: seven curated troubleshooting
  cases (card signature &rarr; ML diagnosis &rarr; field response), an example-card library of
  __N_CARDS__ identified cards across __N_PHEN__ phenomena (synthetic-verified, measured field wells and a
  digitized field archive), and the controller settings &amp; alarms the module recommends from a
  well's own card.</p>

  <section class="panel" style="margin-top:22px">
  <h2>Troubleshooting use cases</h2>
  <div class="tabs" id="tabs"></div>
  <div class="layout">
    <div>
      <h2>Pump (downhole) card — load vs position</h2>
      <div class="chartwrap" id="chart"></div>
      <div class="legend">
        <span><span class="sw" style="background:var(--series-card)"></span>This case</span>
        <span><span class="sw" style="background:var(--series-ref)"></span>Healthy reference (NORMAL)</span>
      </div>
    </div>
    <div>
      <h2>Diagnosis &amp; recommended response</h2>
      <div id="verdict"></div>
    </div>
  </div>

  <div class="tablewrap">
    <table>
      <thead><tr><th>Case</th><th>Status</th><th>ML classification</th><th>Confidence</th>
        <th>Fluid load</th><th>Peak / min load</th><th>Card area</th><th>Area vs healthy</th></tr></thead>
      <tbody id="tbody"></tbody>
    </table>
  </div>
  </section>

  <section class="panel" style="margin-top:26px" id="library">
    <h2>Example-card library — __N_CARDS__ identified cards</h2>
    <p class="sub" style="margin:6px 0 0">Browse every example in the module's library
    (<code>dynacard.example_cards</code>). Synthetic variants are verified against the shipped
    classifier; measured wells are real dynamometer tests (anonymized); archive cards are
    digitized from a hand-labeled field training archive (shape only, normalized units).</p>
    <div class="controls">
      <label for="phen-sel" style="font-size:12.5px;color:var(--muted);font-weight:700">Phenomenon</label>
      <select id="phen-sel"></select>
      <label for="card-sel" style="font-size:12.5px;color:var(--muted);font-weight:700">Card</label>
      <select id="card-sel"></select>
      <button id="prev-btn" type="button">&larr; Prev</button>
      <button id="next-btn" type="button">Next &rarr;</button>
      <span id="lib-count" style="font-size:12.5px;color:var(--muted)"></span>
    </div>
    <div class="layout">
      <div>
        <div class="chartwrap" id="lib-chart"></div>
      </div>
      <div id="lib-info"></div>
    </div>
  </section>

__SETTINGS_SECTION__

  <div class="prov">
    <b>Provenance.</b> Cards, diagnoses, guide entries and setpoints are produced by
    <code>digitalmodel.marine_ops.artificial_lift.dynacard</code> (<code>example_cards</code>,
    <code>troubleshooting</code>, <code>poc_settings</code>) and frozen by
    <code>scripts/capabilities/build_dynacard_troubleshooting.py</code>; CI re-runs every published
    diagnosis against the live classifier. ML classification operates on the pump (downhole) card
    (GradientBoosting over 16 Bezerra projection features, 5,400 synthetic training cards, 89.4%
    five-fold CV on that synthetic set) and is only shown for oilfield-unit cards. Digitized archive
    cards are shape-only (normalized axes, marker positions) from a hand-labeled training archive;
    wells are anonymized throughout. Settings &amp; alarm recipes restate standard field practice in
    original wording, cross-checked between operator automation training (2016-2017), pump-off
    controller vendor guidance and Bommer &amp; Podio, <i>The Beam Lift Handbook</i>.
    Source: <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/marine_ops/artificial_lift/dynacard">dynacard module</a> ·
    data: <a href="dynacard-troubleshooting.json">frozen JSON</a>.
  </div>
</main>

<script id="case-data" type="application/json">__DATA__</script>
<script id="library-data" type="application/json">__LIBDATA__</script>
<script>
(function(){
  "use strict";
  var DATA = JSON.parse(document.getElementById("case-data").textContent);
  var LIB = JSON.parse(document.getElementById("library-data").textContent);
  var CASES = DATA.cases;
  var REF = null;
  CASES.forEach(function(c){ if(c.mode === "NORMAL") REF = c.card; });
  var ST = {normal:"var(--st-normal)", warning:"var(--st-warning)",
            critical:"var(--st-critical)", failure:"var(--st-failure)"};
  var STICON = {normal:"✓", warning:"⚠", critical:"⚠", failure:"✕"};
  var NS = "http://www.w3.org/2000/svg";

  function el(tag, attrs, parent){
    var e = document.createElementNS(NS, tag);
    for(var k in attrs) e.setAttribute(k, attrs[k]);
    if(parent) parent.appendChild(e);
    return e;
  }
  function fmt(n){ n = Number(n); if(Object.is(n, -0) || Math.abs(n) < 1e-9) n = 0;
    return n.toLocaleString("en-US"); }
  function esc(s){
    return String(s).replace(/[&<>"]/g, function(ch){
      return {"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[ch];
    });
  }
  function ticks(lo, hi, n){
    var span = hi - lo, step = Math.pow(10, Math.floor(Math.log10(span/n)));
    var err = span/(n*step);
    if(err >= 7.5) step *= 10; else if(err >= 3.5) step *= 5; else if(err >= 1.5) step *= 2;
    var out = [], v = Math.ceil(lo/step)*step;
    for(; v <= hi + 1e-9; v += step) out.push(v);
    return out;
  }

  // Generic card chart: opts = {ref: overlay card|null, units, render, loadLabel, posLabel}
  function drawCard(wrapId, card, opts){
    var wrap = document.getElementById(wrapId);
    wrap.innerHTML = "";
    var W = 640, H = 420, m = {t:14, r:16, b:44, l:74};
    var xs = card.position.slice(), ys = card.load.slice();
    if(opts.ref){ xs = xs.concat(opts.ref.position); ys = ys.concat(opts.ref.load); }
    var xlo = Math.min.apply(null, xs), xhi = Math.max.apply(null, xs);
    var ylo = Math.min.apply(null, ys), yhi = Math.max.apply(null, ys);
    var ypad = (yhi - ylo) * 0.07 || 1; ylo -= ypad; yhi += ypad;
    var xpad = (xhi - xlo) * 0.04 || 1; xlo -= xpad; xhi += xpad;
    function X(v){ return m.l + (v - xlo)/(xhi - xlo)*(W - m.l - m.r); }
    function Y(v){ return H - m.b - (v - ylo)/(yhi - ylo)*(H - m.t - m.b); }
    var svg = el("svg", {viewBox:"0 0 "+W+" "+H, role:"img",
      "aria-label":"Dynamometer card, load versus position"}, wrap);
    var norm = opts.units === "normalized";
    ticks(ylo, yhi, 6).forEach(function(v){
      el("line", {x1:m.l, x2:W-m.r, y1:Y(v), y2:Y(v), stroke:"#e5ecf6", "stroke-width":1}, svg);
      el("text", {x:m.l-8, y:Y(v)+4, "text-anchor":"end", "font-size":11, fill:"#5b6b86"}, svg)
        .textContent = norm ? v.toFixed(2) : fmt(v);
    });
    ticks(xlo, xhi, 7).forEach(function(v){
      el("text", {x:X(v), y:H-m.b+18, "text-anchor":"middle", "font-size":11, fill:"#5b6b86"}, svg)
        .textContent = norm ? v.toFixed(2) : fmt(v);
    });
    el("line", {x1:m.l, x2:W-m.r, y1:H-m.b, y2:H-m.b, stroke:"#c9d6e8", "stroke-width":1}, svg);
    el("text", {x:(m.l+W-m.r)/2, y:H-8, "text-anchor":"middle", "font-size":11.5, fill:"#5b6b86"}, svg)
      .textContent = opts.posLabel;
    var yl = el("text", {x:16, y:(m.t+H-m.b)/2, "font-size":11.5, fill:"#5b6b86",
      transform:"rotate(-90 16 "+((m.t+H-m.b)/2)+")", "text-anchor":"middle"}, svg);
    yl.textContent = opts.loadLabel;

    function loop(c){
      return c.position.map(function(p,i){
        return (i ? "L" : "M") + X(p).toFixed(1) + " " + Y(c.load[i]).toFixed(1);
      }).join("") + "Z";
    }
    if(opts.ref){
      el("path", {d:loop(opts.ref), fill:"none", stroke:"#0B8F80", "stroke-width":2,
                  "stroke-dasharray":"6 5", opacity:.85}, svg);
    }
    if(opts.render === "dots"){
      card.position.forEach(function(p, i){
        el("circle", {cx:X(p), cy:Y(card.load[i]), r:3.4, fill:"#2563C4", opacity:.9}, svg);
      });
    } else {
      el("path", {d:loop(card), fill:"rgba(37,99,196,.07)", stroke:"#2563C4", "stroke-width":2.25,
                  "stroke-linejoin":"round"}, svg);
    }

    var tip = document.createElement("div");
    tip.className = "tooltip"; wrap.appendChild(tip);
    var hoverDot = el("circle", {r:4.5, fill:"#2563C4", stroke:"#fff", "stroke-width":2,
                                 style:"display:none"}, svg);
    svg.addEventListener("mousemove", function(ev){
      var r = svg.getBoundingClientRect();
      var sx = (ev.clientX - r.left) * W / r.width, sy = (ev.clientY - r.top) * H / r.height;
      var best = -1, bd = 1e12;
      for(var i = 0; i < card.position.length; i++){
        var dx = X(card.position[i]) - sx, dy = Y(card.load[i]) - sy, d = dx*dx + dy*dy;
        if(d < bd){ bd = d; best = i; }
      }
      if(best < 0 || bd > 45*45){ tip.style.display = "none"; hoverDot.style.display = "none"; return; }
      var px = X(card.position[best]), py = Y(card.load[best]);
      hoverDot.setAttribute("cx", px); hoverDot.setAttribute("cy", py);
      hoverDot.style.display = "";
      var lv = card.load[best], pv = card.position[best];
      tip.innerHTML = norm ? "<b>" + lv.toFixed(3) + "</b> @ " + pv.toFixed(3)
                           : "<b>" + fmt(lv) + " lbs</b> @ " + fmt(pv) + " in";
      tip.style.display = "block";
      var wr = wrap.getBoundingClientRect();
      tip.style.left = Math.min(px/W*wr.width + 12, wr.width - 130) + "px";
      tip.style.top = (py/H*wr.height - 34) + "px";
    });
    svg.addEventListener("mouseleave", function(){
      tip.style.display = "none"; hoverDot.style.display = "none";
    });
  }

  // ---------- Section 1: curated use cases ----------
  function drawVerdict(idx){
    var c = CASES[idx];
    var diff = c.differential.slice(0, 3).map(function(d, i){
      return (i+1) + ". <b>" + esc(d.mode) + "</b> " + (d.probability*100).toFixed(1) + "%";
    }).join(" &nbsp;&middot;&nbsp; ");
    var acts = c.actions.map(function(a){ return "<li>" + esc(a) + "</li>"; }).join("");
    var mt = c.metrics;
    document.getElementById("verdict").innerHTML =
      '<div class="verdict"><span class="chip" style="background:' + ST[c.severity] + '">' +
        STICON[c.severity] + " " + esc(c.severity.toUpperCase()) + "</span>" +
        "<b>" + esc(c.classification) + "</b>" +
        '<span class="conf">' + (c.confidence*100).toFixed(1) + "% confidence</span></div>" +
      '<div class="confbar"><span style="width:' + (c.confidence*100).toFixed(1) + '%"></span></div>' +
      '<div class="diff">Differential: ' + diff + "</div>" +
      '<p class="sym">' + esc(c.symptom) + "</p>" +
      '<div class="metrics">' +
        '<div class="m"><div class="v">' + fmt(mt.fluid_load_lbs) + '</div><div class="l">Fluid load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.peak_load_lbs) + '</div><div class="l">Peak load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.min_load_lbs) + '</div><div class="l">Min load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.card_area_inlb) + '</div><div class="l">Card area (in·lb)</div></div>' +
        '<div class="m"><div class="v">' + mt.area_vs_healthy_pct.toFixed(1) + '%</div><div class="l">Area vs healthy</div></div>' +
      "</div>" +
      '<ul class="actions">' + acts + "</ul>";
  }

  function selectCase(idx){
    Array.prototype.forEach.call(document.querySelectorAll("#tabs .tab"), function(t, i){
      t.classList.toggle("active", i === idx);
    });
    var c = CASES[idx];
    drawCard("chart", c.card, {ref: c.mode === "NORMAL" ? null : REF, units:"oilfield",
      render:"line", posLabel:"Plunger position (in)", loadLabel:"Load (lbs)"});
    drawVerdict(idx);
  }

  var tabs = document.getElementById("tabs");
  CASES.forEach(function(c, i){
    var b = document.createElement("button");
    b.className = "tab"; b.type = "button";
    b.innerHTML = '<span class="dot" style="background:' + ST[c.severity] + '"></span>' + esc(c.title);
    b.addEventListener("click", function(){ selectCase(i); });
    tabs.appendChild(b);
  });

  var tbody = document.getElementById("tbody");
  CASES.forEach(function(c, i){
    var tr = document.createElement("tr");
    tr.innerHTML =
      "<td><a href=\\"#\\" data-i=\\"" + i + "\\">" + esc(c.title) + "</a></td>" +
      '<td><span class="stlabel" style="color:' + ST[c.severity] + '">' + STICON[c.severity] + " " +
        esc(c.severity) + "</span></td>" +
      "<td>" + esc(c.classification) + "</td>" +
      '<td class="num">' + (c.confidence*100).toFixed(1) + "%</td>" +
      '<td class="num">' + fmt(c.metrics.fluid_load_lbs) + " lbs</td>" +
      '<td class="num">' + fmt(c.metrics.peak_load_lbs) + " / " + fmt(c.metrics.min_load_lbs) + "</td>" +
      '<td class="num">' + fmt(c.metrics.card_area_inlb) + " in·lb</td>" +
      '<td class="num">' + c.metrics.area_vs_healthy_pct.toFixed(1) + "%</td>";
    tr.querySelector("a").addEventListener("click", function(ev){
      ev.preventDefault(); selectCase(i); window.scrollTo({top:0, behavior:"smooth"});
    });
    tbody.appendChild(tr);
  });

  selectCase(1); // open on gas interference — the most common field case

  // ---------- Section 2: example-card library browser ----------
  var byPhen = {};
  LIB.cards.forEach(function(c){ (byPhen[c.phenomenon] = byPhen[c.phenomenon] || []).push(c); });
  var PHENS = Object.keys(byPhen).sort();
  var SRCLABEL = {"synthetic-verified":"synthetic · verified",
                  "field-measured":"field well · measured",
                  "field-archive-digitized":"field archive · digitized"};
  var phenSel = document.getElementById("phen-sel");
  var cardSel = document.getElementById("card-sel");

  PHENS.forEach(function(p){
    var o = document.createElement("option");
    o.value = p; o.textContent = p + " (" + byPhen[p].length + ")";
    phenSel.appendChild(o);
  });

  function fillCards(phen){
    cardSel.innerHTML = "";
    byPhen[phen].forEach(function(c, i){
      var o = document.createElement("option");
      o.value = String(i);
      o.textContent = c.id + " — " + (SRCLABEL[c.source] || c.source);
      cardSel.appendChild(o);
    });
  }

  function showLibCard(){
    var phen = phenSel.value;
    var i = parseInt(cardSel.value || "0", 10);
    var cards = byPhen[phen];
    if(!cards || !cards.length) return;
    var c = cards[Math.max(0, Math.min(i, cards.length - 1))];
    var norm = c.units === "normalized";
    var surface = c.meta && c.meta.card_type === "surface";
    drawCard("lib-chart", c, {ref:null, units:c.units, render:c.render,
      posLabel: norm ? "Position (normalized)" :
        (surface ? "Polished-rod position (in)" : "Plunger position (in)"),
      loadLabel: norm ? "Load (normalized)" : "Load (lbs)"});
    var g = LIB.guide[c.phenomenon];
    var htmlOut = "";
    if(g){
      var acts = g.actions.map(function(a){ return "<li>" + esc(a) + "</li>"; }).join("");
      htmlOut +=
        '<div class="verdict"><span class="chip" style="background:' + (ST[g.severity]||ST.warning) + '">' +
          (STICON[g.severity]||"⚠") + " " + esc(g.severity.toUpperCase()) + "</span>" +
          "<b>" + esc(g.title) + "</b></div>" +
        '<p class="sym">' + esc(g.symptom) + "</p>" +
        '<p class="mech">' + esc(g.mechanism) + "</p>" +
        '<ul class="actions">' + acts + "</ul>";
    }
    var facts = ['<span class="badge ' + esc(c.source) + '">' + esc(SRCLABEL[c.source]||c.source) + "</span>",
                 '<span class="badge">' + esc(c.units) + " units</span>",
                 '<span class="badge">' + c.position.length + " points</span>"];
    if(c.meta && c.meta.spm) facts.push('<span class="badge">' + c.meta.spm + " SPM</span>");
    if(c.meta && c.meta.stroke_in) facts.push('<span class="badge">' + c.meta.stroke_in + '″ stroke</span>');
    if(c.meta && c.meta.seed !== undefined) facts.push('<span class="badge">seed ' + c.meta.seed + "</span>");
    htmlOut += '<div style="margin-top:12px;display:flex;gap:7px;flex-wrap:wrap">' + facts.join("") + "</div>";
    if(!norm){
      htmlOut += '<div class="metrics" style="margin-top:12px">' +
        '<div class="m"><div class="v">' + fmt(Math.max.apply(null, c.load)) + '</div><div class="l">Peak load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(Math.min.apply(null, c.load)) + '</div><div class="l">Min load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(Math.round(Math.max.apply(null, c.position))) + '</div><div class="l">Stroke (in)</div></div>' +
        "</div>";
    }
    document.getElementById("lib-info").innerHTML = htmlOut;
    document.getElementById("lib-count").textContent =
      "card " + (parseInt(cardSel.value,10)+1) + " of " + cards.length + " in " + phen;
  }

  phenSel.addEventListener("change", function(){ fillCards(phenSel.value); cardSel.value = "0"; showLibCard(); });
  cardSel.addEventListener("change", showLibCard);
  document.getElementById("prev-btn").addEventListener("click", function(){
    var i = parseInt(cardSel.value, 10);
    if(i > 0){ cardSel.value = String(i - 1); showLibCard(); }
  });
  document.getElementById("next-btn").addEventListener("click", function(){
    var i = parseInt(cardSel.value, 10), n = byPhen[phenSel.value].length;
    if(i < n - 1){ cardSel.value = String(i + 1); showLibCard(); }
  });

  phenSel.value = PHENS.indexOf("BUTTERFLY") >= 0 ? "BUTTERFLY" : PHENS[0];
  fillCards(phenSel.value);
  cardSel.value = "0";
  showLibCard();
})();
</script>
</body>
</html>
"""


def render_page(cases: list[dict], lib: dict, settings_html: str) -> str:
    def _embed(obj) -> str:
        return json.dumps(obj, separators=(",", ":")).replace("</", "<\\/")

    n_phen = len({c["phenomenon"] for c in lib["cards"]})
    return (
        _PAGE
        .replace("__N_CARDS__", str(len(lib["cards"])))
        .replace("__N_PHEN__", str(n_phen))
        .replace("__SETTINGS_SECTION__", settings_html)
        .replace("__DATA__", _embed({"cases": cases}))
        .replace("__LIBDATA__", _embed(lib))
    )


def main() -> None:
    cases = build_cases()
    mismatched = [
        (c["mode"], c["classification"]) for c in cases if c["classification"] != c["mode"]
    ]
    if mismatched:
        raise SystemExit(
            f"Refusing to publish: use-case diagnosis drifted from its mode: {mismatched}"
        )
    lib = library_payload()
    settings_html = settings_section_html()
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    slim = [
        {k: v for k, v in c.items() if k != "card"} | {"n_points": len(c["card"]["position"])}
        for c in cases
    ]
    (_OUT_DIR / "dynacard-troubleshooting.json").write_text(
        json.dumps(
            {
                "seed": _SEED,
                "well": _WELL,
                "cases": slim,
                "library_summary": {
                    "n_cards": len(lib["cards"]),
                    "phenomena": sorted({c["phenomenon"] for c in lib["cards"]}),
                },
            },
            indent=2,
        ) + "\n",
        encoding="utf-8",
    )
    page = render_page(cases, lib, settings_html)
    (_OUT_DIR / "dynacard-troubleshooting.html").write_text(page, encoding="utf-8")
    print(
        f"Wrote {len(cases)} use cases + {len(lib['cards'])} library cards -> "
        f"{(_OUT_DIR / 'dynacard-troubleshooting.html').relative_to(_REPO)} "
        f"({(_OUT_DIR / 'dynacard-troubleshooting.html').stat().st_size / 1024:.0f} KB)"
    )
    for c in cases:
        print(
            f"  {c['mode']:18s} -> {c['classification']:18s} "
            f"conf {c['confidence']:.2f}  fluid load {c['metrics']['fluid_load_lbs']:7.0f} lbs  "
            f"area vs healthy {c['metrics']['area_vs_healthy_pct']:5.1f}%"
        )


if __name__ == "__main__":
    main()
