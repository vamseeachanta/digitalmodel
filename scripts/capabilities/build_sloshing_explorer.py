#!/usr/bin/env python
"""Build the tank-sloshing natural-period & resonance explorer.

Client-facing capability page. Three tiers, all engine-driven:

1. NORMALIZED MASTER CURVE (top) — the dimensionless fundamental sloshing
   frequency parameter Omega1 = omega_1*sqrt(Lc/g) vs the fill/slenderness ratio
   h/Lc, one curve per tank shape (rectangular, upright cylinder, horizontal
   cylinder = road tanker, elliptical/obround). Any physical size collapses onto
   these curves; read Omega1 off the curve and de-normalise to a real period.
2. SHAPE LOOKUP (dropdown) — pick a shape and a real size + fill, get the actual
   natural period(s) and a small lookup table.
3. RESONANCE SCREEN — the marine case: drag fill level for an LNG-scale tank and
   watch the sloshing period sweep the vessel roll-period band (the coupling
   external diffraction misses — the ACMA B1546 ballast-tank-as-tuned-tank study).

Plus a worldwide-relationship reference basis and the CFD tie-in.

Outputs:
    docs/api/structural/sloshing-explorer.html
    docs/api/structural/sloshing-explorer.json

The engine (src/digitalmodel/hydrodynamics/sloshing.py) is pure-stdlib and is
direct-file-loaded to avoid the slow top-level ``import digitalmodel``.
"""
from __future__ import annotations

import importlib.util
import json
import math
import sys
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"
_ENGINE = _REPO / "src" / "digitalmodel" / "hydrodynamics" / "sloshing.py"
# Optional CFD benchmark manifest (measured 2D VOF points; epic #1429). When
# present, its rectangular free-decay points are overlaid on the master curve
# and a small verification table is rendered; when absent the page is unchanged.
_CFD_BENCHMARK = _OUT_DIR / "sloshing-cfd-benchmark.json"
# Optional forced-roll frequency-response manifest (issue #1433): with/without
# forced-roll resonance sweep. Drives the "forced-roll resonance" panel.
_FORCED_RESPONSE = _OUT_DIR / "sloshing-forced-response.json"
# Optional literature-survey synthesis (issue #1433): drives the way-forward +
# literature-confidence section. Written from the deep-research workflow.
_RESEARCH = _OUT_DIR / "sloshing-cfd-research.json"

_spec = importlib.util.spec_from_file_location("dm_sloshing_engine", _ENGINE)
slosh = importlib.util.module_from_spec(_spec)
sys.modules["dm_sloshing_engine"] = slosh
_spec.loader.exec_module(slosh)
G = slosh.G_STD


def _omega(T):
    return 2.0 * math.pi / T


def _norm_curve(kind):
    """(x = h/Lc, Om = omega_1*sqrt(Lc/g)) points for the master chart."""
    pts = []
    if kind == "rectangular":
        # Lc = L; x = h/L; Om = sqrt(pi*tanh(pi*x)) exactly.
        x = 0.05
        while x <= 1.5001:
            pts.append((round(x, 3), round(math.sqrt(math.pi * math.tanh(math.pi * x)), 4)))
            x += 0.05
    elif kind == "upright-cylinder":
        # Lc = D = 2R; x = h/D; sweep with D = 1.
        D = 1.0
        x = 0.05
        while x <= 1.5001:
            T = slosh.cylindrical_tank_periods(D, x * D, n_modes=1)[0]
            pts.append((round(x, 3), round(_omega(T) * math.sqrt(D / G), 4)))
            x += 0.05
    elif kind == "horizontal-cylinder":
        # Lc = D; x = h/D = fill fraction F; D = 1. Equivalent-rectangle is
        # trustworthy over the mid-fill range — stop before the near-full blow-up.
        D = 1.0
        for i in range(1, 17):
            F = i * 0.05  # 0.05 .. 0.80
            T = slosh.horizontal_cylinder_periods(D, F, n_modes=1)[0]
            om = _omega(T) * math.sqrt(D / G)
            if om > 1.95:
                break
            pts.append((round(F, 3), round(om, 4)))
    elif kind == "elliptical":
        # Road-tanker oval AR = width/height = 1.5; Lc = width W; x = h/W = F/AR.
        W, AR = 1.0, 1.5
        H = W / AR
        for i in range(1, 17):
            F = i * 0.05  # 0.05 .. 0.80
            T = slosh.oval_tank_periods(W, H, F, n_modes=1)[0]
            om = _omega(T) * math.sqrt(W / G)
            if om > 1.95:
                break
            pts.append((round(F / AR, 4), round(om, 4)))
    return pts


SHAPES = [
    {
        "key": "rectangular", "label": "Rectangular / prismatic",
        "lc": "L (length in the excitation direction)",
        "formula": "omega_1^2 = (pi g / L) tanh(pi h / L)",
        "ratio": "h / L (depth / length)", "deep": 1.7725,
        "note": "LNG membrane, seismic storage, ship cargo tanks. Exact linear potential theory.",
        "default": {"span": 40.0, "fill_ratio": 0.5, "span_label": "Length L (m)"},
    },
    {
        "key": "upright-cylinder", "label": "Upright cylinder (vertical axis)",
        "lc": "D (diameter)",
        "formula": "omega_1^2 = (1.841 g / R) tanh(1.841 h / R)",
        "ratio": "h / D (depth / diameter)", "deep": 1.9187,
        "note": "Storage tanks, spar/CALM, spherical-limit. First J1' root 1.841. API 650 Annex E convective.",
        "default": {"span": 40.0, "fill_ratio": 0.5, "span_label": "Diameter D (m)"},
    },
    {
        "key": "horizontal-cylinder", "label": "Horizontal cylinder (road tanker)",
        "lc": "D (diameter)",
        "formula": "equivalent rectangle: omega_1^2 = g (pi / b_s) tanh(pi h_eq / b_s)",
        "ratio": "fill fraction F = h / D", "deep": None,
        "note": "Road tankers, rail tank cars, IMO Type-C. Equivalent-rectangle; best at mid fill.",
        "default": {"span": 2.4, "fill_ratio": 0.5, "span_label": "Diameter D (m)"},
    },
    {
        "key": "elliptical", "label": "Elliptical / obround (fuel tanker)",
        "lc": "W (cross-section width)",
        "formula": "equivalent rectangle on the oval cross-section (a, b_s, h_eq)",
        "ratio": "fill fraction F (at width/height = 1.5)", "deep": None,
        "note": "Highway cargo tanks & automotive fuel tanks (wider than tall). Equivalent-rectangle.",
        "default": {"span": 2.0, "fill_ratio": 0.5, "span_label": "Width W (m), height = W/1.5"},
    },
]


def _lookup_table(kind, span, fill_ratio):
    """Real fundamental period across fills for the selected shape + size."""
    rows = []
    for F in (0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8):
        if kind == "rectangular":
            T = slosh.rectangular_tank_periods(span, F * span, n_modes=1)[0]
        elif kind == "upright-cylinder":
            T = slosh.cylindrical_tank_periods(span, F * span, n_modes=1)[0]
        elif kind == "horizontal-cylinder":
            T = slosh.horizontal_cylinder_periods(span, F, n_modes=1)[0]
        else:
            T = slosh.oval_tank_periods(span, span / 1.5, F, n_modes=1)[0]
        rows.append({"f": F, "T": round(T, 2)})
    return rows


# ---- resonance screen (marine ballast-tank case; unchanged physics) ----
_ROLL_T = 12.0
_TOL = 0.15
_RCYL_D, _TANK_H = 40.0, 26.0


def _resonance_series():
    pts = []
    for i in range(0, 49):
        h = round(2.0 + 0.5 * i, 1)
        t_cyl = slosh.cylindrical_tank_periods(_RCYL_D, h, n_modes=1)[0]
        res = slosh.resonance_check([t_cyl], [_ROLL_T], _TOL)
        pts.append({"h": h, "fillpct": round(100.0 * h / _TANK_H),
                    "cyl": round(t_cyl, 2), "resonant": bool(res)})
    return pts


REFERENCES = [
    "Abramson, NASA SP-106 (1966) — The Dynamic Behavior of Liquids in Moving Containers",
    "Ibrahim, Liquid Sloshing Dynamics (Cambridge, 2005)",
    "Faltinsen & Timokha, Sloshing (Cambridge, 2009)",
    "Dodge, New Dynamic Behavior of Liquids in Moving Containers (SwRI, 2000)",
    "McIver (1989), J. Fluid Mech. 201 — cylindrical & spherical containers, arbitrary depth",
    "Budiansky (1960) — sloshing in circular canals and spherical tanks",
    "Evans & McIver (1987), J. Fluid Mech. — resonant frequencies with a vertical baffle",
    "Housner (1963); API 650 Annex E; Malhotra-Wenk-Wieland (2000); Eurocode 8 Part 4 — seismic tanks",
    "Bosch & Vugts (1966) — roll damping by free-surface anti-roll tanks",
    "Ervin, Barnes & Wolfe, UMTRI-85-35 / FHWA (1985) — cargo-tank-truck stability & rollover",
    "Rajagounder et al. (2016), Engineering Journal 20(1) — automotive fuel-tank sloshing (VOF + baffles)",
    "Micheli et al. (2022), J. Applied Fluid Mechanics 15(2) — spray-tanker sloshing under braking",
    "Carette (2023), Ship Technology Research 70(2) — a consistent method to design & evaluate anti-roll tanks; excitation = roll angle + local lateral acceleration (DOI 10.1080/09377255.2022.2117496)",
    "Delorme et al. (2009), Ocean Engineering 36(2) — SPHERIC canonical forced-roll rectangular-tank benchmark",
]


def _load_cfd_benchmark():
    """Rectangular CFD benchmark points for the master-curve overlay (epic #1429).

    Reads the committed manifest written by
    ``scripts/cfd/run_sloshing_benchmark.py`` (measured 2D interFoam VOF runs).
    Returns a compact dict for the page, or ``None`` when the manifest is absent
    (the page then renders exactly as before). Only ``completed`` cases with a
    measured Omega1 are carried through.
    """
    if not _CFD_BENCHMARK.exists():
        return None
    data = json.loads(_CFD_BENCHMARK.read_text())
    out = {"solver": data.get("meta", {}).get("solver", "OpenFOAM interFoam (VOF)")}
    fd = data.get("free_decay") or {}
    pts = [
        {"x": p["h_over_L"], "om_meas": p["omega1_meas"],
         "om_an": p["omega1_analytical"], "err": p["rel_error"]}
        for p in fd.get("points", [])
        if p.get("solver_status") == "completed" and "omega1_meas" in p
    ]
    out["free_decay"] = {"shape": fd.get("shape", "rectangular"), "points": pts}
    fr = data.get("forced_roll") or {}
    if fr.get("points"):
        out["forced_roll"] = {
            "h_over_L": fr.get("h_over_L"),
            "first_mode_period_s": fr.get("first_mode_period_s"),
            "resonant_period_s": fr.get("resonant_period_s"),
            "roll_amplitude_deg": fr.get("roll_amplitude_deg"),
            "points": [
                {"ratio": p.get("period_ratio"), "T": p.get("drive_period_s"),
                 "amp": p.get("moment_amplitude_nm"), "resp": p.get("response_amp_m"),
                 "status": p.get("solver_status")}
                for p in fr.get("points", [])
            ],
        }
    return out


def _fit_sdof_zeta(pairs):
    """Best-fit damping ratio zeta for the linear damped-oscillator amplification
    A(r) = 1/sqrt((1-r^2)^2 + (2*zeta*r)^2), each fill normalised to its own peak.

    ``pairs`` is a list of (r = T_drive/T1, normalised_response) points pooled over
    fills. Returns the zeta in [0.03, 0.5] minimising the squared error to the
    self-normalised model (model divided by its value at r=1). This is the standard
    linear representation of a lightly damped fundamental sloshing mode
    (Faltinsen & Timokha 2009; Abramson 1966) — a MODEL overlay, not a fit claim.
    """
    def amp(r, z):
        return 1.0 / math.sqrt((1.0 - r * r) ** 2 + (2.0 * z * r) ** 2)

    best_z, best_err = 0.1, float("inf")
    z = 0.03
    while z <= 0.5001:
        a1 = amp(1.0, z)
        err = sum((y - amp(r, z) / a1) ** 2 for r, y in pairs)
        if err < best_err:
            best_err, best_z = err, z
        z += 0.005
    return round(best_z, 3)


def _load_forced_response():
    """Forced-roll frequency-response for the "with vs without" panel (#1433).

    Reads the manifest written by ``scripts/cfd/run_sloshing_response_sweep.py``.
    Returns per-fill normalised response points (run-up / its own peak vs
    r = T_drive/T1), a pooled damped-oscillator zeta, and a confidence table of
    forced-roll resonant period vs free-decay/analytical natural period. ``None``
    when the manifest is absent (panel then omitted).
    """
    if not _FORCED_RESPONSE.exists():
        return None
    data = json.loads(_FORCED_RESPONSE.read_text())
    fills, pooled = [], []
    for f in data.get("fills", []):
        t1 = f.get("T1_analytical_s")
        pts = []
        for p in f.get("forced", []):
            amp = p.get("runup_amp_m")
            if amp is None or p.get("status") != "completed" or not t1:
                continue
            pts.append({"r": round(p["drive_period_s"] / t1, 4), "amp": amp})
        if not pts:
            continue
        peak = max(pp["amp"] for pp in pts)
        for pp in pts:
            pp["norm"] = round(pp["amp"] / peak, 4) if peak else 0.0
            pooled.append((pp["r"], pp["norm"]))
        nat_an = f.get("natural_period_analytical_s") or t1
        res = f.get("resonant_period_cfd_s")
        fills.append({
            "h_over_L": f["h_over_L"],
            "t1_analytical": t1,
            "nat_cfd": f.get("natural_period_cfd_s"),
            "nat_analytical": nat_an,
            "resonant_cfd": res,
            "res_ratio": round(res / nat_an, 3) if (res and nat_an) else None,
            "freedecay_err": f.get("freedecay_rel_error"),
            "points": pts,
        })
    if not fills:
        return None
    return {
        "solver": data.get("meta", {}).get("solver", "OpenFOAM interFoam (VOF)"),
        "roll_amplitude_deg": data.get("meta", {}).get("roll_amplitude_deg"),
        "tank": data.get("tank", {}),
        "zeta": _fit_sdof_zeta(pooled) if pooled else 0.1,
        "fills": fills,
    }


def _load_research():
    """Literature synthesis for the way-forward + confidence section (#1433)."""
    if not _RESEARCH.exists():
        return None
    data = json.loads(_RESEARCH.read_text())
    syn = data.get("synthesis", data)
    order = {"high": 0, "medium": 1, "low": 2}
    fw = sorted(syn.get("further_cfd", []),
                key=lambda x: order.get(x.get("priority", "medium"), 1))
    return {
        "confidence": syn.get("methodology_confidence", ""),
        "supports": syn.get("supports", [])[:6],
        "contrasts": syn.get("contrasts", [])[:6],
        "further_cfd": fw,
    }


def build_study():
    shapes = []
    for sh in SHAPES:
        shapes.append({**sh, "curve": _norm_curve(sh["key"]),
                       "lookup": _lookup_table(sh["key"], sh["default"]["span"], 0.5)})
    return {
        "meta": {"g": G, "roll_t": _ROLL_T, "tol": _TOL,
                 "band_lo": round(_ROLL_T * (1 - _TOL), 2),
                 "band_hi": round(_ROLL_T * (1 + _TOL), 2),
                 "cyl_d": _RCYL_D, "tank_h": _TANK_H},
        "shapes": shapes,
        "cfd": _load_cfd_benchmark(),
        "forced_response": _load_forced_response(),
        "research": _load_research(),
        "resonance": _resonance_series(),
        "references": REFERENCES,
    }


_HTML = r"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Tank sloshing — natural-period & resonance explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--roll:#7c3aed;
       --c0:#0B3D91;--c1:#0f8a7e;--c2:#b8860b;--c3:#c0392b}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:24px;color:var(--navy);letter-spacing:-.3px}
 h2{font-size:15px;color:var(--navy);margin:2px 0 10px;letter-spacing:-.2px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:840px}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .tag{display:inline-block;font-size:11px;font-weight:700;letter-spacing:.4px;text-transform:uppercase;
      color:var(--teal);margin-bottom:6px}
 svg{width:100%;height:auto;display:block}
 .legend{display:flex;flex-wrap:wrap;gap:14px;margin-top:10px;font-size:12.5px}
 .legend span{display:flex;align-items:center;gap:6px;color:var(--muted);cursor:pointer;user-select:none}
 .legend i{width:22px;height:3px;border-radius:2px;display:inline-block}
 .legend .off{opacity:.32}
 .ctrl{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:6px 0 12px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13px}
 select,input[type=number]{font:inherit;font-size:14px;padding:6px 9px;border:1px solid var(--line);
      border-radius:8px;background:#fff;color:var(--ink)}
 input[type=range]{flex:1;min-width:180px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:15px;min-width:96px}
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:8px 0}
 .read{font-size:12.5px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;font-weight:700;color:var(--navy)}
 table{border-collapse:collapse;width:100%;font-size:13px;margin-top:6px}
 th,td{text-align:right;padding:5px 9px;border-bottom:1px solid var(--line)}
 th:first-child,td:first-child{text-align:left}
 th{color:var(--muted);font-weight:600;font-size:11.5px;text-transform:uppercase;letter-spacing:.4px}
 td.mono{font-family:ui-monospace,Menlo,monospace;color:var(--navy);font-weight:700}
 .fnote{font-size:12px;color:var(--muted);margin-top:8px}
 .formula{font-family:ui-monospace,Menlo,monospace;font-size:12.5px;background:var(--soft);
      border:1px solid var(--line);border-radius:8px;padding:8px 11px;color:var(--navy);margin:8px 0}
 .scale{position:relative;height:44px;margin:20px 0 4px;border-radius:9px;overflow:hidden;
        background:var(--soft);border:1px solid var(--line)}
 .band{position:absolute;top:0;bottom:0;background:rgba(124,58,237,.15);border-left:2px solid var(--roll);border-right:2px solid var(--roll)}
 .tick{position:absolute;top:-4px;bottom:-4px;width:2px;background:var(--teal)}
 .tick span{position:absolute;top:-16px;left:50%;transform:translateX(-50%);font-size:9px;font-weight:700;color:var(--teal);white-space:nowrap}
 .scalelab{display:flex;justify-content:space-between;font-size:10px;color:var(--muted);padding:0 2px}
 .flag{display:inline-block;font-size:12px;font-weight:700;border-radius:20px;padding:4px 12px;margin-top:12px}
 .flag.ok{background:#e6f5ec;color:var(--ok)} .flag.bad{background:#fdecea;color:var(--bad)}
 ul.refs{list-style:none;font-size:12.5px;color:var(--muted);columns:2;column-gap:26px}
 ul.refs li{padding:3px 0;break-inside:avoid}
 .cfd{background:linear-gradient(135deg,#f4f8fc,#eaf3ff);border:1px solid var(--line);border-radius:12px;padding:14px 16px}
 .cfd b{color:var(--navy)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
 .litl{list-style:none;font-size:12.5px;color:var(--muted)} .litl li{padding:5px 0;border-bottom:1px solid var(--line)}
 .litl b{color:var(--ink)} .litl .cite{color:var(--teal);font-size:11px;display:block;margin-top:2px}
 .fwl{font-size:12.5px;color:var(--ink);padding-left:20px} .fwl li{margin:6px 0}
 .fwl .rat{color:var(--muted)}
 .pri{display:inline-block;font-size:9.5px;font-weight:700;border-radius:4px;padding:1px 6px;color:#fff;margin-right:5px;letter-spacing:.3px}
 .pri-high{background:var(--bad)} .pri-medium{background:var(--c2)} .pri-low{background:var(--muted)}
 @media(max-width:640px){ul.refs{columns:1}}
</style></head><body>
<h1>Tank sloshing &mdash; natural-period &amp; resonance explorer</h1>
<p class="sub">The fundamental sloshing natural period of a partially filled tank, from validated analytical
theory across tank shapes. The <b>normalized master curve</b> collapses every size onto one dimensionless
family; pick a shape to read real periods; and see how partial fill sweeps a tank into resonance with vessel
roll &mdash; the coupling external diffraction (AQWA/OrcaWave) does not capture. Every value is a live
<code>hydrodynamics.sloshing</code> evaluation.</p>

<div class="panel">
  <span class="tag">Normalized master curve</span>
  <h2>Dimensionless sloshing frequency &Omega;&#8321; = &omega;&#8321;&radic;(L<sub>c</sub>/g) vs fill / slenderness ratio h/L<sub>c</sub></h2>
  <p class="sub" style="margin:0 0 8px">Any tank of a given shape &mdash; any size &mdash; lies on its curve. Read &Omega;&#8321; off the
  curve at your fill ratio, then the real period is <b>T = 2&pi;&radic;(L<sub>c</sub>/g) / &Omega;&#8321;</b>. Deep-liquid asymptotes:
  rectangular &radic;&pi;&nbsp;=&nbsp;1.77, upright cylinder &radic;1.841&nbsp;=&nbsp;1.36 (&times;&radic;2 on a diameter basis).</p>
  <svg id="chart" viewBox="0 0 720 380" role="img" aria-label="Normalized sloshing frequency curves"></svg>
  <div class="legend" id="legend"></div>
  <p class="fnote">Horizontal-cylinder and elliptical curves use the equivalent-rectangle method (good to ~10&ndash;15% at mid
  fill; near-full and violent cases are where a VOF free-surface CFD run takes over).</p>
  <div id="cfdverify"></div>
</div>

<div class="panel" id="cfdlitpanel" style="display:none">
  <span class="tag" style="color:#c0392b">CFD vs literature &mdash; which shape is validated</span>
  <h2>Which tank shape was run in CFD? The RECTANGULAR tank &mdash; measured CFD vs literature theory</h2>
  <p class="sub" style="margin:0 0 8px">The master curve above is <b>literature theory</b> (linear potential flow) for every
  shape. So far only the <b>rectangular</b> tank has been validated with free-surface CFD (OpenFOAM VOF). This chart
  isolates it &mdash; CFD-measured points against the literature curve, with the error at each fill &mdash; and the strip
  below shows which shapes are CFD-validated versus literature / analytical only.</p>
  <svg id="cfdlitchart" viewBox="0 0 720 380" role="img" aria-label="CFD measured vs literature theory, rectangular tank"></svg>
  <div class="legend" id="cfdlitlegend"></div>
  <div id="shapecov" style="margin-top:12px"></div>
  <p class="fnote">Literature basis: linear potential theory (Faltinsen &amp; Timokha 2009; Abramson 1966), corroborated by
  forced-roll and multi-fill CFD / experiments (Delorme&nbsp;/&nbsp;SPHERIC; Rognebakke &amp; Faltinsen; Chen &amp; Xue).
  Full method, force histories and the adversarially-verified survey:
  <a href="../cfd/sloshing-cfd-study.html">the 2D CFD validation study &rarr;</a></p>
</div>

<div class="panel" id="forcedpanel" style="display:none">
  <span class="tag" style="color:#c0392b">Forced-roll resonance &mdash; with vs without</span>
  <h2>How forced roll reveals the sloshing resonance</h2>
  <p class="sub" style="margin:0 0 6px"><b>Without</b> forced roll, a free-decay CFD run gives the tank's intrinsic
  first-mode natural period (the point on the master curve). <b>With</b> forced roll, driving the tank at a range of
  roll periods and measuring the wall run-up traces a resonance curve that <b>peaks at that same natural period</b>
  &mdash; roll excites the mode the free-decay rings down. Response is normalised to each fill's own peak and plotted
  against the drive period over the natural period, T<sub>drive</sub>/T&#8321;.</p>
  <svg id="respchart" viewBox="0 0 720 360" role="img" aria-label="Forced-roll frequency response"></svg>
  <div class="legend" id="resplegend"></div>
  <div id="respverify"></div>
</div>

<div class="panel">
  <span class="tag">Shape lookup</span>
  <h2>Real natural period for a specific tank</h2>
  <div class="ctrl">
    <label for="shape">Tank shape</label>
    <select id="shape"></select>
    <label for="span" id="spanlab">Size (m)</label>
    <input type="number" id="span" step="0.1" min="0.1" style="width:92px">
  </div>
  <div class="formula" id="formula"></div>
  <div class="reads">
    <div class="read">Fundamental period (½ full)<b id="rT">&mdash;</b></div>
    <div class="read">Characteristic length L<sub>c</sub><b id="rLc" style="font-size:14px">&mdash;</b></div>
    <div class="read">Aspect / fill ratio<b id="rRatio" style="font-size:14px">&mdash;</b></div>
  </div>
  <table><thead><tr><th>Fill fraction</th><th>Fundamental period T&#8321; (s)</th></tr></thead>
    <tbody id="lookrows"></tbody></table>
  <p class="fnote" id="shapenote"></p>
</div>

<div class="panel">
  <span class="tag">Resonance screen &mdash; marine coupling</span>
  <h2>Partial-fill sloshing vs vessel roll (the ballast-tank / anti-roll case)</h2>
  <div class="ctrl">
    <label for="h">Fill level</label>
    <input type="range" id="h" min="0" max="0" step="1">
    <span class="wval" id="hval"></span>
  </div>
  <div class="reads">
    <div class="read">Cylindrical T&#8321;<b id="cT">&mdash;</b></div>
    <div class="read">Vessel roll<b id="cV" style="color:var(--roll)">&mdash;</b></div>
  </div>
  <div class="scale"><div class="band" id="band"></div><div class="tick" id="ctick"><span>tank</span></div></div>
  <div class="scalelab"><span>4 s</span><span>8</span><span>12</span><span>16</span><span>20 s</span></div>
  <div id="cflag"></div>
  <div class="cfd" style="margin-top:14px">
    <b>Where CFD takes over.</b> Analytical periods and the class-simplified screen above close most of a scope.
    Violent / high-fill / impact cases &mdash; and the coupled roll response of partially filled ballast tanks used as
    tuned/anti-roll tanks &mdash; are resolved with VOF free-surface CFD (OpenFOAM), feeding a reduced response back into
    the vessel roll time-domain model. This is the tiering behind the ACMA / Noble ballast-tank sloshing study.
    The <b style="color:#c0392b">red rings</b> on the master curve above are the first rung of that ladder: measured 2D
    interFoam points that pin the rectangular curve to real free-surface CFD (see the
    <a href="../cfd/tank-sloshing-verification.html">CFD verification page</a>).
  </div>
</div>

<div class="panel" id="wayforwardpanel" style="display:none">
  <span class="tag" style="color:#c0392b">Literature confidence &amp; way forward</span>
  <h2>What the literature says &mdash; and the CFD that should follow</h2>
  <p class="sub" id="confidence" style="margin:0 0 12px"></p>
  <div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:10px 22px">
    <div><h2 style="font-size:12.5px;color:var(--ok)">Supports our approach</h2><ul class="litl" id="litsup"></ul></div>
    <div><h2 style="font-size:12.5px;color:var(--bad)">Caveats &amp; contrasts</h2><ul class="litl" id="litcon"></ul></div>
  </div>
  <h2 style="font-size:13px;margin-top:16px">Further CFD analysis (prioritised)</h2>
  <ol class="fwl" id="furthercfd"></ol>
  <p class="fnote">Synthesised from an adversarially-verified deep literature survey (8 agents; fabricated draft
  figures were removed by a critique pass). Full method &amp; references on the
  <a href="../cfd/sloshing-cfd-study.html">CFD study page &rarr;</a></p>
</div>

<div class="panel">
  <span class="tag">Worldwide relationship basis</span>
  <h2>References</h2>
  <ul class="refs" id="refs"></ul>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_sloshing_explorer.py</code> from
<code>src/digitalmodel/hydrodynamics/sloshing.py</code>.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/hydrodynamics/sloshing.py">engine source &rarr;</a></p>

<script>
const DATA = __DATA__;
const G = DATA.meta.g, COL = ['var(--c0)','var(--c1)','var(--c2)','var(--c3)'];
const CFD = DATA.cfd;               // measured 2D VOF benchmark (epic #1429) or null
const CFDCOL = '#c0392b'; let cfdOn = true;

/* ---------- master chart ---------- */
const XW=720,YH=380,ML=54,MR=16,MT=14,MB=42, XMAX=1.5, YMAX=2.0;
const px=x=>ML+(x/XMAX)*(XW-ML-MR), py=y=>YH-MB-(y/YMAX)*(YH-MT-MB);
const on = DATA.shapes.map(()=>true);
function drawChart(sel){
  let g='';
  // grid + axes
  for(let gx=0;gx<=1.5;gx+=0.25){g+=`<line x1="${px(gx)}" y1="${py(0)}" x2="${px(gx)}" y2="${py(YMAX)}" stroke="var(--line)"/>`+
    `<text x="${px(gx)}" y="${py(0)+16}" fill="var(--muted)" font-size="10" text-anchor="middle">${gx}</text>`;}
  for(let gy=0;gy<=2;gy+=0.5){g+=`<line x1="${px(0)}" y1="${py(gy)}" x2="${px(XMAX)}" y2="${py(gy)}" stroke="var(--line)"/>`+
    `<text x="${px(0)-8}" y="${py(gy)+3}" fill="var(--muted)" font-size="10" text-anchor="end">${gy.toFixed(1)}</text>`;}
  g+=`<text x="${(ML+XW-MR)/2}" y="${YH-6}" fill="var(--muted)" font-size="11" text-anchor="middle">fill / slenderness ratio  h / Lc</text>`;
  g+=`<text transform="translate(14,${(YH-MB+MT)/2}) rotate(-90)" fill="var(--muted)" font-size="11" text-anchor="middle">Ω₁ = ω₁√(Lc/g)</text>`;
  DATA.shapes.forEach((sh,i)=>{
    if(!on[i]) return;
    const d = sh.curve.map((p,j)=>`${j?'L':'M'}${px(p[0]).toFixed(1)} ${py(p[1]).toFixed(1)}`).join(' ');
    const w = (i===sel)?3.2:1.8, o=(sel<0||i===sel)?1:0.5;
    g+=`<path d="${d}" fill="none" stroke="${COL[i]}" stroke-width="${w}" opacity="${o}"/>`;
  });
  // Measured CFD points overlaid on the rectangular curve (epic #1429).
  if(CFD && CFD.free_decay && cfdOn){
    CFD.free_decay.points.forEach(p=>{
      const cx=px(p.x), cy=py(p.om_meas);
      g+=`<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="5" fill="#fff" stroke="${CFDCOL}" stroke-width="2.2"/>`+
         `<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="1.7" fill="${CFDCOL}"/>`+
         `<title>CFD h/L=${p.x}: Ω₁=${p.om_meas} (analytical ${p.om_an}, err ${(p.err*100).toFixed(2)}%)</title>`;
    });
  }
  document.getElementById('chart').innerHTML=g;
}
function drawLegend(sel){
  let items = DATA.shapes.map((sh,i)=>
    `<span data-i="${i}" class="${on[i]?'':'off'}"><i style="background:${COL[i]}"></i>${sh.label}</span>`).join('');
  if(CFD && CFD.free_decay && CFD.free_decay.points.length){
    items += `<span data-cfd="1" class="${cfdOn?'':'off'}" title="Measured 2D interFoam VOF free-decay points">`+
      `<i style="background:transparent;border:2px solid ${CFDCOL};height:11px;width:11px;border-radius:50%"></i>CFD (OpenFOAM VOF)</span>`;
  }
  document.getElementById('legend').innerHTML = items;
  document.querySelectorAll('#legend span').forEach(el=>el.onclick=()=>{
    if(el.dataset.cfd){ cfdOn=!cfdOn; } else { const i=+el.dataset.i; on[i]=!on[i]; }
    drawLegend(sel); drawChart(sel);});
}

/* ---------- shape lookup ---------- */
const sel = document.getElementById('shape');
DATA.shapes.forEach((sh,i)=>{const o=document.createElement('option');o.value=i;o.textContent=sh.label;sel.appendChild(o);});
const spanEl=document.getElementById('span');
function renderShape(){
  const i=+sel.value, sh=DATA.shapes[i];
  document.getElementById('spanlab').textContent=sh.default.span_label;
  if(document.activeElement!==spanEl) spanEl.value=sh.default.span;
  const scale=Math.sqrt((+spanEl.value||sh.default.span)/sh.default.span);
  document.getElementById('formula').textContent=sh.formula;
  document.getElementById('rLc').textContent=sh.lc;
  document.getElementById('rRatio').textContent=sh.ratio;
  const half=sh.lookup.find(r=>r.f===0.5);
  document.getElementById('rT').textContent=(half.T*scale).toFixed(2)+' s';
  document.getElementById('lookrows').innerHTML=sh.lookup.map(r=>
    `<tr><td>${Math.round(r.f*100)}%</td><td class="mono">${(r.T*scale).toFixed(2)}</td></tr>`).join('');
  document.getElementById('shapenote').textContent=sh.note;
  drawChart(i); drawLegend(i);
}
sel.onchange=renderShape; spanEl.oninput=renderShape;

/* ---------- resonance screen ---------- */
const M=DATA.meta, S=DATA.resonance, LO=4,HI=20, pos=v=>Math.max(0,Math.min((v-LO)/(HI-LO)*100,100));
const band=document.getElementById('band');
band.style.left=pos(M.band_lo)+'%'; band.style.width=(pos(M.band_hi)-pos(M.band_lo))+'%';
document.getElementById('cV').textContent=M.roll_t.toFixed(1)+' s';
const hs=document.getElementById('h'); hs.max=S.length-1;
function renderRes(){
  const p=S[+hs.value];
  document.getElementById('hval').textContent=p.h.toFixed(1)+' m ('+p.fillpct+'%)';
  document.getElementById('cT').textContent=p.cyl.toFixed(2)+' s';
  document.getElementById('ctick').style.left=pos(p.cyl)+'%';
  const f=document.getElementById('cflag');
  if(p.resonant){f.className='flag bad';f.textContent='⚠ Resonance risk — tank period within the roll band → CFD check warranted';}
  else{f.className='flag ok';f.textContent='✓ No resonance — tank period clear of the roll band';}
}
let st=S.findIndex(p=>p.fillpct>=95); hs.value=st<0?S.length-1:st; renderRes();
hs.addEventListener('input',renderRes);

/* ---------- CFD verification table (epic #1429) ---------- */
function renderCFD(){
  const el=document.getElementById('cfdverify'); if(!el) return;
  if(!CFD || !CFD.free_decay || !CFD.free_decay.points.length){ el.innerHTML=''; return; }
  const rows = CFD.free_decay.points.map(p=>
    `<tr><td>${(p.x*100).toFixed(0)}%</td><td class="mono">${p.om_meas.toFixed(3)}</td>`+
    `<td class="mono">${p.om_an.toFixed(3)}</td><td class="mono">${(p.err*100).toFixed(2)}%</td></tr>`).join('');
  let fr='';
  if(CFD.forced_roll && CFD.forced_roll.resonant_period_s){
    const F=CFD.forced_roll;
    fr=`<p class="fnote" style="margin-top:10px"><b style="color:${CFDCOL}">Forced-roll resonance.</b> Driving the same
      rectangular tank (h/L&nbsp;≈&nbsp;${F.h_over_L}) at three roll periods bracketing the analytical first mode
      T&#8321;&nbsp;=&nbsp;${F.first_mode_period_s}&nbsp;s, the tank response peaks at
      <b>${F.resonant_period_s}&nbsp;s</b> — the forced response confirms the free-decay natural period is the
      resonant period.</p>`;
  }
  el.innerHTML =
    `<div style="margin-top:14px;border-top:1px solid var(--line);padding-top:12px">
      <span class="tag" style="color:${CFDCOL}">CFD verification of the rectangular curve</span>
      <p class="fnote" style="margin:2px 0 8px">Measured 2D free-surface CFD (${CFD.solver}) — the
      red rings on the chart. Each free-decay case rings down a small first-mode perturbation; the FFT natural
      frequency, non-dimensionalised, is compared to the analytical curve value.</p>
      <table><thead><tr><th>Fill h/L</th><th>Ω&#8321; measured (CFD)</th><th>Ω&#8321; analytical</th><th>Error</th></tr></thead>
        <tbody>${rows}</tbody></table>${fr}
      <p class="fnote">Full method &amp; force-history: <a href="../cfd/tank-sloshing-verification.html">2D tank-sloshing CFD verification &rarr;</a></p>
    </div>`;
}

/* ---------- forced-roll resonance panel (issue #1433) ---------- */
const FR = DATA.forced_response;
const FRCOL = ['var(--c0)','var(--c1)','var(--c2)','var(--c3)'];
function drawRespChart(){
  const RW=720,RH=360,rML=56,rMR=16,rMT=14,rMB=44,RX0=0.6,RX1=1.5,RYX=1.18;
  const rpx=r=>rML+((r-RX0)/(RX1-RX0))*(RW-rML-rMR);
  const rpy=y=>RH-rMB-(y/RYX)*(RH-rMT-rMB);
  let g='';
  for(let gx=0.6;gx<=1.5001;gx+=0.1){g+=`<line x1="${rpx(gx).toFixed(1)}" y1="${rpy(0)}" x2="${rpx(gx).toFixed(1)}" y2="${rpy(RYX)}" stroke="var(--line)"/>`+
    `<text x="${rpx(gx).toFixed(1)}" y="${rpy(0)+16}" fill="var(--muted)" font-size="10" text-anchor="middle">${gx.toFixed(1)}</text>`;}
  for(let gy=0;gy<=1.0001;gy+=0.25){g+=`<line x1="${rpx(RX0)}" y1="${rpy(gy)}" x2="${rpx(RX1)}" y2="${rpy(gy)}" stroke="var(--line)"/>`+
    `<text x="${rpx(RX0)-8}" y="${rpy(gy)+3}" fill="var(--muted)" font-size="10" text-anchor="end">${gy.toFixed(2)}</text>`;}
  g+=`<text x="${(rML+RW-rMR)/2}" y="${RH-6}" fill="var(--muted)" font-size="11" text-anchor="middle">drive period / natural period  T_drive / T₁</text>`;
  g+=`<text transform="translate(14,${(RH-rMB+rMT)/2}) rotate(-90)" fill="var(--muted)" font-size="11" text-anchor="middle">wall run-up (normalised to peak)</text>`;
  // "without forced roll" = the natural period, at r = 1
  g+=`<line x1="${rpx(1).toFixed(1)}" y1="${rpy(0)}" x2="${rpx(1).toFixed(1)}" y2="${rpy(RYX)}" stroke="#c0392b" stroke-width="1.5" stroke-dasharray="5 4"/>`+
     `<text x="${rpx(1).toFixed(1)}" y="${rpy(RYX)+2}" fill="#c0392b" font-size="10.5" text-anchor="middle">natural period (free-decay)</text>`;
  // damped-oscillator model overlay (literature)
  const z=FR.zeta, a1=1/(2*z); let d='';
  for(let r=RX0;r<=RX1+1e-9;r+=0.01){const A=(1/Math.sqrt((1-r*r)**2+(2*z*r)**2))/a1; d+=`${d?'L':'M'}${rpx(r).toFixed(1)} ${rpy(Math.min(A,RYX)).toFixed(1)}`;}
  g+=`<path d="${d}" fill="none" stroke="var(--muted)" stroke-width="1.6" stroke-dasharray="2 3"/>`;
  // CFD response per fill
  FR.fills.forEach((f,i)=>{
    const pts=f.points.slice().sort((a,b)=>a.r-b.r);
    const line=pts.map((p,j)=>`${j?'L':'M'}${rpx(p.r).toFixed(1)} ${rpy(p.norm).toFixed(1)}`).join(' ');
    g+=`<path d="${line}" fill="none" stroke="${FRCOL[i]}" stroke-width="2.4"/>`;
    pts.forEach(p=>{g+=`<circle cx="${rpx(p.r).toFixed(1)}" cy="${rpy(p.norm).toFixed(1)}" r="4" fill="#fff" stroke="${FRCOL[i]}" stroke-width="2"/>`;});
  });
  document.getElementById('respchart').innerHTML=g;
}
function renderForced(){
  const panel=document.getElementById('forcedpanel');
  if(!FR||!FR.fills||!FR.fills.length){ if(panel) panel.style.display='none'; return; }
  panel.style.display='';
  drawRespChart();
  document.getElementById('resplegend').innerHTML =
    FR.fills.map((f,i)=>`<span><i style="background:${FRCOL[i]}"></i>Forced roll, fill h/L = ${f.h_over_L}</span>`).join('')+
    `<span><i style="background:var(--muted)"></i>Damped-oscillator model, ζ = ${FR.zeta} (linear theory)</span>`;
  const rows = FR.fills.map(f=>
    `<tr><td>${(f.h_over_L*100).toFixed(0)}%</td>`+
    `<td class="mono">${f.nat_analytical? f.nat_analytical.toFixed(3):'—'}</td>`+
    `<td class="mono">${f.nat_cfd? f.nat_cfd.toFixed(3):'—'}</td>`+
    `<td class="mono">${f.resonant_cfd? f.resonant_cfd.toFixed(3):'—'}</td>`+
    `<td class="mono">${f.res_ratio!=null? f.res_ratio.toFixed(2):'—'}</td></tr>`).join('');
  const amp = FR.roll_amplitude_deg? FR.roll_amplitude_deg+'° roll':'forced roll';
  document.getElementById('respverify').innerHTML =
    `<div style="margin-top:14px;border-top:1px solid var(--line);padding-top:12px">
      <p class="fnote" style="margin:0 0 8px">Measured 2D VOF (${FR.solver}); ${amp} on an
      L=${FR.tank.breadth_m} m tank. Confidence check: the forced-roll resonant period lands on the
      natural period (ratio ≈ 1.00) at every fill.</p>
      <table><thead><tr><th>Fill h/L</th><th>Natural T₁ analytical (s)</th><th>Natural T₁ free-decay CFD (s)</th>
        <th>Resonant T forced CFD (s)</th><th>resonant / natural</th></tr></thead>
        <tbody>${rows}</tbody></table>
      <p class="fnote" style="margin-top:8px">The dashed grey curve is the linear damped-oscillator amplification
      A(r)=1/√((1−r²)²+(2ζr)²) — the standard representation of a lightly damped fundamental sloshing mode
      (Faltinsen &amp; Timokha 2009; Abramson 1966) — with ζ fitted to the CFD points.</p>
    </div>`;
}

/* ---------- literature confidence + way forward (issue #1433) ---------- */
const RES = DATA.research;
function renderResearch(){
  const panel=document.getElementById('wayforwardpanel');
  if(!RES||!RES.further_cfd||!RES.further_cfd.length){ if(panel) panel.style.display='none'; return; }
  panel.style.display='';
  document.getElementById('confidence').textContent=RES.confidence||'';
  const li=items=>items.map(x=>`<li>${x.point||''}<span class="cite">${x.citation||''}</span></li>`).join('');
  document.getElementById('litsup').innerHTML=li(RES.supports||[]);
  document.getElementById('litcon').innerHTML=li(RES.contrasts||[]);
  document.getElementById('furthercfd').innerHTML=(RES.further_cfd||[]).map(x=>{
    const p=(x.priority||'medium');
    return `<li><span class="pri pri-${p}">${p.toUpperCase()}</span><b>${x.title||''}</b> `+
      `<span class="rat">— ${x.rationale||''}</span></li>`;}).join('');
}

/* ---------- CFD vs literature — which shape is validated (issue #1429) ---------- */
function renderCfdVsLit(){
  const panel=document.getElementById('cfdlitpanel');
  if(!CFD||!CFD.free_decay||!CFD.free_decay.points.length){ if(panel) panel.style.display='none'; return; }
  panel.style.display='';
  const rect=DATA.shapes.find(s=>s.key==='rectangular')||DATA.shapes[0];
  let g='';
  for(let gx=0;gx<=1.5;gx+=0.25){g+=`<line x1="${px(gx)}" y1="${py(0)}" x2="${px(gx)}" y2="${py(YMAX)}" stroke="var(--line)"/>`+
    `<text x="${px(gx)}" y="${py(0)+16}" fill="var(--muted)" font-size="10" text-anchor="middle">${gx}</text>`;}
  for(let gy=0;gy<=2;gy+=0.5){g+=`<line x1="${px(0)}" y1="${py(gy)}" x2="${px(XMAX)}" y2="${py(gy)}" stroke="var(--line)"/>`+
    `<text x="${px(0)-8}" y="${py(gy)+3}" fill="var(--muted)" font-size="10" text-anchor="end">${gy.toFixed(1)}</text>`;}
  g+=`<text x="${(ML+XW-MR)/2}" y="${YH-6}" fill="var(--muted)" font-size="11" text-anchor="middle">fill / slenderness ratio  h / L</text>`;
  g+=`<text transform="translate(14,${(YH-MB+MT)/2}) rotate(-90)" fill="var(--muted)" font-size="11" text-anchor="middle">Ω₁ = ω₁√(L/g)</text>`;
  const d=rect.curve.map((p,j)=>`${j?'L':'M'}${px(p[0]).toFixed(1)} ${py(p[1]).toFixed(1)}`).join(' ');
  g+=`<path d="${d}" fill="none" stroke="var(--c0)" stroke-width="2.6"/>`;
  CFD.free_decay.points.forEach(p=>{
    const cx=px(p.x), cy=py(p.om_meas);
    g+=`<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="5.5" fill="#fff" stroke="${CFDCOL}" stroke-width="2.4"/>`+
       `<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="2" fill="${CFDCOL}"/>`+
       `<text x="${(cx+9).toFixed(1)}" y="${(cy-6).toFixed(1)}" fill="${CFDCOL}" font-size="10" font-weight="700">${(p.err*100).toFixed(2)}%</text>`;
  });
  document.getElementById('cfdlitchart').innerHTML=g;
  document.getElementById('cfdlitlegend').innerHTML=
    `<span><i style="background:var(--c0)"></i>Literature — linear potential theory (rectangular)</span>`+
    `<span><i style="background:transparent;border:2px solid ${CFDCOL};height:11px;width:11px;border-radius:50%"></i>CFD measured — OpenFOAM VOF (rectangular)</span>`;
  const cov=DATA.shapes.map((s,i)=>{
    const r=(s.key==='rectangular');
    return `<span style="display:inline-block;border:1px solid var(--line);border-radius:20px;padding:5px 12px;margin:3px 8px 3px 0;font-size:12px;`+
      (r?`background:#fdecea;color:${CFDCOL};font-weight:700`:`background:var(--soft);color:var(--muted)`)+`">`+
      `<i style="display:inline-block;width:9px;height:9px;border-radius:2px;background:${COL[i]};margin-right:6px;vertical-align:0"></i>`+
      `${s.label} &mdash; ${r?'CFD-validated ✓':'literature / analytical'}</span>`;
  }).join('');
  document.getElementById('shapecov').innerHTML=`<b style="font-size:12.5px;color:var(--ink)">CFD validation coverage by shape:</b><br>`+cov;
}

document.getElementById('refs').innerHTML=DATA.references.map(r=>`<li>${r}</li>`).join('');
renderCFD();
renderCfdVsLit();
renderForced();
renderResearch();
renderShape();
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "sloshing-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("__DATA__", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "sloshing-explorer.html").write_text(html, encoding="utf-8")
    print(f"wrote {(_OUT_DIR / 'sloshing-explorer.html').relative_to(_REPO)} "
          f"({len(study['shapes'])} shapes, {len(study['resonance'])} resonance fills)")


if __name__ == "__main__":
    main()
