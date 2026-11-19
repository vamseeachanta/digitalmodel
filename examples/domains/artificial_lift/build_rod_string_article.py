"""Build the rod string article (rod pump series, part 2) and its detail pages from one results file.

Analysis (``--refresh``, needs the solver): the public example well's surface card is run through the
library's finite-difference (Everitt-Jennings) solver, and the results the article uses are saved to
docs/domains/artificial_lift/rod-string-article/results.json:
- the stroke, load and stress at every grid point down the rod string, and the static stretch of each taper;
- the pump card with the estimated damping scaled from 0 to 10 times;
- the sampling (Courant) limit of the depth march and the march above it;
- the +/-1 % and +/-5 % sensitivity of the key results to each input.

Pages (from results.json only): the article page, the Markdown detail pages that its appendices summarise,
and the CSV data behind each figure. The article links the detail pages at DETAIL_COMMIT; when that constant
is empty only the results, the detail pages and the CSV files are written.

Usage (from the repository root):
    PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py [--refresh] [--check]
"""
from __future__ import annotations

import argparse
import csv
import html
import io
import json
import math
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
FOLDER = Path("docs/domains/artificial_lift/rod-string-article")
RESULTS = FOLDER / "results.json"
ARTICLE = FOLDER / "rod-string-article.html"
FIXTURE = "tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json"
BUILDER = "examples/domains/artificial_lift/build_rod_string_article.py"
REPO = "https://github.com/vamseeachanta/digitalmodel"
# Library code the results come from: digitalmodel main, 19 Nov 2026 (UTC).
LIBRARY_COMMIT = "a26efaa413c962105cdd49ce6b5122b5d0ab0b58"
LIBRARY_DATE = "19 Nov 2026"
# Commit a reader checks out to reproduce the results (named on the reusable-prompts page). It holds this
# builder, results.json and the detail pages; empty: the page names no commit yet.
PROMPT_COMMIT = "796e99f91fbf9f47ab6fa8b0c3e21c4a72961088"
# Commit that holds the detail pages the article links; its reusable-prompts page names PROMPT_COMMIT.
# Empty: the article page is not written.
DETAIL_COMMIT = ""
REVISION = "Revision 2, 19 Nov 2026"
ARTICLE_TITLE = "Rod string mechanics: what stretch, load and damping do to the dynamometer card"
OPENING_CARD = FOLDER / "opening-card.png"  # 1200 x 627 link-preview image (og:image)

DAMPING_SCALES = (0.0, 0.5, 1.0, 2.0, 5.0, 10.0)
SHOWN_SCALES = (0.0, 1.0, 10.0)
STEPS = (0.01, 0.05)
VISCOSITY_CP = 10.0          # ASSUMED - to be confirmed (library default)
TUBING_ID_IN = 2.441         # ASSUMED - to be confirmed (library default: inside diameter of 2-7/8 in, 6.5 lb/ft tubing)
N_NODES = 200                # library default grid
CLOSING = ("If you have run your own analysis, see a way to improve this one, or have an idea for a future "
           "issue, write to support@aceengineer.com. We read every reply and credit the ideas we use and "
           "provide results we obtained.")
BLUE, ORANGE, TEAL, GREY, NAVY = "#2a78d6", "#eb6834", "#1b8a8f", "#a3a29b", "#1d3557"

PAGES = {
    "readme": FOLDER / "README.md",
    "stroke": FOLDER / "stroke-and-stress.md",
    "damping": FOLDER / "damping.md",
    "method": FOLDER / "method-and-limits.md",
    "prompts": FOLDER / "reusable-prompts.md",
}
CSV_FILES = {
    "cards": FOLDER / "figure-1-cards-at-depth.csv",
    "profile": FOLDER / "figure-2-3-depth-profile.csv",
    "damping": FOLDER / "figure-4-damping-cards.csv",
    "sensitivity": FOLDER / "sensitivity.csv",
}


# ---------------------------------------------------------------- analysis (needs the solver)

def run_analysis(root: Path = ROOT) -> dict:
    """Run the public example well through the library solver and return the saved results."""
    sys.path.insert(0, str(root / "src"))
    import numpy as np  # noqa: PLC0415
    from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (  # noqa: PLC0415
        calculate_fluid_load, calculate_pump_fillage)
    from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (  # noqa: PLC0415
        get_expected_downhole_card, load_from_json_file)
    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import adapter  # noqa: PLC0415
    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import units as U  # noqa: PLC0415
    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.damping import (  # noqa: PLC0415
        estimate_damping_profile)
    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (  # noqa: PLC0415
        EverittJenningsSolver)
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData  # noqa: PLC0415
    from digitalmodel.marine_ops.artificial_lift.dynacard.power_consumption import (  # noqa: PLC0415
        calculate_card_area)
    from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.rod_string import (  # noqa: PLC0415
        rod_area, rod_elastic_constant)

    base_ctx = load_from_json_file(root / FIXTURE)
    pos0 = list(base_ctx.surface_card.position)
    load0 = list(base_ctx.surface_card.load)
    if pos0[0] == pos0[-1] and load0[0] == load0[-1]:  # drop an exact closing pair: unique phases
        pos0, load0 = pos0[:-1], load0[:-1]

    def solve(ctx=base_ctx, pos=pos0, load=load0, scale=1.0, visc=VISCOSITY_CP, tubing=TUBING_ID_IN,
              gain=1.0, pscale=1.0):
        """One solve; the periodic stencil gets its repeated endpoint, metrics use unique phases."""
        p = np.append(np.asarray(pos, dtype=float), pos[0]) * pscale
        q = np.append(np.asarray(load, dtype=float), load[0]) * gain
        rods = adapter.rod_string_from_context(ctx)
        damping = estimate_damping_profile(
            rods.diameters, rods.coupling_diameters, rods.lengths, rods.densities,
            ctx.pump.diameter * U.IN2M, tubing * U.IN2M, visc * 1.0e-3) * scale
        card = EverittJenningsSolver(n_nodes=N_NODES).solve(
            p * U.IN2M, q * U.LB2N, rods, ctx.spm, ctx.pump.diameter * U.IN2M, tubing * U.IN2M,
            ctx.fluid_density * U.LBPFT32KGPM3, damping=damping)
        pump = CardData(position=(card.position * U.M2IN)[:-1].tolist(),
                        load=(card.load * U.N2LB)[:-1].tolist())
        surface = CardData(position=list(p[:-1]), load=list(q[:-1]))
        return card, pump, surface, damping

    def metrics(ctx, card, pump, surface):
        fill = calculate_pump_fillage(pump)
        fluid = calculate_fluid_load(pump).fluid_load
        s_stroke = float(np.ptp(surface.position))
        er = [rod_elastic_constant(r.diameter, r.modulus_of_elasticity) * r.length for r in ctx.rod_string]
        stretch = [fluid * e for e in er]
        x_ft = card.simulation.x / U.FT2M
        load_env = card.full_load * U.N2LB
        joint_ft = ctx.rod_string[0].length
        area = [rod_area(r.diameter) for r in ctx.rod_string]
        joint_max = float(np.interp(joint_ft, x_ft, load_env.max(axis=1)))
        joint_min = float(np.interp(joint_ft, x_ft, load_env.min(axis=1)))
        a_surf = calculate_card_area(surface)
        a_pump = calculate_card_area(pump)
        return {
            "surface_stroke_in": s_stroke,
            "pump_stroke_in": fill.gross_stroke,
            "stroke_loss_in": s_stroke - fill.gross_stroke,
            "fluid_load_lbf": fluid,
            "fillage_pct": fill.fillage,
            "max_pump_load_lbf": max(pump.load),
            "min_pump_load_lbf": min(pump.load),
            "static_stretch_in": sum(stretch),
            "static_stretch_by_taper_in": stretch,
            "stretch_share_of_loss_pct": sum(stretch) / (s_stroke - fill.gross_stroke) * 100.0,
            "dynamic_remainder_in": (s_stroke - fill.gross_stroke) - sum(stretch),
            "peak_load_polished_rod_lbf": max(surface.load),
            "min_load_polished_rod_lbf": min(surface.load),
            "peak_stress_top_taper1_psi": max(surface.load) / area[0],
            "min_stress_top_taper1_psi": min(surface.load) / area[0],
            "load_at_joint_max_lbf": joint_max,
            "load_at_joint_min_lbf": joint_min,
            "peak_stress_top_taper2_psi": joint_max / area[1],
            "min_stress_top_taper2_psi": joint_min / area[1],
            "surface_card_area_ft_lbf": a_surf,
            "pump_card_area_ft_lbf": a_pump,
            "damping_share_pct": (a_surf - a_pump) / a_surf * 100.0,
        }

    card, pump, surface, damping = solve()
    base = metrics(base_ctx, card, pump, surface)
    sim = card.simulation
    x_ft = sim.x / U.FT2M
    full_pos = card.full_position * U.M2IN
    full_load = card.full_load * U.N2LB
    joint_ft = base_ctx.rod_string[0].length
    areas = [rod_area(r.diameter) for r in base_ctx.rod_string]
    node_area = [areas[0] if x <= joint_ft + 1e-9 else areas[1] for x in x_ft]
    er = [rod_elastic_constant(r.diameter, r.modulus_of_elasticity) for r in base_ctx.rod_string]

    def static_stroke(depth):
        above1 = min(depth, joint_ft)
        above2 = max(0.0, depth - joint_ft)
        return base["surface_stroke_in"] - base["fluid_load_lbf"] * (er[0] * above1 + er[1] * above2)

    profile = [{
        "depth_ft": round(float(x), 2),
        "stroke_in": round(float(np.ptp(full_pos[i])), 4),
        "static_stroke_in": round(static_stroke(float(x)), 4),
        "max_load_lbf": round(float(full_load[i].max()), 2),
        "min_load_lbf": round(float(full_load[i].min()), 2),
        "max_stress_psi": round(float(full_load[i].max() / node_area[i]), 1),
        "min_stress_psi": round(float(full_load[i].min() / node_area[i]), 1),
        "interface_node": bool(i in set(int(k) for k in getattr(sim, "interfaces", []))),
    } for i, x in enumerate(x_ft)]

    k = int(np.searchsorted(x_ft, joint_ft))
    w = (joint_ft - x_ft[k - 1]) / (x_ft[k] - x_ft[k - 1])
    joint_pos = (1 - w) * full_pos[k - 1] + w * full_pos[k]
    joint_load = (1 - w) * full_load[k - 1] + w * full_load[k]

    def as_card(position, load_values):
        return {"position_in": [round(float(v), 4) for v in position],
                "load_lbf": [round(float(v), 2) for v in load_values]}

    cards = {
        "surface": as_card(surface.position, surface.load),
        "joint": as_card(joint_pos[:-1], joint_load[:-1]),
        "pump": as_card(pump.position, pump.load),
    }
    base["stroke_at_joint_in"] = float(np.ptp(joint_pos))

    sweep = []
    sweep_cards = {}
    for scale in DAMPING_SCALES:
        c2, p2, s2, d2 = solve(scale=scale)
        m = metrics(base_ctx, c2, p2, s2)
        diff = np.asarray(p2.load) - np.asarray(pump.load)
        m["pump_card_change_nrmse_pct"] = float(np.sqrt(np.mean(diff ** 2))
                                                / (base["max_pump_load_lbf"] - base["min_pump_load_lbf"]) * 100)
        m["damping_per_s"] = [float(v) for v in d2]
        m["scale"] = scale
        sweep.append(m)
        if scale in SHOWN_SCALES:
            sweep_cards[str(scale)] = as_card(p2.position, p2.load)

    period = sim.period
    c_min = float(np.min(sim.wave_speed))
    limit = int(math.floor(period * c_min / sim.dx))
    native = len(pos0)
    phase_src = np.arange(native) / native
    courant = []
    for n in (native, 1000, limit, limit + 1, 6400):
        if n == native:
            p, q = pos0, load0
        else:
            ph = np.arange(n) / n
            p = list(np.interp(ph, phase_src, pos0, period=1))
            q = list(np.interp(ph, phase_src, load0, period=1))
        c3, p3, _s3, _d3 = solve(pos=p, load=q)
        courant.append({"samples_per_stroke": n, "courant_number": float(sim.dx / (c_min * period / n)),
                        "max_pump_load_lbf": float(max(p3.load)), "min_pump_load_lbf": float(min(p3.load)),
                        "pump_stroke_in": float(np.ptp(p3.position)), "solver_flags": list(c3.solver_flags)})

    def scaled(field, e):
        rods = []
        for r in base_ctx.rod_string:
            if field == "modulus":
                rods.append(r.model_copy(update={"modulus_of_elasticity": r.modulus_of_elasticity * (1 + e)}))
            elif field == "rod_diameter":
                rods.append(r.model_copy(update={"diameter": r.diameter * (1 + e),
                                                 "weight_per_foot": r.weight_per_foot * (1 + e) ** 2}))
            elif field == "rod_weight":
                rods.append(r.model_copy(update={"weight_per_foot": r.weight_per_foot * (1 + e)}))
            else:
                rods.append(r)
        update = {"rod_string": rods}
        if field == "fluid_density":
            update["fluid_density"] = base_ctx.fluid_density * (1 + e)
        if field == "pumping_speed":
            update["spm"] = base_ctx.spm * (1 + e)
        return base_ctx.model_copy(update=update)

    outputs = ["pump_stroke_in", "static_stretch_in", "max_pump_load_lbf", "fillage_pct",
               "peak_stress_top_taper2_psi", "damping_share_pct"]
    inputs = ["modulus", "rod_diameter", "rod_weight", "fluid_density", "pumping_speed", "viscosity",
              "tubing_id", "load_gain", "position_scale"]
    sensitivity = []
    for name in inputs:
        for step in STEPS:
            pair = {}
            for sign in (1, -1):
                e = sign * step
                kwargs, ctx = {}, base_ctx
                if name in ("modulus", "rod_diameter", "rod_weight", "fluid_density", "pumping_speed"):
                    ctx = scaled(name, e)
                elif name == "viscosity":
                    kwargs["visc"] = VISCOSITY_CP * (1 + e)
                elif name == "tubing_id":
                    kwargs["tubing"] = TUBING_ID_IN * (1 + e)
                elif name == "load_gain":
                    kwargs["gain"] = 1 + e
                else:
                    kwargs["pscale"] = 1 + e
                c4, p4, s4, _d4 = solve(ctx=ctx, **kwargs)
                pair[sign] = metrics(ctx, c4, p4, s4)
            row = {"input": name, "step_pct": round(step * 100), "factors": {}, "plus_pct": {}, "minus_pct": {}}
            for out in outputs:
                ref = base[out]
                row["factors"][out] = round(((pair[1][out] - pair[-1][out]) / ref * 100) / (2 * step * 100), 4)
                # One-sided changes: percent change of the result for the + and - input step.
                row["plus_pct"][out] = round((pair[1][out] / ref - 1) * 100, 4)
                row["minus_pct"][out] = round((pair[-1][out] / ref - 1) * 100, 4)
            sensitivity.append(row)

    # Rod grade and the record's own rod stresses (dynaoutputData.Rods of the well record), and the modified
    # Goodman check on both sets of stresses: allowable SA = (T/4 + M * Smin) * SF; loading = (Smax - Smin) /
    # (SA - Smin), the form the record's RodStressSigmaA and RodStress values follow.
    raw = json.loads((root / FIXTURE).read_text(encoding="utf-8"))
    rec_rods = raw["dynaoutputData"]["Rods"]
    ours = [(base["peak_stress_top_taper1_psi"], base["min_stress_top_taper1_psi"]),
            (base["peak_stress_top_taper2_psi"], base["min_stress_top_taper2_psi"])]
    goodman = []
    for rec, (smax, smin) in zip(rec_rods, ours):
        t, m, sf = rec["MinimumTensile"], rec["M"], rec["SF"]
        sa = (t / 4.0 + m * smin) * sf
        goodman.append({
            "grade": rec["Grade"], "minimum_tensile_psi": t, "goodman_m": m, "service_factor": sf,
            "ours": {"max_psi": smax, "min_psi": smin, "allowable_psi": sa,
                     "loading_pct": (smax - smin) / (sa - smin) * 100.0},
            "record": {"max_psi": rec["RodStressMax"], "min_psi": rec["RodStressMin"],
                       "allowable_psi": rec["RodStressSigmaA"], "loading_pct": rec["RodStress"] * 100.0},
        })
    a2 = rod_area(base_ctx.rod_string[1].diameter)
    rods2 = base_ctx.rod_string[1]
    record_offset = {
        "max_lbf": (rec_rods[1]["RodStressMax"] - base["peak_stress_top_taper2_psi"]) * a2,
        "min_lbf": (rec_rods[1]["RodStressMin"] - base["min_stress_top_taper2_psi"]) * a2,
        # Buoyancy of the 3/4 in rods below the joint on two bases: the solver's (fluid displaced by the rod
        # body) and API RP 11L's (0.128 x SG x weight in air, couplings included).
        "buoyancy_below_solver_lbf": base_ctx.fluid_density * a2 / 144.0 * rods2.length,
        "buoyancy_below_rp11l_lbf": rods2.weight_per_foot * rods2.length * 0.128 * base_ctx.fluid_density / 62.4,
    }
    ref_card = get_expected_downhole_card(raw)
    record_offset["record_pump_card_mean_minus_ours_lbf"] = float(np.mean(ref_card.load) - np.mean(pump.load))
    record_well = {"water_cut_pct": raw["WellTestData"]["WaterCut"],
                   "viscosity_cp": raw["InputParameters"]["Viscosity"],
                   "damping_per_s": raw["dynaoutputData"]["DampingCoefficient"]}

    rods_out = [{"diameter_in": round(r.diameter, 4), "length_ft": round(r.length, 2),
                 "modulus_psi": r.modulus_of_elasticity, "weight_lb_ft": r.weight_per_foot,
                 "area_in2": round(rod_area(r.diameter), 6),
                 "elastic_constant_in_per_lbf_ft": rod_elastic_constant(r.diameter, r.modulus_of_elasticity)}
                for r in base_ctx.rod_string]

    def rounded(d):
        return {k: (round(v, 6) if isinstance(v, float) else [round(x, 6) for x in v] if isinstance(v, list)
                    else v) for k, v in d.items()}

    return {
        "source": f"{BUILDER} --refresh",
        "library_commit": LIBRARY_COMMIT,
        "fixture": FIXTURE,
        "well": {"strokes_per_minute": round(base_ctx.spm, 4), "pump_depth_ft": base_ctx.pump.depth,
                 "plunger_diameter_in": round(base_ctx.pump.diameter, 4),
                 "fluid_density_lb_ft3": round(base_ctx.fluid_density, 3), "rod_string": rods_out,
                 "survey_max_inclination_deg": (max(base_ctx.survey.inclination)
                                                if base_ctx.survey and base_ctx.survey.inclination else None)},
        "settings": {"grid_nodes": N_NODES, "viscosity_cp": VISCOSITY_CP, "tubing_id_in": TUBING_ID_IN,
                     "coupling_od": "1.5 x rod body OD (solver default)", "damping_scales": list(DAMPING_SCALES),
                     "sensitivity_steps_pct": [round(s * 100) for s in STEPS],
                     "estimated_damping_per_s": {"upstroke": [float(v) for v in damping[:2]],
                                                 "downstroke": [float(v) for v in damping[2:]]}},
        "grid": {"dx_ft": float(sim.dx / U.FT2M), "dx_m": float(sim.dx), "period_s": float(period),
                 "wave_speed_m_s": c_min, "wave_speed_ft_s": c_min / U.FT2M, "rod_density_kg_m3":
                 [float(v) for v in sim.densities], "card_samples_per_stroke": native,
                 "courant_limit_samples_per_stroke": limit},
        "key_results": rounded(base),
        "profile": profile,
        "cards": cards,
        "damping_sweep": [rounded(m) for m in sweep],
        "damping_cards": sweep_cards,
        "courant": courant,
        "sensitivity": sensitivity,
        "goodman": goodman,
        "record_offset_top_taper2": record_offset,
        "record_well": record_well,
    }


# ---------------------------------------------------------------- formatting helpers

def esc(text) -> str:
    return html.escape(str(text), quote=True)


def num(v: float, nd: int = 0) -> str:
    s = f"{v:,.{nd}f}"
    if s.lstrip("-").strip("0.,") == "":
        s = s.lstrip("-")
    return s.replace("-", "−")


def sig4(v: float) -> str:
    """At most four significant figures, thousands separated (TW-22)."""
    if v == 0:
        return "0"
    nd = 3 - int(math.floor(math.log10(abs(v))))
    return num(round(v, nd), max(nd, 0))


def blob(commit: str, path) -> str:
    return f"{REPO}/blob/{commit}/{Path(path).as_posix()}"


def nice_step(span: float, target: int = 5) -> float:
    raw = span / max(target, 1)
    mag = 10 ** math.floor(math.log10(raw))
    for m in (1, 2, 2.5, 5, 10):
        if raw <= m * mag:
            return m * mag
    return 10 * mag


def ticks(low: float, high: float, target: int = 5):
    step = nice_step(high - low, target)
    start = math.floor(low / step) * step
    out, v = [], start
    while v < high + step * 0.999:
        out.append(round(v, 10))
        v += step
    return out


def fmt_tick(v: float) -> str:
    return num(v, 0) if abs(v) >= 100 or float(v).is_integer() else num(v, 1)


def chart(series, *, width, height, label, x_title, y_title, y_down=False, hlines=(), closed=False,
          x_floor=None):
    """Lines or closed loops on one linear pair of axes; every series and marker carries a hover title."""
    xs = [x for s in series for x in s["x"]]
    ys = [y for s in series for y in s["y"]]
    x_low = min(xs) if x_floor is None else min(x_floor, min(xs))
    xt = ticks(x_low, max(xs), 5)
    yt = ticks(min(0.0, min(ys)), max(ys), 5)
    left, right, top, bottom = 64, 16, 12, 42
    pw, ph = width - left - right, height - top - bottom

    def px(x):
        return left + (x - xt[0]) / (xt[-1] - xt[0]) * pw

    def py(y):
        f = (y - yt[0]) / (yt[-1] - yt[0])
        return top + (f * ph if y_down else ph - f * ph)

    out = [f'<svg viewBox="0 0 {width} {height}" role="img" aria-label="{esc(label)}" class="chart" '
           f'xmlns="http://www.w3.org/2000/svg">']
    for y in yt:
        out.append(f'<line x1="{left}" x2="{left + pw}" y1="{py(y):.1f}" y2="{py(y):.1f}" class="grid"/>')
        out.append(f'<text x="{left - 6}" y="{py(y) + 4:.1f}" class="tick" text-anchor="end">{fmt_tick(y)}</text>')
    for x in xt:
        out.append(f'<line x1="{px(x):.1f}" x2="{px(x):.1f}" y1="{top}" y2="{top + ph}" class="grid"/>')
        out.append(f'<text x="{px(x):.1f}" y="{top + ph + 16}" class="tick" text-anchor="middle">{fmt_tick(x)}</text>')
    out.append(f'<line x1="{left}" x2="{left + pw}" y1="{top + ph}" y2="{top + ph}" class="axis"/>')
    out.append(f'<line x1="{left}" x2="{left}" y1="{top}" y2="{top + ph}" class="axis"/>')
    out.append(f'<text x="{left + pw / 2:.1f}" y="{height - 6}" class="axis-title" text-anchor="middle">'
               f'{esc(x_title)}</text>')
    out.append(f'<text transform="translate(14 {top + ph / 2:.1f}) rotate(-90)" class="axis-title" '
               f'text-anchor="middle">{esc(y_title)}</text>')
    for y, text in hlines:
        out.append(f'<line x1="{left}" x2="{left + pw}" y1="{py(y):.1f}" y2="{py(y):.1f}" class="marker"/>')
        out.append(f'<text x="{left + pw - 4}" y="{py(y) - 5:.1f}" class="direct" text-anchor="end">{esc(text)}</text>')
    for s in series:
        pts = " ".join(f"{px(x):.1f},{py(y):.1f}" for x, y in zip(s["x"], s["y"]))
        tag = "polygon" if closed else "polyline"
        dash = ' stroke-dasharray="6 4"' if s.get("dash") else ""
        out.append(f'<{tag} points="{pts}" fill="none" stroke="{s["colour"]}" stroke-width="2.2"{dash}>'
                   f'<title>{esc(s["name"])}</title></{tag}>')
        for x, y, tip in s.get("hover", []):
            out.append(f'<circle cx="{px(x):.1f}" cy="{py(y):.1f}" r="5" class="hit" fill="{s["colour"]}">'
                       f'<title>{esc(tip)}</title></circle>')
    out.append("</svg>")
    return "".join(out)


def legend(items) -> str:
    return '<p class="legend">' + "".join(
        f'<span class="key" style="border-color:{c}{";border-top-style:dashed" if d else ""}"></span>{esc(t)}'
        for t, c, d in items) + "</p>"


def caption(notes, title) -> str:
    """Numbered notes first, the title below them (FW-FIG-14)."""
    items = "".join(f"<li>{n}</li>" for n in notes)
    return f'<figcaption><ol class="fig-notes">{items}</ol><p class="fig-title">{title}</p></figcaption>'


def shifted(card):
    low = min(card["position_in"])
    return [x - low for x in card["position_in"]], list(card["load_lbf"])


# ---------------------------------------------------------------- facts (one source for every number)

def facts(r: dict) -> dict:
    k = r["key_results"]
    sweep = {m["scale"]: m for m in r["damping_sweep"]}
    rods = r["well"]["rod_string"]
    total_len = sum(x["length_ft"] for x in rods)
    s2 = k["static_stretch_by_taper_in"][1]
    courant = {c["samples_per_stroke"]: c for c in r["courant"]}
    limit = r["grid"]["courant_limit_samples_per_stroke"]
    sens = {(row["input"], row["step_pct"]): row for row in r["sensitivity"]}
    ten = sweep[10.0]
    zero = sweep[0.0]
    g1, g2 = r["goodman"]
    off = r["record_offset_top_taper2"]
    zero_diff = abs(zero["surface_card_area_ft_lbf"] - zero["pump_card_area_ft_lbf"]) / zero["surface_card_area_ft_lbf"] * 100
    f = {
        "r": r, "k": k, "sweep": sweep, "rods": rods, "courant": courant, "sens": sens, "g1": g1, "g2": g2,
        "off": off,
        "S": num(k["surface_stroke_in"], 1), "Sp": num(k["pump_stroke_in"], 1),
        "S2": num(k["surface_stroke_in"], 2), "Sp2": num(k["pump_stroke_in"], 2),
        "loss": num(k["stroke_loss_in"], 1), "loss2": num(k["stroke_loss_in"], 2),
        "Fo": num(k["fluid_load_lbf"], 0), "stretch": num(k["static_stretch_in"], 1),
        "stretch2": num(k["static_stretch_in"], 2),
        "share": num(k["stretch_share_of_loss_pct"], 1),
        "share_about": num(round(k["stretch_share_of_loss_pct"] / 10) * 10, 0),
        "dyn": num(k["dynamic_remainder_in"], 1), "dyn0": num(zero["dynamic_remainder_in"], 2),
        "dyn2": num(k["dynamic_remainder_in"], 2),
        "st1": num(k["static_stretch_by_taper_in"][0], 1), "st2": num(s2, 1),
        "st2_share": num(s2 / k["static_stretch_in"] * 100, 1),
        "len2_share": num(rods[1]["length_ft"] / total_len * 100, 1),
        "joint_ft": num(rods[0]["length_ft"], 0), "rod_ft": num(total_len, 0),
        "pump_depth": num(r["well"]["pump_depth_ft"], 0),
        "S_joint": num(k["stroke_at_joint_in"], 1),
        "sig1": sig4(k["peak_stress_top_taper1_psi"]),
        "sig2": sig4(k["peak_stress_top_taper2_psi"]),
        "sig_up": num((k["peak_stress_top_taper2_psi"] / k["peak_stress_top_taper1_psi"] - 1) * 100, 1),
        "rng1": sig4(k["peak_stress_top_taper1_psi"] - k["min_stress_top_taper1_psi"]),
        "rng2": sig4(k["peak_stress_top_taper2_psi"] - k["min_stress_top_taper2_psi"]),
        "PPRL": num(k["peak_load_polished_rod_lbf"], 0), "MPRL": num(k["min_load_polished_rod_lbf"], 0),
        "joint_max": num(k["load_at_joint_max_lbf"], 0), "joint_min": num(k["load_at_joint_min_lbf"], 0),
        "A_surf": num(k["surface_card_area_ft_lbf"], 0), "A_pump": num(k["pump_card_area_ft_lbf"], 0),
        "damp_share": num(k["damping_share_pct"], 1),
        # Rounded up to the stated precision, so "within" holds.
        "zero_diff": num(math.ceil(zero_diff * 1000) / 1000, 3),
        "ten_nrmse": num(ten["pump_card_change_nrmse_pct"], 1),
        "ten_max": num(abs(ten["max_pump_load_lbf"] / k["max_pump_load_lbf"] - 1) * 100, 2),
        "ten_fill": num(ten["fillage_pct"], 1), "fill": num(k["fillage_pct"], 1),
        "ten_min": num(ten["min_pump_load_lbf"], 0), "min_pump": num(k["min_pump_load_lbf"], 0),
        "ten_area": num((1 - ten["pump_card_area_ft_lbf"] / k["pump_card_area_ft_lbf"]) * 100, 1),
        "ten_stroke": num(abs(ten["pump_stroke_in"] - k["pump_stroke_in"]), 2),
        # The load is continuous across the joint, so the stress steps by the ratio of the two rod areas.
        "joint_step": num((rods[0]["area_in2"] / rods[1]["area_in2"] - 1) * 100, 1),
        "grade": g2["grade"], "tensile": num(g2["minimum_tensile_psi"], 0), "sf": g2["service_factor"],
        "sa2": sig4(g2["ours"]["allowable_psi"]), "load2": num(g2["ours"]["loading_pct"], 0),
        "load2_1": num(g2["ours"]["loading_pct"], 1), "load1": num(g1["ours"]["loading_pct"], 0),
        "load1_1": num(g1["ours"]["loading_pct"], 1),
        "rec_sig2": sig4(g2["record"]["max_psi"]), "rec_min2": sig4(g2["record"]["min_psi"]),
        "rec_load2": num(g2["record"]["loading_pct"], 0), "rec_load2_1": num(g2["record"]["loading_pct"], 1),
        "rec_dpsi": num(round((g2["record"]["max_psi"] - g2["ours"]["max_psi"] + g2["record"]["min_psi"]
                               - g2["ours"]["min_psi"]) / 2, -1), 0),
        "off_lbf": num((off["max_lbf"] + off["min_lbf"]) / 2, 0),
        "buoy_solver": num(off["buoyancy_below_solver_lbf"], 0), "buoy_rp": num(off["buoyancy_below_rp11l_lbf"], 0),
        "pump_mean_diff": num(abs(off["record_pump_card_mean_minus_ours_lbf"]), 0),
        "water_cut": num(r["record_well"]["water_cut_pct"], 0),
        "limit": num(limit, 0), "native": num(r["grid"]["card_samples_per_stroke"], 0),
        "dx_ft": num(r["grid"]["dx_ft"], 2),
        "c_fts_about": num(round(r["grid"]["wave_speed_ft_s"], -2), 0),
        "period": num(r["grid"]["period_s"], 2), "nodes": r["settings"]["grid_nodes"],
        "spm": num(r["well"]["strokes_per_minute"], 2),
        "at_limit_max": num(courant[limit]["max_pump_load_lbf"], 0),
        "over_max": courant[6400]["max_pump_load_lbf"],
        "next_min": num(courant[limit + 1]["min_pump_load_lbf"], 0), "next_n": num(limit + 1, 0),
        # Part 1 wording and case: the +1 % rod diameter and +1 % load-cell gain, one-sided.
        "p_diam": num(abs(sens[("rod_diameter", 1)]["plus_pct"]["max_pump_load_lbf"]), 1),
        "p_gain": num(sens[("load_gain", 1)]["plus_pct"]["max_pump_load_lbf"], 1),
        "p_pos": num(sens[("position_scale", 1)]["plus_pct"]["pump_stroke_in"], 1),
        "p_diam_sig": num(abs(sens[("rod_diameter", 1)]["plus_pct"]["peak_stress_top_taper2_psi"]), 1),
        "p_gain_sig": num(sens[("load_gain", 1)]["plus_pct"]["peak_stress_top_taper2_psi"], 1),
    }
    return f


def sci(v: float) -> str:
    e = int(math.floor(math.log10(abs(v))))
    return f"{v / 10 ** e:.1f} × 10<sup>{e}</sup>".replace("-", "−")


def revision_lower() -> str:
    """'revision 2, 19 Nov 2026': only the first letter is lowered."""
    return REVISION[0].lower() + REVISION[1:]


# ---------------------------------------------------------------- CSV data behind the figures

def csv_text(header, rows) -> str:
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\n")
    w.writerow(header)
    w.writerows(rows)
    return buf.getvalue()


def csv_outputs(r: dict) -> dict:
    cards = r["cards"]
    rows = []
    for name, depth in (("surface", 0.0), ("joint", r["well"]["rod_string"][0]["length_ft"]),
                        ("pump", sum(x["length_ft"] for x in r["well"]["rod_string"]))):
        for i, (x, y) in enumerate(zip(cards[name]["position_in"], cards[name]["load_lbf"])):
            rows.append([name, depth, i, x, y])
    prof = [[p["depth_ft"], p["stroke_in"], p["static_stroke_in"], p["max_load_lbf"], p["min_load_lbf"],
             p["max_stress_psi"], p["min_stress_psi"], int(p["interface_node"])] for p in r["profile"]]
    damp = []
    for scale, card in r["damping_cards"].items():
        for i, (x, y) in enumerate(zip(card["position_in"], card["load_lbf"])):
            damp.append([scale, i, x, y])
    sens = [[row["input"], row["step_pct"], *row["factors"].values()] for row in r["sensitivity"]]
    first = r["sensitivity"][0]["factors"].keys()
    return {
        CSV_FILES["cards"]: csv_text(["card", "depth_ft", "sample", "position_in", "load_lbf"], rows),
        CSV_FILES["profile"]: csv_text(["depth_ft", "stroke_in", "static_stroke_in", "max_load_lbf",
                                        "min_load_lbf", "max_stress_psi", "min_stress_psi",
                                        "taper_joint_grid_point"], prof),
        CSV_FILES["damping"]: csv_text(["damping_scale", "sample", "position_in", "load_lbf"], damp),
        CSV_FILES["sensitivity"]: csv_text(["input", "step_pct", *first], sens),
    }


# ---------------------------------------------------------------- figures

def figure_cards(f) -> str:
    c = f["r"]["cards"]
    joint = f["rods"][0]["length_ft"]
    rod_end = sum(x["length_ft"] for x in f["rods"])
    series = []
    for key, colour, name, depth in (("surface", BLUE, "Surface card, measured", 0),
                                     ("joint", TEAL, "Card at the taper joint, computed", joint),
                                     ("pump", ORANGE, "Pump card, computed at the end of the rods", rod_end)):
        x, y = shifted(c[key])
        tip = (f"{name}: depth {num(depth)} ft, stroke {num(max(x) - min(x), 1)} in, "
               f"peak load {num(max(y))} lbf, minimum load {num(min(y))} lbf")
        i = max(range(len(y)), key=lambda j: y[j])
        series.append({"x": x, "y": y, "colour": colour, "name": tip, "hover": [(x[i], y[i], tip)]})
    svg = chart(series, width=720, height=400, closed=True, label="Load against position for three cards",
                x_title="Position from the bottom of each card's stroke (in)", y_title="Load (lbf)", x_floor=0)
    return ("<figure id=\"figure-1\">" + svg + legend([("Surface card, measured (0 ft)", BLUE, False),
                                                        (f"Taper joint, computed ({num(joint)} ft)", TEAL, False),
                                                        (f"Pump card, computed ({num(rod_end)} ft)", ORANGE, False)])
            + caption([
                "Each card is drawn from the bottom of its own stroke. Loads are the rod load at that depth, "
                "rod weight below it included, so each card sits lower by the weight of the rods above it and is "
                "shorter by their stretch.",
                f"The pump card is computed at the end of the listed rods, {f['rod_ft']} ft; the well record puts the "
                f"pump at {f['pump_depth']} ft (ASSUMED - to be confirmed, Appendix A).",
                f"Data: <a href=\"{blob(DETAIL_COMMIT, CSV_FILES['cards'])}\">figure-1-cards-at-depth.csv</a>."],
                f"Figure 1. The card down the rod string of the example well: the stroke falls from {f['S']} in "
                f"at the surface to {f['S_joint']} in at the taper joint and {f['Sp']} in at the pump. Point at a card, "
                "or its peak marker, to show its depth, stroke and loads.")
            + "</figure>")


def figure_stroke(f) -> str:
    p = f["r"]["profile"]
    depth = [q["depth_ft"] for q in p]
    fd = [q["stroke_in"] for q in p]
    st = [q["static_stroke_in"] for q in p]
    # Markers only at the surface, the taper joint (interpolated to the joint depth) and the pump (TW-100).
    joint = f["rods"][0]["length_ft"]
    i = next(n for n, d in enumerate(depth) if d >= joint)
    w = (joint - depth[i - 1]) / (depth[i] - depth[i - 1])
    fd_j = fd[i - 1] + w * (fd[i] - fd[i - 1])
    st_j = st[i - 1] + w * (st[i] - st[i - 1])
    hover_fd = [(fd[0], 0.0, f"Wave equation: {num(fd[0], 1)} in at the surface"),
                (fd_j, joint, f"Wave equation: {num(fd_j, 1)} in at the taper joint, {f['joint_ft']} ft"),
                (fd[-1], depth[-1], f"Wave equation: {num(fd[-1], 1)} in at the pump")]
    hover_st = [(st[0], 0.0, f"Static stretch only: {num(st[0], 1)} in at the surface"),
                (st_j, joint, f"Static stretch only: {num(st_j, 1)} in at the taper joint, {f['joint_ft']} ft"),
                (st[-1], depth[-1], f"Static stretch only: {num(st[-1], 1)} in at the pump")]
    svg = chart([{"x": fd, "y": depth, "colour": ORANGE, "name": "Stroke from the wave equation", "hover": hover_fd},
                 {"x": st, "y": depth, "colour": NAVY, "name": "Stroke with static stretch only", "dash": True,
                  "hover": hover_st}],
                width=720, height=380, y_down=True, label="Stroke against depth down the rod string",
                x_title="Stroke (in)", y_title="Depth (ft)",
                hlines=[(joint, f"Taper joint, {f['joint_ft']} ft")])
    return ("<figure id=\"figure-2\">" + svg
            + legend([("Wave equation (finite difference)", ORANGE, False), ("Static stretch only", NAVY, True)])
            + caption([
                f"Static stretch: the fluid load of {f['Fo']} lbf times the rod's elastic constant, 12/(A E) per "
                "foot, summed over the rods above each depth [5].",
                f"Data: <a href=\"{blob(DETAIL_COMMIT, CSV_FILES['profile'])}\">figure-2-3-depth-profile.csv</a>."],
                f"Figure 2. Rod stretch takes about {f['share_about']} % of the lost stroke: stroke against depth, "
                "wave equation and static stretch. Point at a line or marker to show the stroke at that depth.")
            + "</figure>")


def figure_stress(f) -> str:
    p = f["r"]["profile"]
    depth = [q["depth_ft"] for q in p]
    smax = [q["max_stress_psi"] for q in p]
    smin = [q["min_stress_psi"] for q in p]
    k = f["k"]
    joint = f["rods"][0]["length_ft"]
    hov_max = [(k["peak_stress_top_taper1_psi"], 0.0, f"Peak stress at the polished rod: {f['sig1']} psi"),
               (k["peak_stress_top_taper2_psi"], joint, f"Peak stress at the top of the 3/4 in rods: {f['sig2']} psi")]
    hov_min = [(k["min_stress_top_taper1_psi"], 0.0,
                f"Minimum stress at the polished rod: {sig4(k['min_stress_top_taper1_psi'])} psi"),
               (k["min_stress_top_taper2_psi"], joint,
                f"Minimum stress at the top of the 3/4 in rods: {sig4(k['min_stress_top_taper2_psi'])} psi")]
    svg = chart([{"x": smax, "y": depth, "colour": ORANGE, "name": "Peak stress over the stroke", "hover": hov_max},
                 {"x": smin, "y": depth, "colour": BLUE, "name": "Minimum stress over the stroke", "hover": hov_min}],
                width=720, height=380, y_down=True, label="Rod stress against depth", x_title="Rod stress (psi)",
                y_title="Depth (ft)", hlines=[(joint, f"Taper joint, {f['joint_ft']} ft")], x_floor=0)
    return ("<figure id=\"figure-3\">" + svg
            + legend([("Peak stress over the stroke", ORANGE, False), ("Minimum stress over the stroke", BLUE, False)])
            + caption([
                "Stress is the rod load at each depth over the rod body area; the grid point at the taper joint "
                "is averaged between its neighbours by the solver, and the stress at the joint is interpolated "
                f"to {f['joint_ft']} ft.",
                f"The well record's own calculation gives {f['rec_sig2']} psi peak and {f['rec_min2']} psi minimum at "
                f"the top of the 3/4 in rods, about {f['rec_dpsi']} psi above these values (Appendix A).",
                f"Data: <a href=\"{blob(DETAIL_COMMIT, CSV_FILES['profile'])}\">figure-2-3-depth-profile.csv</a>."],
                f"Figure 3. The peak stress is highest at the top of the 3/4 in rods, {f['sig_up']} % above the polished "
                "rod: rod stress against depth. Point at a marker at the polished rod or the taper joint to show its "
                "stress.")
            + "</figure>")


def figure_damping(f) -> str:
    series = []
    # The no-damping card nearly coincides with the estimate's, so it is drawn last, dashed, to stay visible.
    names = {"1.0": ("Estimated damping", ORANGE, False), "10.0": ("Ten times the estimate", NAVY, False),
             "0.0": ("No damping", GREY, True)}
    for key, (name, colour, dash) in names.items():
        card = f["r"]["damping_cards"][key]
        x, y = shifted(card)
        m = f["sweep"][float(key)]
        tip = (f"{name}: maximum {num(m['max_pump_load_lbf'])} lbf, minimum {num(m['min_pump_load_lbf'])} lbf, "
               f"fillage {num(m['fillage_pct'], 1)} %, card area {num(m['pump_card_area_ft_lbf'])} ft-lbf")
        i = min(range(len(y)), key=lambda j: y[j])
        series.append({"x": x, "y": y, "colour": colour, "name": tip, "dash": dash, "hover": [(x[i], y[i], tip)]})
    svg = chart(series, width=720, height=380, closed=True, label="Pump cards at three damping levels",
                x_title="Pump position from the bottom of the stroke (in)", y_title="Pump load (lbf)", x_floor=0)
    return ("<figure id=\"figure-4\">" + svg
            + legend([("No damping", GREY, True), ("Estimated damping", ORANGE, False),
                      ("Ten times the estimate", NAVY, False)])
            + caption([
                f"Estimated damping: from the viscous drag of the rods in the tubing at an assumed {num(VISCOSITY_CP)} cP "
                f"and a {TUBING_ID_IN} in tubing inside diameter. The no-damping card (dashed) lies almost on the "
                "estimate's card.",
                f"The well produces {f['water_cut']} % water, so a viscosity near water's, about 1 cP or less, is more "
                "realistic than 10 cP. The library raises any value at or below 1 cP to 10 cP, an open defect, so "
                "10 cP is kept; the cards from zero to ten times the estimate cover that range.",
                f"Data: <a href=\"{blob(DETAIL_COMMIT, CSV_FILES['damping'])}\">figure-4-damping-cards.csv</a>."],
                f"Figure 4. Ten times the damping moves the pump card by {f['ten_nrmse']} % of its load range: the "
                "pump card at three damping levels. Point at a card, or the marker at its lowest load, to show its loads, "
                "fillage and area.")
            + "</figure>")


INFOGRAPHIC = [
    ("Objective", "See what the rod string does to the surface card, and how the pump card is recovered from it."),
    ("Starting point", "Between the surface card and the pump sit thousands of feet of rods that stretch, carry "
                       "stress waves and lose energy to the fluid on every stroke."),
    ("What we did", "Followed the stroke, load and stress down the rods of a public example well with the wave "
                    "equation, and tested how much the damping estimate matters."),
    ("What we found", "About {share_about} % of the lost stroke is plain rod stretch. The top of the thinner rods is "
                      "the most stressed point. Ten times the damping moves the pump card by {ten_nrmse} % of its load "
                      "range."),
    ("What you can do", "Check the stretch and the rod stresses by hand from the computed pump card and load "
                        "profile, and rerun the analysis on your own well with the reusable prompt."),
]


def infographic_panels(f) -> list:
    return [(label, text.format(**f)) for label, text in INFOGRAPHIC]


def infographic_html(f) -> str:
    rows = "".join(f'<div class="ig-row{" ig-last" if i == 4 else ""}"><p class="ig-label">{esc(label)}</p>'
                   f'<p class="ig-text">{esc(text)}</p></div>'
                   for i, (label, text) in enumerate(infographic_panels(f)))
    return ('<figure class="infographic" aria-label="Objective infographic">' + rows
            + '<p class="ig-source">Source: AceEngineer rod string analysis, digitalmodel library, '
            f'{LIBRARY_DATE}.</p></figure>')


# ---------------------------------------------------------------- article

def body(f) -> str:
    joint_ft, rod_ft = f["joint_ft"], f["rod_ft"]
    lib = blob(LIBRARY_COMMIT, "src/digitalmodel/marine_ops/artificial_lift/dynacard/everitt_jennings/solver.py")
    parts = [
        '<p class="label">Energy Street · Rod string mechanics</p>',
        f"<h1>{esc(ARTICLE_TITLE)}</h1>",
        '<blockquote class="takeaway">On a public example well, most of the lost stroke is plain rod stretch, the '
        "top of the thinner rods is the most stressed point, and the rod data and load-cell calibration, not the "
        "damping, decide the pump card.</blockquote>",
        '<p class="ig-intro">At a glance: the objective, what we did and what we found.</p>',
        infographic_html(f),
        "<p>A rod pump is diagnosed from the surface card, but the pump works at the far end of thousands of feet "
        "of steel rod. The rods stretch under the fluid load, carry stress waves at the speed of sound in steel "
        "and lose energy to the fluid around them on every stroke. The card data contains all of this "
        "information, so the rods have to be taken out of the card before the pump can be read. To keep the "
        "problem simple, this article takes the rod string on its own.</p>",
        "<p>This is the second article of our rod pump series. On the series' public example well, we follow the "
        "stroke, the load and the stress down the rods, test how much the damping estimate matters and show where "
        "the calculation breaks down. You get the method, the public data and a reusable prompt to rerun it. The "
        "pumping unit and other component anomalies are the subjects of later articles in this series, and "
        "practical diagnostics and repairs follow the engineering.</p>",
        "<ul><li>Check how much of the stroke the rods take, and in which taper.</li>"
        "<li>Find the most stressed point of the rods and its margin to the allowable.</li>"
        "<li>Reusable prompt to try a public example well or perform own data custom investigations.</li></ul>",
        f"<p>Based on public data, Figure 1 shows the measured surface card of the example well and the cards "
        f"computed from it at the taper joint, {joint_ft} ft down, and at the end of the rods, {rod_ft} ft down.</p>",
        figure_cards(f),
        '<h2 id="key-conclusions">Key conclusions</h2>',
        "<p>The key conclusions for the example well are as follows:</p><ul>"
        f"<li>Rod stretch takes about {f['share_about']} % of the stroke lost between the surface ({f['S']} in) and "
        f"the pump ({f['Sp']} in); the 3/4 in rods give {f['st2_share']} % of it.</li>"
        f"<li>The top of the 3/4 in rods is the most stressed point: {f['sig2']} psi peak, {f['sig_up']} % above "
        f"the polished rod, using about {f['load2']} % of the Grade {f['grade']} allowable stress range.</li>"
        f"<li>Damping hardly changes this pump card: ten times the estimate moves it by {f['ten_nrmse']} % of its "
        "load range.</li>"
        f"<li>A 1 % larger rod diameter lowers the maximum pump load by about {f['p_diam']} %, and a 1 % load-cell "
        f"gain error raises it by about {f['p_gain']} % (Appendix A).</li></ul>",
        '<h2 id="stroke">Where the stroke goes</h2>',
        f"<p>When the traveling valve closes, the fluid load moves from the tubing to the rods, and the rods "
        f"stretch before the plunger moves. A rod stretches by its load times its length over its area and "
        f"modulus [5]. Under the {f['Fo']} lbf fluid load of the computed pump card, the two tapers stretch "
        f"{f['st1']} in and {f['st2']} in, {f['stretch']} in in all, against {f['loss']} in of stroke lost. "
        "Figure 2 compares the stroke down the rods with static stretch alone.</p>",
        figure_stroke(f),
        f"<p>The lines end {f['dyn']} in apart at the pump, and still {f['dyn0']} in with damping set to zero: "
        "the gap is the rods' motion, which the static formula leaves out.</p>",
        '<h2 id="stress">Load and stress down the rods</h2>',
        "<p>Each foot of rod carries the load below it, so the load falls with depth; the stress is that load over "
        "the rod's area. Figure 3 shows the stress over the stroke at every depth.</p>",
        figure_stress(f),
        f"<p>The peak load falls from {f['PPRL']} lbf at the polished rod to {f['joint_max']} lbf at the taper "
        f"joint, but there it acts on the smaller 3/4 in rods, so the peak stress steps up {f['joint_step']} % to "
        f"{f['sig2']} psi. The well record gives Grade {f['grade']} rods, minimum tensile strength {f['tensile']} "
        f"psi. On the modified Goodman diagram [6] at a service factor of {f['sf']}, the allowable stress there is "
        f"about {f['sa2']} psi, and the top of the 3/4 in rods uses about {f['load2']} % of its allowable stress "
        f"range, the 7/8 in rods about {f['load1']} %.</p>",
        '<h2 id="damping">Damping and the pump card</h2>',
        f"<p>Damping is the energy the rods lose to the fluid, and it makes the pump card's area smaller than the "
        f"surface card's. With damping set to zero, the two areas agree within {f['zero_diff']} %, an energy check "
        f"on the calculation. The library's estimate, from the drag of the rods and couplings in the tubing, takes "
        f"{f['damp_share']} % of the work per stroke. Figure 4 shows the pump card at three damping levels.</p>",
        figure_damping(f),
        f"<p>Ten times the damping raises the bottom of the card from {f['min_pump']} lbf to {f['ten_min']} lbf; "
        f"the maximum pump load changes by {f['ten_max']} % and the fillage from {f['fill']} % to "
        f"{f['ten_fill']} %.</p>",
        '<h2 id="method">How the pump card is recovered</h2>',
        f"<p>The finite-difference method marches the damped wave equation down the rods, one grid point at a "
        f"time, from the measured position and load at the polished rod [2]. It is stable only while a stress "
        f"wave, at about {f['c_fts_about']} ft/s in steel [3], takes at least one time step of the card to cross "
        f"one grid step [4]: on this well's {f['nodes']}-point grid, at most {f['limit']} samples a stroke, against "
        f"the {f['native']} of the example card. One sample over the limit, the minimum pump load reaches "
        f"{f['next_min']} lbf with no flag; the library check is in review, so a high-rate card should be "
        "resampled first (Appendix C).</p>",
        f'<p><a href="{blob(DETAIL_COMMIT, PAGES["readme"])}">Read the full study</a> (detail pages and data, '
        f"{REVISION.split(', ')[0].lower()})</p>",
        '<p><a href="#appendix-d">To try it out or adapt the analysis, see reusable prompt.</a></p>',
        "<p>The work leaves open the damping of a real well, because the estimate has not yet been calibrated "
        "against a measured pump card.</p>",
        f"<p>{esc(CLOSING)}</p>",
        f'<p class="signoff">Prepared: Analyst Deckhand. Checked: Editor Deckhand. Approved: Vamsee Achanta. '
        f"{REVISION}.</p>",
        '<h2 id="references">References</h2><ol class="refs">'
        '<li>Gibbs, S. G. (1963). Predicting the Behavior of Sucker-Rod Pumping Systems. SPE-588-PA. '
        '<a href="https://doi.org/10.2118/588-PA">doi:10.2118/588-PA</a>.</li>'
        "<li>Everitt, T. A. and Jennings, J. W. (1992). An Improved Finite-Difference Calculation of Downhole "
        "Dynamometer Cards for Sucker-Rod Pumps. SPE-18189-PA. "
        '<a href="https://doi.org/10.2118/18189-PA">doi:10.2118/18189-PA</a>.</li>'
        "<li>Bower, A. F. Applied Mechanics of Solids, online edition, section 4.3.3, 1-D bar subjected to end "
        'loading: the wave speed of an elastic bar. <a href="https://solidmechanics.org/Text/Chapter4_3/Chapter4_3.php">'
        "solidmechanics.org</a>.</li>"
        "<li>Courant, R., Friedrichs, K. and Lewy, H. (1928). Über die partiellen Differenzengleichungen der "
        "mathematischen Physik. Mathematische Annalen 100, 32–74. "
        '<a href="https://doi.org/10.1007/BF01448839">doi:10.1007/BF01448839</a>.</li>'
        "<li>Hibbeler, R. C. Mechanics of Materials. Pearson. Chapter 4, Axial load: elastic deformation of an "
        "axially loaded member, δ = PL/(AE). Edition to be confirmed.</li>"
        "<li>American Petroleum Institute. API RP 11BR, Recommended Practice for the Care and Handling of Sucker "
        "Rods: the modified Goodman diagram. Edition to be confirmed.</li>"
        f'<li>digitalmodel library, MIT licence, version of {LIBRARY_DATE}. '
        f'<a href="{REPO}/tree/{LIBRARY_COMMIT}/">github.com/vamseeachanta/digitalmodel</a>; finite-difference '
        f'solver: <a href="{lib}">everitt_jennings/solver.py</a>.</li></ol>',
    ]
    return "\n".join(parts)


def appendices(f) -> str:
    def appx(aid, title, opening, bullets, key, text):
        items = "".join(f"<li>{b}</li>" for b in bullets)
        return (f'<h2 id="{aid}">{esc(title)}</h2><p>{opening}</p><ul>{items}</ul>'
                f'<p>{text}: <a href="{blob(DETAIL_COMMIT, PAGES[key])}">digitalmodel on GitHub, '
                f"{PAGES[key].name}</a>.</p>")

    return "\n".join([
        appx("appendix-a", "Appendix A Example well, stretch, stress and sensitivity",
             "The example well and the stretch and stress results are summarised in this appendix.", [
                 f"The well is one anonymised field well in the digitalmodel library: a 1.500 in plunger on "
                 f"{f['joint_ft']} ft of 7/8 in and 3,350 ft of 3/4 in Grade {f['grade']} steel rods, pumped at "
                 f"{f['spm']} strokes a minute; its survey is vertical. The end of the listed rods, {f['rod_ft']} ft, "
                 f"is taken as the pump, which the record puts at {f['pump_depth']} ft, and the tubing is taken as "
                 "anchored (both ASSUMED - to be confirmed).",
                 f"The record's stresses at the top of the 3/4 in rods are about {f['rec_dpsi']} psi above ours, peak "
                 f"and minimum alike: a constant {f['off_lbf']} lbf of load, close to the buoyancy of the 3/4 in rods "
                 f"below the joint ({f['buoy_solver']} to {f['buoy_rp']} lbf), which our calculation deducts.",
                 f"A 1 % larger rod diameter lowers the maximum pump load by about {f['p_diam']} % and the peak "
                 f"stress at the top of the 3/4 in rods by about {f['p_diam_sig']} %; a 1 % load-cell gain error "
                 f"raises them by about {f['p_gain']} % and {f['p_gain_sig']} %. Factors at ±1 % and ±5 % "
                 "are given for nine inputs."],
             "stroke", "Inputs, results, the Goodman check, sensitivity and assumptions in full"),
        appx("appendix-b", "Appendix B Damping",
             "The key conclusion of the damping study is given in this appendix.", [
                 f"With no damping the pump card's area equals the surface card's within {f['zero_diff']} %; the "
                 f"estimated damping takes {f['damp_share']} % of the work per stroke.",
                 f"For this well, across a ten-fold range of damping, the maximum pump load changes by "
                 f"{f['ten_max']} % and the stroke by {f['ten_stroke']} in; the damping changes the card's area and "
                 "the bottom of the card.",
                 "The estimate scales with the assumed viscosity, which is not calibrated; a heavier oil or a "
                 "measured pump card can change this conclusion."],
             "damping", "Damping study in full"),
        appx("appendix-c", "Appendix C Method and its limits",
             "How the pump card is recovered, and where the method breaks down, is summarised in this appendix.", [
                 "The finite-difference method marches the damped wave equation down the rods from the surface card "
                 "[2]; the frequency-domain method of Gibbs [1] is the library's second method.",
                 f"The march is stable up to {f['limit']} samples a stroke on this well's {f['nodes']}-point grid; "
                 "above it the pump card grows without bound, and the library check is in review.",
                 "The well is treated as vertical; the tubing is taken as anchored (ASSUMED - to be confirmed); "
                 "rod-on-tubing friction is not modelled."],
             "method", "Method, stability limit and open items in full"),
        f'<h2 id="appendix-d">Appendix D Reusable prompts</h2><ul>'
        f"<li>Rebuild this analysis from public code with your own assistant, check it against the published "
        f"values, then run it on a surface card of your own well.</li>"
        f'<li>Prompts, data and check values: <a href="{blob(DETAIL_COMMIT, PAGES["prompts"])}">digitalmodel on '
        f"GitHub, reusable-prompts.md</a>.</li></ul>"
        f'<p>Public data set: the example well, file <a href="{blob(LIBRARY_COMMIT, FIXTURE)}">cleansed_well_001.json'
        f"</a> of the digitalmodel library, MIT licence, version of {LIBRARY_DATE}.</p>",
    ])


CSS = """
:root{--ink:#0b0b0b;--ink2:#52514e;--muted:#8a8984;--line:#e4e3de;--surface:#fcfcfb;--navy:%s;--teal:%s}
*{box-sizing:border-box}
body{margin:0;background:#fff;color:var(--ink);font:16px/1.6 Georgia,'Times New Roman',serif}
.page{max-width:1180px;margin:0 auto;padding:24px 20px 60px;display:grid;grid-template-columns:minmax(0,760px) 220px;gap:40px;justify-content:center}
main{min-width:0;grid-column:1;grid-row:1}
.contents{grid-column:2;grid-row:1;position:sticky;top:20px;align-self:start;font:14px/1.5 system-ui,sans-serif;color:var(--ink2);border-left:2px solid var(--line);padding-left:14px}
.contents p{margin:0 0 6px;font-weight:600}.contents ol{margin:0;padding-left:18px}
@media(max-width:1060px){.page{display:block;max-width:800px}.contents{position:static;margin-bottom:16px}}
.label{font:600 13px system-ui,sans-serif;letter-spacing:.04em;text-transform:uppercase;color:var(--teal);margin:0}
h1{font-size:30px;line-height:1.25;margin:6px 0 16px;color:var(--navy)}
h2{font-size:22px;color:var(--navy);margin:36px 0 10px}
blockquote.takeaway{margin:0 0 20px;padding:12px 18px;border-left:4px solid var(--teal);background:#f3f8f8;font-size:17px}
figure{margin:18px 0}figcaption{font:13px/1.45 system-ui,sans-serif;color:var(--ink2);margin-top:6px}
figcaption .fig-notes{margin:6px 0 2px;padding-left:18px}figcaption .fig-title{margin:2px 0 0}
.chart{width:100%%;height:auto;display:block;background:var(--surface)}
.chart .grid{stroke:var(--line);stroke-width:1}.chart .axis{stroke:var(--muted);stroke-width:1}
.chart .marker{stroke:var(--muted);stroke-width:1;stroke-dasharray:3 3}
.chart .tick{font:11px system-ui,sans-serif;fill:var(--ink2)}.chart .axis-title{font:12px system-ui,sans-serif;fill:var(--ink2)}
.chart .direct{font:12px system-ui,sans-serif;fill:var(--ink2)}
.chart .hit{opacity:.85}.chart .hit:hover{opacity:1;stroke:#0b0b0b;stroke-width:1.5}
.chart polyline:hover,.chart polygon:hover{stroke-width:3.5}
.legend{font:13px system-ui,sans-serif;color:var(--ink2);margin-top:4px}
.key{display:inline-block;width:20px;height:0;border-top:3px solid;vertical-align:middle;margin:0 6px 0 12px}
.key:first-child{margin-left:0}
figure.infographic{border:1px solid var(--line);border-radius:6px;overflow:hidden;font-family:system-ui,sans-serif}
.ig-intro{font:14px system-ui,sans-serif;color:var(--ink2);margin:0 0 6px}
.ig-row{border-left:6px solid var(--navy);background:#eef3f8;padding:8px 14px}.ig-row.ig-last{border-left-color:var(--teal);background:#e3f2f1}
.ig-label{margin:0;font:600 12px system-ui,sans-serif;letter-spacing:.12em;text-transform:uppercase;color:var(--teal)}
.ig-text{margin:2px 0 0;font-size:17px;color:var(--navy)}
.ig-source{margin:0;padding:8px 14px;font-size:12px;color:var(--muted)}
.signoff{font:12px system-ui,sans-serif;color:var(--muted)}
ol.refs{font-size:14px}
@media print{.contents{display:none}.page{display:block}}
""" % (NAVY, TEAL)


def build_html(r: dict) -> str:
    f = facts(r)
    sections = [("key-conclusions", "Key conclusions"), ("stroke", "Where the stroke goes"),
                ("stress", "Load and stress down the rods"), ("damping", "Damping and the pump card"),
                ("method", "How the pump card is recovered"), ("references", "References"),
                ("appendix-a", "A Example well"), ("appendix-b", "B Damping"), ("appendix-c", "C Method"),
                ("appendix-d", "D Reusable prompts")]
    nav = ('<nav class="contents" aria-label="Contents"><p>Contents</p><ol>'
           + "".join(f'<li><a href="#{a}">{esc(t)}</a></li>' for a, t in sections) + "</ol></nav>")
    csp = ("default-src 'none'; style-src 'unsafe-inline'; img-src data:; base-uri 'none'; form-action 'none'")
    description = ("The rod string of a public rod-pumped well: how much of the stroke is rod stretch, where the "
                   "rods are most stressed, how much damping changes the pump card, and where the "
                   "finite-difference method breaks down.")
    # Link-preview image (L3.4): the 1200 x 627 opening card, pinned to the commit that holds it.
    og_image = f"https://raw.githubusercontent.com/vamseeachanta/digitalmodel/{DETAIL_COMMIT}/{OPENING_CARD.as_posix()}"
    return (
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        f"<meta http-equiv=\"Content-Security-Policy\" content=\"{csp}\">\n"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        f"<title>{esc(ARTICLE_TITLE)}</title>\n"
        f"<meta name=\"description\" content=\"{esc(description)}\">\n"
        f"<meta property=\"og:title\" content=\"{esc(ARTICLE_TITLE)}\">\n"
        f"<meta property=\"og:description\" content=\"{esc(description)}\">\n"
        "<meta property=\"og:type\" content=\"article\">\n"
        f"<meta property=\"og:image\" content=\"{og_image}\">\n"
        "<meta property=\"og:image:width\" content=\"1200\">\n<meta property=\"og:image:height\" content=\"627\">\n"
        "<meta property=\"og:image:alt\" content=\"The Deckhand and AceEngineer marks, a pumping unit, the article "
        "title and the example well's surface, taper-joint and pump cards.\">\n"
        f"<style>{CSS}</style>\n</head>\n<body>\n<div class=\"page\">\n{nav}\n<main>\n"
        + body(f) + "\n" + appendices(f) + "\n</main>\n</div>\n</body>\n</html>\n")


# ---------------------------------------------------------------- detail pages (Markdown)

def md_table(headers, rows) -> str:
    out = ["| " + " | ".join(headers) + " |", "| " + " | ".join("---" for _ in headers) + " |"]
    out += ["| " + " | ".join(str(c) for c in row) + " |" for row in rows]
    return "\n".join(out)


def page_head(title, appendix, purposes) -> str:
    lines = [f"# {title}", "",
             f"Rod string article (\"{ARTICLE_TITLE}\"), {revision_lower()}. "
             + (f"This page holds in full what {appendix} of the article summarises. " if appendix else "")
             + f"`{BUILDER}` writes it from `results.json`; the results come from the library at "
             f"[{LIBRARY_COMMIT[:8]}]({REPO}/tree/{LIBRARY_COMMIT}/) (version of {LIBRARY_DATE}).", "", "Purpose:", ""]
    lines += [f"- {p}" for p in purposes]
    return "\n".join(lines) + "\n"


def page_readme(f) -> str:
    return page_head("Rod string article: detail pages", None, [
        "Hold the detail behind the rod string article, part 2 of the rod pump series.",
        "Give the data behind every figure, so each number can be checked."]) + "\n".join([
        "", "## Pages", "",
        "- [Example well, stretch, stress and sensitivity](stroke-and-stress.md): Appendix A.",
        "- [Damping](damping.md): Appendix B.",
        "- [Method and its limits](method-and-limits.md): Appendix C.",
        "- [Reusable prompts](reusable-prompts.md): Appendix D.",
        "", "## Data", "",
        "- [results.json](results.json): every saved result, written by `--refresh`.",
        "- [figure-1-cards-at-depth.csv](figure-1-cards-at-depth.csv): Figure 1.",
        "- [figure-2-3-depth-profile.csv](figure-2-3-depth-profile.csv): Figures 2 and 3.",
        "- [figure-4-damping-cards.csv](figure-4-damping-cards.csv): Figure 4.",
        "- [sensitivity.csv](sensitivity.csv): sensitivity factors.",
        "- [opening-card.png](opening-card.png): the article's 1200 x 627 link-preview image; its inset draws the "
        "cards of Figure 1.",
        "", "## Rebuild", "",
        "```",
        f"PYTHONPATH=src python {BUILDER} --refresh",
        f"PYTHONPATH=src python {BUILDER} --check",
        "```",
        ""])


SENS_NAMES = {"modulus": "Rod modulus of elasticity", "rod_diameter": "Rod diameter (weight with area)",
              "rod_weight": "Rod weight per foot", "fluid_density": "Fluid density", "pumping_speed": "Pumping speed",
              "viscosity": "Fluid viscosity (damping)", "tubing_id": "Tubing inside diameter (damping)",
              "load_gain": "Load-cell gain", "position_scale": "Surface position scale"}
OUT_NAMES = {"pump_stroke_in": "Pump stroke", "static_stretch_in": "Static stretch",
             "max_pump_load_lbf": "Maximum pump load", "fillage_pct": "Fillage",
             "peak_stress_top_taper2_psi": "Peak stress, top of 3/4 in rods", "damping_share_pct": "Damping share"}


def page_stroke(f) -> str:
    k, rods = f["k"], f["rods"]
    w = f["r"]["well"]
    g1, g2, off = f["g1"], f["g2"], f["off"]
    inputs = md_table(["Input", "Value", "Unit"], [
        ["Pumping speed", f["spm"], "strokes/min"], ["Pump depth", num(w["pump_depth_ft"]), "ft"],
        ["Plunger diameter", "1.500", "in"],
        ["Rod string, surface first", f"{rods[0]['diameter_in']} in × {num(rods[0]['length_ft'])} ft; "
                                      f"{rods[1]['diameter_in']} in × {num(rods[1]['length_ft'])} ft", "in; ft"],
        ["Rod grade and minimum tensile strength, from the record", f"Grade {f['grade']}; {f['tensile']}", "psi"],
        ["Goodman slope M and service factor, from the record", f"{g2['goodman_m']}; {f['sf']}", "-"],
        ["Rod modulus of elasticity, from the record", num(rods[0]["modulus_psi"]), "psi"],
        ["Rod weight per foot, from the record", f"{rods[0]['weight_lb_ft']}; {rods[1]['weight_lb_ft']}", "lb/ft"],
        ["Fluid density", num(w["fluid_density_lb_ft3"], 3), "lb/ft³"],
        ["Water cut, from the well test", f["water_cut"], "%"],
        ["Survey, maximum inclination", num(w["survey_max_inclination_deg"] or 0, 1), "deg"]])
    assumed = md_table(["Assumed value", "Value", "Why it is used"], [
        ["Fluid viscosity", f"{num(VISCOSITY_CP)} cP, ASSUMED - to be confirmed",
         f"Library default; the record gives no viscosity. At a {f['water_cut']} % water cut a value near water's, "
         "about 1 cP or less, is more realistic, but the library raises any value at or below 1 cP to 10 cP "
         "(issue 4176). It sets the damping only."],
        ["Tubing inside diameter", f"{TUBING_ID_IN} in, ASSUMED - to be confirmed",
         "Inside diameter of 2-7/8 in, 6.5 lb/ft tubing (drift 2.347 in), the library default; the record gives "
         "no tubing. It sets the damping only."],
        ["Coupling diameter", "1.5 × rod diameter, ASSUMED - to be confirmed",
         "Solver default; the record gives no coupling size. It sets the damping only."],
        ["Rod end at the pump", f"{f['rod_ft']} ft, ASSUMED - to be confirmed",
         f"The record lists {f['rod_ft']} ft of rods and a pump at {f['pump_depth']} ft; the end of the listed "
         "rods is taken as the pump."],
        ["Tubing", "anchored, ASSUMED - to be confirmed", "Unanchored tubing movement is not modelled."]])
    results = md_table(["Result", "Value", "Unit"], [
        ["Surface stroke", f["S2"], "in"], ["Pump stroke", f["Sp2"], "in"], ["Stroke lost", f["loss2"], "in"],
        ["Fluid load, from the computed pump card", f["Fo"], "lbf"],
        ["Static stretch, 7/8 in; 3/4 in; total",
         f"{num(k['static_stretch_by_taper_in'][0], 2)}; {num(k['static_stretch_by_taper_in'][1], 2)}; "
         f"{f['stretch2']}", "in"],
        ["Static stretch as a share of the stroke lost", f["share"], "%"],
        ["Remainder: estimated damping; no damping", f"{f['dyn2']}; {f['dyn0']}", "in"],
        ["Stroke at the taper joint", num(k["stroke_at_joint_in"], 2), "in"],
        ["Peak and minimum load, polished rod", f"{f['PPRL']}; {f['MPRL']}", "lbf"],
        ["Peak and minimum load, taper joint", f"{f['joint_max']}; {f['joint_min']}", "lbf"],
        ["Peak and minimum stress, top of 7/8 in rods",
         f"{f['sig1']}; {sig4(k['min_stress_top_taper1_psi'])}", "psi"],
        ["Peak and minimum stress, top of 3/4 in rods",
         f"{f['sig2']}; {sig4(k['min_stress_top_taper2_psi'])}", "psi"],
        ["Stress range, top of 7/8 in; top of 3/4 in rods", f"{f['rng1']}; {f['rng2']}", "psi"]])
    goodman = md_table(["Point", "Basis", "Peak stress (psi)", "Minimum stress (psi)", "Allowable SA (psi)",
                        "Loading (%)"], [
        ["Top of 7/8 in rods", "This article", sig4(g1["ours"]["max_psi"]), sig4(g1["ours"]["min_psi"]),
         sig4(g1["ours"]["allowable_psi"]), num(g1["ours"]["loading_pct"], 1)],
        ["Top of 7/8 in rods", "Well record", sig4(g1["record"]["max_psi"]), sig4(g1["record"]["min_psi"]),
         sig4(g1["record"]["allowable_psi"]), num(g1["record"]["loading_pct"], 1)],
        ["Top of 3/4 in rods", "This article", sig4(g2["ours"]["max_psi"]), sig4(g2["ours"]["min_psi"]),
         sig4(g2["ours"]["allowable_psi"]), num(g2["ours"]["loading_pct"], 1)],
        ["Top of 3/4 in rods", "Well record", sig4(g2["record"]["max_psi"]), sig4(g2["record"]["min_psi"]),
         sig4(g2["record"]["allowable_psi"]), num(g2["record"]["loading_pct"], 1)]])
    outs = list(OUT_NAMES)
    sens_rows = []
    for row in f["r"]["sensitivity"]:
        sens_rows.append([SENS_NAMES[row["input"]], f"±{row['step_pct']} %"]
                         + [num(row["factors"][o], 2) for o in outs])
    sens = md_table(["Input", "Step"] + [OUT_NAMES[o] for o in outs], sens_rows)
    return page_head("Example well, stretch, stress and sensitivity", "Appendix A", [
        "Give the inputs, assumptions and results of the example well behind Figures 1 to 3.",
        "Check the rod stresses against the Grade D allowable, and compare them with the well record's own.",
        "Give the sensitivity of the key results to each input at ±1 % and ±5 %."]) + "\n".join([
        "", f"The well is one anonymised field well in this library: [cleansed_well_001.json]"
            f"({blob(LIBRARY_COMMIT, FIXTURE)}), version of {LIBRARY_DATE}.", "",
        "## Inputs", "", inputs, "", "Table 1. Example well inputs, from the well record.", "",
        "## Assumed values", "", assumed, "", "Table 2. Assumed values.", "",
        "## Results", "", results, "",
        "Table 3. Example well results, finite-difference method, 200 grid points, estimated damping.", "",
        "## How the stretch and stress are worked", "",
        "- Static stretch of each taper: fluid load × 12 × length / (area × modulus), in inches: the axial "
        "deformation of a bar, δ = PL/(AE) (Hibbeler, Mechanics of Materials, chapter 4, edition to be "
        "confirmed), with the rod elastic constant of `rod_pump/rod_string.py` (`rod_elastic_constant`).",
        "- The fluid load, 3,310 lbf, is read from the computed pump card; the stress at the polished rod needs "
        "only the surface card; the stress at the taper joint needs the computed load profile.",
        f"- Hand check: 7/8 in rods, area 0.6013 in²; 3/4 in rods, area 0.4418 in²; modulus "
        f"{num(rods[0]['modulus_psi'])} psi. Stretch = {f['Fo']} × (12 × 1,400 / (0.6013 × 30,500,000) "
        f"+ 12 × 3,350 / (0.4418 × 30,500,000)) = {f['stretch2']} in.",
        f"- Stress at the polished rod: {f['PPRL']} lbf / 0.6013 in² = {f['sig1']} psi. Stress at the top of "
        f"the 3/4 in rods: the peak rod load interpolated to {f['joint_ft']} ft, {f['joint_max']} lbf, over "
        f"0.4418 in² = {f['sig2']} psi.",
        "- The solver averages the load at the grid point at the taper joint between its neighbours, because the "
        "strain-derived load steps where the area steps.",
        "", "## Goodman check", "",
        f"The modified Goodman diagram of API RP 11BR (edition to be confirmed) gives the allowable stress "
        f"SA = (T/4 + M × Smin) × SF, with T = {f['tensile']} psi (Grade {f['grade']}), M = {g2['goodman_m']} "
        f"and SF = {f['sf']}, all from the well record. The loading is (Smax − Smin) / (SA − Smin), the share "
        "of the allowable stress range in use. The well record's own RodStressSigmaA and RodStress values follow "
        "the same two relations.", "",
        goodman, "", "Table 4. Goodman check at the top of each taper, on the article's stresses and the record's.", "",
        f"- Hand check, top of the 3/4 in rods: SA = {f['tensile']} / 4 + {g2['goodman_m']} × "
        f"{sig4(g2['ours']['min_psi'])} = {sig4(g2['ours']['allowable_psi'])} psi; loading = "
        f"({f['sig2']} − {sig4(g2['ours']['min_psi'])}) / ({sig4(g2['ours']['allowable_psi'])} − "
        f"{sig4(g2['ours']['min_psi'])}) = {f['load2_1']} %.",
        "", "## Difference from the well record at the top of the 3/4 in rods", "",
        f"- At the polished rod the article and the record agree. At the top of the 3/4 in rods the record's peak "
        f"and minimum stresses are about {f['rec_dpsi']} psi above the article's: {num(off['max_lbf'], 0)} lbf and "
        f"{num(off['min_lbf'], 0)} lbf of load, the same in both, so a static offset, not a dynamic one.",
        f"- At the pump the record's stored pump card is {f['pump_mean_diff']} lbf below the article's on average, "
        "so the offset builds up between the pump and the joint.",
        f"- Its size is close to the buoyancy of the 3/4 in rods below the joint: {f['buoy_solver']} lbf for the "
        f"fluid displaced by the rod body (the article's basis) and {f['buoy_rp']} lbf on the API RP 11L basis, "
        "0.128 × SG × weight in air. The article deducts that buoyancy; the record's convention is not "
        "documented, so the cause is the closest match, not a confirmed one.",
        f"- On the record's stresses the loading at the top of the 3/4 in rods is {f['rec_load2_1']} %, against "
        f"{f['load2_1']} % on the article's.",
        "", "## Sensitivity", "",
        "Each input is changed by ±1 % and ±5 %, one at a time, all rod sections together; a diameter change "
        "keeps the rod's density by scaling its weight with its area. The factor is the percent change of the "
        "result divided by the percent change of the input, from the central difference. The article quotes the "
        f"+1 % cases: rod diameter, maximum pump load −{f['p_diam']} %; load-cell gain, +{f['p_gain']} %.", "",
        sens, "", "Table 5. Sensitivity factors (percent change of the result per percent change of the input).", "",
        "- Fillage and fluid load step with the corner samples the library detects on the pump card, so their "
        "factors and the static stretch factor can differ between the 1 % and 5 % steps.",
        "- Data: [sensitivity.csv](sensitivity.csv); the one-sided changes are in [results.json](results.json).", ""])


def page_damping(f) -> str:
    rows = []
    for m in f["r"]["damping_sweep"]:
        rows.append([f"{m['scale']:g}", num(m["max_pump_load_lbf"]), num(m["min_pump_load_lbf"]),
                     num(m["fillage_pct"], 2), num(m["fluid_load_lbf"]), num(m["pump_card_area_ft_lbf"]),
                     num(m["damping_share_pct"], 2), num(m["pump_card_change_nrmse_pct"], 2),
                     num(m["dynamic_remainder_in"], 2)])
    est = f["r"]["settings"]["estimated_damping_per_s"]
    rec = f["r"]["record_well"]["damping_per_s"]
    return page_head("Damping", "Appendix B", [
        "Show how the damping estimate changes the pump card of the example well.",
        "State the energy check and what the damping estimate rests on."]) + "\n".join([
        "", "## Key conclusion", "",
        f"- With no damping the pump card's area equals the surface card's within {f['zero_diff']} %: the energy "
        "put in at the polished rod reaches the pump, an energy check on the calculation.",
        f"- The estimated damping takes {f['damp_share']} % of the {f['A_surf']} ft-lbf of work per stroke.",
        f"- For this well, across a ten-fold range of damping, the maximum pump load changes by {f['ten_max']} % "
        f"and the stroke by {f['ten_stroke']} in; the damping changes the card's area and raises the bottom of the card.",
        f"- The gap between the wave-equation stroke and the static-stretch stroke at the pump is {f['dyn0']} in "
        f"with no damping and {f['dyn2']} in with the estimate: the rods' motion makes it, not the damping.",
        "", "## The estimate", "",
        "- The library estimates a viscous damping coefficient for each taper and each stroke direction from the "
        "drag of the rod body and couplings in the tubing (`everitt_jennings/damping.py`).",
        f"- Inputs: fluid viscosity {num(VISCOSITY_CP)} cP, tubing inside diameter {TUBING_ID_IN} in and coupling "
        "diameter 1.5 × rod diameter, all ASSUMED - to be confirmed.",
        f"- Coefficients (1/s), 7/8 in and 3/4 in rods: upstroke {num(est['upstroke'][0], 4)} and "
        f"{num(est['upstroke'][1], 4)}; downstroke {num(est['downstroke'][0], 4)} and {num(est['downstroke'][1], 4)}. "
        f"The well record stores {num(rec[0], 4)}, {num(rec[1], 4)}, {num(rec[2], 4)} and {num(rec[3], 4)}, within "
        f"{num(max(abs(a / b - 1) for a, b in zip(est['upstroke'] + est['downstroke'], rec)) * 100, 1)} % of the "
        "estimate.",
        f"- The well produces {f['water_cut']} % water, so a viscosity near water's, about 1 cP or less, is more "
        "realistic than 10 cP. The library raises any value at or below 1 cP to 10 cP (issue 4176), so 10 cP is "
        "kept. The coefficient is proportional to the viscosity, so the scales below also stand for viscosities "
        "from 0 to 100 cP and cover a water-like fluid.",
        "- The estimate has not been calibrated against a measured pump card.",
        "", "## Damping sweep", "",
        md_table(["Damping scale", "Maximum pump load (lbf)", "Minimum pump load (lbf)", "Fillage (%)",
                  "Fluid load (lbf)", "Pump card area (ft-lbf)", "Damping share (%)",
                  "Change of pump card (% of load range)", "Stroke gap to static stretch (in)"], rows), "",
        "Table 1. Pump card against damping scale; 1 is the estimate. Change of pump card: root-mean-square load "
        "difference from the estimate's card, over its load range.", "",
        "- Data: [figure-4-damping-cards.csv](figure-4-damping-cards.csv) (scales 0, 1 and 10) and "
        "[results.json](results.json).", ""])


def page_method(f) -> str:
    rows = []
    for c in f["r"]["courant"]:
        rows.append([num(c["samples_per_stroke"]), num(c["courant_number"], 5),
                     sci(c["max_pump_load_lbf"]).replace("<sup>", "^").replace("</sup>", "")
                     if abs(c["max_pump_load_lbf"]) > 1e6 else num(c["max_pump_load_lbf"]),
                     sci(c["min_pump_load_lbf"]).replace("<sup>", "^").replace("</sup>", "")
                     if abs(c["min_pump_load_lbf"]) > 1e6 else num(c["min_pump_load_lbf"]),
                     "none" if not c["solver_flags"] else ", ".join(c["solver_flags"])])
    g = f["r"]["grid"]
    return page_head("Method and its limits", "Appendix C", [
        "Describe how the pump card is recovered from the surface card.",
        "State where the method breaks down and which items are open in the library."]) + "\n".join([
        "", "## Key conclusion", "",
        f"- The march is stable up to {f['limit']} samples a stroke on this well's {f['nodes']}-point grid; the "
        f"example card has {f['native']}.",
        "- Above the limit the pump card grows without bound and the library raises no flag; the check that "
        "refuses such a card is in review, not merged.",
        "", "## Method", "",
        "- Finite-difference method, the default: the damped wave equation for axial rod motion is marched down "
        "the rods one grid point at a time, from the measured position and load at the polished rod, after "
        "Everitt and Jennings (1992), [SPE-18189-PA](https://doi.org/10.2118/18189-PA). The load at each depth is "
        "rebuilt from the strain, F = E A du/dx, and the buoyant weight of the rods below is added back.",
        "- The pump card is the last grid point, the end of the listed rods (ASSUMED - to be confirmed as the "
        "pump); its load is smoothed with a Savitzky-Golay filter, as in the library.",
        "- Frequency-domain method: the rod model of Gibbs (1963), [SPE-588-PA](https://doi.org/10.2118/588-PA), "
        "harmonic by harmonic. Its coupled load transfer was corrected in the library on 13 Nov 2026; this "
        "article uses the default method only.",
        "", "## Stability limit", "",
        "- Condition: a stress wave takes at least one time step to cross one grid step, Δt ≥ Δx / c "
        "(Courant, Friedrichs and Lewy, 1928, [doi:10.1007/BF01448839](https://doi.org/10.1007/BF01448839)), "
        "with depth as the marching direction. The wave speed of an elastic bar is c = (E/ρ)^0.5 (Bower, "
        "Applied Mechanics of Solids, online edition, section 4.3.3).",
        f"- This well: Δx = 4,750 ft / {f['nodes'] - 1} = {num(g['dx_ft'], 2)} ft ({num(g['dx_m'], 3)} m); "
        f"c = {num(g['wave_speed_m_s'], 0)} m/s (about {f['c_fts_about']} ft/s), with the rod density of "
        f"{num(g['rod_density_kg_m3'][0], 0)} kg/m³ from the record's weight per foot; stroke period "
        f"{num(g['period_s'], 2)} s. Limit: floor({num(g['period_s'], 2)} × {num(g['wave_speed_m_s'], 0)} / "
        f"{num(g['dx_m'], 3)}) = {f['limit']} samples a stroke.",
        "- Demonstration: the example card resampled periodically to each count and run through the same solver.",
        "", md_table(["Samples a stroke", "Courant number", "Maximum pump load (lbf)", "Minimum pump load (lbf)",
                      "Solver flags"], rows), "",
        "Table 1. Pump card against card sampling; a Courant number above 1 breaks the condition.", "",
        "- Below the limit the minimum pump load also changes with the sampling, because the smoothing window is "
        "a fixed number of samples; the maximum load and the stroke change by less than 1 %.",
        "", "## Open items in the library", "",
        "- The Python march has no sampling check (digitalmodel issue 4159); the fix is in review in pull "
        "request 4162, not merged.",
        "- Deviation and rod-on-tubing friction are off by default (issue 1894); this well's survey is vertical, "
        "so they do not change it.",
        "- Damping is not calibrated against a measured pump card (pull request 3964, limits), and a viscosity "
        "at or below 1 cP is replaced by 10 cP in the damping estimate (issue 4176).",
        "- The rod-string functions used for the hand checks have value tests in this change; issue 3355 asks "
        "for tests of the rest of `rod_string.py` and `kinematics.py`.", ""])


def page_prompts(f) -> str:
    commit = PROMPT_COMMIT or "<the commit that holds this page>"
    return "\n".join([
        "# Reusable prompts: rod string article", "",
        f"Rod string article (\"{ARTICLE_TITLE}\"), {revision_lower()}. This page holds in full what Appendix D "
        "of the article summarises.", "",
        "This prompt lets your team rebuild the article's rod string results from public code and data, check them "
        "against the published values, and then run the same steps on a well of your own: the stretch of each "
        "taper, the stress at the top of each taper and its Goodman loading, the effect of damping and the "
        "sampling limit. To have a study of your own rod string done for you instead, see below.", "",
        "## What you need to run this", "",
        "- Data required: none beyond the public repository; the example well is in it.",
        "- Data required to adapt: one surface card (position and load, evenly spaced in time over one stroke), "
        "the pumping speed, the rod taper (diameter, length, modulus, weight per foot and grade of each section), "
        "the pump diameter and depth, the fluid density; for damping, the fluid viscosity and the tubing inside "
        "diameter.",
        "- Paid or behind an account: nothing.",
        "- Changes over time: the library changes; use the pinned commit below to reproduce the published values.",
        "", "## Reproduce the published result", "", "Give this prompt to your assistant:", "",
        "```text",
        f"Clone {REPO} and check out commit {commit}.",
        "Install the package with its test extra, then run from the repository root:",
        f"  PYTHONPATH=src python {BUILDER} --refresh --check",
        "The command reruns the analysis and reports any page that differs from the committed one.",
        "Check these values in docs/domains/artificial_lift/rod-string-article/results.json:",
        f"  pump stroke {f['Sp2']} in; static stretch {f['stretch2']} in ({f['share']} % of the stroke lost);",
        f"  peak stress at the top of the 3/4 in rods {f['sig2']} psi; at the polished rod {f['sig1']} psi;",
        f"  Goodman loading at the top of the 3/4 in rods {f['load2_1']} %;",
        f"  damping share {f['damp_share']} %; sampling limit {f['limit']} samples a stroke.",
        "Then work the static stretch, the polished-rod stress and the Goodman loading by hand from the inputs "
        "and compare.",
        "```", "",
        "## Adapt it to your project", "",
        "- Write your well in the layout of the example file and load it with `load_from_json_file`.",
        "- Change the rod taper, the pumping speed, the pump and the fluid density to your well's values; replace "
        "the assumed viscosity, tubing inside diameter and coupling size where you know them.",
        "- Checks that still apply: the static stretch by hand against the stroke loss; the pump card area with no "
        "damping against the surface card area; the card's samples a stroke against the sampling limit; the stress "
        "at the top of each taper against the Goodman allowable for your rod grade and service factor.", "",
        f"Public data set: the example well, file [cleansed_well_001.json]({blob(LIBRARY_COMMIT, FIXTURE)}) of the "
        f"digitalmodel library, MIT licence, version of {LIBRARY_DATE}.", "",
        "To have a study of your own rod string done for you, write to support@aceengineer.com.", ""])


def detail_pages(f) -> dict:
    return {PAGES["readme"]: page_readme(f), PAGES["stroke"]: page_stroke(f), PAGES["damping"]: page_damping(f),
            PAGES["method"]: page_method(f), PAGES["prompts"]: page_prompts(f)}


# ---------------------------------------------------------------- main

def outputs(r: dict) -> dict:
    out = dict(csv_outputs(r))
    out.update(detail_pages(facts(r)))
    if DETAIL_COMMIT:
        out[ARTICLE] = build_html(r)
    return out


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--refresh", action="store_true", help="rerun the analysis and rewrite results.json")
    parser.add_argument("--check", action="store_true", help="compare outputs without writing")
    args = parser.parse_args(argv)
    written = {}
    if args.refresh:
        results = run_analysis(ROOT)
        written[RESULTS] = json.dumps(results, indent=1, ensure_ascii=False) + "\n"
    else:
        results = json.loads((ROOT / RESULTS).read_text(encoding="utf-8"))
    written.update(outputs(results))
    if args.check:
        stale = [str(p) for p, text in written.items()
                 if not (ROOT / p).exists() or (ROOT / p).read_text(encoding="utf-8") != text]
        for path in stale:
            print(f"stale: {path}")
        return 1 if stale else 0
    for path, text in written.items():
        (ROOT / path).parent.mkdir(parents=True, exist_ok=True)
        (ROOT / path).write_text(text, encoding="utf-8", newline="\n")
        print(f"wrote {path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
