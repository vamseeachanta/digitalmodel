#!/usr/bin/env python3
"""Fender design study: charts, results JSON, workbook and guide from a case.

Writes, into ``--out``:

* ``charts/*.png``: method comparison, Cm against keel clearance, berthing
  velocity by method, one sensitivity chart per input, tornado, neighbouring
  sizes, and (with ``--database``) the database coverage chart;
* ``results.json``: every number the report quotes;
* ``<stem>.xlsx``: the universal workbook (:mod:`build_fender_workbook`);
* ``<stem>.md`` and ``<stem>.html`` when ``--template`` is given: the guide
  rendered from a markdown template whose ``{{name}}`` fields are filled from
  the results. The Markdown keeps a static image for every figure; the HTML
  page draws each chart from its data (:mod:`fender_interactive`).

Usage::

    python fender_study.py --case generic_case.yaml --out <dir> \
        [--template report_template.md] [--stem report] [--database fender_database.csv]
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
from collections import Counter
from dataclasses import replace
from decimal import ROUND_HALF_UP, Decimal
from pathlib import Path

from digitalmodel.hydrodynamics import fender_design as fd

from build_fender_workbook import build_workbook
from fender_case import compute, load_case, size_basis
import fender_charts as fc
import fender_interactive as fi

SWEEP_TITLES = {
    "velocity_m_s": "Berthing velocity",
    "displacement_t": "Displacement",
    "water_depth_m": "Water depth (added mass Cm through keel clearance)",
    "contact_x_over_lbp": "First-contact point (eccentricity Ce)",
    "berthing_angle_deg": "Berthing angle (eccentricity Ce)",
    "cs": "Softness factor Cs",
    "cc": "Berth configuration factor Cc",
    "factor": "Abnormal / partial energy factor",
}

#: Report figure of each sweep chart: parameter -> (alt text, caption).
SWEEP_FIGURES = {
    "velocity_m_s": ("Velocity", "Berthing velocity."),
    "displacement_t": ("Displacement", "Displacement."),
    "water_depth_m": ("Water depth", "Water depth (C_m through keel clearance)."),
    "contact_x_over_lbp": ("Contact point", "First-contact point (C_e)."),
    "berthing_angle_deg": ("Berthing angle", "Berthing angle (C_e)."),
    "cs": ("Cs", "Softness factor C_s."),
    "cc": ("Cc", "Berth configuration factor C_c."),
    "factor": ("Factor", "Abnormal (or partial) energy factor."),
}
#: Report figure number of the first sweep chart (after the ranking chart, Figure 8).
FIRST_SWEEP_FIGURE = 9

#: Sensitivity group of each swept input (guide revision 6). Project and design sensitivities
#: are choices of the design; every other swept input is an operational uncertainty.
SWEEP_GROUPS = {"factor": "design", "cs": "design", "cc": "design"}
GROUP_NAMES = {"design": "Project and design", "operational": "Operational"}


def sweep_group(parameter: str) -> str:
    return SWEEP_GROUPS.get(parameter, "operational")

#: The two printed energy tolerances of a pneumatic fender, which conflict:
#: (tolerance, where printed). The case chooses; the generic case takes
#: 0.10, the reading that gives the lower capacity.
PNEUMATIC_ENERGY_TOLERANCES = (
    (0.10, "TFADM p. 76"),
    (0.0, "SFT p. 26 for guaranteed energy absorption (GEA)"),
)

#: Performance tolerance on reaction and energy printed in the TFADM p. 76
#: table, for the catalogue types it names; a case tolerance that differs is
#: listed in Table 14 as assumed.
TFADM_P76_TOLERANCE = {"pneumatic": 0.10, "foam": 0.10, "cylindrical": 0.10}


def _f(x, nd=1):
    return "-" if x is None else f"{x:,.{nd}f}"


def _util(x, limit=1.0, nd=3):
    """A value judged against ``limit`` (utilisation): three decimals, and more
    where three would round it onto the limit (1.0004 is "1.0004", never
    "1.000" or "1.00"), so a value over the limit never reads as on it."""
    if x is None:
        return "-"
    for d in range(nd, 8):
        txt = f"{x:.{d}f}"
        if x == limit or float(txt) != limit:
            return txt
    return f"{x:.8f}"


def _sig(x, n=4):
    """A number to ``n`` significant figures without e-notation: thousands
    separators from 1,000 up (61,689 not 6.169e+04)."""
    if x is None:
        return "-"
    if x == 0:
        return "0"
    if abs(x) >= 1000:
        return f"{x:,.0f}"
    d = max(0, n - 1 - math.floor(math.log10(abs(x))))
    return f"{x:,.{d}f}"


def _pct3(x) -> str:
    """A percentage margin to three significant figures, half up, after
    rounding to 12 figures to drop floating-point noise (5 / 1,600 is
    0.313 %, not 0.312 from 0.31249999999; 60 / 1,600 is 3.75 %)."""
    d = Decimal(f"{float(x):.12g}")
    if d == 0:
        return "0.00"
    r = d.quantize(Decimal(1).scaleb(d.copy_abs().adjusted() - 2), rounding=ROUND_HALF_UP)
    # rounding up past a power of ten (0.9999 to 1.000) keeps three figures (1.00)
    r = r.quantize(Decimal(1).scaleb(r.copy_abs().adjusted() - 2), rounding=ROUND_HALF_UP)
    return f"{r:f}"


def _g(x):
    """An input value as given, up to four significant figures, trailing zeros
    dropped and no e-notation: thousands separators from 1,000 up."""
    if x is None:
        return "-"
    if abs(x) >= 1000:
        return f"{x:,.0f}"
    return f"{x:.4g}"


def _unit(u):
    """A unit after a number; a dimensionless value takes none (never a bare "-")."""
    return "" if u in ("-", "") else f" {u}"


def md_table(header, rows):
    out = ["| " + " | ".join(header) + " |", "|" + "|".join("---" for _ in header) + "|"]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in rows]
    return "\n".join(out)


def method_chart(res, path: Path, fender_label: str) -> Path:
    """Utilisation of the base fender by method, each against its own
    capacity (:meth:`CaseResult.method_capacity_knm`), with the limit 1.0."""
    n = res.case["design"].get("fenders_in_contact", 1)
    caps = sorted({(r.capacity_divisor, res.method_capacity_knm(r)) for r in res.methods})
    note = (f"Capacity of {fender_label} per fender ({n} in contact): "
            + "; ".join(f"{c:,.0f} kNm" + (f" where the method divides by {d:.2f} (WG211 gamma_m)" if d != 1.0
                                            else " (no capacity divisor)") for d, c in caps) + ".")
    return fc.method_utilisation_chart(
        res.methods, [res.method_utilisation(r) for r in res.methods], path,
        "Base fender utilisation by method (same ship and berth)", note=note)


def cm_series(res):
    """Added mass against keel clearance over draught for the ship's draught and beam:
    (series of (name, KC/D values, Cm values), base KC/D)."""
    s = res.ship
    ratios = [0.02 + 0.01 * i for i in range(99)]

    def over(fn):
        return [fn(s.draft_m * (1 + r)) for r in ratios]
    series = [
        ("PIANC 2002 / ROM / WG211 (keel clearance)", ratios, over(lambda d: fd.added_mass_factor(s, d, "pianc2002"))),
        ("Vasco Costa 1 + 2T/B (BS 6349, IS 4651)", ratios, over(lambda d: fd.added_mass_factor(s, d, "vasco_costa"))),
        ("Ueda 1 + pi T/(2 Cb B) (Japan)", ratios, over(lambda d: fd.added_mass_factor(s, d, "ueda"))),
    ]
    ufc = next((r for r in res.methods if r.method == "ufc"), None)
    if ufc is not None:
        # the Cb in F of the base case, by the case's reading (berthing coefficient
        # Ce Cg Cd Cc of the base contact point, or the block coefficient)
        cbf = ufc.geometry["ufc_f_cb"]
        what = "berthing coeff." if res.fcase.ufc_f_basis == "berthing_coefficient" else "block coeff."
        series.append((f"UFC 4-152-01 surface vessel (Cb in F: {what} {cbf:.3f})", ratios,
                       over(lambda d: fd.ufc_added_mass(s, d, cbf))))
    return series, (res.berth.water_depth_m - s.draft_m) / s.draft_m


def velocity_series():
    """Design berthing velocity against ship size by method: (name, sizes, velocities)."""
    sizes = [1_000 * (500 ** (i / 80)) for i in range(81)]
    vser = [(f"Brolsma {c} ({fd.BROLSMA_CONDITIONS[c]})", sizes, [fd.brolsma_velocity(x, c) for x in sizes]) for c in "abcde"]
    rs = [x for x in sizes if x >= 2_000]
    vser.append(("ROM 2.0-11 moderate, tugs (digitized)", rs, [fd.rom_velocity(x, "moderate") for x in rs]))
    us = [x for x in sizes if 1_016.1 <= x <= 101_604]
    vser.append(("UFC 4-152-01 moderate (digitized)", us, [fd.ufc_velocity(x, "moderate") for x in us]))
    return vser


def size_rows(res) -> list[dict]:
    """Neighbouring sizes, smallest capacity first, with the change from the base fender in percent."""
    base = next(c for c in res.size_checks if c.is_base)
    rows = [dict(label=c.fender_id, is_base=c.is_base, energy_knm=c.capacity_knm, reaction_kn=c.reaction_kn,
                 utilisation=c.utilisation,
                 energy_change_pct=(c.capacity_knm / base.capacity_knm - 1.0) * 100.0,
                 reaction_change_pct=(c.reaction_kn / base.reaction_kn - 1.0) * 100.0,
                 utilisation_change_pct=(c.utilisation / base.utilisation - 1.0) * 100.0)
            for c in res.size_checks]
    rows.sort(key=lambda r: r["energy_knm"])
    return rows


def charts(res, out: Path, fender_label: str, db_counts=None, size_summary=None) -> dict[str, str]:
    cdir = out / "charts"
    made = {}
    n = res.case["design"].get("fenders_in_contact", 1)
    made["methods"] = method_chart(res, cdir / "methods.png", fender_label).name
    # Cm against keel clearance for the ship's draught and beam
    s = res.ship
    series, kcd = cm_series(res)
    made["cm"] = fc.line_family_chart(
        series, cdir / "cm_methods.png", "Added mass factor Cm against keel clearance",
        "keel clearance / draught KC/D (-)", "Cm (-)",
        marker=(kcd, fd.added_mass_factor(s, res.berth.water_depth_m, "pianc2002"), f"base KC/D {kcd:.3f}"),
        note=f"Ship T {s.draft_m:g} m, B {s.beam_m:g} m, Cb {s.cb:.3f}.").name
    # velocity by method against size
    vser = velocity_series()
    size0 = s.dwt_t or s.displacement_t
    made["velocity"] = fc.line_family_chart(
        vser, cdir / "velocity_methods.png", "Design berthing velocity against ship size",
        "ship size (t): DWT for PIANC 2002, displacement for BS 6349-4, ROM, UFC", "velocity (m/s)", logx=True,
        marker=(size0, res.selected.velocity_m_s.value, f"selected {res.selected.velocity_m_s.value:.3f} m/s"),
        note="WG211 defaults without site data: 0.15 m/s moderate, 0.25 m/s unfavourable (not size dependent).").name
    for sw in res.sweeps:
        made[f"sweep_{sw.parameter}"] = fc.sweep_chart(
            sw, cdir / f"sweep_{sw.parameter}.png", SWEEP_TITLES.get(sw.parameter, sw.label), fender_label).name
    made["tornado"] = fc.tornado_chart(res.bars, cdir / "tornado.png",
                                       "Which inputs move the design energy most (one at a time)",
                                       res.selected.design_energy_knm).name
    if res.size_checks:
        rows = size_rows(res)
        made["sizes"] = fc.size_comparison_chart(
            rows, cdir / "sizes.png", "Neighbouring fender sizes against the base",
            res.selected.design_energy_knm / n, res.case["design"].get("reaction_limit_kn"),
            basis_note=size_basis(res)).name
    if db_counts:
        made["database"] = database_chart(db_counts, cdir / "database_coverage.png")
    if size_summary:
        made["database_sizes"] = database_size_chart(size_summary, cdir / "database_sizes.png")
    return made


def index_counts(index_path: Path) -> tuple[Counter, int, int]:
    """Row counts by fender type from the public catalogue index (no values)."""
    cnt: Counter = Counter()
    rows = sources = 0
    with index_path.open(newline="", encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            sources += 1
            rows += int(r["rows"])
            for part in r["fender_types"].split(";"):
                name, _, n = part.strip().rpartition(" ")
                cnt[name] += int(n)
    return cnt, rows, sources


def database_chart(cnt: Counter, path):
    """Row count by fender type (no catalogue values)."""
    import matplotlib.pyplot as plt
    items = sorted(cnt.items(), key=lambda kv: kv[1])
    fig, ax = fc._fig(1, height=0.3 * len(items) + 1.2)
    fc._style(ax)
    ys = range(len(items))
    ax.barh(list(ys), [v for _, v in items], color=fc.SERIES[0], edgecolor=fc.SURFACE, height=0.6)
    for y, (_, v) in zip(ys, items):
        ax.text(v, y, f"  {v}", va="center", fontsize=7.5, color=fc.INK)
    ax.set_yticks(list(ys))
    ax.set_yticklabels([k for k, _ in items], fontsize=8)
    ax.set_xlim(0, max(v for _, v in items) * 1.15)   # room for the count labels inside the axes
    ax.set_xlabel("rows in the fender database")
    ax.set_title("Fender database coverage by type", fontsize=9.5, loc="left")
    fc._save(fig, path)
    plt.close("all")
    return path.name


def results_dict(res) -> dict:
    n = res.case["design"].get("fenders_in_contact", 1)
    out = dict(
        selected=res.selected.method,
        design_energy_knm=res.selected.design_energy_knm,
        energy_per_fender_knm=res.selected.design_energy_knm / n,
        capacity_knm=res.capacity_knm,
        utilisation=res.selected.design_energy_knm / n / res.capacity_knm,
        methods=[dict(method=r.method, label=r.label, velocity=r.velocity_m_s.value, cm=r.cm.value,
                      ce=r.ce.value, cs=r.cs.value, cc=r.cc.value,
                      other=math.prod(c.value for c in r.extra) if r.extra else 1.0,
                      normal=r.normal_energy_knm, factor=r.factor.value, design=r.design_energy_knm,
                      divisor=r.capacity_divisor, capacity=res.method_capacity_knm(r),
                      utilisation=res.method_utilisation(r), flags=list(r.flags),
                      outside_validity=fc.outside_validity(r),
                      sources={c.name: f"{c.source}: {c.note}" for c in r.coefficients()},
                      citations=[dict(code_id=c.code_id, section=c.section, wiki_path=c.wiki_path)
                                 for c in r.citations],
                      citation_status=dict(r.citation_status))
                 for r in res.methods],
        skipped=res.skipped,
        reaction_factor=res.reaction_factor,
        design_reaction_kn=res.design.reaction_kn,
        sweeps=[dict(parameter=s.parameter, base=s.base_value, low=s.range_low, high=s.range_high,
                     basis=s.range_basis, change_low=s.change_pct(s.range_low),
                     change_high=s.change_pct(s.range_high),
                     util_low=s.at(s.range_low).utilisation, util_high=s.at(s.range_high).utilisation,
                     limit_value=s.limit_value(), crossing_basis=s.crossing_basis,
                     crossing_step=None if s.crossing_step is None else list(s.crossing_step),
                     reaction_factor=s.reaction_factor,
                     reaction_low=s.at(s.range_low).reaction_kn, reaction_high=s.at(s.range_high).reaction_kn,
                     reaction_base=s.at(s.base_value).reaction_kn,
                     reaction_limit_value=s.reaction_limit_value(),
                     citation_status=dict(s.citation_status), flags=list(s.flags)) for s in res.sweeps],
        sweeps_skipped=res.sweeps_skipped,
        tornado=[dict(parameter=b.parameter, span_pct=b.span_pct) for b in res.bars],
        geometry=res.selected.geometry,
        size_checks=[dict(fender_id=c.fender_id, is_base=c.is_base, rated_energy=c.rated_energy_knm,
                          capacity=c.capacity_knm, utilisation=c.utilisation, rated_reaction=c.rated_reaction_kn,
                          reaction=c.reaction_kn, hull_pressure=c.hull_pressure_kpa,
                          meets_energy=c.meets_energy, meets_reaction=c.meets_reaction,
                          meets_pressure=c.meets_pressure, pressure_missing=c.pressure_missing,
                          meets_all=c.meets_all) for c in res.size_checks],
        citation_mode=res.citation_mode,
    )
    return out


def method_notes(r) -> str:
    """Flags of a method result for a table cell (validity first); the
    citation qualification is reported in its own table."""
    flags = [f for f in r.flags if not f.startswith("citation not resolved") and f != fd.CITATIONS_NOT_CHECKED]
    flags.sort(key=lambda f: fc.VALIDITY_MARK not in f)
    return "; ".join(("**" + f + "**") if fc.VALIDITY_MARK in f else f for f in flags) or "-"


def _r(x):
    return "not rated" if x is None else f"{x:,.0f}"


def tables(res) -> dict[str, str]:
    t = {}
    t["methods_table"] = md_table(
        ["Method", "v (m/s)", "Cm", "Ce", "Cs", "Cc", "Other", "Normal (kNm)", "Factor", "Design (kNm)",
         "Per fender / capacity", "Flags"],
        [[r.label + (" (outside validity)" if fc.outside_validity(r) else ""), f"{r.velocity_m_s.value:.3f}",
          f"{r.cm.value:.3f}", f"{r.ce.value:.3f}", f"{r.cs.value:.2f}",
          f"{r.cc.value:.2f}", f"{math.prod(c.value for c in r.extra) if r.extra else 1.0:.2f}",
          _f(r.normal_energy_knm), f"{r.factor.value:.2f}", _f(r.design_energy_knm),
          _util(res.method_utilisation(r)),
          method_notes(r)]
         for r in res.methods])
    t["citations_table"] = md_table(
        ["Citation (code_id)", "Wiki page", "Methods", "Status"],
        [[code, path, ", ".join(ms), st] for (code, path), (ms, st) in _citation_rows(res).items()])
    t["sweeps_skipped_table"] = md_table(
        ["Input", "Why it is not swept for this method"],
        [[k, why] for k, why in res.sweeps_skipped.items()]) if res.sweeps_skipped else "None."

    t["method_sources_table"] = md_table(
        ["Method", "Velocity basis", "Cm basis", "Factor basis"],
        [[r.label, f"{r.velocity_m_s.source}: {r.velocity_m_s.note}", f"{r.cm.source}: {r.cm.note}",
          f"{r.factor.source}: {r.factor.note}"] for r in res.methods])
    t["sweep_table"] = md_table(
        ["Input", "Group", "Base", "Credible range", "Basis", "Design energy change at range ends",
         "Utilisation at range ends", "Value at utilisation 1",
         f"Reaction x {res.reaction_factor:.2f} at range ends (kN)", "Value at reaction limit"],
        [[s.label, GROUP_NAMES[sweep_group(s.parameter)], f"{_g(s.base_value)}{_unit(s.unit)}",
          f"{_g(s.range_low)} to {_g(s.range_high)}{_unit(s.unit)}", s.range_basis,
          f"{s.change_pct(s.range_low):+.0f} % / {s.change_pct(s.range_high):+.0f} %",
          f"{_util(s.at(s.range_low).utilisation)} / {_util(s.at(s.range_high).utilisation)}",
          _limit_cell(s),
          f"{_r(s.at(s.range_low).reaction_kn)} / {_r(s.at(s.range_high).reaction_kn)}",
          "-" if s.reaction_limit_value() is None else f"{_sig(s.reaction_limit_value())}{_unit(s.unit)}"]
         for s in res.sweeps])
    t["crossing_note"] = crossing_note(res)
    steps = base_step_notes(res)
    t["step_note"] = ("Results on a coefficient step: " + "; ".join(steps) + ". Both sides are shown so the "
                      "step is not mistaken for the sensitivity of the input." if steps else
                      "No method's base result sits on a coefficient step (a 1 mm or 0.001 deg move of the "
                      "first contact, angle or depth changes no design energy by more than 1 %).")
    t["tornado_table"] = md_table(
        ["Rank", "Input", "Span of design energy (percentage points)"],
        [[i, b.label, f"{b.span_pct:.0f}"] for i, b in enumerate(res.bars, start=1)])
    t.update(method_sensitivity(res))
    if res.size_checks:
        d = res.case["design"]
        r_lim, p_lim = d.get("reaction_limit_kn"), d.get("hull_pressure_limit_kpa")

        base_c = next(c for c in res.size_checks if c.is_base)

        def yn(x):
            return "-" if x is None else ("yes" if x else "**no**")
        t["sizes_table"] = md_table(
            ["Fender", "Rated energy (kNm)", "Design capacity (kNm)", "Utilisation",
             "Rated reaction (kN)", "Design reaction (kN)", "Design hull pressure (kPa)",
             "Energy", f"Reaction <= {_f(r_lim, 0)} kN", f"Pressure <= {_f(p_lim, 0)} kPa",
             "Capacity against base (%)", "Utilisation against base (%)", "Reaction against base (%)"],
            [[c.fender_id + (" (base)" if c.is_base else ""), _f(c.rated_energy_knm, 0), _f(c.capacity_knm, 0),
              _util(c.utilisation), _f(c.rated_reaction_kn, 0), _f(c.reaction_kn, 0),
              _f(c.hull_pressure_kpa, 0), yn(c.meets_energy), yn(c.meets_reaction),
              "**no** (no datum)" if c.pressure_missing else yn(c.meets_pressure),
              _signed_pct((c.capacity_knm / base_c.capacity_knm - 1.0) * 100.0),
              _signed_pct((c.utilisation / base_c.utilisation - 1.0) * 100.0),
              _signed_pct((c.reaction_kn / base_c.reaction_kn - 1.0) * 100.0)]
             for c in res.size_checks])
        t["sizes_basis"] = size_basis(res)
    t["sources_table"] = md_table(
        ["Id", "Document", "Edition", "Clause", "Verification", "Read in", "Date read"],
        [[s.source_id, s.title, s.edition, s.clause, s.verification, s.read_in, s.accessed_on]
         for s in fd.SOURCES.values()])
    t["assumptions_table"] = assumptions_table(res)
    t["velocity_table"] = md_table(
        ["Size (t)"] + [f"{c}" for c in "abcde"],
        [[f"{x:,.0f}"] + [f"{fd.brolsma_velocity(x, c):.3f}" for c in "abcde"]
         for x in (5_000, 10_000, 20_000, 50_000, 100_000, 200_000)])
    return t


#: Small moves of the berth inputs that test whether a method's base result
#: sits on a coefficient step: (berth field, move, words).
STEP_PROBES = (
    ("contact_x_from_bow_m", 0.001, "first contact 1 mm nearer midships"),
    ("contact_x_from_bow_m", -0.001, "first contact 1 mm nearer the bow"),
    ("berthing_angle_deg", 0.001, "berthing angle 0.001 deg larger"),
    ("berthing_angle_deg", -0.001, "berthing angle 0.001 deg smaller"),
    ("water_depth_m", 0.001, "water depth 1 mm larger"),
    ("water_depth_m", -0.001, "water depth 1 mm smaller"),
)


def base_step_notes(res, threshold: float = 0.01) -> list[str]:
    """Methods whose base result sits on a coefficient step: a move of the
    berth from :data:`STEP_PROBES` changes the design energy by more than
    ``threshold`` (1 %). The UFC Cg, for one, is 0.95 at or beyond the
    quarter points and 1.0 nearer midships."""
    d = res.case["design"]
    kw = dict(velocity_m_s=d.get("velocity_m_s"), factor=d.get("factor"),
              factor_position=d.get("factor_position", "smallest"), citations="off")
    notes = []
    for r in res.methods:
        for field, dx, words in STEP_PROBES:
            try:
                b = replace(res.berth, **{field: getattr(res.berth, field) + dx})
                e = fd.energy_by_method(replace(res.fcase, berth=b), method=r.method, **kw).design_energy_knm
            except ValueError:
                continue
            change = e / r.design_energy_knm - 1.0
            if abs(change) > threshold:
                notes.append(f"{r.label}: the base sits on a coefficient step; {words} gives {_f(e)} kNm "
                             f"({change * 100:+.1f} %)")
    return notes


def _limit_cell(s) -> str:
    """Table 7 value at utilisation 1: the solved value, or at a coefficient
    step the step with the utilisation on each side (no value gives 1)."""
    if s.limit_value() is None:
        return "-"
    if s.crossing_step is not None:
        lo, hi = s.crossing_step
        return f"step at {_sig(s.limit_value())}{_unit(s.unit)} ({_util(lo)} to {_util(hi)})"
    return f"{_sig(s.limit_value())}{_unit(s.unit)}"


def crossing_note(res) -> str:
    """How each value at utilisation 1 in Table 7 was solved, grouped by method of solution."""
    groups: dict[str, list[str]] = {}
    for s in res.sweeps:
        if s.crossing_basis and s.limit_value() is not None:
            key = s.crossing_basis
            if key.startswith("bisection"):
                key = "bisection on the method between the bracketing swept points"
            groups.setdefault(key, []).append(s.label)
    if not groups:
        return "no input reaches utilisation 1 within its swept values"
    return "; ".join(f"{', '.join(labels)}: {how}" for how, labels in groups.items())


def pneumatic_tolerance_readings(res) -> list[tuple[float, str, float]]:
    """(energy tolerance, where printed, base-fender utilisation) under each
    printed pneumatic energy tolerance (:data:`PNEUMATIC_ENERGY_TOLERANCES`);
    empty when the base fender is not pneumatic or no size check was run.
    Capacity is rated x (1 - tolerance) x the other factors, so the
    utilisation scales by (1 - case tolerance) / (1 - reading)."""
    base = next((c for c in res.size_checks if c.is_base), None)
    if base is None or res.base_fender.fender_type != "pneumatic":
        return []
    et = res.case["design"].get("energy_tolerance", 0.10)
    return [(v, src, base.utilisation * (1.0 - et) / (1.0 - v)) for v, src in PNEUMATIC_ENERGY_TOLERANCES]


def pneumatic_tolerance_note(res) -> str:
    """Section 4.6 sentence: the base fender's energy verdict under each reading."""
    readings = pneumatic_tolerance_readings(res)
    if not readings:
        return "The base fender of this build is not pneumatic, so the conflict does not enter it."
    et = res.case["design"].get("energy_tolerance", 0.10)
    parts = [f"{_util(u)} at {v * 100:.0f} % ({'met' if u <= 1.0 else '**not met**'})" for v, _, u in readings]
    return (f"For the base fender {res.base_fender.fender_id} the energy utilisation is "
            + " and ".join(parts) + f"; this build takes {et * 100:.0f} %.")


def assumptions(res) -> list[tuple[str, str, str, str]]:
    """Every assumed value the module and the case use: (value, where it
    enters, used in this case, basis). Each is ASSUMED - to be confirmed."""
    c, d = res.fcase, res.case["design"]
    b = res.berth
    by_type = d.get("tolerances_by_type") or {}
    used = {r.method for r in res.methods}

    def yes(cond):
        return "yes" if cond else "no"
    rows = [
        ("UFC 4-152-01 C_c 0.9 for a partly closed berth", "UFC method, `semi_closed` structure",
         yes("ufc" in used and b.structure == "semi_closed"),
         "UFC prints 1.0 (open) and 0.8 (solid) and 'in between'; the midpoint is taken"),
        ("C_c 1.0 for a partly closed berth (PIANC basis)",
         "PIANC 2002, Ueda, BS 6349-4, EAU; `semi_closed`, angle 5° or less",
         yes(c.cc_basis == "pianc2002" and b.structure == "semi_closed"),
         "PIANC WG33 (2002) 4.2.7 gives no partly closed value; the open-berth value (no reduction) is taken"),
        (f"WG211 γ_m = {fd.WG211_GAMMA_M:.2f}", "WG211 fender capacity divisor", yes("wg211" in used),
         "the R24B worked example (Table 4); the final guideline value was not read"),
        ("WG211 favourable and monitored velocities 0.125 and 0.115 m/s", "WG211 velocity",
         yes("wg211" in used and c.condition in ("favourable", "monitored")),
         "derived from the R24A Weibull fits at 0.02 % exceedance; not printed values"),
        (f"WG211 γ_n not extrapolated below {fd.WG211_GAMMA_N_MIN_PER_YEAR:g} berthing a year",
         "WG211 frequency correction", yes("wg211" in used and c.berthings_per_year < 1),
         "the range of validity of R24A Table 9 was not read"),
        (f"WG211 CoV_M `{c.cov_m}`", "WG211 γ_Eref column", yes("wg211" in used),
         "generic case: a berth used by ships of widely varying displacement; the module default is `low`, "
         "the largest factor"),
        ("ROM 2.0-11 return period T_R = 500 yr", "ROM accidental factor (T_R/500)^(1/20)", yes("rom" in used),
         "the table's reference period; no site return period is given"),
        *[(f"{t} tolerances: energy {v['energy_tolerance']:.2f}, reaction {v['reaction_tolerance']:.2f}",
           "neighbouring sizes", "yes",
           f"not the TFADM p. 76 value ({TFADM_P76_TOLERANCE[t]:.2f})" if t in TFADM_P76_TOLERANCE else
           "no tolerance printed for this type in TFADM p. 76")
          for t, v in by_type.items()
          if not (TFADM_P76_TOLERANCE.get(t) == v["energy_tolerance"] == v["reaction_tolerance"])],
        *[(f"Pneumatic energy tolerance {d.get('energy_tolerance', 0.10):.2f}",
           "base fender capacity (Table 9)", "yes",
           "the sources conflict: TFADM p. 76 ±10 %, SFT p. 26 0 % on GEA; "
           + ("10 % is taken (the lower capacity of the two); 0 % needs the supplier's GEA rating to ISO 17357-1. "
              if d.get("energy_tolerance", 0.10) >= 0.10 else "the GEA reading is taken. ")
           + "Base utilisation " + " / ".join(f"{_util(u)} at {v * 100:.0f} %" for v, _, u in readings))
          for readings in [pneumatic_tolerance_readings(res)] if readings],
        (f"C_c solid quay {fd.CC_SOLID_QUAY_KCD_LIMIT:g} KC/D limit (PIANC basis)",
         "PIANC 2002, Ueda, BS 6349-4, EAU; `closed`, angle 5° or less, Vasco Costa or Ueda C_m: 0.9 below "
         "the limit, 1.0 at or above it",
         yes(c.cc_basis == "pianc2002" and b.structure == "closed"),
         "PIANC WG33 (2002) 4.2.7 prints 'generally 0.9' and says the cushion effect 'will hardly occur' "
         "with a large keel clearance, with no threshold; 0.15 is the Trelleborg p. 27 reading"),
        (f"Block coefficient plausibility range {fd.CB_PLAUSIBLE[0]:g} to {fd.CB_PLAUSIBLE[1]:g}",
         "a flag on every method result outside it (above 1 the ship is refused)", "check only",
         "span of merchant hull forms; not a value from the cited standards"),
        (f"Reaction limit {_f(d.get('reaction_limit_kn'), 0)} kN", "reaction checks",
         yes(d.get("reaction_limit_kn") is not None), "illustrative structural limit of the generic case"),
    ]
    return rows


def assumptions_table(res) -> str:
    return md_table(["Assumed value", "Where it enters", "Used in this case", "Status", "Basis"],
                    [[v, w, u, "ASSUMED - to be confirmed", basis] for v, w, u, basis in assumptions(res)])


def summary_values(res) -> dict[str, str]:
    """Template values of the summary's check outcomes for the base fender
    (energy, reaction and hull pressure, each with its limit)."""
    base = next((c for c in res.size_checks if c.is_base), None)
    d = res.case["design"]
    if base is None:
        return dict(base_checks="No neighbouring-size check was run for the base fender.")
    r_lim, p_lim = d.get("reaction_limit_kn"), d.get("hull_pressure_limit_kpa")
    et = d.get("energy_tolerance", 0.10)
    alt = [(v, src, u) for v, src, u in pneumatic_tolerance_readings(res) if abs(v - et) > 1e-12]
    parts = [f"energy utilisation {_util(base.utilisation)} against 1.0"
             + (f" at the {et * 100:.0f} % pneumatic energy tolerance" if alt else "")
             + f" ({'met' if base.meets_energy else '**not met**'}"
             + "".join(f"; {_util(u)}, {'met' if u <= 1.0 else 'not met'}, at the {v * 100:.0f} % of {src}"
                       for v, src, u in alt)
             + ("; Section 4.6)" if alt else ")")]
    if r_lim is not None:
        margin = (r_lim - base.reaction_kn) / r_lim * 100.0
        parts.append(f"design reaction {_f(base.reaction_kn, 0)} kN against the {_f(r_lim, 0)} kN limit "
                     + (f"(met, margin {_pct3(margin)} %)" if base.meets_reaction else
                        f"(**not met**, {_pct3(-margin)} % over)"))
    if p_lim is not None:
        p = base.hull_pressure_kpa
        parts.append("hull pressure: no datum against the "
                     f"{_f(p_lim, 0)} kPa limit (**not met**)" if p is None else
                     f"design hull pressure {_f(p, 0)} kPa against the {_f(p_lim, 0)} kPa limit "
                     + ("(met)" if base.meets_pressure else
                        f"(**not met**, {_pct3((p / p_lim - 1) * 100)} % over)"))
    failed = [name for name, ok in (("energy", base.meets_energy), ("reaction", base.meets_reaction),
                                    ("hull pressure", base.meets_pressure)) if ok is False]
    verdict = ("The base fender meets every check." if base.meets_all else
               f"The base fender therefore **fails** the {' and '.join(failed)} "
               f"check{'s' if len(failed) > 1 else ''}, and is not a solution for this case as it stands.")
    ok = [c for c in res.size_checks if not c.is_base and c.meets_all]

    def margins(c):
        out = [f"utilisation {_util(c.utilisation)}"]
        if r_lim is not None:
            out.append(f"design reaction {_f(c.reaction_kn, 0)} kN, margin {_pct3((r_lim - c.reaction_kn) / r_lim * 100)} %")
        if p_lim is not None and c.hull_pressure_kpa is not None:
            out.append(f"hull pressure {_f(c.hull_pressure_kpa, 0)} kPa, "
                       f"margin {_pct3((p_lim - c.hull_pressure_kpa) / p_lim * 100)} %")
        return ", ".join(out)
    sizes = ("Of the neighbouring sizes in Table 9, "
             + (("only " if len(ok) == 1 else "") + " and ".join(c.fender_id for c in ok)
                + (" meets" if len(ok) == 1 else " meet") + " every check at the tolerances of its type ("
                + "; ".join(f"{c.fender_id}: {margins(c)}" for c in ok) + ")"
                if ok else "none meets every check either")
             + "; the fenders are illustrative, so this is not a selection.")
    return dict(base_checks=f"Base fender {base.fender_id} (Table 9): " + "; ".join(parts) + ". " + verdict
                + " " + sizes)


def _citation_rows(res) -> dict:
    rows: dict = {}
    for r in res.methods:
        for c in r.citations:
            st = dict(r.citation_status).get(c.code_id, "not checked")
            ms, _ = rows.setdefault((c.code_id, c.wiki_path), ([], st))
            if r.method not in ms:
                ms.append(r.method)
    return rows


def geometry_values(res) -> dict[str, str]:
    """Template values of the ship and berth geometry.

    ``cb``, ``K``, ``R``, ``phi``, ``ce`` and ``ce_err`` are the common
    illustrative geometry: the PIANC 2002 4.2.4 eccentricity from the ship
    and berth (:func:`fd.eccentricity`), the same whichever method is
    selected. ``selected_geometry`` lists the selected method's own geometry
    fields, only those the method gives (UFC has no R or phi, IS 4651 no
    C_b or K); ``selected_method`` names the method.
    """
    g = fd.eccentricity(res.ship, res.berth)
    own = res.selected.geometry
    return dict(
        cb=f"{g['cb']:.3f}", K=f"{g['k_m']:.2f}", R=f"{g['r_m']:.2f}", phi=f"{g['phi_deg']:.1f}",
        ce=f"{g['ce']:.3f}", ce_err=f"{fd.eccentricity_k2_numerator(g['k_m'], g['r_m'], g['phi_deg']):.3f}",
        selected_method=res.selected.method,
        selected_geometry=", ".join(f"`{k}` {v:.3f}" for k, v in own.items()) or "none",
    )


def method_values(res) -> dict[str, str]:
    """Template values that name the selected method: ``selected_label`` is
    its label (:data:`fd.METHODS`), used wherever the report quotes the
    selected method's energy, so a number is never attributed to another
    method."""
    return dict(selected_label=res.selected.label)


def ufc_f_readings(res) -> dict[str, float | None]:
    """UFC design energy (kNm) of the case under each reading of the Cb in
    the factor F (:data:`fd.UFC_F_BASES`), the case's velocity and factor
    overrides applied; None where UFC cannot be evaluated for the case."""
    d = res.case["design"]
    out = {}
    for basis in fd.UFC_F_BASES:
        try:
            r = fd.energy_by_method(replace(res.fcase, ufc_f_basis=basis), method="ufc",
                                    velocity_m_s=d.get("velocity_m_s"), factor=d.get("factor"),
                                    factor_position=d.get("factor_position", "smallest"), citations="off")
        except ValueError:
            out[basis] = None
            continue
        out[basis] = r.design_energy_knm
    return out


def ufc_f_values(res) -> dict[str, str]:
    """Template values of the UFC factor F reading: the reading this build
    uses and the UFC design energy under each reading."""
    e = ufc_f_readings(res)
    return dict(ufc_f_basis=res.fcase.ufc_f_basis, E_ufc_berthing=_f(e["berthing_coefficient"]),
                E_ufc_block=_f(e["block_coefficient"]))


def sweep_figure_values(res, made: dict[str, str], first: int = FIRST_SWEEP_FIGURE) -> dict[str, str]:
    """Template values of the sensitivity figures.

    ``sweep_figures`` embeds one figure per sweep chart generated for the
    selected method (``made`` from :func:`charts`), in sweep order and
    numbered from ``first``; a sweep the method does not admit has no chart
    and no figure. Revision 6 groups them: ``sweep_figures_design`` (project and
    design sensitivities, numbered from ``first``), then the neighbouring-sizes
    figure ``fig_sizes``, then ``sweep_figures_oper`` (operational
    uncertainties). ``sweep_figures`` is both groups in that order.
    """
    shown = [s for s in res.sweeps if made.get(f"sweep_{s.parameter}")]
    design = [s for s in shown if sweep_group(s.parameter) == "design"]
    oper = [s for s in shown if sweep_group(s.parameter) == "operational"]
    fig_sizes = first + len(design)          # the neighbouring-sizes figure closes the design group

    def blocks(group, start):
        out = []
        for i, s in enumerate(group):
            alt, caption = SWEEP_FIGURES.get(s.parameter, (s.label, f"{s.label}."))
            name = made[f"sweep_{s.parameter}"]
            out.append(f"![{alt}](charts/{name})\n\nFigure {start + i} – {caption} {INTERACTIVE_NOTE}")
        return out
    d, o = blocks(design, first), blocks(oper, fig_sizes + 1)
    none = "No input of this group is swept for this method."
    return dict(sweep_figures_design="\n\n".join(d) or none, sweep_figures_oper="\n\n".join(o) or none,
                sweep_figures="\n\n".join(d + o) or "No sweep chart was generated for this method.",
                fig_sizes=str(fig_sizes))


def render(template: Path, values: dict, out_md: Path, out_html: Path, title: str, interactive: dict | None = None):
    """Template to Markdown and to one HTML page. The Markdown keeps the static images; in the
    HTML each image named in ``interactive`` (image file name -> figure HTML) is replaced by its
    interactive figure. Bold is removed: only table header rows stand apart."""
    text = template.read_text(encoding="utf-8")
    missing = sorted(set(re.findall(r"\{\{(\w+)\}\}", text)) - set(values))
    if missing:
        raise KeyError(f"template fields without values: {missing}")
    text = re.sub(r"\{\{(\w+)\}\}", lambda m: str(values[m.group(1)]), text)
    text = re.sub(r"\*\*(.+?)\*\*", r"\1", text)
    out_md.write_text(text, encoding="utf-8", newline="\n")
    import markdown
    body = markdown.markdown(text, extensions=["tables", "toc"])
    interactive = interactive or {}

    def swap(m):
        alt, name = m.group(1), m.group(2)
        return interactive.get(name, m.group(0)).replace("__ALT__", alt)
    body = re.sub(r'<p><img alt="([^"]*)" src="charts/([^"]+)"\s*/?></p>', swap, body)
    css = ("body{font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;"
           "color:#0b0b0b;line-height:1.45}table{border-collapse:collapse;font-size:.85rem;margin:.6rem 0}"
           "th,td{border:1px solid #c3c2b7;padding:.25rem .45rem;text-align:left;vertical-align:top}"
           "th{background:#f0efec}img{max-width:100%;border:1px solid #e1e0d9}code{font-size:.85em}"
           "h1,h2,h3{line-height:1.2}" + fi.CSS)
    out_html.write_text(
        f"<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>{title}</title>"
        f"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        f"<style>{css}</style></head><body>{body}{fi.SCRIPT if interactive else ''}</body></html>",
        encoding="utf-8", newline="\n")


#: Closing words of a caption whose figure is interactive in the HTML page.
INTERACTIVE_NOTE = "In the HTML page, hover gives each value with its unit."

#: Main fender types of the size chart, in display order (others are in Table B.1).
SIZE_CHART_TYPES = ("cone", "cell", "element", "arch", "v", "cylindrical", "pneumatic", "foam", "foam-filled",
                    "hydropneumatic")


def read_csv(path: Path) -> list[dict]:
    """Rows of a public data file; an absent file gives no rows (the figure is then left out)."""
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def _signed_pct(x: float, nd: int = 0) -> str:
    """A percentage change with its sign and the true minus sign; the base reads 0."""
    q = round(x, nd)
    if q == 0:
        return "0"
    return f"{'+' if q > 0 else '−'}{abs(q):.{nd}f}"


def _short_vendor(name: str) -> str:
    return name.split(" Marine")[0].split(" Rubber")[0].split(" Industrial")[0].replace("Qingdao ", "")


def method_sensitivity(res) -> dict[str, str]:
    """Method (code) sensitivity: each method's design energy against the selected method, in percent."""
    sel = res.selected
    rows = sorted(res.methods, key=lambda r: r.design_energy_knm)
    lo, hi = rows[0], rows[-1]
    table = md_table(
        ["Method", "Velocity (m/s)", "Design energy (kNm)", f"Against {sel.label} (%)", "Utilisation of the base fender"],
        [[r.label + (" (selected)" if r.method == sel.method else "")
          + (" (outside validity)" if fc.outside_validity(r) else ""),
          f"{r.velocity_m_s.value:.3f}", _f(r.design_energy_knm),
          _signed_pct((r.design_energy_knm / sel.design_energy_knm - 1.0) * 100.0),
          _util(res.method_utilisation(r))] for r in rows])
    return dict(method_sens_table=table,
                spread_pct=f"{(hi.design_energy_knm / lo.design_energy_knm - 1.0) * 100.0:.0f}",
                method_lo=lo.label, method_hi=hi.label, E_method_lo=_f(lo.design_energy_knm),
                E_method_hi=_f(hi.design_energy_knm))


def vendor_rows(summary: list[dict]) -> list[dict]:
    """The database by vendor: rows, fender types, and the largest solid and floating sizes held."""
    out: dict[str, dict] = {}
    for r in summary:
        v = out.setdefault(r["vendor"], dict(vendor=r["vendor"], rows=0, types=[], solid=None, floating=None))
        v["rows"] += int(r["rows"])
        v["types"].append(r["fender_type"])
        if r["largest_size_mm"]:
            key = "solid" if r["size_measure"] == "height" else "floating"
            size = int(r["largest_size_mm"])
            if v[key] is None or size > v[key][0]:
                v[key] = (size, r["fender_type"])
    return sorted(out.values(), key=lambda v: -v["rows"])


def database_values(summary: list[dict]) -> dict[str, str]:
    """Tables of the database by vendor (Table 6) and by vendor, type and size (Table B.1)."""
    if not summary:
        return dict(db_vendor_table="The size summary of the database was not available for this build.",
                    db_size_table="Not available for this build.", db_vendors="0", db_types="0")

    def cell(x):
        return "-" if x is None else f"{x[0]:,} ({x[1]})"
    vend = vendor_rows(summary)
    t6 = md_table(["Vendor", "Rows", "Fender types", "Largest solid fender height (mm)",
                   "Largest floating fender diameter (mm)"],
                  [[v["vendor"], f"{v['rows']:,}", len(v["types"]), cell(v["solid"]), cell(v["floating"])] for v in vend])
    tb = md_table(["Vendor", "Fender type", "Rows", "Size measure", "Smallest size (mm)", "Largest size (mm)"],
                  [[r["vendor"], r["fender_type"], f"{int(r['rows']):,}", r["size_measure"],
                    f"{int(r['smallest_size_mm']):,}" if r["smallest_size_mm"] else "-",
                    f"{int(r['largest_size_mm']):,}" if r["largest_size_mm"] else "-"]
                   for r in sorted(summary, key=lambda r: (r["vendor"], r["fender_type"]))])
    return dict(db_vendor_table=t6, db_size_table=tb, db_vendors=str(len(vend)),
                db_types=str(len({r["fender_type"] for r in summary})))


def _size_grid(summary: list[dict]):
    """Largest size (mm) by main fender type (categories) and vendor (series), with the row counts."""
    vendors = [v["vendor"] for v in vendor_rows(summary)]
    by = {(r["vendor"], r["fender_type"]): r for r in summary}
    types = [t for t in SIZE_CHART_TYPES if any((v, t) in by for v in vendors)]
    grid = {v: [int(by[(v, t)]["largest_size_mm"]) if (v, t) in by and by[(v, t)]["largest_size_mm"] else None
                for t in types] for v in vendors}
    notes = {v: [f"{by[(v, t)]['size_measure']}; {int(by[(v, t)]['rows']):,} rows; smallest "
                 f"{int(by[(v, t)]['smallest_size_mm']):,} mm" if (v, t) in by and by[(v, t)]["largest_size_mm"] else ""
                 for t in types] for v in vendors}
    return vendors, types, grid, notes


def database_size_chart(summary: list[dict], path):
    """Largest size held by fender type and vendor (static image of the interactive Figure 6)."""
    import matplotlib.pyplot as plt
    vendors, types, grid, _ = _size_grid(summary)
    fig, ax = fc._fig(1, height=3.8, width=8.5)
    fc._style(ax)
    w = 0.8 / len(vendors)
    for j, v in enumerate(vendors):
        xs = [i - 0.4 + w * (j + 0.5) for i, y in enumerate(grid[v]) if y is not None]
        ax.bar(xs, [y for y in grid[v] if y is not None], width=w * 0.9, color=fc.SERIES[j % len(fc.SERIES)],
               label=_short_vendor(v))
    ax.set_xticks(range(len(types)))
    ax.set_xticklabels(types, fontsize=7.5, color=fc.INK)
    ax.set_ylabel("largest size held (mm)")
    ax.set_ylim(0, max(y for v in vendors for y in grid[v] if y is not None) * 1.3)
    ax.set_title("Largest fender size in the database by type and vendor", fontsize=9.5, loc="left")
    ax.legend(fontsize=7, ncol=3, frameon=False, loc="upper left")
    fig.savefig(path, dpi=160)
    plt.close("all")
    return Path(path).name


def n_governing_bodies(bodies: list[dict]) -> int:
    """Bodies that publish a code: two PIANC working groups count once, a published formula not at all."""
    return len({b["governing_body"].split(" (")[0] for b in bodies if not b["governing_body"].startswith("Published formula")})


def summary_figure_values(res, cnt, summary, bodies) -> dict:
    """Values shown on Figure 1 (every one is a value of this build)."""
    ms = method_sensitivity(res)
    top = res.bars[0]
    return dict(n_methods=len(res.methods), n_bodies=n_governing_bodies(bodies),
                db_rows=f"{sum(cnt.values()):,}" if cnt else "0",
                db_vendors=len(vendor_rows(summary)), db_types=len({r["fender_type"] for r in summary}),
                selected=res.selected.label, e_design=_f(res.selected.design_energy_knm),
                util=_util(res.selected.design_energy_knm / res.case["design"].get("fenders_in_contact", 1)
                           / res.capacity_knm),
                spread_pct=ms["spread_pct"], top_input=top.label.split(" (")[0], top_span=f"{top.span_pct:.0f}")


def guide_values(res, cnt, summary, bodies, matrix, cdir: Path) -> dict[str, str]:
    """Template values of guide revision 6 and the two schematics written beside the charts."""
    v = database_values(summary)
    v["bodies_table"] = md_table(
        ["Governing body", "Region", "Code", "Edition", "Held as"],
        [[b["governing_body"], b["region"], b["code"], b["edition"], b["held_as"]] for b in bodies]) if bodies else "Not available for this build."
    v["n_bodies"] = str(n_governing_bodies(bodies))
    sel_v = res.selected.velocity_m_s.value
    pianc = next((r for r in res.methods if r.method == "pianc2002"), res.selected)
    v["v_ratio_pct"] = f"{((0.15 / pianc.velocity_m_s.value) ** 2 - 1.0) * 100.0:.0f}"
    v["interactive_note"] = INTERACTIVE_NOTE
    v["top_span"] = f"{res.bars[0].span_pct:.0f}"
    groups = {g: [s.label for s in res.sweeps if sweep_group(s.parameter) == g] for g in GROUP_NAMES}
    v["design_inputs"] = "; ".join(groups["design"]) or "none for this method"
    v["oper_inputs"] = "; ".join(groups["operational"]) or "none for this method"
    cdir.mkdir(parents=True, exist_ok=True)
    if bodies and matrix and summary:
        (cdir / "guide_summary.svg").write_text(fi.summary_svg(summary_figure_values(res, cnt, summary, bodies)),
                                                encoding="utf-8", newline="\n")
        (cdir / "guide_codes.svg").write_text(fi.codes_svg(bodies, {m["method"]: m for m in matrix}),
                                              encoding="utf-8", newline="\n")
    return v


def interactive_figures(res, made, fender_label, cnt, summary, bodies, matrix, cdir: Path) -> dict[str, str]:
    """Image file name -> interactive figure for the HTML page, from the data the static charts plot."""
    out: dict[str, str] = {}
    n = res.case["design"].get("fenders_in_contact", 1)
    s = res.ship
    if (cdir / "guide_summary.svg").exists():
        out["guide_summary.svg"] = fi.figure("fig-summary", [(cdir / "guide_summary.svg").read_text(encoding="utf-8")])
        out["guide_codes.svg"] = fi.figure("fig-codes", [(cdir / "guide_codes.svg").read_text(encoding="utf-8")])
    size0 = s.dwt_t or s.displacement_t
    out[made["velocity"]] = fi.figure("fig-velocity", [fi.line(
        "Design berthing velocity against ship size",
        [(name, xs, ys, dict(hidden=name.startswith(("Brolsma d", "Brolsma e")))) for name, xs, ys in velocity_series()],
        "Ship size (t): DWT for PIANC 2002; displacement for BS 6349-4, ROM and UFC (log scale)",
        "Berthing velocity (m/s)", xname="Ship size", yname="Velocity", xunit="t", yunit="m/s", logx=True,
        xticks=[1_000, 2_000, 5_000, 10_000, 20_000, 50_000, 100_000, 200_000, 500_000], xdec=0, xhover=0, yhover=3,
        ymin=0, marks=[dict(x=size0, y=res.selected.velocity_m_s.value,
                            label=f"selected {res.selected.velocity_m_s.value:.3f} m/s")],
        alt="__ALT__")], f"charts/{made['velocity']}", "__ALT__")
    cms, kcd = cm_series(res)
    out[made["cm"]] = fi.figure("fig-cm", [fi.line(
        "Added mass factor against keel clearance", cms, "Keel clearance / draught, KC/D (-)", "Added mass factor C_m (-)",
        xname="KC/D", yname="C_m", xhover=2, yhover=3, ymin=1.0,
        vlines=[dict(x=kcd)], keys=[dict(name=f"Base KC/D {kcd:.3f} (dotted line)", color=fi.MUTED, dash=True)],
        alt="__ALT__")], f"charts/{made['cm']}", "__ALT__")
    utils = [res.method_utilisation(r) for r in res.methods]
    out[made["methods"]] = fi.figure("fig-methods", [fi.bars(
        "Utilisation of the base fender by method",
        [r.label.replace(" (", "|(", 1).replace(", ", ",|", 1) for r in res.methods],
        [dict(name="Utilisation", values=utils,
              colors=[fi.MUTED if fc.outside_validity(r) else fi.SERIES[0] for r in res.methods],
              notes=[f"Design energy {_f(r.design_energy_knm)} kNm"
                     + ("; outside the stated validity of a formula it uses" if fc.outside_validity(r) else "")
                     for r in res.methods])],
        "Energy per fender / capacity (-)", yname="Utilisation", hlines=[dict(y=1.0, label="limit 1.0")],
        keys=[dict(name="Within the method's validity", color=fi.SERIES[0])]
        + ([dict(name="Outside a formula's stated validity", color=fi.MUTED)]
           if any(fc.outside_validity(r) for r in res.methods) else [])
        + [dict(name="Limit 1.0 (dashed red line)", color=fi.LIMIT, dash=True)],
        label_lines=3, vdec=2, yhover=3, alt="__ALT__")], f"charts/{made['methods']}", "__ALT__")
    for sw in res.sweeps:
        name = made.get(f"sweep_{sw.parameter}")
        if name is None:
            continue
        xs = [p.value for p in sw.points]
        band = [dict(x0=sw.range_low, x1=sw.range_high)]
        u = "" if sw.unit in ("-", "") else f" {sw.unit}"
        keys = [dict(name=f"Credible range {_g(sw.range_low)} to {_g(sw.range_high)}{u} (shaded)", color="#e8e7e1"),
                dict(name=f"Base {_g(sw.base_value)}{u} (dotted line)", color=fi.MUTED, dash=True),
                dict(name="Limit (dashed red line)", color=fi.LIMIT, dash=True)]
        xlabel = f"{sw.label} ({sw.unit})"
        energy = fi.line(f"(a) {SWEEP_TITLES.get(sw.parameter, sw.label)}: energy per fender",
                         [("Energy per fender", xs, [p.energy_per_fender_knm for p in sw.points])],
                         xlabel, "Energy per fender (kNm)", xname=sw.label, yname="Energy per fender", xunit=u.strip(),
                         yunit="kNm", ymin=0, yhover=1, bands=band, vlines=[dict(x=sw.base_value)], keys=keys,
                         hlines=[dict(y=sw.capacity_knm, label=f"energy capacity {sw.capacity_knm:,.0f} kNm ({fender_label})")],
                         alt="__ALT__")
        rf = getattr(sw, "reaction_factor", 1.0)
        reaction = fi.line(f"(b) Reaction per fender" + ("" if rf == 1.0 else f" x {rf:.2f}"),
                           [("Reaction per fender", xs, [p.reaction_kn for p in sw.points], dict(color=fi.SERIES[1]))],
                           xlabel, "Reaction per fender (kN)", xname=sw.label, yname="Reaction per fender",
                           xunit=u.strip(), yunit="kN", ymin=0, yhover=0, bands=band, vlines=[dict(x=sw.base_value)],
                           hlines=([dict(y=sw.reaction_limit_kn, label=f"reaction limit {sw.reaction_limit_kn:,.0f} kN")]
                                   if sw.reaction_limit_kn is not None else []), alt="__ALT__")
        out[name] = fi.figure(f"fig-sweep-{sw.parameter.replace('_', '-')}", [energy, reaction], f"charts/{name}", "__ALT__")
    out[made["tornado"]] = fi.figure("fig-ranking", [fi.ranges(
        "Inputs ranked by the change of design energy over their credible ranges",
        [dict(name=b.label, lo=b.low_change_pct, hi=b.high_change_pct,
              tip=f"{GROUP_NAMES[sweep_group(getattr(b, 'parameter', ''))] if hasattr(b, 'parameter') else 'Input'}: "
                  f"{_g(b.low_value)} gives {_signed_pct(b.low_change_pct)} %; "
                  f"{_g(b.high_value)} gives {_signed_pct(b.high_change_pct)} %; span {b.span_pct:.0f} points")
         for b in res.bars],
        f"Change of design energy from the base {res.selected.design_energy_knm:,.0f} kNm (%)", alt="__ALT__")],
        f"charts/{made['tornado']}", "__ALT__")
    if res.size_checks and "sizes" in made:
        rows = size_rows(res)
        cats = [r["label"] + ("|(base)" if r["is_base"] else "") for r in rows]
        hi = next(i for i, r in enumerate(rows) if r["is_base"])
        r_lim = res.case["design"].get("reaction_limit_kn")
        per = res.selected.design_energy_knm / n
        cap = fi.bars("(a) Design energy capacity", cats,
                      [dict(name="Design capacity", values=[r["energy_knm"] for r in rows],
                            notes=[f"{_signed_pct(r['energy_change_pct'])} % against the base; utilisation "
                                   f"{_util(r['utilisation'])} ({_signed_pct(r['utilisation_change_pct'])} %)" for r in rows])],
                      "Design energy capacity (kNm)", yname="Design capacity", yunit="kNm", highlight=hi, label_lines=2,
                      vdec=0, yhover=0, hlines=[dict(y=per, label=f"energy per fender {per:,.0f} kNm")], alt="__ALT__")
        rea = fi.bars("(b) Design reaction at rated deflection", cats,
                      [dict(name="Design reaction", color=fi.SERIES[1], values=[r["reaction_kn"] for r in rows],
                            notes=[f"{_signed_pct(r['reaction_change_pct'])} % against the base" for r in rows])],
                      "Design reaction (kN)", yname="Design reaction", yunit="kN", highlight=hi, label_lines=2, vdec=0,
                      yhover=0, hlines=([dict(y=r_lim, label=f"reaction limit {r_lim:,.0f} kN")] if r_lim else []),
                      alt="__ALT__")
        out[made["sizes"]] = fi.figure("fig-sizes", [cap, rea], f"charts/{made['sizes']}", "__ALT__")
    if cnt and "database" in made:
        items = sorted(cnt.items(), key=lambda kv: -kv[1])
        out[made["database"]] = fi.figure("fig-db-types", [fi.ranges(
            "Rows in the fender database by fender type",
            [dict(name=k, lo=0, hi=v, tip=f"{v:,} rows") for k, v in items], "Rows in the fender database (-)",
            alt="__ALT__")], f"charts/{made['database']}", "__ALT__")
    if summary and "database_sizes" in made:
        vendors, types, grid, notes = _size_grid(summary)
        out[made["database_sizes"]] = fi.figure("fig-db-sizes", [fi.bars(
            "Largest fender size in the database by type and vendor",
            [{"hydropneumatic": "hydro-|pneumatic", "foam-filled": "foam-|filled"}.get(x, x) for x in types],
            [dict(name=_short_vendor(v), values=grid[v], notes=notes[v]) for v in vendors],
            "Largest size held (mm)", yname="Largest size", yunit="mm", vdec=0, yhover=0, value_labels=False,
            label_lines=2,
            alt="__ALT__")], f"charts/{made['database_sizes']}", "__ALT__")
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--case", required=True, type=Path)
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--catalogue", type=Path)
    ap.add_argument("--index", type=Path, default=Path(__file__).resolve().parents[3] / "data" / "fender_design" / "catalogue_index.csv",
                    help="catalogue index (row counts by type) for the coverage chart")
    ap.add_argument("--data", type=Path, default=Path(__file__).resolve().parents[3] / "data" / "fender_design",
                    help="public data folder: governing bodies, method matrix, size summary of the database")
    ap.add_argument("--template", type=Path)
    ap.add_argument("--stem", default="report")
    ap.add_argument("--fender-label", default=None)
    ap.add_argument("--extra-values", type=Path, help="JSON of further template values")
    ap.add_argument("--allow-unresolved-citations", action="store_true",
                    help="explicit opt-out of the fail-closed citation check: run in citation mode "
                         "'record', flag every unresolved citation and state the opt-out in the report")
    a = ap.parse_args(argv)
    res = compute(load_case(a.case), a.catalogue, citations="record" if a.allow_unresolved_citations else None)
    a.out.mkdir(parents=True, exist_ok=True)
    label = a.fender_label or res.base_fender.fender_id
    cnt = None
    if a.index and a.index.exists():
        cnt, n_rows, n_src = index_counts(a.index)
    summary = read_csv(a.data / "catalogue_size_summary.csv")
    bodies, matrix = read_csv(a.data / "governing_bodies.csv"), read_csv(a.data / "method_matrix.csv")
    made = charts(res, a.out, label, cnt, summary)
    data = results_dict(res)
    data["charts"] = made
    data["ufc_f_basis"] = res.fcase.ufc_f_basis
    data["ufc_design_energy_knm_by_f_reading"] = ufc_f_readings(res)
    if cnt:
        data["database_rows"] = n_rows
        data["database_by_type"] = dict(cnt.most_common())
        data["database_sources"] = n_src
    (a.out / "results.json").write_text(json.dumps(data, indent=2, default=str), encoding="utf-8", newline="\n")
    build_workbook(res, a.out / f"{a.stem}.xlsx")
    if a.template:
        values = tables(res)
        for r in res.methods:
            values[f"E_{r.method}"] = _f(r.design_energy_knm)
            values[f"N_{r.method}"] = _f(r.normal_energy_knm)
            values[f"v_{r.method}"] = f"{r.velocity_m_s.value:.3f}"
            values[f"f_{r.method}"] = f"{r.factor.value:.2f}"
        values.update(geometry_values(res))
        values.update(method_values(res))
        values.update(ufc_f_values(res))
        values.update(sweep_figure_values(res, made))
        values.update(summary_values(res))
        values.update(pneumatic_tolerance_note=pneumatic_tolerance_note(res))
        values.update(
            E_design=_f(res.selected.design_energy_knm), E_normal=_f(res.selected.normal_energy_knm),
            capacity=_f(res.capacity_knm), util=_util(data["utilisation"]),
            cs_basis=res.fcase.cs_basis, cc_basis=res.fcase.cc_basis,
            cb_range=f"{fd.CB_PLAUSIBLE[0]:g} to {fd.CB_PLAUSIBLE[1]:g}",
            n_methods=len(res.methods), n_tests="see the test count in the pull request",
            top1=res.bars[0].label, top2=res.bars[1].label, top3=res.bars[2].label,
            rf=f"{res.reaction_factor:.2f}", R_design=_f(res.design.reaction_kn, 0),
            citation_mode=res.citation_mode,
            citation_mode_note=(
                "This report was generated with the explicit citation opt-out (citation mode `record`, "
                "`--allow-unresolved-citations`): the default mode `strict` stops the calculation while the "
                "cited wiki pages are missing. Every unresolved citation is flagged on its result and listed below."
                if res.citation_mode == "record" else
                "This report was generated in the default citation mode `strict`: every cited wiki page resolved."),
        )
        if cnt:
            values["db_rows"] = f"{n_rows:,}"
            values["db_sources"] = n_src
        values.update(guide_values(res, cnt, summary, bodies, matrix, a.out / "charts"))
        if a.extra_values:
            values.update(json.loads(a.extra_values.read_text(encoding="utf-8")))
        render(a.template, values, a.out / f"{a.stem}.md", a.out / f"{a.stem}.html",
               res.case.get("report_title", "Fender design guide"),
               interactive_figures(res, made, label, cnt, summary, bodies, matrix, a.out / "charts"))
    print(json.dumps({k: data[k] for k in ("selected", "design_energy_knm", "utilisation")}, default=str))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
