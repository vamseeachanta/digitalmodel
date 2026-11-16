#!/usr/bin/env python3
"""Fender design study: charts, results JSON, workbook and report from a case.

Writes, into ``--out``:

* ``charts/*.png``: method comparison, Cm against keel clearance, berthing
  velocity by method, one sensitivity chart per input, tornado, neighbouring
  sizes, and (with ``--database``) the database coverage chart;
* ``results.json``: every number the report quotes;
* ``<stem>.xlsx``: the universal workbook (:mod:`build_fender_workbook`);
* ``<stem>.md`` and ``<stem>.html`` when ``--template`` is given: the report
  rendered from a markdown template whose ``{{name}}`` fields are filled from
  the results.

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
from pathlib import Path

from digitalmodel.hydrodynamics import fender_design as fd

from build_fender_workbook import build_workbook
from fender_case import compute, load_case, size_basis
import fender_charts as fc

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
#: Report figure number of the first sweep chart (after the tornado, Figure 5).
FIRST_SWEEP_FIGURE = 6

#: The two printed energy tolerances of a pneumatic fender, which conflict:
#: (tolerance, where printed). The case chooses; the generic case takes
#: 0.10, the reading that gives the lower capacity.
PNEUMATIC_ENERGY_TOLERANCES = (
    (0.10, "TFADM p. 76"),
    (0.0, "SFT p. 26 for guaranteed energy absorption (GEA)"),
)

#: Performance tolerance on reaction and energy printed in the TFADM p. 76
#: table, for the catalogue types it names; a case tolerance that differs is
#: listed in Table 10 as assumed.
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


def charts(res, out: Path, fender_label: str, db_counts=None) -> dict[str, str]:
    cdir = out / "charts"
    made = {}
    n = res.case["design"].get("fenders_in_contact", 1)
    made["methods"] = method_chart(res, cdir / "methods.png", fender_label).name
    # Cm against keel clearance for the ship's draught and beam
    s = res.ship
    ratios = [0.02 + 0.01 * i for i in range(99)]
    def cm_series(fn):
        return [fn(s.draft_m * (1 + r)) for r in ratios]
    series = [
        ("PIANC 2002 / ROM / WG211 (keel clearance)", ratios, cm_series(lambda d: fd.added_mass_factor(s, d, "pianc2002"))),
        ("Vasco Costa 1 + 2T/B (BS 6349, IS 4651)", ratios, cm_series(lambda d: fd.added_mass_factor(s, d, "vasco_costa"))),
        ("Ueda 1 + pi T/(2 Cb B) (Japan)", ratios, cm_series(lambda d: fd.added_mass_factor(s, d, "ueda"))),
    ]
    ufc = next((r for r in res.methods if r.method == "ufc"), None)
    if ufc is not None:
        # the Cb in F of the base case, by the case's reading (berthing coefficient
        # Ce Cg Cd Cc of the base contact point, or the block coefficient)
        cbf = ufc.geometry["ufc_f_cb"]
        what = "berthing coeff." if res.fcase.ufc_f_basis == "berthing_coefficient" else "block coeff."
        series.append((f"UFC 4-152-01 surface vessel (Cb in F: {what} {cbf:.3f})", ratios,
                       cm_series(lambda d: fd.ufc_added_mass(s, d, cbf))))
    kcd = (res.berth.water_depth_m - s.draft_m) / s.draft_m
    made["cm"] = fc.line_family_chart(
        series, cdir / "cm_methods.png", "Added mass factor Cm against keel clearance",
        "keel clearance / draught KC/D (-)", "Cm (-)",
        marker=(kcd, fd.added_mass_factor(s, res.berth.water_depth_m, "pianc2002"), f"base KC/D {kcd:.3f}"),
        note=f"Ship T {s.draft_m:g} m, B {s.beam_m:g} m, Cb {s.cb:.3f}.").name
    # velocity by method against size
    sizes = [1_000 * (500 ** (i / 80)) for i in range(81)]
    vser = [(f"Brolsma {c} ({fd.BROLSMA_CONDITIONS[c]})", sizes, [fd.brolsma_velocity(x, c) for x in sizes]) for c in "abcde"]
    rs = [x for x in sizes if x >= 2_000]
    vser.append(("ROM 2.0-11 moderate, tugs (digitized)", rs, [fd.rom_velocity(x, "moderate") for x in rs]))
    us = [x for x in sizes if 1_016.1 <= x <= 101_604]
    vser.append(("UFC 4-152-01 moderate (digitized)", us, [fd.ufc_velocity(x, "moderate") for x in us]))
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
        base = next(c for c in res.size_checks if c.is_base)
        rows = [dict(label=c.fender_id, is_base=c.is_base, energy_knm=c.capacity_knm, reaction_kn=c.reaction_kn,
                     energy_change_pct=(c.capacity_knm / base.capacity_knm - 1.0) * 100.0,
                     reaction_change_pct=(c.reaction_kn / base.reaction_kn - 1.0) * 100.0)
                for c in res.size_checks]
        rows.sort(key=lambda r: r["energy_knm"])
        made["sizes"] = fc.size_comparison_chart(
            rows, cdir / "sizes.png", "Neighbouring fender sizes against the base",
            res.selected.design_energy_knm / n, res.case["design"].get("reaction_limit_kn"),
            basis_note=size_basis(res)).name
    if db_counts:
        made["database"] = database_chart(db_counts, cdir / "database_coverage.png")
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
                     limit_value=s.limit_value(), reaction_factor=s.reaction_factor,
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
        ["Input", "Base", "Credible range", "Basis", "Design energy change at range ends",
         "Utilisation at range ends", "Value at utilisation 1",
         f"Reaction x {res.reaction_factor:.2f} at range ends (kN)", "Value at reaction limit"],
        [[s.label, f"{_g(s.base_value)}{_unit(s.unit)}",
          f"{_g(s.range_low)} to {_g(s.range_high)}{_unit(s.unit)}", s.range_basis,
          f"{s.change_pct(s.range_low):+.0f} % / {s.change_pct(s.range_high):+.0f} %",
          f"{_util(s.at(s.range_low).utilisation)} / {_util(s.at(s.range_high).utilisation)}",
          "-" if s.limit_value() is None else f"{_sig(s.limit_value())}{_unit(s.unit)}",
          f"{_r(s.at(s.range_low).reaction_kn)} / {_r(s.at(s.range_high).reaction_kn)}",
          "-" if s.reaction_limit_value() is None else f"{_sig(s.reaction_limit_value())}{_unit(s.unit)}"]
         for s in res.sweeps])
    t["crossing_note"] = crossing_note(res)
    t["tornado_table"] = md_table(
        ["Rank", "Input", "Span of design energy (percentage points)"],
        [[i, b.label, f"{b.span_pct:.0f}"] for i, b in enumerate(res.bars, start=1)])
    if res.size_checks:
        d = res.case["design"]
        r_lim, p_lim = d.get("reaction_limit_kn"), d.get("hull_pressure_limit_kpa")

        def yn(x):
            return "-" if x is None else ("yes" if x else "**no**")
        t["sizes_table"] = md_table(
            ["Fender", "Rated energy (kNm)", "Design capacity (kNm)", "Utilisation",
             "Rated reaction (kN)", "Design reaction (kN)", "Design hull pressure (kPa)",
             "Energy", f"Reaction <= {_f(r_lim, 0)} kN", f"Pressure <= {_f(p_lim, 0)} kPa"],
            [[c.fender_id + (" (base)" if c.is_base else ""), _f(c.rated_energy_knm, 0), _f(c.capacity_knm, 0),
              _util(c.utilisation), _f(c.rated_reaction_kn, 0), _f(c.reaction_kn, 0),
              _f(c.hull_pressure_kpa, 0), yn(c.meets_energy), yn(c.meets_reaction),
              "**no** (no datum)" if c.pressure_missing else yn(c.meets_pressure)]
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


def crossing_note(res) -> str:
    """How each value at utilisation 1 in Table 5 was solved, grouped by method of solution."""
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
    """Section 3.6 sentence: the base fender's energy verdict under each reading."""
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
           "neighbouring sizes and the workbook Fenders sheet", "yes",
           f"not the TFADM p. 76 value ({TFADM_P76_TOLERANCE[t]:.2f})" if t in TFADM_P76_TOLERANCE else
           "no tolerance printed for this type in TFADM p. 76")
          for t, v in by_type.items()
          if not (TFADM_P76_TOLERANCE.get(t) == v["energy_tolerance"] == v["reaction_tolerance"])],
        *[(f"Pneumatic energy tolerance {d.get('energy_tolerance', 0.10):.2f}",
           "base fender capacity (Table 7; workbook ETOL)", "yes",
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
             + ("; Section 3.6)" if alt else ")")]
    if r_lim is not None:
        margin = (r_lim - base.reaction_kn) / r_lim * 100.0
        parts.append(f"design reaction {_f(base.reaction_kn, 0)} kN against the {_f(r_lim, 0)} kN limit "
                     + (f"(met, margin {margin:.1f} %)" if base.meets_reaction else
                        f"(**not met**, {-margin:.1f} % over)"))
    if p_lim is not None:
        p = base.hull_pressure_kpa
        parts.append("hull pressure: no datum against the "
                     f"{_f(p_lim, 0)} kPa limit (**not met**)" if p is None else
                     f"design hull pressure {_f(p, 0)} kPa against the {_f(p_lim, 0)} kPa limit "
                     + ("(met)" if base.meets_pressure else
                        f"(**not met**, {(p / p_lim - 1) * 100:.1f} % over)"))
    failed = [name for name, ok in (("energy", base.meets_energy), ("reaction", base.meets_reaction),
                                    ("hull pressure", base.meets_pressure)) if ok is False]
    verdict = ("The base fender meets every check." if base.meets_all else
               f"The base fender therefore **fails** the {' and '.join(failed)} "
               f"check{'s' if len(failed) > 1 else ''}, and is not a solution for this case as it stands.")
    ok = [c for c in res.size_checks if not c.is_base and c.meets_all]

    def margins(c):
        out = [f"utilisation {_util(c.utilisation)}"]
        if r_lim is not None:
            out.append(f"design reaction {_f(c.reaction_kn, 0)} kN, margin {(r_lim - c.reaction_kn) / r_lim * 100:.1f} %")
        if p_lim is not None and c.hull_pressure_kpa is not None:
            out.append(f"hull pressure {_f(c.hull_pressure_kpa, 0)} kPa, "
                       f"margin {(p_lim - c.hull_pressure_kpa) / p_lim * 100:.1f} %")
        return ", ".join(out)
    sizes = ("Of the neighbouring sizes in Table 7, "
             + (("only " if len(ok) == 1 else "") + " and ".join(c.fender_id for c in ok)
                + (" meets" if len(ok) == 1 else " meet") + " every check at the tolerances of its type ("
                + "; ".join(f"{c.fender_id}: {margins(c)}" for c in ok) + ")"
                if ok else "none meets every check either")
             + "; the fenders are illustrative, so this is not a selection.")
    return dict(base_checks=f"Base fender {base.fender_id} (Table 7): " + "; ".join(parts) + ". " + verdict
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
    and no figure. ``fig_sizes`` is the number of the figure that follows.
    """
    blocks = []
    for s in res.sweeps:
        name = made.get(f"sweep_{s.parameter}")
        if name is None:
            continue
        alt, caption = SWEEP_FIGURES.get(s.parameter, (s.label, f"{s.label}."))
        blocks.append(f"![{alt}](charts/{name})\n\nFigure {first + len(blocks)} – {caption}")
    return dict(sweep_figures="\n\n".join(blocks) or "No sweep chart was generated for this method.",
                fig_sizes=str(first + len(blocks)))


def render(template: Path, values: dict, out_md: Path, out_html: Path, title: str):
    text = template.read_text(encoding="utf-8")
    missing = sorted(set(re.findall(r"\{\{(\w+)\}\}", text)) - set(values))
    if missing:
        raise KeyError(f"template fields without values: {missing}")
    text = re.sub(r"\{\{(\w+)\}\}", lambda m: str(values[m.group(1)]), text)
    out_md.write_text(text, encoding="utf-8", newline="\n")
    import markdown
    body = markdown.markdown(text, extensions=["tables", "toc"])
    css = ("body{font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;"
           "color:#0b0b0b;line-height:1.45}table{border-collapse:collapse;font-size:.85rem;margin:.6rem 0}"
           "th,td{border:1px solid #c3c2b7;padding:.25rem .45rem;text-align:left;vertical-align:top}"
           "th{background:#f0efec}img{max-width:100%;border:1px solid #e1e0d9}code{font-size:.85em}"
           "h1,h2,h3{line-height:1.2}")
    out_html.write_text(
        f"<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>{title}</title>"
        f"<style>{css}</style></head><body>{body}</body></html>", encoding="utf-8", newline="\n")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--case", required=True, type=Path)
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--catalogue", type=Path)
    ap.add_argument("--index", type=Path, default=Path(__file__).resolve().parents[3] / "data" / "fender_design" / "catalogue_index.csv",
                    help="catalogue index (row counts by type) for the coverage chart")
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
    made = charts(res, a.out, label, cnt)
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
        if a.extra_values:
            values.update(json.loads(a.extra_values.read_text(encoding="utf-8")))
        render(a.template, values, a.out / f"{a.stem}.md", a.out / f"{a.stem}.html",
               res.case.get("title", "Fender design report"))
    print(json.dumps({k: data[k] for k in ("selected", "design_energy_knm", "utilisation")}, default=str))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
