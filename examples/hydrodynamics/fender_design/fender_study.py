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


def _f(x, nd=1):
    return "-" if x is None else f"{x:,.{nd}f}"


def md_table(header, rows):
    out = ["| " + " | ".join(header) + " |", "|" + "|".join("---" for _ in header) + "|"]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in rows]
    return "\n".join(out)


def charts(res, out: Path, fender_label: str, db_counts=None) -> dict[str, str]:
    cdir = out / "charts"
    made = {}
    cap_label = f"capacity of {fender_label} per fender x {res.case['design'].get('fenders_in_contact', 1)}"
    n = res.case["design"].get("fenders_in_contact", 1)
    made["methods"] = fc.method_chart(
        res.methods, cdir / "methods.png", "Design berthing energy by method (same ship and berth)",
        capacity_knm=res.capacity_knm * n, capacity_label=cap_label).name
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
        # F uses the berthing coefficient Ce Cg Cd Cc of the base contact point
        cbb = ufc.geometry["cb_berthing"]
        series.append((f"UFC 4-152-01 surface vessel (berthing coeff. {cbb:.3f})", ratios,
                       cm_series(lambda d: fd.ufc_added_mass(s, d, cbb))))
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
                      divisor=r.capacity_divisor, flags=list(r.flags),
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
                     reaction_limit_value=s.reaction_limit_value()) for s in res.sweeps],
        sweeps_skipped=res.sweeps_skipped,
        tornado=[dict(parameter=b.parameter, span_pct=b.span_pct) for b in res.bars],
        geometry=res.selected.geometry,
        size_checks=[dict(fender_id=c.fender_id, is_base=c.is_base, rated_energy=c.rated_energy_knm,
                          capacity=c.capacity_knm, utilisation=c.utilisation, rated_reaction=c.rated_reaction_kn,
                          reaction=c.reaction_kn, hull_pressure=c.hull_pressure_kpa,
                          meets_energy=c.meets_energy, meets_reaction=c.meets_reaction,
                          meets_pressure=c.meets_pressure, meets_all=c.meets_all) for c in res.size_checks],
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
    n = res.case["design"].get("fenders_in_contact", 1)
    t = {}
    t["methods_table"] = md_table(
        ["Method", "v (m/s)", "Cm", "Ce", "Cs", "Cc", "Other", "Normal (kNm)", "Factor", "Design (kNm)",
         "Per fender / capacity", "Flags"],
        [[r.label + (" (outside validity)" if fc.outside_validity(r) else ""), f"{r.velocity_m_s.value:.3f}",
          f"{r.cm.value:.3f}", f"{r.ce.value:.3f}", f"{r.cs.value:.2f}",
          f"{r.cc.value:.2f}", f"{math.prod(c.value for c in r.extra) if r.extra else 1.0:.2f}",
          _f(r.normal_energy_knm), f"{r.factor.value:.2f}", _f(r.design_energy_knm),
          f"{r.design_energy_knm / n / (res.capacity_knm * res.selected.capacity_divisor / r.capacity_divisor):.2f}",
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
        [[s.label, f"{s.base_value:.4g} {s.unit}", f"{s.range_low:g} to {s.range_high:g}", s.range_basis,
          f"{s.change_pct(s.range_low):+.0f} % / {s.change_pct(s.range_high):+.0f} %",
          f"{s.at(s.range_low).utilisation:.2f} / {s.at(s.range_high).utilisation:.2f}",
          "-" if s.limit_value() is None else f"{s.limit_value():.4g}",
          f"{_r(s.at(s.range_low).reaction_kn)} / {_r(s.at(s.range_high).reaction_kn)}",
          "-" if s.reaction_limit_value() is None else f"{s.reaction_limit_value():.4g}"] for s in res.sweeps])
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
              f"{c.utilisation:.2f}", _f(c.rated_reaction_kn, 0), _f(c.reaction_kn, 0),
              _f(c.hull_pressure_kpa, 0), yn(c.meets_energy), yn(c.meets_reaction), yn(c.meets_pressure)]
             for c in res.size_checks])
        t["sizes_basis"] = size_basis(res)
    t["sources_table"] = md_table(
        ["Id", "Document", "Edition", "Clause", "Verification", "Read in"],
        [[s.source_id, s.title, s.edition, s.clause, s.verification, s.read_in] for s in fd.SOURCES.values()])
    t["velocity_table"] = md_table(
        ["Size (t)"] + [f"{c}" for c in "abcde"],
        [[f"{x:,.0f}"] + [f"{fd.brolsma_velocity(x, c):.3f}" for c in "abcde"]
         for x in (5_000, 10_000, 20_000, 50_000, 100_000, 200_000)])
    return t


def _citation_rows(res) -> dict:
    rows: dict = {}
    for r in res.methods:
        for c in r.citations:
            st = dict(r.citation_status).get(c.code_id, "not checked")
            ms, _ = rows.setdefault((c.code_id, c.wiki_path), ([], st))
            if r.method not in ms:
                ms.append(r.method)
    return rows


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
        g = res.selected.geometry
        values.update(
            E_design=_f(res.selected.design_energy_knm), E_normal=_f(res.selected.normal_energy_knm),
            capacity=_f(res.capacity_knm), util=f"{data['utilisation']:.2f}",
            cb=f"{g['cb']:.3f}", K=f"{g['k_m']:.2f}", R=f"{g['r_m']:.2f}", phi=f"{g['phi_deg']:.1f}",
            ce=f"{g['ce']:.3f}", ce_err=f"{fd.eccentricity_k2_numerator(g['k_m'], g['r_m'], g['phi_deg']):.3f}",
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
