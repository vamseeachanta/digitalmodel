"""Generic viscous roll damping study: reproduction, benchmarks, sweeps, sensitivity.

Regenerates the calculated numbers of the generic roll damping analysis report
(living report, revision 5 onward) from public study inputs, with source metadata
and the Q6 coefficient table resolved from the private wiki for a complete run:

* the printed numbers of Chakrabarti (2001), Ocean Engineering 28, 915-932;
* the measured barge benchmarks in ``data/roll_damping/benchmark_cases.csv``;
* two illustrative hulls: the hull-library ``generic_barge`` (L 100 m, B 30 m)
  at 5.0 m draught, and a container-ship-proportioned hull (the paper's model
  scaled x100 in main dimensions).

Every verdict is graded against ``data/roll_damping/acceptance_criteria.yaml``
through :mod:`digitalmodel.hydrodynamics.roll_damping_benchmarks`.

Usage::

    python examples/hydrodynamics/roll_damping/generic_roll_damping_study.py \
        --out study.json [--wiki-root <llm-wiki clone>]

``--wiki-root`` is used to resolve both hulls' citations fail-closed; without it
the citation resolver's search order is used. Both sidecars are retained in the
sensitivity and amplitude outputs.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import warnings
from dataclasses import asdict, replace
from pathlib import Path

from digitalmodel.hydrodynamics import roll_damping_components as rc
from digitalmodel.hydrodynamics.diffraction.roll_damping import critical_damping
from digitalmodel.hydrodynamics.roll_damping_benchmarks import (
    load_benchmark_cases,
    verdict,
)

RHO, NU, G = 1025.0, 1.19e-6, 9.80665

# --------------------------------------------------------------------------
# Illustrative hulls
# --------------------------------------------------------------------------
BARGE0 = dict(
    L=100.0, B=30.0, T=5.0, T0=5.0, KG=7.0, KG0=7.0, GM0=None, Cb=1.0,
    kxx=0.35 * 30.0, added_frac=0.25, amp=10.0, bk=0.0, bkl=0.0, Rb=0.0,
)
SHIP0 = dict(
    L=175.0, B=25.4, T=9.5, KG=10.5, KG0=10.5, GM0=1.0, kxx=0.40 * 25.4,
    added_frac=0.20, amp=10.0, sig_mid=0.97,
    Rb=rc.bilge_radius_from_area_coefficient(25.4, 9.5, 0.97),
    bk=0.45, bkl=0.25 * 175.0,
)


def _box_gm(beam: float, draft: float, kg: float) -> float:
    return draft / 2.0 + beam * beam / (12.0 * draft) - kg


def barge_case(p: dict, wiki_root):
    L, B, T, KG = p["L"], p["B"], p["T"], p["KG"]
    vol = p["Cb"] * L * B * T
    m = RHO * vol
    gm = _box_gm(B, T, KG)
    inertia = m * p["kxx"] ** 2
    added = p["added_frac"] * inertia
    stiff = RHO * G * vol * gm
    wn = math.sqrt(stiff / (inertia + added))
    w = wn * p.get("w_factor", 1.0)
    parts = rc.barge_roll_damping(
        rho=RHO, nu=NU, length=L, beam=B, draft=T, kg=KG,
        amplitude=math.radians(p["amp"]), omega=w, block_coefficient=p["Cb"],
        bilge_keel_breadth=p["bk"], bilge_keel_length=p["bkl"],
        bilge_radius=p["Rb"], citation_root=wiki_root,
    )
    z = parts.percent_critical(stiff, inertia, added)
    return parts, z, dict(GM=gm, wn=wn, Tn=2 * math.pi / wn, I=inertia, A=added, C=stiff, m=m)


def _ship_stations(L, B, T, sig_mid, rb_mid, n=21):
    st = []
    for i in range(n):
        xi = 2 * i / (n - 1) - 1
        b = B * max(0.12, (1 - abs(xi) ** 1.8)) ** 0.5
        s = max(0.5, sig_mid - (sig_mid - 0.5) * abs(xi) ** 1.6)
        rb = rb_mid if abs(xi) < 0.3 else None
        st.append(rc.HullSection(x=(xi + 1) * L / 2, beam=b, draft=T, sigma=s, bilge_radius=rb))
    area = sum(
        0.5 * (st[k].beam * st[k].sigma + st[k + 1].beam * st[k + 1].sigma) * (st[k + 1].x - st[k].x)
        for k in range(n - 1)
    ) * T
    return st, area / (L * B * T)


def ship_case(p: dict, wiki_root=None):
    L, B, T, KG = p["L"], p["B"], p["T"], p["KG"]
    st, cb = _ship_stations(L, B, T, p["sig_mid"], p["Rb"])
    vol = cb * L * B * T
    m = RHO * vol
    gm = p["GM0"] - (KG - p["KG0"])
    inertia = m * p["kxx"] ** 2
    added = p["added_frac"] * inertia
    stiff = RHO * G * vol * gm
    wn = math.sqrt(stiff / (inertia + added))
    w = wn * p.get("w_factor", 1.0)
    og = T - KG
    amp = math.radians(p["amp"])
    area = rc.ship_wetted_area(L, T, B, cb)
    rf = rc.friction_radius(cb, area, L, og)
    bf = rc.friction_damping_turbulent(RHO, NU, area, rf, amp, w)
    strip = rc.eddy_damping_strip(st, RHO, og, amp, w, exclude_non_positive=True)
    bbk = 0.0
    if p["bk"] > 0:
        bbk = p["bkl"] * rc.bilge_keel_damping_section(
            RHO, B, T, p["sig_mid"], og, p["bk"], amp, w, p["Rb"])
    flags = list(strip.flags)
    if amp > rc.PAPER_EXAMPLE_AMPLITUDE:
        flags.append(rc.FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES)
    sections = [
        "eqs. 4-5, p. 918 (equivalent linearisation and component sum)",
        "eqs. 8-10, pp. 919-920 (skin friction)",
        "eqs. 18-34, pp. 920-923 (sectional eddy making and strip integration)",
    ]
    if p["bk"] > 0:
        sections.append("eqs. 46-60, pp. 926-927 (bilge keel)")
    citations = tuple(replace(rc.SOURCE_CITATION, section=s) for s in sections)
    parts = rc.RollDampingComponents(bf, strip.damping, 0.0, 0.0, bbk, tuple(flags), citations)
    rc.validate_citations(parts, wiki_root)
    return parts, parts.percent_critical(stiff, inertia, added), dict(
        GM=gm, wn=wn, Tn=2 * math.pi / wn, cb=cb, I=inertia, A=added, C=stiff, m=m,
        eddy_excluded_length_share=strip.excluded_length_share,
    )


# --------------------------------------------------------------------------
# 1. Reproduction of the paper's printed numbers
# --------------------------------------------------------------------------
def reproduction(wiki_root=None) -> dict:
    out = {}
    out["laminar_coef"] = 4 / (3 * math.pi) * 1.328 * math.sqrt(2 * math.pi / 3.22)
    B, D, RB, SIG = 120.0, 19.0, 1.97, 0.999
    out["CR"] = {v: rc.eddy_pressure_ratio(B, D, SIG, 0.0, RB, variant=v)
                 for v in ("chakrabarti2001", "ittc2011", "ittc2024", "as_printed")}
    out["CR_og"] = {og: rc.eddy_pressure_ratio(B, D, SIG, og, RB) for og in (-20.0, -10.0, 0.0, 5.0)}
    out["CR_og_minus10_by_variant"] = {
        v: rc.eddy_pressure_ratio(B, D, SIG, -10.0, RB, variant=v) for v in ("chakrabarti2001", "ittc2011", "ittc2024")}
    rho, L, CB, R0, w = 1.99, 400.0, 0.923, math.radians(10), 0.5

    def zeta(b, d):
        m = rho * L * B * d * CB
        return b / (2 * w * 2.0 * m * (B * B + d * d) / 12)

    st = [rc.HullSection(x=20.0 * i, beam=B, draft=D, sigma=SIG, bilge_radius=RB) for i in range(21)]
    out["t1_eq35_prismatic"] = zeta(rc.barge_eddy_damping(rho, L, B, D, 0.0, R0, w), D)
    per = rc.barge_eddy_damping(rho, 1.0, B, D, 0.0, R0, w)
    vals = [per] * 21
    vals[0] *= 0.7
    vals[-1] *= 0.7
    out["t1_eq35_end_factor"] = zeta(sum(0.5 * (vals[i] + vals[i + 1]) * 20 for i in range(20)), D)
    out["t1_sectional_s0999"] = zeta(rc.eddy_damping_sections(st, rho, 0.0, R0, w), D)
    st99 = [rc.HullSection(x=20.0 * i, beam=B, draft=D, sigma=0.99, bilge_radius=RB) for i in range(21)]
    st99e = [rc.HullSection(x=20.0 * i, beam=B, draft=D, sigma=0.99, bilge_radius=None) for i in range(21)]
    out["t1_sectional_s099_R197"] = zeta(rc.eddy_damping_sections(st99, rho, 0.0, R0, w), D)
    out["t1_sectional_s099_Rsigma"] = zeta(rc.eddy_damping_sections(st99e, rho, 0.0, R0, w), D)
    out["R_from_sigma_099"] = rc.bilge_radius_from_area_coefficient(B, D, 0.99)
    out["d169_eddy"] = zeta(rc.barge_eddy_damping(rho, L, B, 16.9, 0.0, R0, w), 16.9)
    out["container"] = container_model(wiki_root)
    return out


# Simplified Ikeda C_R regression (Kawahara, Maekawa and Ikeda 2009, as restated
# in ITTC 7.5-02-07-04.5 2024), used only to bracket the paper's Fig. 4 model.
# Its Q6 coefficient table is held in the private wiki (pointer in
# data/roll_damping/wiki_tables.csv) and read from the wiki root at run time.
def _load_q6(wiki_root: Path) -> list[list[float]]:
    from digitalmodel.hydrodynamics.roll_damping_benchmarks import RollDampingDataError, load_wiki_tables

    rel = {r["table_id"]: r["wiki_path"] for r in load_wiki_tables()}["ittc2024-simplified-ikeda-q6"]
    reader = csv.DictReader((Path(wiki_root) / rel).read_text(encoding="utf-8").splitlines())
    cols = ("c4", "c3", "c2", "c1", "c0")
    header = reader.fieldnames or []
    if len(set(header)) != len(header) or any(c not in header for c in ("row", *cols)):
        raise RollDampingDataError(table=rel, reason=f"bad Q6 header {header}")
    rows: dict[str, dict] = {}
    for r in reader:
        if r["row"] in rows:
            raise RollDampingDataError(table=rel, reason=f"duplicate Q6 row {r['row']!r}")
        rows[r["row"]] = r
    need = ("q6_ae", "q6_be1", "q6_be2")
    missing = [k for k in need if k not in rows]
    if missing:
        raise RollDampingDataError(table=rel, reason=f"missing Q6 rows {missing}")
    try:
        return [[float(rows[k][c]) for c in cols] for k in need]
    except (TypeError, ValueError) as exc:
        raise RollDampingDataError(table=rel, reason=f"non-numeric Q6 value: {exc}") from None


def _simplified_cr(q, x1, x2, x3, x4):
    ae = (-0.0182 * x2 + 0.0155) * (x1 - 1.8) ** 3 + sum(q[0][i] * x2 ** (4 - i) for i in range(5))
    be1 = ((-0.2 * x1 + 1.6) * (3.98 * x2 - 5.1525) * x4
           * (x4 * sum(q[1][i] * x2 ** (2 - i) for i in range(3))
              + sum(q[1][i + 3] * x2 ** (1 - i) for i in range(2))))
    be2 = (0.25 * x4 + 0.95) * x4 + sum(q[2][i] * x2 ** (4 - i) for i in range(5))
    be3 = (46.5 - 15 * x1) * x2 + 11.2 * x1 - 28.6
    return ae * math.exp(be1 + be2 * x3**be3)


def container_model(wiki_root) -> dict:
    """Fig. 4 container-ship model at Fn 0: eddy and bilge-keel parts (% critical)."""
    rho, L, B, d, cb, r0 = 1000.0, 1.75, 0.254, 0.095, 0.571, math.radians(10)
    inertia = 2 * rho * cb * L * B * d * (B**2 + d**2) / 12
    out = {}
    if wiki_root is None:
        # rows 9 and 10 need the Q6 table from the private wiki; report them as not graded
        return {"missing": "Q6 table not available: run with --wiki-root"}
    q = _load_q6(wiki_root)
    for cm in (0.95, 0.97, 0.99):
        cr = _simplified_cr(q, B / d, cb, cm, 0.0)
        be = 4 / (3 * math.pi) * rho * L * d**4 * r0 * cr  # omega = 1
        bk = rc.bilge_keel_damping_section(rho, B, d, cm, 0.0, 0.0045, r0, 1.0)
        out[str(cm)] = dict(eddy_pct=100 * be / (2 * inertia),
                            bilge_keel_pct=100 * bk * 0.25 * L / (2 * inertia))
    return out


# --------------------------------------------------------------------------
# 2. Literature benchmarks
# --------------------------------------------------------------------------
def literature() -> dict:
    rho = 1000.0
    out = {"kim2015": [], "gu_day2015": []}
    L, B, T, R, TN = 1.0, 0.25, 0.08, 0.025, 1.2
    w = 2 * math.pi / TN
    sigma = 1 - (4 - math.pi) * R**2 / (2 * B * T)
    vol = L * B * T * sigma
    kg, gm = 0.077, 0.0296
    total_inertia = rho * G * vol * gm / w**2
    for deg in (5, 10, 15):
        a = math.radians(deg)
        eq35 = rc.barge_eddy_damping(rho, L, B, T, T - kg, a, w)
        sect = L * rc.eddy_damping_section(rho, B, T, sigma, T - kg, R, a, w)
        out["kim2015"].append(dict(
            amp=deg, eq35_pct=100 * eq35 / (2 * w * total_inertia),
            sectional_pct=100 * sect / (2 * w * total_inertia)))
    B2, T2, I2, W2 = 0.30, 0.05, 0.364, 6.746
    for og in (0.0, -0.025):
        for deg in (10, 15):
            b = rc.barge_eddy_damping(rho, 1.0, B2, T2, og, math.radians(deg), W2)
            out["gu_day2015"].append(dict(OG=og, amp=deg, zeta_pct=100 * b / (2 * I2 * W2)))
    return out


def grade_benchmarks(lit: dict) -> list[dict]:
    """Grade each calculated value against the measured benchmark rows.

    Basis: the calculated value is the eddy part only; the measured value is
    total decay damping, which also holds the wave (radiation) and friction
    parts. The quantity is therefore ``partial_vs_measured_total``, which
    grades only an excess over the measurement. A verdict is given only where
    the condition matches the test (amplitude and roll axis); other rows are
    kept as a bracket with their deviation and ``graded`` False.
    """
    cases = {c.case_id: c for c in load_benchmark_cases()}
    basis = "calculated eddy part only; measured total decay damping"
    rows = []
    for r in lit["kim2015"]:
        meas = cases[f"kim2015-k0-{r['amp']:02d}deg"].damping_pct
        for method, key in (("sectional C_R (strip)", "sectional_pct"), ("eq. 35 sharp corner", "eq35_pct")):
            v = verdict("partial_vs_measured_total", r[key], meas)
            rows.append(dict(case=f"Kim 2015 bare box, {r['amp']} deg", method=method, graded=True,
                             basis=basis, measured=meas, calculated=r[key], **v))
    meas_gd = cases["gu-day2015-rect-15deg"].damping_pct
    for r in lit["gu_day2015"]:
        graded = r["amp"] == 15 and r["OG"] == 0.0  # release amplitude, axis at the waterline
        v = verdict("partial_vs_measured_total", r["zeta_pct"], meas_gd)
        if not graded:
            v["colour"] = "bracket"
        rows.append(dict(case=f"Gu and Day 2015 rectangle B/T 6, {r['amp']} deg, OG {r['OG']} m",
                         method="eq. 35 sharp corner", graded=graded, basis=basis, measured=meas_gd,
                         calculated=r["zeta_pct"], **v))
    return rows


# --------------------------------------------------------------------------
# 3. Illustrative hulls: base, sensitivity, sweeps
# --------------------------------------------------------------------------
def oat(fn, base, variations, wiki_root):
    p0, z0, info0 = fn(base, wiki_root)
    rows = []
    for label, change, input_change_pct in variations:
        q = dict(base)
        q.update(change)
        p, z, info = fn(q, wiki_root)
        dz = 100 * (z / z0 - 1)
        rows.append(dict(label=label, dB=100 * (p.total / p0.total - 1), dz=dz, z=z,
                         Tn=info["Tn"], input_change_pct=input_change_pct, flags=list(p.flags),
                         citations=[asdict(c) for c in p.citations],
                         factor=(dz / input_change_pct) if input_change_pct else None))
    return dict(base=dict(parts=p0.as_dict(), z=z0, flags=list(p0.flags),
                          citations=[asdict(c) for c in p0.citations], **info0), rows=rows)


def illustrative(wiki_root) -> dict:
    b0, s0 = BARGE0, SHIP0
    barge_var = [
        ("Draught -10 % (B/T 6.67)", dict(T=b0["T"] * 0.9), -10.0),
        ("Draught +10 % (B/T 5.45)", dict(T=b0["T"] * 1.1), 10.0),
        ("Roll amplitude 5 deg", dict(amp=5.0), -50.0),
        ("Roll amplitude 15 deg", dict(amp=15.0), 50.0),
        ("Frequency -20 %", dict(w_factor=0.8), -20.0),
        ("Frequency +20 %", dict(w_factor=1.2), 20.0),
        ("KG -10 %", dict(KG=b0["KG"] * 0.9), -10.0),
        ("KG +10 %", dict(KG=b0["KG"] * 1.1), 10.0),
        ("kxx -10 %", dict(kxx=b0["kxx"] * 0.9), -10.0),
        ("kxx +10 %", dict(kxx=b0["kxx"] * 1.1), 10.0),
        ("A44 0.15 I (base 0.25 I)", dict(added_frac=0.15), None),
        ("A44 0.40 I (base 0.25 I)", dict(added_frac=0.40), None),
        ("Bilge keel 0.3 m over 0.5 L (none at base)", dict(bk=0.3, bkl=0.5 * b0["L"]), None),
        ("Bilge keel 0.6 m over 0.5 L (none at base)", dict(bk=0.6, bkl=0.5 * b0["L"]), None),
    ]
    ship_var = [
        ("Draught -10 %", dict(T=s0["T"] * 0.9), -10.0),
        ("Draught +10 %", dict(T=s0["T"] * 1.1), 10.0),
        ("Bilge radius -30 %", dict(Rb=s0["Rb"] * 0.7), -30.0),
        ("Bilge radius +30 %", dict(Rb=s0["Rb"] * 1.3), 30.0),
        ("Bilge keel breadth -30 %", dict(bk=0.45 * 0.7), -30.0),
        ("Bilge keel breadth +30 %", dict(bk=0.45 * 1.3), 30.0),
        ("Roll amplitude 5 deg", dict(amp=5.0), -50.0),
        ("Roll amplitude 15 deg", dict(amp=15.0), 50.0),
        ("Frequency -20 %", dict(w_factor=0.8), -20.0),
        ("Frequency +20 %", dict(w_factor=1.2), 20.0),
        ("KG -2 %", dict(KG=s0["KG"] * 0.98), -2.0),
        ("KG +2 %", dict(KG=s0["KG"] * 1.02), 2.0),
    ]
    out = {"barge": oat(barge_case, b0, barge_var, wiki_root),
           "ship": oat(ship_case, s0, ship_var, wiki_root)}

    amps = [2, 4, 6, 8, 10, 12, 15, 20]
    sweep = []
    for a in amps:
        pb, zb, _ = barge_case(dict(b0, amp=a), wiki_root)
        pk, zk, _ = barge_case(dict(b0, amp=a, bk=0.3, bkl=0.5 * b0["L"]), wiki_root)
        ps, zs, _ = ship_case(dict(s0, amp=a), wiki_root)
        ps0, zs0, _ = ship_case(dict(s0, amp=a, bk=0.0), wiki_root)
        sweep.append(dict(amp=a, barge=zb, barge_bk=zk,
                          ship_no_bk=zs0, ship_bk=zs,
                          barge_citations=[asdict(c) for c in pb.citations],
                          barge_bk_citations=[asdict(c) for c in pk.citations],
                          ship_citations=[asdict(c) for c in ps.citations],
                          ship_no_bk_citations=[asdict(c) for c in ps0.citations],
                          barge_flags=list(pb.flags), barge_bk_flags=list(pk.flags), ship_flags=list(ps.flags)))
    out["amp_sweep"] = sweep

    # B/T sweep at 10 deg, G at the waterline: eq. 35 eddy against the
    # Journee and Massie (2001) eq. 7.174 decay fit (total damping).
    bt = []
    for ratio in (3, 4, 5, 6, 7, 8, 10):
        t = b0["B"] / ratio
        p, z, info = barge_case(dict(b0, T=t, KG=t), wiki_root)
        crit = critical_damping(info["C"], info["I"], info["A"])
        bt.append(dict(B_over_T=ratio, eq35_eddy=100 * p.eddy / crit,
                       jm_kappa=100 * (0.0013 * ratio**2 + 0.5 * math.radians(10)),
                       Tn=info["Tn"]))
    out["bt_sweep_g_at_waterline"] = bt

    # kxx sweep at the base condition
    out["kxx_sweep"] = [dict(kxx_over_B=f, z=barge_case(dict(b0, kxx=f * b0["B"]), wiki_root)[1])
                        for f in (0.30, 0.33, 0.35, 0.38, 0.40, 0.45)]

    # Bilge radius: sectional formula against eq. 35 (base barge, 10 deg)
    p, z, info = barge_case(b0, wiki_root)
    crit = critical_damping(info["C"], info["I"], info["A"])
    og, amp, w = b0["T"] - b0["KG"], math.radians(10), info["wn"]
    eq35 = 100 * rc.barge_eddy_damping(RHO, b0["L"], b0["B"], b0["T"], og, amp, w) / crit
    radius = []
    for r in (0.05, 0.1, 0.2, 0.3, 0.45, 0.6, 0.8, 1.0, 1.5):
        s = 1 - (4 - math.pi) * r**2 / (2 * b0["B"] * b0["T"])
        sect = 100 * b0["L"] * rc.eddy_damping_section(RHO, b0["B"], b0["T"], s, og, r, amp, w) / crit
        radius.append(dict(R=r, R_over_T=r / b0["T"], sigma=s, sectional=sect, eq35=eq35))
    out["radius_sweep"] = radius

    # Methods side by side for the base barge at 10 deg
    pw, zw, iw = barge_case(dict(b0, KG=b0["T"]), wiki_root)
    p5 = barge_case(dict(b0, amp=5.0), wiki_root)[0]
    out["barge_friction_5_10deg"] = dict(friction_5deg=p5.friction, friction_10deg=p.friction,
                                         rise_pct=100 * (p.friction / p5.friction - 1),
                                         share_of_total_pct=100 * p.friction / p.total)
    out["methods_base_barge"] = dict(
        eq35_viscous=z,
        eq35_eddy=100 * p.eddy / crit,
        friction=100 * p.friction / crit,
        eq35_og_factor_1p5=100 * rc.barge_eddy_damping(
            RHO, b0["L"], b0["B"], b0["T"], og, amp, w, og_factor=1.5) / crit,
        sectional_R0p3=[r["sectional"] for r in radius if r["R"] == 0.3][0],
        eq35_g_at_waterline=100 * pw.eddy / critical_damping(iw["C"], iw["I"], iw["A"]),
        jm_kappa_g_at_waterline=100 * (0.0013 * (b0["B"] / b0["T"]) ** 2 + 0.5 * math.radians(10)),
        abs_default=10.0,
    )
    return out


def reproduction_verdicts(rep: dict) -> list[dict]:
    rows = [
        ("1", "Eq. 6-7 coefficient against eq. 10 (0.787)", "identity", rep["laminar_coef"], 0.787, "complete"),
        ("2", "Fig. 5 midbody C_R, derrick barge (about 120)", "figure_read", rep["CR"]["chakrabarti2001"], 120.0, "complete"),
        ("3", "Same, eq. 25 bracketed as printed", "figure_read", rep["CR"]["as_printed"], 120.0, "complete"),
        ("4", "Table 1 eq. 35 + end factor, reduced rakes (0.16)", "printed_table_value", rep["t1_eq35_end_factor"], 0.16, "partly (inertia, KG assumed)"),
        ("5", "Table 1 eq. 35 + end factor, high rakes (0.20)", "printed_table_value", rep["t1_eq35_end_factor"], 0.20, "partly (rakes not modelled)"),
        ("6", "Table 1 sectional formula, sigma 0.999, high rakes (0.59)", "printed_table_value", rep["t1_sectional_s0999"], 0.59, "no (raked ends dominate)"),
        ("7", "Table 1 sectional formula, sigma 0.99, reduced rakes (0.225)", "printed_table_value", rep["t1_sectional_s099_R197"], 0.225, "no (station data not printed)"),
        ("8", "Barge 16.9 ft, total factor, zero speed (text 14 %)", "printed_table_value", 100 * rep["d169_eddy"], 14.0, "no (inertia, KG not printed)"),
    ]
    cont = rep["container"]
    for cm in ("0.95", "0.99"):
        c = cont.get(cm, {})
        rows.append(("9", f"Fig. 4 container-ship model, eddy part (about 2.7 %), C_M {cm}", "figure_read", c.get("eddy_pct"), 2.7, "no (sections, GM, inertia not printed)"))
        rows.append(("10", f"Fig. 4 container-ship model, bilge-keel part (about 2.1 %), C_M {cm}", "figure_read", c.get("bilge_keel_pct"), 2.1, "no (keel section not printed)"))
    out = []
    for n, item, qty, calc, ref, inputs in rows:
        if calc is None:
            out.append(dict(row=n, item=item, quantity=qty, calculated=None, reference=ref, inputs=inputs,
                            colour="not graded", reason=cont.get("missing", "no calculated value")))
            continue
        out.append(dict(row=n, item=item, quantity=qty, calculated=calc, reference=ref,
                        inputs=inputs, **verdict(qty, calc, ref)))
    return out


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--wiki-root", type=Path, default=None)
    args = ap.parse_args()
    warnings.filterwarnings("ignore")
    rep = reproduction(args.wiki_root)
    lit = literature()
    res = dict(
        reproduction=rep,
        reproduction_verdicts=reproduction_verdicts(rep),
        literature=lit,
        benchmark_verdicts=grade_benchmarks(lit),
        illustrative=illustrative(args.wiki_root),
        inputs=dict(barge=BARGE0, ship=SHIP0, rho=RHO, nu=NU, g=G),
    )
    args.out.write_text(json.dumps(res, indent=1, default=str))
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
