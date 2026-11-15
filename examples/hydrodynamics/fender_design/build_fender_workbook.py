#!/usr/bin/env python3
"""Universal fender design workbook (xlsx) from a case file and a fender catalogue.

Sheets:

* ``README``: what the workbook is, how to use it, the method chosen.
* ``Inputs``: ship, berth and design values with unit and basis. Editing a
  yellow value here updates ``Calc``, ``Velocity``, ``Methods`` and
  ``Fenders``. Choices that select a table row or column (navigation
  condition, condition class, ship type, consequence class, factor
  position, method) are listed below them as fixed at generation: change
  them in the case file and regenerate.
* ``Calc``: block coefficient, radius of gyration, contact geometry, Ce (and
  the K-squared-numerator error, for comparison), keel clearance and Cm by
  three methods, as live formulas.
* ``Velocity``: the Brolsma table (conditions a to e) and log-log
  interpolation at the ship's DWT and displacement, as live formulas.
* ``Methods``: berthing energy by every method. Every coefficient that
  depends on an Inputs value is a formula (velocity by size, Cm, Ce, Cs, Cc,
  Cg, the ROM accidental factor and the WG211 frequency correction); the
  remaining constants depend only on the fixed-at-generation choices.
* ``Fenders``: the catalogue with live utilisation, design reaction and
  checks against the limits in ``Inputs``.
* ``Sensitivity``: one block per input with a chart (values written by the
  generator from the Python module; re-run the generator after editing inputs).
* ``Tornado``, ``Sizes`` (neighbouring sizes against the base), ``Sources``.

Usage::

    python build_fender_workbook.py --case case.yaml --out workbook.xlsx \
        [--catalogue fender_catalogue.csv]
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

from openpyxl import Workbook
from openpyxl.chart import Reference, ScatterChart, Series
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.workbook.defined_name import DefinedName

from digitalmodel.hydrodynamics import fender_design as fd

try:  # run as a script or imported from tests
    from fender_case import compute, load_case
except ImportError:  # pragma: no cover
    from .fender_case import compute, load_case

HEAD = Font(bold=True)
INPUT_FILL = PatternFill("solid", fgColor="FFF4CC")
WARN_FONT = Font(bold=True, color="9C0006")
WARN_FILL = PatternFill("solid", fgColor="FFC7CE")
WRAP = Alignment(wrap_text=True, vertical="top")


def _name(wb, name, ref):
    wb.defined_names[name] = DefinedName(name, attr_text=ref)


def _header(ws, row, labels, widths=None):
    for j, label in enumerate(labels, start=1):
        c = ws.cell(row=row, column=j, value=label)
        c.font = HEAD
    if widths:
        for j, w in enumerate(widths, start=1):
            ws.column_dimensions[chr(64 + j)].width = w


INPUT_ROWS = (
    # name, label, unit, getter
    ("M", "Displacement at berthing, M", "t", lambda c: c["ship"]["displacement_t"]),
    ("DWT", "Deadweight", "t", lambda c: c["ship"].get("dwt_t")),
    ("LBP", "Length between perpendiculars", "m", lambda c: c["ship"]["lbp_m"]),
    ("BEAM", "Beam", "m", lambda c: c["ship"]["beam_m"]),
    ("T", "Draught at berthing (not moulded depth)", "m", lambda c: c["ship"]["draft_m"]),
    ("RHO", "Water density", "t/m3", lambda c: c["ship"].get("water_density_t_m3", 1.025)),
    ("DEPTH", "Water depth at the berth", "m", lambda c: c["berth"]["water_depth_m"]),
    ("X", "First contact aft of the forward perpendicular", "m", lambda c: c["berth"]["contact_x_from_bow_m"]),
    ("ALPHA", "Berthing angle", "deg", lambda c: c["berth"].get("berthing_angle_deg", 0.0)),
    ("STRUCT", "Berth structure: open, semi_closed or closed", "-", lambda c: c["berth"].get("structure", "open")),
    ("DEFL", "Fender deflection (for Cs)", "m", lambda c: c["berth"].get("fender_deflection_m", 1.0)),
    ("TUGS", "Tug-assisted berthing (1 yes, 0 no)", "-", lambda c: int(bool(c.get("conditions", {}).get("tugs", True)))),
    ("NCALLS", "Berthings a year", "1/yr", lambda c: c.get("conditions", {}).get("berthings_per_year", 100.0)),
    ("CONT", "Continuous berth (1 yes, 0 no: dolphins)", "-",
     lambda c: int(bool(c.get("conditions", {}).get("continuous_berth", True)))),
    ("VOVR", "Velocity override for every method (blank: each method's own)", "m/s",
     lambda c: c["design"].get("velocity_m_s")),
    ("N", "Fenders in contact (equal share)", "-", lambda c: c["design"].get("fenders_in_contact", 1)),
    ("ETOL", "Fender energy tolerance (fraction)", "-", lambda c: c["design"].get("energy_tolerance", 0.0)),
    ("RTOL", "Fender reaction tolerance (fraction)", "-", lambda c: c["design"].get("reaction_tolerance", 0.10)),
    ("ANG", "Angular factor on fender energy", "-", lambda c: c["design"].get("angular_factor", 1.0)),
    ("RLIM", "Reaction limit per fender", "kN", lambda c: c["design"].get("reaction_limit_kn")),
    ("PLIM", "Hull pressure limit", "kPa", lambda c: c["design"].get("hull_pressure_limit_kpa")),
)


def _inputs(wb, case):
    ws = wb.create_sheet("Inputs")
    _header(ws, 1, ["Input", "Value", "Unit", "Basis"], [44, 14, 8, 90])
    basis = case.get("basis", {})
    for i, (name, label, unit, get) in enumerate(INPUT_ROWS, start=2):
        ws.cell(row=i, column=1, value=label)
        c = ws.cell(row=i, column=2, value=get(case))
        c.fill = INPUT_FILL
        ws.cell(row=i, column=3, value=unit)
        ws.cell(row=i, column=4, value=basis.get(name, ""))
        _name(wb, name, f"Inputs!$B${i}")
    r = len(INPUT_ROWS) + 3
    head = ws.cell(row=r, column=1, value="Fixed at generation: change in the case file and REGENERATE the workbook")
    head.font = WARN_FONT
    head.fill = WARN_FILL
    ws.cell(row=r, column=4, value=(
        "These choices select table rows, columns or constants in Methods; editing them here would "
        "not update Methods. Sensitivity, Tornado and Sizes are also written by the generator."))
    for k, (label, get) in enumerate(FIXED_ROWS, start=r + 1):
        ws.cell(row=k, column=1, value=label)
        v = get(case)
        ws.cell(row=k, column=2, value="-" if v is None else str(v))
        ws.cell(row=k, column=3, value="regenerate after change").font = WARN_FONT
    return ws


FIXED_ROWS = (
    ("Selected method", lambda c: c["design"]["method"]),
    ("Brolsma navigation condition (a to e)", lambda c: c["berth"].get("navigation", "b")),
    ("Condition class (WG211, ROM, UFC, EAU, IS 4651)", lambda c: c.get("conditions", {}).get("condition", "moderate")),
    ("Ship type (abnormal factor, hull pressure)", lambda c: c["ship"].get("ship_type", "bulk")),
    ("Abnormal factor position (largest or smallest ships)", lambda c: c["design"].get("factor_position", "smallest")),
    ("Factor override (blank: each method's own)", lambda c: c["design"].get("factor")),
    ("WG211 consequence class", lambda c: c.get("conditions", {}).get("consequence_class", "B")),
    ("WG211 spread of displacements CoV_M", lambda c: c.get("conditions", {}).get("cov_m", "high")),
    ("WG211 multiple contact", lambda c: c.get("conditions", {}).get("multiple_contact", False)),
    ("Base fender, curve, catalogue", lambda c: f"{c['design'].get('base_fender_id')}; {c['design'].get('curve')}; "
                                              f"{c['database'].get('catalogue')}"),
)


CALC_ROWS = (
    ("CB", "Block coefficient Cb = M / (rho LBP B T)", "=M/(RHO*LBP*BEAM*T)", "PIANC 2002 4.2.4"),
    ("K", "Radius of gyration K = (0.19 Cb + 0.11) LBP", "=(0.19*CB+0.11)*LBP", "PIANC 2002 4.2.4"),
    ("XG", "Contact forward of the centre of mass (LBP/2 - x)", "=LBP/2-X", "centre of mass at mid-LBP"),
    ("RCG", "R = distance contact to centre of mass", "=SQRT(XG^2+(BEAM/2)^2)", "PIANC 2002 4.2.4"),
    ("RN", "Component of R along the berth normal", "=ABS(XG*SIN(RADIANS(ALPHA))+BEAM/2*COS(RADIANS(ALPHA)))", "geometry"),
    ("PHI", "phi = angle between velocity and R", "=DEGREES(ACOS(MIN(1,RN/RCG)))", "PIANC 2002 4.2.4; SFT p. 19 gamma"),
    ("CE", "Ce = (K^2 + R^2 cos^2 phi) / (K^2 + R^2)", "=(K^2+RCG^2*COS(RADIANS(PHI))^2)/(K^2+RCG^2)", "PIANC 2002 4.2.4"),
    ("CE_ERR", "Error form (K^2 + K^2 cos^2 phi)/(K^2 + R^2): do not use", "=(K^2+K^2*COS(RADIANS(PHI))^2)/(K^2+RCG^2)", "for comparison only"),
    ("KCD", "Keel clearance / draught KC/D", "=(DEPTH-T)/T", ""),
    ("CM_P", "Cm, PIANC 2002 keel clearance", "=IF(KCD<=0.1,1.8,IF(KCD>=0.5,1.5,1.875-0.75*KCD))", "PIANC 2002 4.2.5 (SFT p. 18)"),
    ("CM_VC", "Cm, Vasco Costa 1 + 2T/B", "=1+2*T/BEAM", "Vasco Costa 1964"),
    ("CM_U", "Cm, Ueda 1 + pi T / (2 Cb B)", "=1+PI()*T/(2*CB*BEAM)", "Ueda 1981"),
    ("XA", "Contact distance from midships along the ship, a = |LBP/2 - x|", "=ABS(LBP/2-X)", "UFC, IS 4651"),
    ("CE_UFC", "Ce, UFC k^2 / (a^2 + k^2)", "=K^2/(XA^2+K^2)", "UFC 4-152-01 5-2.1"),
    ("CG_UFC", "Cg, UFC 0.95 at or beyond the quarter points, else 1.0", "=IF(XA>=LBP/4-1E-9,0.95,1)",
     "UFC 4-152-01 5-2.1"),
    ("CE_IS", "Ce, IS 4651 (1 + (l/r)^2 sin^2 theta) / (1 + (l/r)^2), r = L/4",
     "=(1+(XA/(0.25*LBP))^2*SIN(RADIANS(ALPHA))^2)/(1+(XA/(0.25*LBP))^2)", "IS 4651-3 5.2.1.3"),
    ("CS_P", "Cs, PIANC 2002: 1.0 above 0.15 m deflection, else 0.9", "=IF(DEFL>0.15,1,0.9)", "PIANC 2002 4.2.6"),
    ("CS_ROM", "Cs, ROM 2.0-11: 1.0 above 0.15 m and LBP below 300 m, else 0.9",
     "=IF(AND(DEFL>0.15,LBP<300),1,0.9)", "ROM 2.0-11 p. 272"),
    ("CC_P", "Cc, PIANC 2002 table (with Vasco Costa or Ueda Cm)",
     '=IF(OR(STRUCT="open",ABS(ALPHA)>5),1,IF(STRUCT="closed",IF(KCD<=0.5,0.8,0.9),IF(KCD<=0.5,0.9,1)))',
     "PIANC 2002 4.2.7 (SFT p. 20)"),
    ("CC_UFC", "Cc, UFC: open or angled 1.0, solid 0.8, between 0.9",
     '=IF(OR(STRUCT="open",ABS(ALPHA)>5),1,IF(STRUCT="closed",0.8,0.9))', "UFC 4-152-01 5-2.1"),
    ("CB_UFC", "Berthing coefficient, UFC Cb = Ce Cg Cd Cc (Cd 1.0)", "=CE_UFC*CG_UFC*1*CC_UFC",
     "UFC 4-152-01 Eq. 5-2, p. 106"),
    ("CM_UFC", "Cm, UFC C_M0 + (C_m1 - C_M0)(T/d)^3.5, F = 1.5 Cb (Cb < 0.6) or 0.9, Cb berthing coefficient",
     "=(1.3+1.5*T/BEAM)+(IF(CB_UFC<0.6,1.5*CB_UFC,0.9)*(12.4*(T/BEAM)^0.3-50*T/LBP)-(1.3+1.5*T/BEAM))*(T/DEPTH)^3.5",
     "UFC 4-152-01 5-2.1.1 e, p. 109"),
    ("CG_ROM", "Cg, ROM: 0.95 continuous berth, 1.0 dolphins", "=IF(CONT=1,0.95,1)", "ROM 2.0-11"),
)


def _calc(wb):
    ws = wb.create_sheet("Calc")
    _header(ws, 1, ["Quantity", "Value", "Formula", "Source"], [52, 14, 60, 34])
    for i, (name, label, formula, src) in enumerate(CALC_ROWS, start=2):
        ws.cell(row=i, column=1, value=label)
        ws.cell(row=i, column=2, value=formula)
        ws.cell(row=i, column=3, value=formula[1:])
        ws.cell(row=i, column=4, value=src)
        _name(wb, name, f"Calc!$B${i}")
    return ws


def _velocity(wb):
    ws = wb.create_sheet("Velocity")
    ws.cell(row=1, column=1, value="Brolsma design berthing velocity (m/s), tug-assisted; "
            "PIANC 2002 keyed by DWT (TFADM p. 23), BS 6349-4 by displacement (SFT p. 21)").font = HEAD
    col = 1
    for cond in "abcde":
        pts = fd.BROLSMA_TABLE[cond]
        ws.cell(row=3, column=col, value=f"{cond}: {fd.BROLSMA_CONDITIONS[cond]}").font = HEAD
        ws.cell(row=4, column=col, value="size (t)")
        ws.cell(row=4, column=col + 1, value="v (m/s)")
        for k, (x, v) in enumerate(pts, start=5):
            ws.cell(row=k, column=col, value=x)
            ws.cell(row=k, column=col + 1, value=v)
        last = 4 + len(pts)
        xs = f"Velocity!${_c(col)}$5:${_c(col)}${last}"
        ys = f"Velocity!${_c(col + 1)}$5:${_c(col + 1)}${last}"
        _name(wb, f"VX_{cond}", xs)
        _name(wb, f"VY_{cond}", ys)
        col += 3
    r0 = 28
    ws.cell(row=r0, column=1, value="Interpolated (log-log)").font = HEAD
    ws.cell(row=r0 + 1, column=1, value="condition")
    ws.cell(row=r0 + 1, column=2, value="at DWT")
    ws.cell(row=r0 + 1, column=3, value="at displacement")
    for i, cond in enumerate("abcde"):
        r = r0 + 2 + i
        ws.cell(row=r, column=1, value=cond)
        for j, size in ((2, "DWT"), (3, "M")):
            ws.cell(row=r, column=j, value=_loglog_formula(size, cond))
            _name(wb, f"V_{cond}_{'DWT' if size == 'DWT' else 'M'}", f"Velocity!${_c(j)}${r}")
    _code_velocity_tables(wb, ws, 40)
    return ws


def _table(wb, ws, row, title, header, rows, names):
    """Write a small table with a title; name each column (``names[j]`` or None)."""
    ws.cell(row=row, column=1, value=title).font = HEAD
    for j, h in enumerate(header, start=1):
        ws.cell(row=row + 1, column=j, value=h)
    for k, vals in enumerate(rows, start=row + 2):
        for j, v in enumerate(vals, start=1):
            ws.cell(row=k, column=j, value=v)
    last = row + 1 + len(rows)
    for j, n in enumerate(names, start=1):
        if n:
            _name(wb, n, f"Velocity!${_c(j)}${row + 2}:${_c(j)}${last}")
    return last + 2


def _code_velocity_tables(wb, ws, row):
    """UFC, ROM and EAU velocity tables (digitized, SI) for the Methods formulas."""
    lt, ft = 1.0160469088, 0.3048
    row = _table(wb, ws, row, "UFC 4-152-01 Figs. 5-3/5-4 (digitized), converted to t and m/s",
                 ["displacement (t)", "sheltered", "moderate", "exposed"],
                 [(x * lt, a * ft, b * ft, c * ft) for x, a, b, c in fd.UFC_VELOCITY_FT_S],
                 ["UFX", "UFY_1", "UFY_2", "UFY_3"])
    row = _table(wb, ws, row, "ROM 2.0-11 Tables 4.6.4.36 (tugs) and 4.6.4.37 (no tugs), digitized; "
                 "linear in ln(displacement), constant outside 2,000 to 100,000 t",
                 ["ln(displacement)", "tugs favourable", "tugs moderate", "tugs unfavourable",
                  "no tugs favourable", "no tugs moderate", "no tugs unfavourable"],
                 [(math.log(t[0]), t[1], t[2], t[3], n[1], n[2], n[3])
                  for t, n in zip(fd.ROM_VELOCITY_TUGS, fd.ROM_VELOCITY_NO_TUGS)],
                 ["RLX", "RT_1", "RT_2", "RT_3", "RN_1", "RN_2", "RN_3"])
    _table(wb, ws, row, "EAU 2012 characteristic velocity (digitized at two sizes); constant above 50,000 DWT",
           ["DWT", "favourable", "normal", "unfavourable"], list(fd.EAU_VELOCITY),
           ["EAX", "EAY_1", "EAY_2", "EAY_3"])


def _loglog_named(size, x, y):
    """Log-log interpolation formula body between named columns; #N/A outside."""
    i = f"MATCH({size},{x},1)"
    x0, x1 = f"INDEX({x},{i})", f"INDEX({x},{i}+1)"
    y0, y1 = f"INDEX({y},{i})", f"INDEX({y},{i}+1)"
    return (f"IF(OR({size}<INDEX({x},1),{size}>INDEX({x},COUNT({x}))),NA(),"
            f"IF({size}=INDEX({x},COUNT({x})),INDEX({y},COUNT({y})),"
            f"EXP(LN({y0})+(LN({size})-LN({x0}))/(LN({x1})-LN({x0}))*(LN({y1})-LN({y0})))))")


def _rom_velocity_formula(j):
    x = "MIN(MAX(LN(M),INDEX(RLX,1)),INDEX(RLX,COUNT(RLX)))"

    def lin(y):
        i = f"MATCH({x},RLX,1)"
        return (f"IF({x}>=INDEX(RLX,COUNT(RLX)),INDEX({y},COUNT({y})),"
                f"INDEX({y},{i})+({x}-INDEX(RLX,{i}))/(INDEX(RLX,{i}+1)-INDEX(RLX,{i}))"
                f"*(INDEX({y},{i}+1)-INDEX({y},{i})))")
    return f"IF(TUGS=1,{lin(f'RT_{j}')},{lin(f'RN_{j}')})"


def _c(col):
    s = ""
    while col:
        col, rem = divmod(col - 1, 26)
        s = chr(65 + rem) + s
    return s


def _loglog_formula(size, cond):
    x, y = f"VX_{cond}", f"VY_{cond}"
    i = f"MATCH({size},{x},1)"
    x0, x1 = f"INDEX({x},{i})", f"INDEX({x},{i}+1)"
    y0, y1 = f"INDEX({y},{i})", f"INDEX({y},{i}+1)"
    return (f"=IF({size}=INDEX({x},COUNT({x})),INDEX({y},COUNT({y})),"
            f"EXP(LN({y0})+(LN({size})-LN({x0}))/(LN({x1})-LN({x0}))*(LN({y1})-LN({y0}))))")


VEL_REF = {"pianc2002": "DWT", "ueda": "DWT", "bs6349": "M"}
CM_REF = {"pianc2002": "=CM_P", "wg211": "=CM_P", "eau": "=CM_P", "rom": "=CM_P",
          "bs6349": "=CM_VC", "ueda": "=CM_U", "ufc": "=CM_UFC", "is4651": "=CM_VC"}
CE_REF = {"pianc2002": "=CE", "wg211": "=CE", "eau": "=CE", "rom": "=CE", "bs6349": "=CE", "ueda": "=CE",
          "ufc": "=CE_UFC", "is4651": "=CE_IS"}
#: Cs and Cc formulas; a method absent here has a constant the method fixes
#: (WG211 removes both, UFC Cd 1.0, IS 4651 Cs 0.9 and no Cc, Cc 1.0 with the
#: PIANC keel-clearance Cm).
CS_REF = {"pianc2002": "=CS_P", "ueda": "=CS_P", "bs6349": "=CS_P", "eau": "=CS_P", "rom": "=CS_ROM"}
CC_REF = {"ueda": "=CC_P", "bs6349": "=CC_P", "ufc": "=CC_UFC"}
OTHER_REF = {"ufc": "=CG_UFC", "rom": "=CG_ROM"}
_CLASS = {"monitored": 1, "favourable": 1, "moderate": 2, "unfavourable": 3}


def _velocity_formula(r, res, case):
    """Velocity cell: a formula on Inputs for every size-dependent table; the
    WG211 value depends only on the condition class (fixed at generation)."""
    j = _CLASS[res.fcase.condition]
    if r.method in VEL_REF:
        own = f"V_{res.berth.navigation}_{VEL_REF[r.method]}"
    elif r.method == "ufc":
        own = _loglog_named("M", "UFX", f"UFY_{j}")
    elif r.method == "rom":
        own = _rom_velocity_formula(j)
    elif r.method == "eau":
        own = f"IF(DWT<INDEX(EAX,1),NA(),IF(DWT>=INDEX(EAX,2),INDEX(EAY_{j},2),{_loglog_named('DWT', 'EAX', f'EAY_{j}')}))"
    elif r.method == "is4651":
        a, b, c, d = fd.IS4651_VELOCITY[res.fcase.condition]
        own = f"IF(M<=5000,{a},IF(M<=10000,{b},IF(M<=100000,{c},{d})))"
    else:  # wg211
        own = repr(fd.WG211_VELOCITY[res.fcase.condition][0])
    return f'=IF(VOVR="",{own},VOVR)'


def _factor_formula(r, res, i):
    """Factor cell: the ROM product and the WG211 frequency correction follow
    Inputs; other factors depend only on fixed-at-generation choices."""
    if r.factor.source == "caller":
        return r.factor.value
    if r.method == "rom":
        return (f"=MAX(IF(M<=85000,1.5,1.25)*IF(TUGS=1,1,1.25)*IF(B{i}<=0.1,1.2,1)"
                f"*IF(NCALLS>300,1.15,1),1.1)")
    if r.method == "wg211":
        c = res.fcase
        g_ref = fd.wg211_gamma_eref(c.condition, c.cov_m, c.consequence_class, c.multiple_contact)
        (a1, b1), (a2, b2) = fd.WG211_GAMMA_N[c.consequence_class]
        return f"={g_ref!r}*(IF(NCALLS<=100,{a1!r},{a2!r})*LN(NCALLS)+IF(NCALLS<=100,{b1!r},{b2!r}))"
    return r.factor.value


def _rom_cc_formula(i):
    return (f'=IF(OR(KCD>0.5,ABS(ALPHA)>5,B{i}<0.2,STRUCT="open",CONT=0),1,'
            f'IF(STRUCT="closed",0.9,1))')


def _methods(wb, res):
    ws = wb.create_sheet("Methods")
    cols = ["Method", "v (m/s)", "Cm", "Ce", "Cs", "Cc", "Other (Cg)", "Normal energy (kNm)", "Factor",
            "Design energy (kNm)", "Capacity divisor", "Velocity basis", "Cm basis", "Ce basis",
            "Factor basis", "Flags (at generation: regenerate after input change)"]
    _header(ws, 1, cols)
    for j, w in enumerate([34, 9, 8, 8, 6, 6, 9, 14, 8, 14, 9, 55, 40, 40, 55, 50], start=1):
        ws.column_dimensions[_c(j)].width = w
    sel_row = None
    for i, r in enumerate(res.methods, start=2):
        ws.cell(row=i, column=1, value=r.label)
        ws.cell(row=i, column=2, value=_velocity_formula(r, res, res.case))
        ws.cell(row=i, column=3, value=CM_REF.get(r.method, r.cm.value))
        ws.cell(row=i, column=4, value=CE_REF.get(r.method, r.ce.value))
        ws.cell(row=i, column=5, value=CS_REF.get(r.method, r.cs.value))
        ws.cell(row=i, column=6, value=_rom_cc_formula(i) if r.method == "rom" else CC_REF.get(r.method, r.cc.value))
        other = 1.0
        for c in r.extra:
            other *= c.value
        ws.cell(row=i, column=7, value=OTHER_REF.get(r.method, other))
        ws.cell(row=i, column=8, value=f"=0.5*M*B{i}^2*C{i}*D{i}*E{i}*F{i}*G{i}")
        ws.cell(row=i, column=9, value=_factor_formula(r, res, i))
        ws.cell(row=i, column=10, value=f"=H{i}*I{i}")
        ws.cell(row=i, column=11, value=r.capacity_divisor)
        ws.cell(row=i, column=12, value=f"{r.velocity_m_s.source}: {r.velocity_m_s.note}")
        ws.cell(row=i, column=13, value=f"{r.cm.source}: {r.cm.note}")
        ws.cell(row=i, column=14, value=f"{r.ce.source}: {r.ce.note}")
        ws.cell(row=i, column=15, value=f"{r.factor.source}: {r.factor.note}")
        ws.cell(row=i, column=16, value="; ".join(r.flags))
        if r.method == res.selected.method:
            sel_row = i
    n = len(res.methods) + 3
    ws.cell(row=n, column=1, value="Selected method").font = HEAD
    ws.cell(row=n, column=2, value=res.selected.label)
    ws.cell(row=n + 1, column=1, value="Design energy, selected (kNm)").font = HEAD
    ws.cell(row=n + 1, column=10, value=f"=J{sel_row}")
    _name(wb, "E_DESIGN", f"Methods!$J${n + 1}")
    ws.cell(row=n + 2, column=1, value="Energy per fender (kNm)").font = HEAD
    ws.cell(row=n + 2, column=10, value="=E_DESIGN/N")
    _name(wb, "E_FENDER", f"Methods!$J${n + 2}")
    ws.cell(row=n + 3, column=1, value="Capacity divisor, selected (WG211 gamma_m)").font = HEAD
    ws.cell(row=n + 3, column=10, value=f"=K{sel_row}")
    _name(wb, "GAMMA_M", f"Methods!$J${n + 3}")
    if res.skipped:
        ws.cell(row=n + 5, column=1, value="Methods not evaluated for this case").font = HEAD
        for k, (m, why) in enumerate(res.skipped.items(), start=n + 6):
            ws.cell(row=k, column=1, value=m)
            ws.cell(row=k, column=2, value=why)
    return ws, sel_row


def _fenders(wb, res):
    ws = wb.create_sheet("Fenders")
    cols = ["Fender id", "Manufacturer", "Type", "D or H (mm)", "L (mm)", "p0 (kPa)",
            "Rated E (kNm)", "Rated R (kN)", "Hull p (kPa)", "Defl (%)", "Capacity (kNm)",
            "Utilisation", "Design R (kN)", "Design p (kPa)", "Energy OK", "Reaction OK",
            "Pressure OK", "Source"]
    _header(ws, 1, cols)
    for j, w in enumerate([16, 22, 12, 9, 8, 7, 10, 10, 9, 7, 11, 10, 11, 10, 8, 9, 9, 80], start=1):
        ws.column_dimensions[_c(j)].width = w
    for i, f in enumerate(res.catalogue, start=2):
        vals = [f.fender_id, f.manufacturer, f.fender_type, f.diameter_mm, f.length_mm,
                f.internal_pressure_kpa, f.rated_energy_knm, f.reaction_kn, f.hull_pressure_kpa,
                f.rated_deflection_pct]
        for j, v in enumerate(vals, start=1):
            ws.cell(row=i, column=j, value=v)
        ws.cell(row=i, column=11, value=f"=G{i}*(1-ETOL)*ANG/GAMMA_M")
        ws.cell(row=i, column=12, value=f"=E_FENDER/K{i}")
        # a curve above the rated reaction before the rated point raises rows of its type
        # (fender_case.size_checks); otherwise the rated-deflection value x (1 + tolerance)
        k = res.reaction_peak_factor(f)
        ws.cell(row=i, column=13, value=f"=H{i}*(1+RTOL)" if k == 1.0 else f"=H{i}*{k:.12g}*(1+RTOL)")
        ws.cell(row=i, column=14, value=f'=IF(I{i}="","",I{i}*(1+RTOL))')
        ws.cell(row=i, column=15, value=f'=IF(L{i}<=1,"yes","no")')
        ws.cell(row=i, column=16, value=f'=IF(RLIM="","-",IF(M{i}<=RLIM,"yes","no"))')
        # a set limit with no hull-pressure datum is not met (as size_checks and select_fenders)
        ws.cell(row=i, column=17,
                value=f'=IF(PLIM="","-",IF(N{i}="","no (no datum)",IF(N{i}<=PLIM,"yes","no")))')
        ws.cell(row=i, column=18, value=f.source)
    ws.freeze_panes = "B2"
    ws.auto_filter.ref = f"A1:R{len(res.catalogue) + 1}"
    return ws


def _sensitivity(wb, res):
    ws = wb.create_sheet("Sensitivity")
    note = ws.cell(row=1, column=1, value=(
        "REGENERATE AFTER ANY INPUT CHANGE: one-at-a-time sensitivity around the selected method, written "
        "by the generator from the Python module (not live formulas). Capacity = rated energy x "
        "(1 - tolerance) x angular factor of the base fender"
        + ("" if res.selected.capacity_divisor == 1.0 else
           f" / {res.selected.capacity_divisor:.2f} ({res.selected.label} capacity divisor)")
        + ". Reaction = curve reaction x reaction factor "
        f"{res.reaction_factor:.3f} (1 + tolerance)."))
    note.alignment = WRAP
    note.font = WARN_FONT
    note.fill = WARN_FILL
    row = 3
    for s in res.sweeps:
        ws.cell(row=row, column=1, value=f"{s.label} ({s.unit})").font = HEAD
        ws.cell(row=row, column=6, value=f"base {s.base_value:.6g}; range {s.range_low:g} to {s.range_high:g} ({s.range_basis})")
        hdr = row + 1
        for j, h in enumerate(["value", "design energy (kNm)", "per fender (kNm)", "capacity (kNm)",
                               "utilisation", "reaction x reaction factor (kN)", "reaction limit (kN)"], start=1):
            ws.cell(row=hdr, column=j, value=h).font = HEAD
        for k, p in enumerate(s.points, start=hdr + 1):
            ws.cell(row=k, column=1, value=p.value)
            ws.cell(row=k, column=2, value=p.design_energy_knm)
            ws.cell(row=k, column=3, value=p.energy_per_fender_knm)
            ws.cell(row=k, column=4, value=s.capacity_knm)
            ws.cell(row=k, column=5, value=p.utilisation)
            ws.cell(row=k, column=6, value=p.reaction_kn)
            ws.cell(row=k, column=7, value=s.reaction_limit_kn)
        last = hdr + len(s.points)
        ch = ScatterChart()
        ch.title = s.label
        ch.style = 13
        ch.x_axis.title = f"{s.label} ({s.unit})"
        ch.y_axis.title = "kNm"
        xref = Reference(ws, min_col=1, min_row=hdr + 1, max_row=last)
        for col in (3, 4):
            yref = Reference(ws, min_col=col, min_row=hdr, max_row=last)
            ser = Series(yref, xref, title_from_data=True)
            ser.marker.symbol = "none"
            ch.series.append(ser)
        ch.height, ch.width = 7, 14
        ws.add_chart(ch, f"I{row}")
        row = last + 3
    return ws


def _tornado(wb, res):
    ws = wb.create_sheet("Tornado")
    _header(ws, 1, ["Input", "Low", "High", "Change at low (%)", "Change at high (%)", "Span (pp)", "Range basis"],
            [40, 10, 10, 16, 16, 10, 70])
    for i, b in enumerate(res.bars, start=2):
        for j, v in enumerate([b.label, b.low_value, b.high_value, b.low_change_pct,
                               b.high_change_pct, b.span_pct, b.range_basis], start=1):
            ws.cell(row=i, column=j, value=v)
    _banner(ws, "I1")
    return ws


def _banner(ws, ref):
    c = ws[ref]
    c.value = "Written by the generator: REGENERATE after any input change"
    c.font = WARN_FONT
    c.fill = WARN_FILL


def _sizes(wb, res):
    """Neighbouring sizes with the case's design adjustments (tolerances,
    angular factor, capacity divisor) applied before the limit checks."""
    ws = wb.create_sheet("Sizes")
    _header(ws, 1, ["Fender", "Base", "Rated E (kNm)", "Design capacity (kNm)", "Utilisation",
                    "Rated R (kN)", "Design R (kN)", "Design hull p (kPa)", "Energy met", "Reaction met",
                    "Pressure met", "Deflection (mm)", "dE rated (%)", "dR rated (%)"],
            [18, 6, 12, 14, 11, 12, 12, 13, 10, 11, 11, 13, 11, 11])
    rows = {r.fender.fender_id: r for r in res.sizes}

    def yn(x):
        return "-" if x is None else ("yes" if x else "no")
    for i, c in enumerate(res.size_checks, start=2):
        r = rows[c.fender_id]
        vals = [c.fender_id, "base" if c.is_base else "", c.rated_energy_knm, c.capacity_knm, c.utilisation,
                c.rated_reaction_kn, c.reaction_kn, c.hull_pressure_kpa, yn(c.meets_energy), yn(c.meets_reaction),
                "no (no datum)" if c.pressure_missing else yn(c.meets_pressure), r.deflection_mm, r.change_pct["rated_energy"], r.change_pct["reaction"]]
        for j, v in enumerate(vals, start=1):
            ws.cell(row=i, column=j, value=v)
    from fender_case import size_basis
    ws.cell(row=len(res.size_checks) + 3, column=1, value=size_basis(res))
    _banner(ws, "P1")
    return ws


def _sources(wb):
    ws = wb.create_sheet("Sources")
    _header(ws, 1, ["Id", "Title", "Edition", "Clause", "Verification", "Read in", "Note"],
            [20, 60, 10, 40, 12, 60, 60])
    for i, s in enumerate(fd.SOURCES.values(), start=2):
        for j, v in enumerate([s.source_id, s.title, s.edition, s.clause, s.verification, s.read_in, s.note], start=1):
            ws.cell(row=i, column=j, value=v)
    return ws


def _readme(wb, res):
    ws = wb.active
    ws.title = "README"
    ws.column_dimensions["A"].width = 120
    lines = [
        res.case.get("title", "Fender design workbook"),
        "",
        "Generated by digitalmodel examples/hydrodynamics/fender_design/build_fender_workbook.py "
        "from hydrodynamics.fender_design. The method report is docs/reports/modules/fender_design/.",
        f"Selected method: {res.selected.label}. Design energy {res.selected.design_energy_knm:,.1f} kNm "
        f"({res.case['design'].get('fenders_in_contact', 1)} fender(s) in contact).",
        f"Base fender: {res.base_fender.fender_id} ({res.base_fender.fender_type}); capacity after tolerance "
        + ("and angular factor" if res.selected.capacity_divisor == 1.0 else
           f"angular factor and the {res.selected.label} capacity divisor {res.selected.capacity_divisor:.2f}")
        + f" {res.capacity_knm:,.1f} kNm.",
        "",
        "Yellow cells on Inputs are inputs. Calc, Velocity, Methods and Fenders recalculate in Excel. "
        "The choices listed on Inputs as fixed at generation (red), and the Sensitivity, Tornado and Sizes "
        "sheets and the Methods flags, are written by the generator: REGENERATE after changing them or any input.",
        "The workbook gives numbers only; it does not clear a design. Every value marked secondary "
        "in Sources was read in a vendor manual that reproduces the standard.",
    ]
    for i, t in enumerate(lines, start=1):
        c = ws.cell(row=i, column=1, value=t)
        c.alignment = WRAP
    ws["A1"].font = Font(bold=True, size=13)


def build_workbook(res, out_path) -> Path:
    wb = Workbook()
    _readme(wb, res)
    _inputs(wb, res.case)
    _calc(wb)
    _velocity(wb)
    _methods(wb, res)
    _fenders(wb, res)
    _sensitivity(wb, res)
    _tornado(wb, res)
    if res.sizes:
        _sizes(wb, res)
    _sources(wb)
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    wb.save(out_path)
    return out_path


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--case", required=True, type=Path)
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--catalogue", type=Path, default=None)
    a = ap.parse_args(argv)
    res = compute(load_case(a.case), a.catalogue)
    build_workbook(res, a.out)
    print(f"wrote {a.out}: {res.selected.label} design energy {res.selected.design_energy_knm:.1f} kNm")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
