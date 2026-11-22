#!/usr/bin/env python3
"""Universal fender design workbook (xlsx) from a case file and a fender catalogue.

Sheets:

* ``README``: what the workbook is, how to use it, the method chosen.
* ``Inputs``: ship, berth and design values with unit and basis. Editing a
  yellow value here updates ``Calc``, ``Velocity``, ``Methods`` and
  ``Fenders``. Choices that select a table row or column (navigation
  condition, condition class, ship type, consequence class, factor
  position, method) are listed below them as fixed at generation: change
  them in the case file and regenerate. Every input has one checked cell on
  ``Calc`` (``INPUT_RULES``: the rule the Python module applies to the
  argument it mirrors), the only cell that reads it; the ``Check`` column
  shows the error text when the module would refuse the value, and the
  input cell carries a data-validation rule (stop) with the same formula.
  docs/reports/modules/fender_design/workbook-input-checks.md lists every
  input and rule.
* ``Calc``: block coefficient, radius of gyration, contact geometry, Ce (and
  the K-squared-numerator error, for comparison), keel clearance and Cm by
  three methods, as live formulas, and the checked inputs (``MC``, ``XC``,
  ``ETOLC`` ...), #N/A when the module refuses the value, so the error
  carries into every energy, capacity, reaction and check that uses it.
  ``INPUTS_OK`` is 1 when every input and catalogue row is accepted, else
  #N/A; every energy, capacity, reaction and check reads it, as the module
  refuses the whole case for any refused input.
* ``Velocity``: the Brolsma table (conditions a to e) and log-log
  interpolation at the ship's DWT and displacement, as live formulas.
* ``Methods``: berthing energy by every method. Every coefficient that
  depends on an Inputs value is a formula (velocity by size, Cm, Ce, Cs, Cc,
  Cg, the ROM accidental factor and the WG211 frequency correction); the
  remaining constants depend only on the fixed-at-generation choices.
* ``Fenders``: the catalogue with live utilisation, design reaction and
  checks against the limits in ``Inputs``. The rated energy, reaction and
  hull pressure are read through checked columns; the row check applies the
  catalogue loader's rules (``FENDER_ROW_RULES``) to every value of the row.
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
from dataclasses import dataclass
from pathlib import Path

from openpyxl import Workbook
from openpyxl.chart import Reference, ScatterChart, Series
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.worksheet.datavalidation import DataValidation
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
    ("ETOL", "Fender energy tolerance (fraction)", "-", lambda c: c["design"].get("energy_tolerance", 0.10)),
    ("RTOL", "Fender reaction tolerance (fraction)", "-", lambda c: c["design"].get("reaction_tolerance", 0.10)),
    ("ANG", "Angular factor on fender energy", "-", lambda c: c["design"].get("angular_factor", 1.0)),
    ("RLIM", "Reaction limit per fender", "kN", lambda c: c["design"].get("reaction_limit_kn")),
    ("PLIM", "Hull pressure limit", "kPa", lambda c: c["design"].get("hull_pressure_limit_kpa")),
    ("UFCF", "UFC factor F reads Cb as: 1 berthing coefficient Ce Cg Cd Cc (printed wording, default), "
     "0 block coefficient", "-",
     lambda c: int(c.get("conditions", {}).get("ufc_f_basis", "berthing_coefficient") == "berthing_coefficient")),
)


def _inputs(wb, case):
    ws = wb.create_sheet("Inputs")
    _header(ws, 1, ["Input", "Value", "Unit", "Basis", "Check"], [44, 14, 8, 90, 60])
    basis = case.get("basis", {})
    for i, (name, label, unit, get) in enumerate(INPUT_ROWS, start=2):
        ws.cell(row=i, column=1, value=label)
        c = ws.cell(row=i, column=2, value=get(case))
        c.fill = INPUT_FILL
        ws.cell(row=i, column=3, value=unit)
        ws.cell(row=i, column=4, value=basis.get(name, ""))
        rule = INPUT_RULES[name]
        ws.cell(row=i, column=5, value=_check_text_formula(name)).font = WARN_FONT
        dv = DataValidation(type="custom", formula1=rule.valid(f"B{i}"), allow_blank=rule.blank is not None,
                            showErrorMessage=True, errorStyle="stop",
                            errorTitle=f"{name} not accepted", error=rule.prompt[:255])
        dv.add(f"B{i}")
        ws.add_data_validation(dv)
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
    ("WG211 spread of displacements CoV_M", lambda c: c.get("conditions", {}).get("cov_m", "low")),
    ("Cs basis (pianc2002: 4.2.6 general value 1.0; tfadm-2024: 0.9 at or below 0.15 m)",
     lambda c: c.get("conditions", {}).get("cs_basis", "pianc2002")),
    ("Cc basis (pianc2002: 4.2.7 values; sft-2026: SFT p. 20 table)",
     lambda c: c.get("conditions", {}).get("cc_basis", "pianc2002")),
    ("Tolerances of fender types other than the base fender's (energy, reaction)",
     lambda c: "; ".join(f"{t}: {v['energy_tolerance']:g}, {v['reaction_tolerance']:g}"
                         for t, v in (c["design"].get("tolerances_by_type") or {}).items()) or None),
    ("WG211 multiple contact", lambda c: c.get("conditions", {}).get("multiple_contact", False)),
    ("Base fender, curve, catalogue", lambda c: f"{c['design'].get('base_fender_id')}; {c['design'].get('curve')}; "
                                              f"{c['database'].get('catalogue')}"),
)


@dataclass(frozen=True)
class InputRule:
    """The Python module's rule for one Inputs value, applied in one checked cell.

    ``kind`` is the type the module takes (``number`` or ``text``); ``cond``
    is the range or allowed set on the value ``{v}`` (``None``: any value of
    the type), evaluated only when the type matches so that a wrong type
    gives #N/A and never another error. ``blank`` is the meaning of an empty
    cell where the module takes None (``None``: blank is refused); the
    checked cell then passes on "", which the formulas read as "not given".
    The same rule text is the data-validation formula on the Inputs cell and
    the condition of the checked cell, so the two limits cannot differ.
    """

    kind: str
    cond: str | None
    error: str
    module: str
    blank: str | None = None

    def _type(self, v):
        return f"ISNUMBER({v})" if self.kind == "number" else f"ISTEXT({v})"

    def valid(self, v):
        """Data-validation formula: TRUE when the module accepts ``v`` (a blank is
        handled by the rule's allow-blank flag)."""
        if self.cond is None:
            return self._type(v)
        cond = self.cond.format(v=v)
        if cond.startswith("AND(") and cond.endswith(")"):
            cond = cond[4:-1]           # one AND: AND(type, a, b) rather than AND(type, AND(a, b))
        return f"AND({self._type(v)},{cond})"

    def checked(self, name):
        """Checked-cell formula: the value when the module accepts it, else #N/A."""
        inner = name if self.cond is None else f"IF({self.cond.format(v=name)},{name},NA())"
        f = f"IF({self._type(name)},{inner},NA())"
        if self.blank is not None:
            f = f'IF({name}="","",{f})'
        return "=" + f

    @property
    def prompt(self):
        """Message Excel shows when a refused value is typed."""
        return self.error.replace("ERROR: ", "") + (f". Blank: {self.blank}" if self.blank else "")


def _pos(what):
    return f"ERROR: {what} must be a number above 0"


def _either(v0, v1):
    return f"OR({{v}}={v0},{{v}}={v1})"


#: Every Inputs value with the rule the module applies to the argument it
#: mirrors (docs/reports/modules/fender_design/workbook-input-checks.md lists
#: each rule with the module function that applies it).
INPUT_RULES: dict[str, InputRule] = {
    # a block coefficient M / (rho LBP B T) above 1 is refused (Ship), with the module's 1e-12 allowance
    # so that M = rho LBP B T (Cb 1, the box) is accepted despite floating-point rounding; the bound
    # reads the checked cells
    "M": InputRule("number", "AND({v}>0,{v}<=RHOC*LBPC*BEAMC*TC*(1+1E-12))",
                   "ERROR: displacement must be a number above 0 and at most rho LBP B T (block coefficient "
                   "up to 1); every energy is #N/A", "Ship.displacement_t, Ship block coefficient"),
    "DWT": InputRule("number", "{v}>0", _pos("deadweight") + " or blank; the methods keyed by DWT are #N/A",
                     "Ship.dwt_t",
                     blank="no DWT: the methods that read a velocity table keyed by DWT (PIANC 2002, Ueda, EAU) "
                           "are #N/A, as the module leaves them out; with a velocity override (VOVR) they are "
                           "evaluated, as in the module"),
    "LBP": InputRule("number", "{v}>0", _pos("LBP") + "; every energy is #N/A", "Ship.lbp_m"),
    "BEAM": InputRule("number", "{v}>0", _pos("beam") + "; every energy is #N/A", "Ship.beam_m"),
    "T": InputRule("number", "{v}>0", _pos("draught") + "; every energy is #N/A", "Ship.draft_m"),
    "RHO": InputRule("number", "{v}>0", _pos("water density") + "; every energy is #N/A",
                     "Ship.water_density_t_m3"),
    "DEPTH": InputRule("number", "AND({v}>0,{v}>TC)",
                       "ERROR: water depth must be a number above 0 and above the draught T (keel clearance); "
                       "every energy is #N/A", "Berth.water_depth_m, _ukc_ratio"),
    "X": InputRule("number", "AND({v}>=0,{v}<=LBPC)",
                   "ERROR: first contact is not on the ship (0 to LBP); every energy and energy check is #N/A",
                   "Berth.contact_x_from_bow_m, contact_offset_m"),
    # below 90 deg in magnitude, and the first contact on the end nearer the berth (contact_offset_m)
    "ALPHA": InputRule("number", "AND(ABS({v})<90,OR({v}=0,AND({v}>0,XC<=LBPC/2),AND({v}<0,XC>=LBPC/2)))",
                       "ERROR: berthing angle must be a number below 90 deg in magnitude, with the first contact "
                       "on the end nearer the berth (angle above 0: X up to LBP/2; below 0: X from LBP/2); every "
                       "energy is #N/A", "Berth.berthing_angle_deg, contact_offset_m"),
    "STRUCT": InputRule("text", 'OR(EXACT({v},"open"),EXACT({v},"semi_closed"),EXACT({v},"closed"))',
                        "ERROR: berth structure must be open, semi_closed or closed (lower case); every energy "
                        "is #N/A", "Berth.structure"),
    "DEFL": InputRule("number", "{v}>0", _pos("fender deflection") + "; every energy is #N/A",
                      "Berth.fender_deflection_m"),
    "TUGS": InputRule("number", _either(0, 1), "ERROR: tug-assisted must be 1 (yes) or 0 (no); the ROM energy is "
                      "#N/A", "Case.tugs"),
    "NCALLS": InputRule("number", "{v}>0", _pos("berthings a year") + "; the ROM and WG211 energies are #N/A "
                        "(below 1 a year the WG211 row alone is #N/A: gamma_n is not extrapolated)",
                        "Case.berthings_per_year, wg211_gamma_n"),
    "CONT": InputRule("number", _either(0, 1), "ERROR: continuous berth must be 1 (yes) or 0 (no); the ROM energy "
                      "is #N/A", "Case.continuous_berth"),
    "VOVR": InputRule("number", "{v}>0", "ERROR: velocity override must be a number above 0 or blank; every "
                      "energy is #N/A", "energy_by_method(velocity_m_s=...)",
                      blank="each method's own velocity"),
    "N": InputRule("number", "AND({v}>=1,{v}=INT({v}))",
                   "ERROR: fenders in contact must be a whole number of 1 or more; energy per fender, "
                   "utilisation and energy checks are #N/A", "check_fenders_in_contact"),
    "ETOL": InputRule("number", "AND({v}>=0,{v}<1)",
                      "ERROR: energy tolerance must be in [0, 1); fender capacity and energy checks are #N/A",
                      "design_performance(energy_tolerance=...)"),
    "RTOL": InputRule("number", "{v}>=0",
                      "ERROR: reaction tolerance must not be negative; design reaction, hull pressure "
                      "and their checks are #N/A", "design_performance(reaction_tolerance=...)"),
    "ANG": InputRule("number", "AND({v}>0,{v}<=1)",
                     "ERROR: angular factor must be in (0, 1]; fender capacity and energy checks are #N/A",
                     "design_performance(angular_factor=...)"),
    "RLIM": InputRule("number", "{v}>0", _pos("reaction limit") + " or blank; Reaction OK is #N/A",
                      "check_limit(reaction_limit_kn)", blank="no reaction limit; Reaction OK shows -"),
    "PLIM": InputRule("number", "{v}>0", _pos("hull pressure limit") + " or blank; Pressure OK is #N/A",
                      "check_limit(hull_pressure_limit_kpa)", blank="no hull-pressure limit; Pressure OK shows -"),
    "UFCF": InputRule("number", _either(0, 1),
                      "ERROR: UFC F reading must be 1 (berthing coefficient) or 0 (block coefficient); "
                      "UFC energy is #N/A", "Case.ufc_f_basis"),
}


def checked_name(name):
    """Defined name of the checked cell of input ``name``: the only cell that reads it."""
    return f"{name}C"


def _check_text_formula(name):
    """Inputs Check column: the error text when the checked cell is #N/A."""
    rule, c = INPUT_RULES[name], checked_name(name)
    if rule.blank is None:
        return f'=IF(ISNA({c}),"{rule.error}","ok")'
    # a blank is reported with its meaning before the checked value is looked at
    return f'=IF({name}="","blank: {rule.blank}",IF(ISNA({c}),"{rule.error}","ok"))'


_LABELS = {n: label for n, label, *_ in INPUT_ROWS}


def _checked_row(name):
    rule = INPUT_RULES[name]
    label = f"{_LABELS[name]} ({name}), checked: #N/A unless the module accepts it"
    return (checked_name(name), label, rule.checked(name), f"fender_design: {rule.module}")


#: Checked cells written in the Calc rows that held them before the other checks were added.
_EARLY_CHECKS = ("X", "ANG", "UFCF", "ETOL", "RTOL")

CALC_ROWS = (
    ("CB", "Block coefficient Cb = M / (rho LBP B T)", "=MC/(RHOC*LBPC*BEAMC*TC)", "PIANC 2002 4.2.4"),
    ("K", "Radius of gyration K = (0.19 Cb + 0.11) LBP", "=(0.19*CB+0.11)*LBPC", "PIANC 2002 4.2.4"),
    ("XG", "Contact forward of the centre of mass (LBP/2 - x), x checked (XC)", "=LBPC/2-XC",
     "centre of mass at mid-LBP"),
    ("RCG", "R = distance contact to centre of mass", "=SQRT(XG^2+(BEAMC/2)^2)", "PIANC 2002 4.2.4"),
    ("RN", "Component of R along the berth normal",
     "=ABS(XG*SIN(RADIANS(ALPHAC))+BEAMC/2*COS(RADIANS(ALPHAC)))", "geometry"),
    ("PHI", "phi = angle between velocity and R", "=DEGREES(ACOS(MIN(1,RN/RCG)))", "PIANC 2002 4.2.4; SFT p. 19 gamma"),
    ("CE", "Ce = (K^2 + R^2 cos^2 phi) / (K^2 + R^2)", "=(K^2+RCG^2*COS(RADIANS(PHI))^2)/(K^2+RCG^2)", "PIANC 2002 4.2.4"),
    ("CE_ERR", "Error form (K^2 + K^2 cos^2 phi)/(K^2 + R^2): do not use", "=(K^2+K^2*COS(RADIANS(PHI))^2)/(K^2+RCG^2)", "for comparison only"),
    ("KCD", "Keel clearance / draught KC/D", "=(DEPTHC-TC)/TC", ""),
    ("CM_P", "Cm, PIANC 2002 keel clearance", "=IF(KCD<=0.1,1.8,IF(KCD>=0.5,1.5,1.875-0.75*KCD))", "PIANC 2002 4.2.5 (SFT p. 18)"),
    ("CM_VC", "Cm, Vasco Costa 1 + 2T/B", "=1+2*TC/BEAMC", "Vasco Costa 1964"),
    ("CM_U", "Cm, Ueda 1 + pi T / (2 Cb B)", "=1+PI()*TC/(2*CB*BEAMC)", "Ueda 1981"),
    ("XA", "Contact distance from midships along the ship, a = |LBP/2 - x|, x checked (XC)", "=ABS(LBPC/2-XC)",
     "UFC, IS 4651"),
    ("CE_UFC", "Ce, UFC k^2 / (a^2 + k^2)", "=K^2/(XA^2+K^2)", "UFC 4-152-01 5-2.1"),
    ("CG_UFC", "Cg, UFC 0.95 at or beyond the quarter points, else 1.0", "=IF(XA>=LBPC/4-1E-9,0.95,1)",
     "UFC 4-152-01 5-2.1"),
    ("CE_IS", "Ce, IS 4651 (1 + (l/r)^2 sin^2 theta) / (1 + (l/r)^2), r = L/4",
     "=(1+(XA/(0.25*LBPC))^2*SIN(RADIANS(ALPHAC))^2)/(1+(XA/(0.25*LBPC))^2)", "IS 4651-3 5.2.1.3"),
    # Cs and Cc of PIANC 2002, Ueda, BS 6349-4 and EAU: the PIANC values here; a case that selects
    # a vendor table (cs_basis, cc_basis) gets the rows of VENDOR_ROWS (calc_rows)
    ("CS_P", "Cs, PIANC 2002 4.2.6 general value 1.0 (option cs_basis: pianc2002)", "=1",
     "PIANC WG33 (2002) 4.2.6, p. 21"),
    ("CS_ROM", "Cs, ROM 2.0-11: 1.0 above 0.15 m and LBP below 300 m, else 0.9",
     "=IF(AND(DEFLC>0.15,LBPC<300),1,0.9)", "ROM 2.0-11 p. 272"),
    ("CC_P", "Cc with Vasco Costa or Ueda Cm, PIANC 2002 4.2.7: open or angle above 5 deg 1.0, solid quay 0.9 "
     "below KC/D 0.15 and 1.0 at or above it (large keel clearance; 0.15 from TFADM p. 27), partly closed 1.0 "
     "(ASSUMED - to be confirmed: no PIANC value) (option cc_basis: pianc2002)",
     '=IF(OR(STRUCTC="open",ABS(ALPHAC)>5),1,IF(STRUCTC="closed",IF(KCD<0.15,0.9,1),1))',
     "PIANC WG33 (2002) 4.2.7, p. 22; KC/D limit TFADM p. 27"),
    ("CC_UFC", "Cc, UFC: open or angled 1.0, solid 0.8, between 0.9",
     '=IF(OR(STRUCTC="open",ABS(ALPHAC)>5),1,IF(STRUCTC="closed",0.8,0.9))', "UFC 4-152-01 5-2.1"),
    ("CB_UFC", "Berthing coefficient, UFC Cb = Ce Cg Cd Cc (Cd 1.0)", "=CE_UFC*CG_UFC*1*CC_UFC",
     "UFC 4-152-01 Eq. 5-2, p. 106"),
    ("CM_UFC", "Cm, UFC C_M0 + (C_m1 - C_M0)(T/d)^3.5, F = 1.5 Cb (Cb < 0.6) or 0.9, Cb in F per UFCF (CF_UFC)",
     "=(1.3+1.5*TC/BEAMC)+(IF(CF_UFC<0.6,1.5*CF_UFC,0.9)*(12.4*(TC/BEAMC)^0.3-50*TC/LBPC)-(1.3+1.5*TC/BEAMC))"
     "*(TC/DEPTHC)^3.5",
     "UFC 4-152-01 5-2.1.1 e, p. 109"),
    ("CG_ROM", "Cg, ROM: 0.95 continuous berth, 1.0 dolphins", "=IF(CONTC=1,0.95,1)", "ROM 2.0-11"),
    # Checked inputs: each is the only cell that reads its input (the Inputs Check column
    # aside). Where the module refuses the value it is #N/A, which carries into every
    # result that uses the input. Rules: INPUT_RULES.
    _checked_row("X"),
    _checked_row("ANG"),
    # The Cb inside the UFC factor F: the printed wording (berthing coefficient, the default)
    # or the block coefficient (the 0.6 threshold read as a hull-form limit); fender_design.UFC_F_BASES.
    _checked_row("UFCF"),
    ("CF_UFC", "Cb in UFC F: berthing coefficient CB_UFC (UFCF 1) or block coefficient CB (UFCF 0)",
     "=IF(UFCFC=1,CB_UFC,CB)", "UFC 4-152-01 5-2.1.1 e, p. 109; fender_design.ufc_f_cb"),
    _checked_row("ETOL"),
    _checked_row("RTOL"),
    # the other checked inputs follow, so that no earlier cell moves
    *(_checked_row(n) for n, *_ in INPUT_ROWS if n not in _EARLY_CHECKS),
)

#: Calc rows replacing the PIANC Cs and Cc rows when a case selects a vendor table.
VENDOR_ROWS = {
    ("CS_P", "tfadm-2024"): (
        "CS_P", "Cs, manuals' rule: 1.0 above 0.15 m deflection, else 0.9 (option cs_basis: tfadm-2024)",
        "=IF(DEFLC>0.15,1,0.9)", "TFADM p. 27; SFT p. 20"),
    ("CC_P", "sft-2026"): (
        "CC_P", "Cc with Vasco Costa or Ueda Cm, SFT table: solid quay 0.8 (KC/D <= 0.5) or 0.9, partly "
        "closed 0.9 or 1.0 (option cc_basis: sft-2026)",
        '=IF(OR(STRUCTC="open",ABS(ALPHAC)>5),1,IF(STRUCTC="closed",IF(KCD<=0.5,0.8,0.9),IF(KCD<=0.5,0.9,1)))',
        "SFT p. 20"),
}


def calc_rows(fcase=None):
    """:data:`CALC_ROWS` with the Cs and Cc rows of the case's bases (fd.CS_BASES, fd.CC_BASES)."""
    if fcase is None:
        return CALC_ROWS
    swap = {"CS_P": VENDOR_ROWS.get(("CS_P", fcase.cs_basis)), "CC_P": VENDOR_ROWS.get(("CC_P", fcase.cc_basis))}
    return tuple(swap.get(r[0]) or r for r in CALC_ROWS)


#: Catalogue columns on Fenders that a formula reads, each through its checked column:
#: column -> (checked column, header). The row-check column applies FenderRating's
#: rules to the whole row; a row it refuses is #N/A in every result of the row.
FENDER_CHECKED_COLS = {"G": ("S", "Rated E checked (kNm)"), "H": ("T", "Rated R checked (kN)"),
                       "I": ("U", "Hull p checked (kPa)")}
FENDER_ROW_CHECK_COL = "V"

#: FenderRating rules on the catalogue columns (berthing_energy.FenderRating):
#: (column, field, condition on {v}, blank allowed, error text), checked in this order.
FENDER_ROW_RULES = (
    ("G", "rated_energy_knm", "AND(ISNUMBER({v}),{v}>0)", False, "rated energy must be a number above 0"),
    ("H", "reaction_kn", "AND(ISNUMBER({v}),{v}>0)", False, "rated reaction must be a number above 0"),
    ("D", "diameter_mm", "AND(ISNUMBER({v}),{v}>0)", True, "D or H must be blank or a number above 0"),
    ("E", "length_mm", "AND(ISNUMBER({v}),{v}>0)", True, "L must be blank or a number above 0"),
    ("F", "internal_pressure_kpa", "AND(ISNUMBER({v}),{v}>0)", True, "p0 must be blank or a number above 0"),
    ("I", "hull_pressure_kpa", "AND(ISNUMBER({v}),{v}>0)", True, "hull pressure must be blank or a number above 0"),
    ("J", "rated_deflection_pct", "AND(ISNUMBER({v}),{v}>0,{v}<=100)", True,
     "rated deflection must be blank or in (0, 100] %"),
)


def _fender_row_ok(col, cond, blank, i):
    v = f"{col}{i}"
    return f'OR({v}="",{cond.format(v=v)})' if blank else cond.format(v=v)


def _fender_row_check(i):
    """Row-check formula: the first FenderRating rule the row breaks, else "ok"."""
    f = '"ok"'
    for col, _field, cond, blank, error in reversed(FENDER_ROW_RULES):
        f = (f'IF({_fender_row_ok(col, cond, blank, i)},{f},'
             f'"ERROR: {error}; the capacity, reaction, pressure and checks of this row are #N/A")')
    return "=" + f


def inputs_ok_formula(n_fenders):
    """1 when the module accepts every input and catalogue row, else #N/A.

    The module refuses the whole case for any refused input (it raises), so
    every result reads this cell: the energy of every method (Methods H and J,
    hence the selected design energy), and the capacity, design reaction and
    hull pressure and every check on Fenders. No result or check is shown while
    an input is refused. A method whose own table does not cover the ship (a
    Brolsma, UFC or EAU velocity range, no DWT) is #N/A in its own row only, as
    compare_methods leaves out only that method.
    """
    s_e = FENDER_CHECKED_COLS["G"][0]
    na = [f"ISNA({checked_name(n)})" for n, *_ in INPUT_ROWS]
    na += [f"ISNA(Fenders!{s_e}{i})" for i in range(2, n_fenders + 2)]
    return f"=IF(OR({','.join(na)}),NA(),1)"


def _calc(wb, n_fenders, fcase=None):
    ws = wb.create_sheet("Calc")
    _header(ws, 1, ["Quantity", "Value", "Formula", "Source"], [52, 14, 60, 34])
    rows = (*calc_rows(fcase), ("INPUTS_OK", "Every input and catalogue row accepted by the module: 1, else #N/A "
                                      "(every energy, capacity, reaction and check reads it)",
                         inputs_ok_formula(n_fenders), "fender_case.compute refuses the case"))
    for i, (name, label, formula, src) in enumerate(rows, start=2):
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
            ws.cell(row=r, column=j, value=_loglog_formula(checked_name(size), cond, size == "DWT"))
            _name(wb, f"V_{cond}_{size}", f"Velocity!${_c(j)}${r}")
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
    x = "MIN(MAX(LN(MC),INDEX(RLX,1)),INDEX(RLX,COUNT(RLX)))"

    def lin(y):
        i = f"MATCH({x},RLX,1)"
        return (f"IF({x}>=INDEX(RLX,COUNT(RLX)),INDEX({y},COUNT({y})),"
                f"INDEX({y},{i})+({x}-INDEX(RLX,{i}))/(INDEX(RLX,{i}+1)-INDEX(RLX,{i}))"
                f"*(INDEX({y},{i}+1)-INDEX({y},{i})))")
    return f"IF(TUGSC=1,{lin(f'RT_{j}')},{lin(f'RN_{j}')})"


def _c(col):
    s = ""
    while col:
        col, rem = divmod(col - 1, 26)
        s = chr(65 + rem) + s
    return s


def _loglog_formula(size, cond, optional=False):
    """Brolsma velocity at ``size`` (a checked cell), #N/A outside the table as
    brolsma_velocity refuses (no extrapolation); ``optional``: #N/A for a blank
    size (no DWT), as the methods keyed by DWT refuse a missing DWT."""
    f = _loglog_named(size, f"VX_{cond}", f"VY_{cond}")
    return f'=IF({size}="",NA(),{f})' if optional else "=" + f


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
        own = _loglog_named("MC", "UFX", f"UFY_{j}")
    elif r.method == "rom":
        own = _rom_velocity_formula(j)
    elif r.method == "eau":
        own = (f'IF(DWTC="",NA(),IF(DWTC<INDEX(EAX,1),NA(),IF(DWTC>=INDEX(EAX,2),INDEX(EAY_{j},2),'
               f"{_loglog_named('DWTC', 'EAX', f'EAY_{j}')})))")
    elif r.method == "is4651":
        a, b, c, d = fd.IS4651_VELOCITY[res.fcase.condition]
        own = f"IF(MC<=5000,{a},IF(MC<=10000,{b},IF(MC<=100000,{c},{d})))"
    else:  # wg211
        own = repr(fd.WG211_VELOCITY[res.fcase.condition][0])
    return f'=IF(VOVRC="",{own},VOVRC)'


def _factor_formula(r, res, i):
    """Factor cell: the ROM product and the WG211 frequency correction follow
    Inputs; other factors depend only on fixed-at-generation choices."""
    if r.factor.source == "caller":
        return r.factor.value
    if r.method == "rom":
        return (f"=MAX(IF(MC<=85000,1.5,1.25)*IF(TUGSC=1,1,1.25)*IF(B{i}<=0.1,1.2,1)"
                f"*IF(NCALLSC>300,1.15,1),1.1)")
    if r.method == "wg211":
        c = res.fcase
        g_ref = fd.wg211_gamma_eref(c.condition, c.cov_m, c.consequence_class, c.multiple_contact)
        (a1, b1), (a2, b2) = fd.WG211_GAMMA_N[c.consequence_class]
        lo = fd.WG211_GAMMA_N_MIN_PER_YEAR
        # gamma_n is not extrapolated below the fit's lowest frequency (fd.wg211_gamma_n)
        return (f"=IF(NCALLSC<{lo!r},NA(),{g_ref!r}*(IF(NCALLSC<=100,{a1!r},{a2!r})*LN(NCALLSC)"
                f"+IF(NCALLSC<=100,{b1!r},{b2!r})))")
    return r.factor.value


def _rom_cc_formula(i):
    return (f'=IF(OR(KCD>0.5,ABS(ALPHAC)>5,B{i}<0.2,STRUCTC="open",CONTC=0),1,'
            f'IF(STRUCTC="closed",0.9,1))')


SELECTED_NOT_COMPARED_NOTE = (
    "Selected method is not in the case's comparison list (design.methods): design energy and "
    "capacity divisor are the module's values at generation, not live formulas. REGENERATE after "
    "any input change, or add the selected method to design.methods.")


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
        # INPUTS_OK: 1, or #N/A when the module would refuse the case
        ws.cell(row=i, column=8, value=f"=0.5*MC*B{i}^2*C{i}*D{i}*E{i}*F{i}*G{i}*INPUTS_OK")
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
    ws.cell(row=n + 3, column=1, value="Capacity divisor, selected (WG211 gamma_m)").font = HEAD
    if sel_row is None:
        # The case's comparison list (design.methods) excludes the selected method, so no
        # Methods row holds it: write no formula, only the module's values at generation.
        ws.cell(row=n + 1, column=10, value=res.selected.design_energy_knm)
        ws.cell(row=n + 3, column=10, value=res.selected.capacity_divisor)
        ws.cell(row=n + 1, column=12, value=SELECTED_NOT_COMPARED_NOTE)
    else:
        ws.cell(row=n + 1, column=10, value=f"=J{sel_row}")
        ws.cell(row=n + 3, column=10, value=f"=K{sel_row}")
    _name(wb, "E_DESIGN", f"Methods!$J${n + 1}")
    ws.cell(row=n + 2, column=1, value="Energy per fender (kNm)").font = HEAD
    ws.cell(row=n + 2, column=10, value="=E_DESIGN/NC")
    _name(wb, "E_FENDER", f"Methods!$J${n + 2}")
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
            "Pressure OK", "Source", *(h for _, h in FENDER_CHECKED_COLS.values()), "Row check"]
    _header(ws, 1, cols)
    for j, w in enumerate([16, 22, 12, 9, 8, 7, 10, 10, 9, 7, 11, 10, 11, 10, 8, 9, 9, 80, 11, 11, 11, 60],
                          start=1):
        ws.column_dimensions[_c(j)].width = w
    last = len(res.catalogue) + 1
    s_e, s_r, s_p = (FENDER_CHECKED_COLS[c][0] for c in "GHI")
    chk = FENDER_ROW_CHECK_COL
    by_type = res.case["design"].get("tolerances_by_type") or {}
    for i, f in enumerate(res.catalogue, start=2):
        vals = [f.fender_id, f.manufacturer, f.fender_type, f.diameter_mm, f.length_mm,
                f.internal_pressure_kpa, f.rated_energy_knm, f.reaction_kn, f.hull_pressure_kpa,
                f.rated_deflection_pct]
        for j, v in enumerate(vals, start=1):
            ws.cell(row=i, column=j, value=v)
        # checked catalogue values: the only cells that read G, H and I (with the row check)
        ws[f"{chk}{i}"] = _fender_row_check(i)
        ws[f"{chk}{i}"].font = WARN_FONT
        ws[f"{s_e}{i}"] = f'=IF({chk}{i}="ok",G{i},NA())'
        ws[f"{s_r}{i}"] = f'=IF({chk}{i}="ok",H{i},NA())'
        ws[f"{s_p}{i}"] = f'=IF({chk}{i}="ok",IF(I{i}="","",I{i}),NA())'
        # tolerances: the Inputs values (ETOL, RTOL) for the base fender's type; the case's
        # tolerances_by_type (fixed at generation) for any other type (fender_case.type_tolerances)
        if f.fender_type in by_type:
            etol = repr(float(by_type[f.fender_type]["energy_tolerance"]))
            rtol = repr(float(by_type[f.fender_type]["reaction_tolerance"]))
        else:
            etol, rtol = "ETOLC", "RTOLC"
        # INPUTS_OK: 1, or #N/A when the module would refuse the case
        ws.cell(row=i, column=11, value=f"={s_e}{i}*(1-{etol})*ANGC/GAMMA_M*INPUTS_OK")
        ws.cell(row=i, column=12, value=f"=E_FENDER/K{i}")
        # a curve above the rated reaction before the rated point raises rows of its type
        # (fender_case.size_checks); otherwise the rated-deflection value x (1 + tolerance)
        k = res.reaction_peak_factor(f)
        ws.cell(row=i, column=13, value=f"={s_r}{i}*(1+{rtol})*INPUTS_OK" if k == 1.0
                else f"={s_r}{i}*{k:.12g}*(1+{rtol})*INPUTS_OK")
        ws.cell(row=i, column=14, value=f'=IF({s_p}{i}="","",{s_p}{i}*(1+{rtol})*INPUTS_OK)')
        ws.cell(row=i, column=15, value=f'=IF(L{i}<=1,"yes","no")')
        # Energy OK reads the gated energy and capacity; the limit checks read INPUTS_OK first,
        # so that a blank limit cannot show "-" while an input is refused
        ws.cell(row=i, column=16, value=f'=IF(ISNA(INPUTS_OK),NA(),IF(RLIMC="","-",IF(M{i}<=RLIMC,"yes","no")))')
        # a set limit with no hull-pressure datum is not met (as size_checks and select_fenders)
        ws.cell(row=i, column=17,
                value=f'=IF(ISNA(INPUTS_OK),NA(),IF(PLIMC="","-",IF(N{i}="","no (no datum)",'
                      f'IF(N{i}<=PLIMC,"yes","no"))))')
        ws.cell(row=i, column=18, value=f.source)
    # the same FenderRating rules as data validation on the catalogue values
    for col, field, cond, blank, error in FENDER_ROW_RULES:
        dv = DataValidation(type="custom", formula1=cond.format(v=f"{col}2"), allow_blank=blank,
                            showErrorMessage=True, errorStyle="stop", errorTitle=f"{field} not accepted",
                            error=f"{error[0].upper()}{error[1:]}, as the catalogue loader (FenderRating) requires.")
        dv.add(f"{col}2:{col}{last}")
        ws.add_data_validation(dv)
    ws.freeze_panes = "B2"
    ws.auto_filter.ref = f"A1:R{last}"
    return ws


def _sensitivity(wb, res):
    ws = wb.create_sheet("Sensitivity")
    note = ws.cell(row=1, column=1, value=(
        "REGENERATE AFTER ANY INPUT CHANGE: one-at-a-time sensitivity around the selected method, written "
        "by the generator from the Python module (not live formulas). Capacity = rated energy x "
        "(1 - tolerance) x angular factor of the base fender"
        + ("" if res.selected.capacity_divisor == 1.0 else
           f" / {res.selected.capacity_divisor:.2f} ({res.selected.label} capacity divisor)")
        + ". Reaction = nominal curve reaction at energy / rated energy x reaction factor "
        f"{res.reaction_factor:.3f} (1 + tolerance, applied once: TFADM p. 76); not rated above the "
        "rated energy."))
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
    _header(ws, 1, ["Id", "Title", "Edition", "Clause", "Verification", "Read in", "Note", "Date read"],
            [20, 60, 10, 40, 12, 60, 60, 11])
    for i, s in enumerate(fd.SOURCES.values(), start=2):
        for j, v in enumerate([s.source_id, s.title, s.edition, s.clause, s.verification, s.read_in, s.note,
                               s.accessed_on], start=1):
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
        "Every input is read only through its checked cell on Calc, which applies the rule the Python module "
        "applies to the same value (for example the first contact x on the ship, 0 to LBP; the angular factor "
        "in (0, 1]; a whole number of fenders of 1 or more; a velocity override above 0 or blank). A value the "
        "module refuses shows ERROR in the Check column on Inputs and #N/A in every quantity that depends on "
        "it, including the energy, reaction and pressure checks; the input cells also refuse such a value "
        "when typed. The catalogue rows on Fenders are checked the same way (Row check column). As the module "
        "refuses the whole case for any such value, every energy, capacity, reaction and check is then #N/A "
        "as well (Calc INPUTS_OK); the Check column names the value to correct.",
        "The tolerances on Inputs (ETOL, RTOL) apply to fenders of the base fender's type; other types on "
        "Fenders take the case's tolerances by type, listed on Inputs as fixed at generation.",
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
    _calc(wb, len(res.catalogue), res.fcase)
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
