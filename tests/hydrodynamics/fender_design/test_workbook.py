"""Universal workbook: built from the generic case, its live formulas recompute
to the Python module's values (evaluated with the ``formulas`` package)."""

from __future__ import annotations

import math
import re
import sys
import warnings
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"
sys.path.insert(0, str(EXAMPLE))

openpyxl = pytest.importorskip("openpyxl")
pytest.importorskip("yaml")

from build_fender_workbook import build_workbook  # noqa: E402
from fender_case import compute, load_case, type_tolerances  # noqa: E402


@pytest.fixture(scope="module")
def built(tmp_path_factory):
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    out = tmp_path_factory.mktemp("wb") / "generic.xlsx"
    build_workbook(res, out)
    return res, out


def test_sheets_and_names(built):
    res, out = built
    wb = openpyxl.load_workbook(out)
    for name in ("README", "Inputs", "Calc", "Velocity", "Methods", "Fenders", "Sensitivity",
                 "Tornado", "Sizes", "Sources"):
        assert name in wb.sheetnames
    for name in ("M", "LBP", "CE", "CM_P", "E_DESIGN", "E_FENDER", "GAMMA_M", "V_b_DWT"):
        assert name in wb.defined_names
    assert wb["Fenders"].max_row == len(res.catalogue) + 1
    assert len(wb["Sensitivity"]._charts) == len(res.sweeps)


def _calculate(path):
    with warnings.catch_warnings():  # the package emits SyntaxWarnings on import
        warnings.simplefilter("ignore")
        formulas = pytest.importorskip("formulas")
        sol = formulas.ExcelModel().loads(str(path)).finish().calculate()
    vals = {}
    for k, v in sol.items():
        key = str(k).upper().replace("'", "").split("]")[-1]
        try:
            vals[key] = float(v.value[0][0])
        except (TypeError, ValueError, IndexError, AttributeError):
            continue
    return vals


def _set_inputs(path, out, values):
    wb = openpyxl.load_workbook(path)
    for name, value in values.items():
        sheet, ref = list(wb.defined_names[name].destinations)[0]
        wb[sheet][ref.replace("$", "")].value = value
    wb.save(out)


# Every input a method coefficient depends on, moved so that thresholds are
# crossed: ROM 85,000 t, ROM and WG211 300 / 100 calls, quarter point (UFC Cg),
# fender deflection 0.15 m (ROM Cs), solid quay (Cc), larger angle (Ce). LBP and
# beam grow with the displacement, so the block coefficient stays below 1 (0.884).
CHANGED = dict(M=90_000.0, DWT=70_000.0, LBP=230.0, BEAM=36.0, DEPTH=13.5, X=90.0, ALPHA=3.0, STRUCT="closed",
               DEFL=0.12, NCALLS=400.0, CONT=1, TUGS=1)


def test_changed_inputs_recompute_every_method(built, tmp_path):
    res, out = built
    changed = tmp_path / "changed.xlsx"
    _set_inputs(out, changed, CHANGED)
    vals = _calculate(changed)
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["ship"].update(displacement_t=CHANGED["M"], dwt_t=CHANGED["DWT"], lbp_m=CHANGED["LBP"],
                        beam_m=CHANGED["BEAM"])
    case["berth"].update(water_depth_m=CHANGED["DEPTH"], contact_x_from_bow_m=CHANGED["X"],
                         berthing_angle_deg=CHANGED["ALPHA"], structure="closed", fender_deflection_m=0.12)
    case["conditions"].update(berthings_per_year=400, continuous_berth=True, tugs=True)
    new = {r.method: r for r in compute(case, citations="record").methods}
    assert set(new) == {r.method for r in res.methods}
    for i, r in enumerate(res.methods, start=2):
        m = new[r.method]
        for col, coef in zip("BCDEFI", (m.velocity_m_s, m.cm, m.ce, m.cs, m.cc, m.factor)):
            assert vals[f"METHODS!{col}{i}"] == pytest.approx(coef.value, rel=1e-9), (r.method, coef.name)
        assert vals[f"METHODS!J{i}"] == pytest.approx(m.design_energy_knm, rel=1e-9), r.method
    # the change moved coefficients the old workbook held as constants
    old = {r.method: r for r in res.methods}
    assert new["ufc"].ce.value != pytest.approx(old["ufc"].ce.value)
    assert new["rom"].factor.value != pytest.approx(old["rom"].factor.value)
    assert new["bs6349"].cc.value != old["bs6349"].cc.value


def test_velocity_override_cell_drives_every_method(built, tmp_path):
    res, out = built
    changed = tmp_path / "vovr.xlsx"
    _set_inputs(out, changed, dict(VOVR=0.08))
    vals = _calculate(changed)
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["velocity_m_s"] = 0.08
    new = {r.method: r for r in compute(case, citations="record").methods}
    for i, r in enumerate(res.methods, start=2):
        assert vals[f"METHODS!B{i}"] == pytest.approx(0.08)
        assert vals[f"METHODS!J{i}"] == pytest.approx(new[r.method].design_energy_knm, rel=1e-9), r.method
    rom_row = [r.method for r in res.methods].index("rom") + 2
    assert vals[f"METHODS!I{rom_row}"] == pytest.approx(1.80)   # ROM Table 4.6.4.43 at v <= 0.10


def test_ufc_row_reproduces_the_hand_regression(built, tmp_path):
    # UFC 4-152-01 Eq. 5-2 / p. 109 hand case (test_methods_codes): block coefficient 0.40,
    # midships, 0.10 m/s -> berthing coefficient 1.0, F 0.9, Cm 3.8755, 823.7 kNm
    res, out = built
    changed = tmp_path / "ufc.xlsx"
    _set_inputs(out, changed, dict(M=0.40 * 1.025 * 180 * 32 * 12, X=90.0, ALPHA=0.0, VOVR=0.10))
    vals = _calculate(changed)
    assert vals["CALC!B2"] == pytest.approx(0.40)
    row = [r.method for r in res.methods].index("ufc") + 2
    assert vals[f"METHODS!C{row}"] == pytest.approx(3.87552, abs=5e-5)
    assert vals[f"METHODS!J{row}"] == pytest.approx(823.7, abs=0.05)


def test_ufc_f_basis_input_reads_the_block_coefficient_at_0(built, tmp_path):
    # the same hand case with the block coefficient 0.40 in F: F 0.6, Cm 2.84256, 604.2 kNm
    res, out = built
    wb = openpyxl.load_workbook(out)
    sheet, ref = list(wb.defined_names["UFCF"].destinations)[0]
    assert wb[sheet][ref.replace("$", "")].value == 1                # generic case: printed wording
    changed = tmp_path / "ufc_block.xlsx"
    _set_inputs(out, changed, dict(M=0.40 * 1.025 * 180 * 32 * 12, X=90.0, ALPHA=0.0, VOVR=0.10, UFCF=0))
    vals = _calculate(changed)
    row = [r.method for r in res.methods].index("ufc") + 2
    assert vals[f"METHODS!C{row}"] == pytest.approx(2.84256, abs=5e-5)
    assert vals[f"METHODS!J{row}"] == pytest.approx(604.2, abs=0.05)


def test_workbook_from_a_block_coefficient_case_matches_the_module(tmp_path):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["conditions"]["ufc_f_basis"] = "block_coefficient"
    res = compute(case, citations="record")
    out = tmp_path / "block.xlsx"
    build_workbook(res, out)
    wb = openpyxl.load_workbook(out)
    sheet, ref = list(wb.defined_names["UFCF"].destinations)[0]
    assert wb[sheet][ref.replace("$", "")].value == 0
    vals = _calculate(out)
    ufc = next(r for r in res.methods if r.method == "ufc")
    row = [r.method for r in res.methods].index("ufc") + 2
    assert vals[f"METHODS!C{row}"] == pytest.approx(ufc.cm.value, rel=1e-9)
    assert vals[f"METHODS!J{row}"] == pytest.approx(ufc.design_energy_knm, rel=1e-9)


def test_fixed_choices_marked_for_regeneration(built):
    _, out = built
    wb = openpyxl.load_workbook(out)
    texts = [c.value for row in wb["Inputs"].iter_rows() for c in row if isinstance(c.value, str)]
    assert any("REGENERATE" in t for t in texts)
    assert sum(t == "regenerate after change" for t in texts) >= 8
    assert "REGENERATE" in wb["Sensitivity"]["A1"].value
    assert "REGENERATE" in wb["Tornado"]["I1"].value


def _strings(path, cells):
    """Evaluate the workbook and return the text values of ``cells`` (``SHEET!A1``)."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        formulas = pytest.importorskip("formulas")
        sol = formulas.ExcelModel().loads(str(path)).finish().calculate()
    out = {}
    for k, v in sol.items():
        key = str(k).upper().replace("'", "").split("]")[-1]
        if key in cells:
            out[key] = v.value[0][0]
    return out


@pytest.fixture(scope="module")
def no_datum_row(tmp_path_factory):
    """The generic case with one extra catalogue row that has no hull-pressure datum
    (fictional; energy and reaction adequate) under the case's 200 kPa limit."""
    import dataclasses

    from digitalmodel.hydrodynamics.berthing_energy import FenderRating
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    assert res.case["design"]["hull_pressure_limit_kpa"] == 200.0
    row = FenderRating("GEN-FM-X", "generic", "foam", 2800, 5000, None, 1000.0, 1400.0, None, 60.0,
                       "fictional test row")
    res = dataclasses.replace(res, catalogue=[*res.catalogue, row])
    out = tmp_path_factory.mktemp("wbnd") / "no_datum.xlsx"
    build_workbook(res, out)
    return res, out, len(res.catalogue) + 1


def test_fenders_sheet_missing_pressure_under_a_limit_is_not_met(no_datum_row):
    # second-provider review at e57362bb: the Fenders sheet showed "-" here, while
    # size_checks, the Sizes sheet and fender_study.py report "no (no datum)"
    _, out, i = no_datum_row
    ws = openpyxl.load_workbook(out)["Fenders"]
    assert ws.cell(row=1, column=17).value == "Pressure OK"
    assert ws.cell(row=i, column=1).value == "GEN-FM-X" and ws.cell(row=i, column=9).value is None
    assert "no (no datum)" in ws.cell(row=i, column=17).value
    vals = _strings(out, {f"FENDERS!Q{i}", "FENDERS!Q2"})
    assert vals[f"FENDERS!Q{i}"] == "no (no datum)"
    assert vals["FENDERS!Q2"] in ("yes", "no")          # rows with a datum are compared as before


def test_fenders_sheet_missing_pressure_without_a_limit_is_not_checked(no_datum_row, tmp_path):
    _, out, i = no_datum_row
    free = tmp_path / "no_limit.xlsx"
    _set_inputs(out, free, dict(PLIM=None))
    vals = _strings(free, {f"FENDERS!Q{i}", "FENDERS!Q2"})
    assert vals[f"FENDERS!Q{i}"] == "-"
    assert vals["FENDERS!Q2"] == "-"


def test_formulas_recompute_to_module_values(built):
    res, out = built
    vals = _calculate(out)
    geo = res.selected.geometry
    assert vals["CALC!B2"] == pytest.approx(geo["cb"], rel=1e-9)
    assert vals["CALC!B3"] == pytest.approx(geo["k_m"], rel=1e-9)
    assert vals["CALC!B5"] == pytest.approx(geo["r_m"], rel=1e-9)
    assert vals["CALC!B7"] == pytest.approx(geo["phi_deg"], rel=1e-9)
    assert vals["CALC!B8"] == pytest.approx(geo["ce"], rel=1e-9)
    # every method row: design energy equals the module's
    for i, r in enumerate(res.methods, start=2):
        assert vals[f"METHODS!J{i}"] == pytest.approx(r.design_energy_knm, rel=1e-9), r.method
    # fender utilisation of the base fender row
    ids = [f.fender_id for f in res.catalogue]
    row = ids.index(res.base_fender.fender_id) + 2
    per = res.selected.design_energy_knm / res.case["design"]["fenders_in_contact"]
    assert vals[f"FENDERS!L{row}"] == pytest.approx(per / res.capacity_knm, rel=1e-9)


def test_fenders_sheet_design_reaction_carries_the_curve_peak(tmp_path):
    # second-provider review at 944a85c7: a curve peaking above the rated reaction
    # before the rated point raises the design reaction of rows of its type
    import dataclasses

    from digitalmodel.hydrodynamics import fender_design as fd
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    above = fd.PerformanceCurve("fictional above rated", ((0, 0, 0), (30, 110, 40), (60, 100, 100)),
                                "fictional test curve")
    res = dataclasses.replace(res, curve=above)
    out = tmp_path / "peak.xlsx"
    build_workbook(res, out)
    ws = openpyxl.load_workbook(out)["Fenders"]
    vals = _calculate(out)
    for i, f in enumerate(res.catalogue, start=2):
        k = 1.10 if f.fender_type == res.base_fender.fender_type else 1.0
        rtol = type_tolerances(res.case["design"], f.fender_type)[1]
        assert vals[f"FENDERS!M{i}"] == pytest.approx(f.reaction_kn * k * (1 + rtol), rel=1e-9), f.fender_id
        if k == 1.0:   # another type: its own tolerance, fixed at generation
            assert ws.cell(row=i, column=13).value == f"=T{i}*(1+{rtol!r})*INPUTS_OK"   # T: rated R checked


def test_fenders_sheet_design_reaction_formula_unchanged_for_a_monotonic_curve(built):
    res, out = built
    ws = openpyxl.load_workbook(out)["Fenders"]
    for i, f in enumerate(res.catalogue, start=2):
        rtol = "RTOLC" if f.fender_type == res.base_fender.fender_type else \
            repr(type_tolerances(res.case["design"], f.fender_type)[1])
        assert ws.cell(row=i, column=13).value == f"=T{i}*(1+{rtol})*INPUTS_OK"


def test_capacity_text_names_the_wg211_divisor(tmp_path):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["method"] = "wg211"
    res = compute(case, citations="record")
    out = tmp_path / "wg211.xlsx"
    build_workbook(res, out)
    wb = openpyxl.load_workbook(out)
    readme = " ".join(str(c.value) for c in wb["README"]["A"] if c.value)
    assert f"capacity divisor 1.10 {res.capacity_knm:,.1f} kNm" in readme
    assert res.capacity_knm == pytest.approx(700.0 * (1 - 0.10) / 1.10)   # pneumatic 10 % (TFADM p. 76)
    assert "/ 1.10 (PIANC WG211 (2024) capacity divisor)" in wb["Sensitivity"]["A1"].value
    for s in res.sweeps:
        assert s.capacity_knm == pytest.approx(res.capacity_knm)


def test_capacity_text_unchanged_without_a_divisor(built):
    res, out = built
    wb = openpyxl.load_workbook(out)
    readme = " ".join(str(c.value) for c in wb["README"]["A"] if c.value)
    assert f"capacity after tolerance and angular factor {res.capacity_knm:,.1f} kNm." in readme
    assert "capacity divisor" not in wb["Sensitivity"]["A1"].value


# Second-provider review at 39213804: the workbook computed the contact arm and the
# fender capacity without the module's input checks, so editing Inputs bypassed them
# (X = 1,800 m on LBP 180 m gave UFC 0.265 kNm, IS 4651 15.655 kNm, PIANC 7.855 kNm;
# ANG = 1.2 raised the 700 kNm rating to 840 kNm). X and ANG are now read only through
# the checked cells XC and ANGC, which are #N/A outside 0 to LBP and (0, 1].

XC_FORMULA = "=IF(ISNUMBER(X),IF(AND(X>=0,X<=LBPC),X,NA()),NA())"
ANGC_FORMULA = "=IF(ISNUMBER(ANG),IF(AND(ANG>0,ANG<=1),ANG,NA()),NA())"


def _ref(wb, name):
    sheet, ref = list(wb.defined_names[name].destinations)[0]
    return f"{sheet.upper()}!{ref.replace('$', '')}"


def _formulas_reading(wb, name):
    """Cells (``SHEET!A1``) whose formula reads the defined name ``name`` (quoted text ignored)."""
    pat = re.compile(rf"\b{name}\b")
    hits = set()
    for ws in wb.worksheets:
        for row in ws.iter_rows():
            for c in row:
                if isinstance(c.value, str) and c.value.startswith("="):
                    if pat.search(re.sub(r'"[^"]*"', '""', c.value)):
                        hits.add(f"{ws.title.upper()}!{c.coordinate}")
    return hits


def test_contact_and_angular_factor_are_read_only_through_checked_cells(built):
    res, out = built
    wb = openpyxl.load_workbook(out)
    assert _formulas_reading(wb, "X") == {_ref(wb, "XC")}
    assert _formulas_reading(wb, "ANG") == {_ref(wb, "ANGC")}
    calc = wb["Calc"]
    cell = lambda n: calc[_ref(wb, n).split("!")[1]].value  # noqa: E731
    assert cell("XC") == XC_FORMULA
    assert cell("ANGC") == ANGC_FORMULA
    assert cell("XG") == "=LBPC/2-XC" and cell("XA") == "=ABS(LBPC/2-XC)"
    fen = wb["Fenders"]
    for i, f in enumerate(res.catalogue, start=2):
        etol = "ETOLC" if f.fender_type == res.base_fender.fender_type else \
            repr(type_tolerances(res.case["design"], f.fender_type)[0])
        assert fen.cell(row=i, column=11).value == f"=S{i}*(1-{etol})*ANGC/GAMMA_M*INPUTS_OK"   # S: checked


def test_inputs_check_column_names_the_error(built):
    _, out = built
    wb = openpyxl.load_workbook(out)
    ws = wb["Inputs"]
    assert ws.cell(row=1, column=5).value == "Check"
    for name, checked, text in (("X", "XC", "ERROR: first contact is not on the ship (0 to LBP)"),
                                ("ANG", "ANGC", "ERROR: angular factor must be in (0, 1]")):
        row = int(_ref(wb, name).split("!B")[1])
        f = ws.cell(row=row, column=5).value
        assert f.startswith(f'=IF(ISNA({checked}),"{text}')


def _evaluate(path):
    """Evaluate the workbook; every cell value with errors kept (``SHEET!A1`` -> value)."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        formulas = pytest.importorskip("formulas")
        sol = formulas.ExcelModel().loads(str(path)).finish().calculate()
    vals = {}
    for k, v in sol.items():
        key = str(k).upper().replace("'", "").split("]")[-1]
        try:
            vals[key] = v.value[0][0]
        except (TypeError, IndexError, AttributeError):
            continue
    return vals


def _na(v):
    return str(v) == "#N/A"


def _number(v):
    return isinstance(v, (int, float)) and not isinstance(v, bool) and math.isfinite(v)


@pytest.mark.parametrize("x", [1800.0, 180.5, -1.0])
def test_off_ship_contact_is_an_error_in_every_energy_and_check(built, tmp_path, x):
    res, out = built
    changed = tmp_path / "offship.xlsx"
    _set_inputs(out, changed, dict(X=x))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    for name in ("XC", "XG", "XA", "RCG", "CE", "CE_UFC", "CG_UFC", "CE_IS", "E_DESIGN", "E_FENDER"):
        assert _na(vals[_ref(wb, name)]), (name, vals[_ref(wb, name)])
    for i, r in enumerate(res.methods, start=2):
        assert _na(vals[f"METHODS!H{i}"]) and _na(vals[f"METHODS!J{i}"]), r.method
    for i in range(2, len(res.catalogue) + 2):
        assert _na(vals[f"FENDERS!L{i}"]) and _na(vals[f"FENDERS!O{i}"]), i   # utilisation, Energy OK
        assert _na(vals[f"FENDERS!K{i}"])                 # the module refuses the case (INPUTS_OK)
    row = int(_ref(wb, "X").split("!B")[1])
    assert vals[f"INPUTS!E{row}"].startswith("ERROR: first contact is not on the ship")
    assert vals[f"INPUTS!E{int(_ref(wb, 'ANG').split('!B')[1])}"] == "ok"


@pytest.mark.parametrize("x", [0.0, 180.0])
def test_contact_at_the_ends_of_the_ship_matches_the_module(built, tmp_path, x):
    res, out = built
    changed = tmp_path / "ends.xlsx"
    # the first contact is on the end nearer the berth: stern nearer (-5 deg) for the aft end
    alpha = 5.0 if x <= 90.0 else -5.0
    _set_inputs(out, changed, dict(X=x, ALPHA=alpha))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert vals[_ref(wb, "XC")] == pytest.approx(x)
    case = load_case(EXAMPLE / "generic_case.yaml")
    assert case["ship"]["lbp_m"] == 180.0
    case["berth"]["contact_x_from_bow_m"] = x
    case["berth"]["berthing_angle_deg"] = alpha
    if alpha < 0:   # sweeps on the aft end: no forward contact range, angles 0 and below
        case["sweeps"].pop("contact_x_over_lbp")
        case["sweeps"]["berthing_angle_deg"] = dict(low=-10.0, high=0.0, basis="test")
    new = {r.method: r for r in compute(case, citations="record").methods}
    for i, r in enumerate(res.methods, start=2):
        assert vals[f"METHODS!J{i}"] == pytest.approx(new[r.method].design_energy_knm, rel=1e-9), r.method
    assert vals[f"INPUTS!E{int(_ref(wb, 'X').split('!B')[1])}"] == "ok"
    for i in range(2, len(res.catalogue) + 2):
        assert vals[f"FENDERS!O{i}"] in ("yes", "no")


@pytest.mark.parametrize("ang", [0.0, 1.0000001, 1.2])
def test_angular_factor_outside_0_to_1_is_an_error_in_capacity_and_checks(built, tmp_path, ang):
    res, out = built
    changed = tmp_path / "ang.xlsx"
    _set_inputs(out, changed, dict(ANG=ang))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert _na(vals[_ref(wb, "ANGC")])
    for i in range(2, len(res.catalogue) + 2):
        for col in "KLO":                                   # capacity, utilisation, Energy OK
            assert _na(vals[f"FENDERS!{col}{i}"]), (col, i)
    assert _na(vals[_ref(wb, "E_DESIGN")])                   # the module refuses the case (INPUTS_OK)
    row = int(_ref(wb, "ANG").split("!B")[1])
    assert vals[f"INPUTS!E{row}"].startswith("ERROR: angular factor must be in (0, 1]")


def test_angular_factor_1_keeps_the_rated_capacity(built, tmp_path):
    res, out = built
    changed = tmp_path / "ang1.xlsx"
    _set_inputs(out, changed, dict(ANG=1.0))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert vals[_ref(wb, "ANGC")] == pytest.approx(1.0)
    row = [f.fender_id for f in res.catalogue].index(res.base_fender.fender_id) + 2
    base = next(c for c in res.size_checks if c.is_base)
    assert vals[f"FENDERS!K{row}"] == pytest.approx(base.capacity_knm, rel=1e-12)
    assert vals[f"FENDERS!L{row}"] == pytest.approx(base.utilisation, rel=1e-9)
    assert vals[f"FENDERS!O{row}"] == ("yes" if base.meets_energy else "no")
    assert vals[f"INPUTS!E{int(_ref(wb, 'ANG').split('!B')[1])}"] == "ok"


@pytest.mark.parametrize("ufcf", [2, 0.5, "berthing"])
def test_ufc_f_reading_other_than_0_or_1_is_an_error_in_the_ufc_row(built, tmp_path, ufcf):
    res, out = built
    changed = tmp_path / "ufcf.xlsx"
    _set_inputs(out, changed, dict(UFCF=ufcf))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert _na(vals[_ref(wb, "UFCFC")])
    methods = [r.method for r in res.methods]
    row = methods.index("ufc") + 2
    for col in "CJ":                                         # Cm, design energy
        assert _na(vals[f"METHODS!{col}{row}"]), col
    other = methods.index("pianc2002") + 2
    assert _na(vals[f"METHODS!J{other}"])                    # the module refuses the case (INPUTS_OK)
    irow = int(_ref(wb, "UFCF").split("!B")[1])
    assert vals[f"INPUTS!E{irow}"].startswith("ERROR: UFC F reading must be 1")


# Second-provider review at 1be46a8d (P2): the Fenders sheet read ETOL and RTOL
# without the bounds design_performance applies, so a negative tolerance raised
# the capacity (ETOL = -0.20: 700 kNm became 840 kNm against the UFC demand of
# 773.0 kNm) or lowered the design reaction (RTOL = -0.10: 1,440 kN instead of
# 1,760 kN against the 1,600 kN limit). ETOL and RTOL are now read only through
# the checked cells ETOLC and RTOLC, and both input cells carry a data-validation
# rule with the same bounds.

ETOLC_FORMULA = "=IF(ISNUMBER(ETOL),IF(AND(ETOL>=0,ETOL<1),ETOL,NA()),NA())"
RTOLC_FORMULA = "=IF(ISNUMBER(RTOL),IF(RTOL>=0,RTOL,NA()),NA())"


def _input_row(wb, name):
    return int(_ref(wb, name).split("!B")[1])


def test_tolerances_are_read_only_through_checked_cells(built):
    # fails if the generator ever writes a formula that reads a tolerance unchecked
    res, out = built
    wb = openpyxl.load_workbook(out)
    assert _formulas_reading(wb, "ETOL") == {_ref(wb, "ETOLC")}
    assert _formulas_reading(wb, "RTOL") == {_ref(wb, "RTOLC")}
    calc = wb["Calc"]
    assert calc[_ref(wb, "ETOLC").split("!")[1]].value == ETOLC_FORMULA
    assert calc[_ref(wb, "RTOLC").split("!")[1]].value == RTOLC_FORMULA
    fen = wb["Fenders"]
    by_type = res.case["design"]["tolerances_by_type"]
    for i, f in enumerate(res.catalogue, start=2):
        if f.fender_type == res.base_fender.fender_type:
            assert "ETOLC" in fen.cell(row=i, column=11).value                   # capacity
            assert "RTOLC" in fen.cell(row=i, column=13).value                   # design reaction
            assert "RTOLC" in fen.cell(row=i, column=14).value                   # design hull pressure
        else:   # another type: the case's tolerances by type, fixed at generation (gate P3 at 2b6bb8a0)
            t = by_type[f.fender_type]
            assert f"(1-{float(t['energy_tolerance'])!r})" in fen.cell(row=i, column=11).value
            assert f"(1+{float(t['reaction_tolerance'])!r})" in fen.cell(row=i, column=13).value
            assert f"(1+{float(t['reaction_tolerance'])!r})" in fen.cell(row=i, column=14).value


def test_tolerance_inputs_carry_a_validation_rule_and_a_check(built):
    _, out = built
    wb = openpyxl.load_workbook(out)
    ws = wb["Inputs"]
    rules = {}
    for dv in ws.data_validations.dataValidation:
        for rng in dv.sqref.ranges:
            rules[rng.coord] = dv
    for name, formula, text in (
            ("ETOL", "AND(ISNUMBER(B{r}),B{r}>=0,B{r}<1)", "ERROR: energy tolerance must be in [0, 1)"),
            ("RTOL", "AND(ISNUMBER(B{r}),B{r}>=0)", "ERROR: reaction tolerance must not be negative")):
        r = _input_row(wb, name)
        dv = rules[f"B{r}"]
        assert dv.type == "custom" and dv.formula1 == formula.format(r=r), name
        assert dv.showErrorMessage and dv.errorStyle == "stop" and not dv.allow_blank, name
        assert ws.cell(row=r, column=5).value.startswith(f'=IF(ISNA({name}C),"{text}'), name


def _python_refuses(base, **kw):
    from digitalmodel.hydrodynamics import fender_design as fd
    try:
        fd.design_performance(base, **kw)
    except ValueError:
        return True
    return False


@pytest.mark.parametrize("etol", [-0.20, -1e-9, 1.0, 1.5])
def test_energy_tolerance_outside_0_to_1_is_an_error_in_capacity_and_checks(built, tmp_path, etol):
    res, out = built
    assert _python_refuses(res.base_fender, energy_tolerance=etol)          # the same condition
    changed = tmp_path / "etol.xlsx"
    _set_inputs(out, changed, dict(ETOL=etol))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert _na(vals[_ref(wb, "ETOLC")])
    for i in range(2, len(res.catalogue) + 2):
        for col in "KLO":                                   # capacity, utilisation, Energy OK
            assert _na(vals[f"FENDERS!{col}{i}"]), (col, i)
        assert _na(vals[f"FENDERS!M{i}"])                    # the module refuses the case (INPUTS_OK)
    assert _na(vals[_ref(wb, "E_DESIGN")])
    assert vals[f"INPUTS!E{_input_row(wb, 'ETOL')}"].startswith("ERROR: energy tolerance must be in [0, 1)")
    assert vals[f"INPUTS!E{_input_row(wb, 'RTOL')}"] == "ok"


@pytest.mark.parametrize("rtol", [-0.10, -1e-9])
def test_negative_reaction_tolerance_is_an_error_in_reaction_and_checks(built, tmp_path, rtol):
    res, out = built
    assert _python_refuses(res.base_fender, reaction_tolerance=rtol)        # the same condition
    assert res.case["design"]["reaction_limit_kn"] is not None
    changed = tmp_path / "rtol.xlsx"
    _set_inputs(out, changed, dict(RTOL=rtol))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    assert _na(vals[_ref(wb, "RTOLC")])
    for i, f in enumerate(res.catalogue, start=2):
        assert _na(vals[f"FENDERS!M{i}"]) and _na(vals[f"FENDERS!P{i}"]), i   # design R, Reaction OK
        if f.hull_pressure_kpa is not None:
            assert _na(vals[f"FENDERS!N{i}"]) and _na(vals[f"FENDERS!Q{i}"]), i  # design p, Pressure OK
        assert _na(vals[f"FENDERS!K{i}"])                    # the module refuses the case (INPUTS_OK)
    assert vals[f"INPUTS!E{_input_row(wb, 'RTOL')}"].startswith("ERROR: reaction tolerance must not be negative")
    assert vals[f"INPUTS!E{_input_row(wb, 'ETOL')}"] == "ok"


def test_zero_tolerances_are_accepted_and_match_the_module(built, tmp_path):
    res, out = built
    assert not _python_refuses(res.base_fender, energy_tolerance=0.0, reaction_tolerance=0.0)
    changed = tmp_path / "tol0.xlsx"
    _set_inputs(out, changed, dict(ETOL=0.0, RTOL=0.0))
    vals = _evaluate(changed)
    wb = openpyxl.load_workbook(changed)
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["design"]["energy_tolerance"] = 0.0
    case["design"]["reaction_tolerance"] = 0.0
    checks = {c.fender_id: c for c in compute(case, citations="record").size_checks}
    ids = [f.fender_id for f in res.catalogue]
    for fid, c in checks.items():
        i = ids.index(fid) + 2
        assert vals[f"FENDERS!K{i}"] == pytest.approx(c.capacity_knm, rel=1e-12), fid
        assert vals[f"FENDERS!M{i}"] == pytest.approx(c.reaction_kn, rel=1e-12), fid
    for name in ("ETOL", "RTOL"):
        assert vals[f"INPUTS!E{_input_row(wb, name)}"] == "ok"


def test_committed_workbook_reads_tolerances_only_through_checked_cells():
    path = ROOT / "docs" / "reports" / "modules" / "fender_design" / "fender-design-generic.xlsx"
    wb = openpyxl.load_workbook(path)
    assert _formulas_reading(wb, "ETOL") == {_ref(wb, "ETOLC")}
    assert _formulas_reading(wb, "RTOL") == {_ref(wb, "RTOLC")}
    ranges = {rng.coord for dv in wb["Inputs"].data_validations.dataValidation for rng in dv.sqref.ranges}
    assert {f"B{_input_row(wb, n)}" for n in ("ETOL", "RTOL")} <= ranges
