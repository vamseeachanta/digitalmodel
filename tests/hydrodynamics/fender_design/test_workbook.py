"""Universal workbook: built from the generic case, its live formulas recompute
to the Python module's values (evaluated with the ``formulas`` package)."""

from __future__ import annotations

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
from fender_case import compute, load_case  # noqa: E402


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
# fender deflection 0.15 m (Cs), solid quay (Cc), larger angle (Ce).
CHANGED = dict(M=90_000.0, DWT=70_000.0, DEPTH=15.5, X=90.0, ALPHA=3.0, STRUCT="closed", DEFL=0.12,
               NCALLS=400.0, CONT=1, TUGS=1)


def test_changed_inputs_recompute_every_method(built, tmp_path):
    res, out = built
    changed = tmp_path / "changed.xlsx"
    _set_inputs(out, changed, CHANGED)
    vals = _calculate(changed)
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["ship"].update(displacement_t=CHANGED["M"], dwt_t=CHANGED["DWT"])
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
