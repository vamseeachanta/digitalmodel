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
    res = compute(load_case(EXAMPLE / "generic_case.yaml"))
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
    new = {r.method: r for r in compute(case).methods}
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
    new = {r.method: r for r in compute(case).methods}
    for i, r in enumerate(res.methods, start=2):
        assert vals[f"METHODS!B{i}"] == pytest.approx(0.08)
        assert vals[f"METHODS!J{i}"] == pytest.approx(new[r.method].design_energy_knm, rel=1e-9), r.method
    rom_row = [r.method for r in res.methods].index("rom") + 2
    assert vals[f"METHODS!I{rom_row}"] == pytest.approx(1.80)   # ROM Table 4.6.4.43 at v <= 0.10


def test_fixed_choices_marked_for_regeneration(built):
    _, out = built
    wb = openpyxl.load_workbook(out)
    texts = [c.value for row in wb["Inputs"].iter_rows() for c in row if isinstance(c.value, str)]
    assert any("REGENERATE" in t for t in texts)
    assert sum(t == "regenerate after change" for t in texts) >= 8
    assert "REGENERATE" in wb["Sensitivity"]["A1"].value
    assert "REGENERATE" in wb["Tornado"]["I1"].value


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
