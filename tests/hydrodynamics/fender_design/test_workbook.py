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


def test_formulas_recompute_to_module_values(built):
    with warnings.catch_warnings():  # the package emits SyntaxWarnings on import
        warnings.simplefilter("ignore")
        formulas = pytest.importorskip("formulas")
        res, out = built
        sol = formulas.ExcelModel().loads(str(out)).finish().calculate()
    vals = {}
    for k, v in sol.items():
        key = str(k).upper().replace("'", "").split("]")[-1]
        try:
            vals[key] = float(v.value[0][0])
        except (TypeError, ValueError, IndexError, AttributeError):
            continue
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
