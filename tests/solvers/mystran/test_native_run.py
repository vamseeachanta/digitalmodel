# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Native rectangular CBAR bending and recorded-output regressions.

Assumed inputs: L=1 m, b=1/20 m along y, h=1/10 m along z,
E=210 GPa, nu=0.3, P=1000 N; four CBARs with orientation (0,1,0).
Independent rectangle integrals give I1=h*b**3/12=1/960000 m**4
(about z, y displacement), I2=b*h**3/12=1/240000 m**4
(about y, z displacement). With delta=P*L**3/(3*E*I):
  delta_y = 1000/(3*210e9/960000) = 4/2625 m;
  delta_z = 1000/(3*210e9/240000) = 1/2625 m.
The signed y/z tip ratio is 4.0; either inertia swap fails the 1% criterion.
Blank PBAR shear factors match Euler-Bernoulli bending. References and
native provenance are in the module's cantilever-bar-example.md.
"""

import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.mystran import MystranChain, MystranResultParser
from digitalmodel.solvers.mystran.fem_chain import find_mystran

FIXTURES = Path(__file__).with_name("fixtures") / "native_rectangular"
EXPECTED = {"y": 4 / 2625, "z": 1 / 2625}
COMPONENT = {"y": "t2", "z": "t3"}


def _write_case(work_dir, plane):
    """Use the public writer with the documented, unswapped PBAR axes."""
    chain = MystranChain(work_dir)
    chain.create_cantilever_bar_mesh(1.0, 4)
    chain.setup_analysis(
        material={"name": "STEEL", "E": 210e9, "nu": 0.3},
        sections=[{
            "element_type": "Line 2", "type": "bar", "A": 0.05 * 0.1,
            "I1": 0.1 * 0.05**3 / 12, "I2": 0.05 * 0.1**3 / 12,
            "J": 0.141 * 0.05 * 0.1**3, "orientation": (0, 1, 0),
        }],
        loads=[{
            "node_set": "TIP", "magnitude": 1000.0,
            "direction": (0, 1, 0) if plane == "y" else (0, 0, 1),
        }],
        boundary_conditions=[{"node_set": "FIX", "dofs": "123456"}],
        title="CBAR cantilever validation",
    )
    return chain


def _assert_bending(summary, plane):
    """Check signed deflection, uncoupled planes and root equilibrium."""
    component = COMPONENT[plane]
    other = "t3" if plane == "y" else "t2"
    assert not summary["errors"]
    assert set(summary["displacements"]) == {1, 2, 3, 4, 5}
    tip = summary["displacements"][5]
    assert tip[component] == pytest.approx(EXPECTED[plane], rel=0.01, abs=0)
    assert tip[other] == pytest.approx(0.0, abs=1e-12)
    assert summary["spc_forces"][1][component] == pytest.approx(-1000, rel=1e-6)
    assert summary["epsilon"] is not None
    assert abs(summary["epsilon"]) < 1e-8
    return abs(tip[component] - EXPECTED[plane]) / EXPECTED[plane] * 100


@pytest.mark.parametrize("plane", ["y", "z"])
def test_native_rectangular_bending(tmp_path, plane):
    """Run both planes when MYSTRAN_EXE or mystran on PATH is available."""
    if find_mystran() is None:
        pytest.skip("MYSTRAN unavailable: set MYSTRAN_EXE or put mystran on PATH")
    if plane == "z":
        # Exercise the corrected validation helper itself, including I2 reference.
        chain = MystranChain(tmp_path)
        result = chain.run_cantilever_bar_validation(b=0.05, h=0.1)
        assert result["exact"] == pytest.approx(EXPECTED[plane])
        assert result["error_pct"] < 1.0
    else:
        chain = _write_case(tmp_path, plane)
        status = chain.solve()
        assert status["success"], status
    parser = MystranResultParser(chain.JOB_NAME, tmp_path)
    error_pct = _assert_bending(parser.parse_f06(), plane)
    result = chain.extract_results(COMPONENT[plane])
    assert result["tip_displacement"] == pytest.approx(EXPECTED[plane], rel=0.01)
    assert error_pct < 1.0


@pytest.mark.parametrize("plane", ["y", "z"])
def test_recorded_rectangular_bending(tmp_path, plane):
    """Bind the current writer and parsed evidence to stored native agreement."""
    manifest = json.loads((FIXTURES / "manifest.json").read_text(encoding="utf-8"))
    assert manifest["criteria_version"] == "rectangular-bending-v1"
    assert manifest["max_error_pct"] == 1.0
    case = manifest["cases"][plane]
    chain = _write_case(tmp_path, plane)
    actual_deck = (tmp_path / f"{chain.JOB_NAME}.bdf").read_bytes()
    assert actual_deck == (FIXTURES / case["deck"]).read_bytes()
    parser = MystranResultParser(plane, FIXTURES)
    summary = parser.parse_f06()
    error_pct = _assert_bending(summary, plane)
    assert case["expected_m"] == pytest.approx(EXPECTED[plane], rel=1e-14)
    assert summary["displacements"][5][COMPONENT[plane]] == case["tip_m"]
    assert error_pct == pytest.approx(case["error_pct"], rel=1e-12, abs=1e-12)
    assert error_pct < manifest["max_error_pct"]


@pytest.mark.parametrize("artifact", ["manifest.json", "y.F06", "z.F06"])
def test_recorded_version_banners(artifact):
    """Retain solver version and date without personal attribution."""
    text = (FIXTURES / artifact).read_text(encoding="utf-8")
    if artifact == "manifest.json":
        banners = [json.loads(text)["source"]["version_string"]]
    else:
        banners = [line.strip() for line in text.splitlines() if "MYSTRAN" in line]
    if banners != ["MYSTRAN Version 19.0.0   JUN 28 2026"]:
        pytest.fail("Solver banner must contain only the recorded version and date")


def test_recorded_evidence_digests():
    """Detect changes to either committed deck or documented F06 excerpt."""
    manifest = json.loads((FIXTURES / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["sha256"]) == {"y.bdf", "z.bdf", "y.F06", "z.F06"}
    for name, expected in manifest["sha256"].items():
        assert hashlib.sha256((FIXTURES / name).read_bytes()).hexdigest() == expected, (
            "Rebuild native output with MYSTRAN_EXE=<executable> python -m pytest "
            "tests/solvers/mystran/test_native_run.py -k test_native_rectangular_bending; "
            "then re-extract per the manifest extraction metadata and refresh sha256"
        )
