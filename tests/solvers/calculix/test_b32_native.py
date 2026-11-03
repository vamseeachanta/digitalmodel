# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""B32 connectivity and native cantilever checks with independent references.

Ordering references: Gmsh manual, "Node ordering", Line3 diagram (end, end,
middle); CalculiX 2.22 manual, section 6.2.33, Figure 73 (end, middle, end).
Native comparison assumptions and tolerances are declared before the solve.
"""

import runpy
import subprocess
from pathlib import Path
from unittest.mock import Mock

import numpy as np
import pytest

from digitalmodel.solvers.calculix import FEMChain, INPWriter
from digitalmodel.solvers.calculix.fem_chain import is_calculix_available


def test_b32_deck_connectivity(tmp_path):
    """Two quadratic elements share an endpoint, never a middle node.

    Synthetic coordinates in arbitrary length units: endpoints at 0, 2, 4;
    middle nodes at 1 and 3. IDs deliberately differ from spatial ordering.
    Expected deck rows are hand-transcribed using the cited node diagrams.
    """
    nodes = np.array([[0, 0, 0], [2, 0, 0], [4, 0, 0],
                      [1, 0, 0], [3, 0, 0]], dtype=float)
    connectivity = np.array([[0, 1, 3], [1, 2, 4]])
    writer = INPWriter(nodes, {"Line 3": {"connectivity": connectivity}})
    lines = writer.write(tmp_path / "connectivity.inp").read_text(
        encoding="utf-8",
    ).splitlines()
    keyword = "*ELEMENT, TYPE=B32, ELSET=EBEAM_B32"
    start = lines.index(keyword) + 1
    rows = [[int(value) for value in line.split(",")]
            for line in lines[start:start + 2]]
    assert rows == [[1, 1, 4, 2], [2, 2, 5, 3]]
    for _, left, middle, right in rows:
        np.testing.assert_array_equal(
            nodes[middle - 1], (nodes[left - 1] + nodes[right - 1]) / 2,
        )
    np.testing.assert_array_equal(connectivity, [[0, 1, 3], [1, 2, 4]])


def test_b32_chain_beam_section(tmp_path):
    """The chain must pass a rectangular section to the existing writer.

    Synthetic section dimensions 2 by 4 and E=100000, nu=0.3 are test inputs,
    not a material specification. No displacement is inferred from this deck.
    """
    chain = FEMChain(tmp_path)
    chain.set_mesh([[0, 0, 0], [2, 0, 0], [1, 0, 0]],
                   {"Line 3": {"connectivity": [[0, 1, 2]]}})
    path = chain.setup_analysis(
        material={"name": "MAT", "E": 100000., "nu": .3},
        loads=[], boundary_conditions=[],
        beam_sections=[{"elset": "EBEAM_B32", "material": "MAT",
                        "section_type": "RECT", "dimensions": (2., 4.)}],
    )
    assert ("*BEAM SECTION, ELSET=EBEAM_B32, MATERIAL=MAT, SECTION=RECT\n"
            "2.0, 4.0\n") in path.read_text(encoding="utf-8")


def _example():
    source = Path(__file__).resolve().parents[3] / "examples/calculix/b32_cantilever.py"
    return runpy.run_path(str(source))


def test_b32_saved_example_deck(tmp_path):
    """Check output requests and the independent calculation without a solver."""
    example = _example()
    chain = example["build_cantilever"](tmp_path)
    text = (tmp_path / f"{chain.JOB_NAME}.inp").read_text(encoding="utf-8")
    assert "*ELEMENT, TYPE=B32," in text
    assert "*NODE FILE, OUTPUT=2D\nU\n" in text
    assert "*NODE PRINT, NSET=ROOT, TOTALS=ONLY\nRF\n" in text
    assert "TIP, 2, 1.0" in text
    # Independent hand integration: I=2*4^3/12=32/3 mm^4;
    # delta=1*100^3/(3*100000*(32/3))=0.3125 mm; root Fy=-1 N.
    reference = example["analytical_reference"]()
    assert reference["tip_dy"] == pytest.approx(.3125)
    assert reference["root_fy"] == -1.


def _mock_solver(example, monkeypatch, *, stdout="This is Version 2.23\n",
                 returncode=201, error=None):
    """Simulate only the version probe; no native result is fabricated."""
    probe = example["require_supported_calculix"]
    monkeypatch.setitem(probe.__globals__, "is_calculix_available", lambda: True)
    run = Mock(return_value=subprocess.CompletedProcess(
        ["ccx", "-v"], returncode, stdout=stdout, stderr=""), side_effect=error)
    monkeypatch.setattr(subprocess, "run", run)
    return run


@pytest.mark.parametrize("returncode", [0, 201])
def test_b32_supported_release(monkeypatch, returncode):
    """Only the release qualified for this case may run native acceptance."""
    # Official 2.23 ccx_2.23.c calls stop.f after -v; stop.f exits 201.
    example = _example()
    run = _mock_solver(example, monkeypatch,
                       stdout="\nThis is Version 2.23\n\n", returncode=returncode)
    assert example["require_supported_calculix"]() == "2.23"
    run.assert_called_once_with(
        ["ccx", "-v"], capture_output=True, text=True, encoding="utf-8", timeout=10,
    )


@pytest.mark.parametrize("returncode", [0, 201])
@pytest.mark.parametrize("banner", [
    "This is Version 2.21\n", "This is Version 2.22\n",
    "This is Version 2.24\n", "This is Version 2.230\n",
    "This is Version 2.23.1\n", "unrecognized output\n", "",
])
def test_b32_rejects_unqualified_release(monkeypatch, banner, returncode):
    """Old, future and unidentified releases provide no acceptance evidence."""
    example = _example()
    _mock_solver(example, monkeypatch, stdout=banner, returncode=returncode)
    with pytest.raises(RuntimeError, match="requires CalculiX 2.23"):
        example["require_supported_calculix"]()


@pytest.mark.parametrize("error", [
    OSError("probe unavailable"), subprocess.TimeoutExpired(["ccx", "-v"], 10),
])
def test_b32_version_probe_error(monkeypatch, error):
    """A failed or stalled probe cannot admit a solve."""
    example = _example()
    _mock_solver(example, monkeypatch, error=error)
    with pytest.raises(RuntimeError, match="Cannot determine CalculiX release"):
        example["require_supported_calculix"]()


@pytest.mark.parametrize("returncode", [1, 127, 200, 202, -9])
def test_b32_version_probe_nonzero(monkeypatch, returncode):
    """Even a qualified banner is insufficient when the probe fails."""
    example = _example()
    _mock_solver(example, monkeypatch, returncode=returncode)
    with pytest.raises(RuntimeError, match="Cannot determine CalculiX release"):
        example["require_supported_calculix"]()


def test_b32_version_probe_missing_solver(monkeypatch):
    """Missing ccx is reported before attempting its version command."""
    example = _example()
    run = _mock_solver(example, monkeypatch)
    monkeypatch.setitem(example["require_supported_calculix"].__globals__,
                        "is_calculix_available", lambda: False)
    with pytest.raises(RuntimeError, match="requires installed CalculiX 2.23"):
        example["require_supported_calculix"]()
    run.assert_not_called()


def test_b32_saved_example_rejects_release_before_build(monkeypatch, tmp_path):
    """The command must reject an incompatible solver before deck creation."""
    example = _example()
    _mock_solver(example, monkeypatch, stdout="This is Version 2.21\n")
    build = Mock(side_effect=AssertionError("Unexpected deck creation"))
    monkeypatch.setitem(example["main"].__globals__, "build_cantilever", build)
    monkeypatch.setattr("sys.argv", ["b32_cantilever.py", "--solve",
                                   "--work-dir", str(tmp_path)])
    with pytest.raises(RuntimeError, match="requires CalculiX 2.23"):
        example["main"]()
    build.assert_not_called()


@pytest.mark.skipif(not is_calculix_available(), reason="Requires open-source ccx")
def test_b32_native_cantilever(tmp_path):
    """Compare native fields against hand integration of E*I*v''=P*(L-x).

    Assumed mm/N inputs: L=100, width along z=2, height along y=4,
    E=100000 N/mm^2, nu=0.3 and tip Fy=+1 N. Twenty equal B32 elements;
    root translations and rotations fixed. I_z=2*4^3/12=32/3 mm^4,
    v_tip=P*L^3/(3*E*I_z)=0.3125 mm, root Fy=-1 N by force balance.
    Predeclared tolerances: 1 percent displacement discretization error and
    1e-5 N absolute force residual (text output precision). These are test
    tolerances, not design acceptance criteria. Native acceptance requires
    this test to pass without a skip on CalculiX 2.23; serialization alone
    is insufficient. An installed, unqualified release fails before solving.
    """
    example = _example()
    example["require_supported_calculix"]()
    chain = example["build_cantilever"](tmp_path)
    status = chain.solve(n_threads=1)
    assert status["success"], status
    assert "*ERROR" not in status["message"] + status["stderr"]
    results = chain.extract_results()
    # OUTPUT=2D preserves original IDs, unlike expanded solid corner nodes.
    tip = results["fields"]["displacements"][21]
    assert tip["dy"] == pytest.approx(.3125, rel=.01, abs=0.)
    assert tip["dx"] == pytest.approx(0., abs=1e-5)
    assert tip["dz"] == pytest.approx(0., abs=1e-5)
    reactions = results["dat"]["reactions"]
    root = [row for row in reactions if row["set"] == "ROOT"]
    assert len(root) == 1
    assert sum(row["fy"] for row in root) == pytest.approx(-1., abs=1e-5)
    assert sum(row["fx"] for row in root) == pytest.approx(0., abs=1e-5)
    assert sum(row["fz"] for row in root) == pytest.approx(0., abs=1e-5)
