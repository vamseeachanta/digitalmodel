"""Validation tests for the Kleefsman dam-break-with-obstacle case (#1172).

ALWAYS (no OpenFOAM): the official experimental data loads with known
characteristics, the geometry constants match the SPHERIC Test 2
description, and the builder produces a structurally valid case with the
pressure probes placed in fluid cells at any mesh density.

CONDITIONALLY (solver-capable host AND DIGITALMODEL_RUN_LONG_CFD=1): the
fast variant is solved end-to-end (~25 min serial — far heavier than the
other suite solves, hence the extra opt-in) and the impact gates asserted.
The reference results behind the report come from the 161x50x50 / 6 s run.
"""

from __future__ import annotations

import json
import os

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)
from digitalmodel.solvers.openfoam.validation import (
    HEIGHT_MAE_TOLERANCE,
    KLEEFSMAN_CASE,
    PRESSURE_ARRIVAL_TOLERANCE,
    PRESSURE_PEAK_TOLERANCE,
    KleefsmanConfig,
    build_kleefsman_case,
    extract_impact_metrics,
    load_experiment,
)
from digitalmodel.solvers.openfoam.validation.kleefsman import (
    BOX,
    HEIGHT_GAUGES,
    PRESSURE_SENSORS,
)

from .conftest import assert_valid_case_dir, solver_capable


# --------------------------------------------------------------------------- #
#  Official experimental data — always run                                    #
# --------------------------------------------------------------------------- #


def test_experiment_data_loads_with_known_signature() -> None:
    exp = load_experiment()
    assert set(exp) == {"Time"} | {f"P{i}" for i in range(1, 9)} | {
        f"H{i}" for i in range(1, 5)
    }
    assert len(exp["Time"]) == 7395
    # Known characteristics of the official traces.
    assert exp["P2"].max() == pytest.approx(8127.5, abs=1.0)
    assert exp["H4"].max() == pytest.approx(0.5511, abs=1e-3)  # ~initial fill
    assert 0.30 <= exp["H2"].max() <= 0.36


def test_geometry_matches_spheric_description() -> None:
    # Box 0.161 x 0.403 x 0.161, front face at x = 0.8245.
    (x0, y0, z0), (x1, y1, z1) = BOX
    assert x1 - x0 == pytest.approx(0.161)
    assert y1 - y0 == pytest.approx(0.403)
    assert z1 - z0 == pytest.approx(0.161)
    assert x1 == pytest.approx(0.8245)
    # P1-P4 on the front face at y = 0.471; P5-P8 on the top.
    for name in ("P1", "P2", "P3", "P4"):
        assert PRESSURE_SENSORS[name][0] == pytest.approx(0.8245)
        assert PRESSURE_SENSORS[name][1] == pytest.approx(0.471)
    for name in ("P5", "P6", "P7", "P8"):
        assert PRESSURE_SENSORS[name][2] == pytest.approx(0.161)
    assert HEIGHT_GAUGES["H2"] == pytest.approx(0.992)
    assert HEIGHT_GAUGES["H4"] == pytest.approx(2.638)


def test_validation_case_registered() -> None:
    assert KLEEFSMAN_CASE.metadata["issue"] == "#1172"
    assert KLEEFSMAN_CASE.tolerance == PRESSURE_PEAK_TOLERANCE == 0.20
    assert KLEEFSMAN_CASE.reference["p2_peak_pa"] == pytest.approx(8127, abs=1)


# --------------------------------------------------------------------------- #
#  Case builder — always run                                                  #
# --------------------------------------------------------------------------- #


def test_build_produces_valid_case_with_probes(tmp_path) -> None:
    case_dir = build_kleefsman_case(KleefsmanConfig(), parent_dir=tmp_path)
    assert_valid_case_dir(case_dir)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "interFoam" in control
    assert "probes" in control
    assert "interfaceHeight" in control
    assert (case_dir / "system" / "topoSetDict").is_file()


@pytest.mark.parametrize("nx,ny,nz", [(81, 25, 25), (161, 50, 50)])
def test_probes_land_in_fluid_cells_at_any_density(tmp_path, nx, ny, nz) -> None:
    """The front-face probes must sit outside the carved obstacle cells —
    regression for the half-cell offset (a fixed offset valid at one density
    lands inside the hole at another)."""
    cfg = KleefsmanConfig(nx=nx, ny=ny, nz=nz, name=f"k{nx}")
    case_dir = build_kleefsman_case(cfg, parent_dir=tmp_path)
    control = (case_dir / "system" / "controlDict").read_text()
    probes = [
        [float(v) for v in line.strip().strip("()").split()]
        for line in control.splitlines()
        if line.strip().startswith("(0.")
    ]
    # front-face sensors (P1-P4) are the ones offset in +x; identify them by
    # their z (0.021..0.141, above the floor, below the box top) — this also
    # excludes the interfaceHeight gauge locations (z = 0)
    front = [p for p in probes if 0.0 < p[2] < 0.161]
    assert len(front) == 4
    dx = 3.22 / nx
    # cells with centre <= 0.8245 are carved; the hole face lies at the next
    # face boundary, so any probe past (last carved centre + dx/2) is fluid
    import math
    last_hole_centre = (math.floor(0.8245 / dx - 0.5) + 0.5) * dx
    hole_face = last_hole_centre + dx / 2
    for x, _y, _z in front:
        assert x > hole_face, f"probe x={x} inside the obstacle at nx={nx}"


def test_build_stamps_provenance_with_citation(tmp_path) -> None:
    case_dir = build_kleefsman_case(KleefsmanConfig(), parent_dir=tmp_path)
    prov = json.loads((case_dir / "provenance.json").read_text())
    assert prov["issue"] == "#1172"
    assert any("10.1016/j.jcp.2004.12.007" in c or "Kleefsman" in c
               for c in prov["citations"])
    assert any("spheric" in c.lower() for c in prov["citations"])


# --------------------------------------------------------------------------- #
#  Solve assertion — solver-capable host + explicit long-run opt-in           #
# --------------------------------------------------------------------------- #


def test_kleefsman_solve_matches_experiment(tmp_path) -> None:
    """End-to-end fast variant (81x25x25, 1.5 s; ~25 min serial). Opt-in via
    DIGITALMODEL_RUN_LONG_CFD=1 on top of the solver gate — this is an order
    of magnitude heavier than the other suite solves. Reference (161x50x50,
    6 s) results are in docs/api/cfd/kleefsman-impact-verification.html."""
    if not solver_capable(tmp_path):
        pytest.skip("OpenFOAM not available — run gated to solver-capable hosts")
    if os.environ.get("DIGITALMODEL_RUN_LONG_CFD") != "1":
        pytest.skip("long CFD solve — set DIGITALMODEL_RUN_LONG_CFD=1 to run")

    cfg = KleefsmanConfig()
    case_dir = build_kleefsman_case(cfg, parent_dir=tmp_path)
    runner = OpenFOAMRunner(OpenFOAMRunConfig(
        run_topo_set=True,
        subset_mesh_set="c0",
        subset_mesh_patch="obstacle",
        run_set_fields=True,
        to_vtk=False,
        timeout_seconds=3 * 3600,
    ))
    result = runner.run(case_dir)
    assert result.status == OpenFOAMRunStatus.COMPLETED, result.error_message

    res = extract_impact_metrics(case_dir, height_window=(0.0, cfg.end_time - 0.05))
    assert abs(res["P2"]["peak_err"]) <= PRESSURE_PEAK_TOLERANCE, res["P2"]
    assert abs(res["P2"]["arrival_err"]) <= PRESSURE_ARRIVAL_TOLERANCE, res["P2"]
    assert res["H2_mae_norm"] <= HEIGHT_MAE_TOLERANCE, res["H2_mae_norm"]
    assert res["H4_mae_norm"] <= HEIGHT_MAE_TOLERANCE, res["H4_mae_norm"]
    assert res["mass_drift"] <= 0.01
