"""Optional native verification of generated shell and buckling decks.

Only the open-source ccx executable is used. Expected values are independent
hand calculations, with their assumptions written in each test.
"""

import importlib.util
import math
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

from digitalmodel.solvers.calculix import CalculiXResultParser, INPWriter


def run_ccx(path):
    result = subprocess.run([shutil.which("ccx"), "-i", path.stem], cwd=path.parent,
                            capture_output=True, text=True, timeout=60,
                            env={**os.environ, "OMP_NUM_THREADS": "1"})
    assert result.returncode == 0, result.stdout[-2000:]
    assert "*ERROR" not in result.stdout, result.stdout[-2000:]


def internal_buckling_factor(nodes):
    """Cross-check the same synthetic column with the structural/fe backend."""
    from digitalmodel.infrastructure.base_solvers.structural.buckling.elastic_buckling import (
        ElasticBucklingSolver,
    )
    from digitalmodel.infrastructure.base_solvers.structural.elements import BeamElement

    n = len(nodes) - 1
    elements = []
    for i in range(n):
        element = BeamElement(i, i, i+1, nodes[i, :2], nodes[i+1, :2],
                              E=1000., I=1/12, A=1.)
        element.axial_force = .01
        elements.append(element)
    solver = ElasticBucklingSolver()
    solver.set_nodes({i: node[:2] for i, node in enumerate(nodes)})
    solver.set_elements(elements)
    solver.set_boundary_conditions({"pinned_nodes": [0, n]})
    solver.set_loads([{"node": n, "direction": "x", "magnitude": -.01}])
    # Synthetic density/yield inputs satisfy the common solver contract;
    # neither enters this linear elastic buckling eigenproblem.
    solver.set_material_properties({"E": 1000., "nu": 0., "rho": 1., "sigma_y": 10.})
    solver.set_num_modes(1)
    result = solver.solve()
    assert result["status"] == "completed"
    return result["eigenvalues"][0]


@pytest.mark.skipif(shutil.which("ccx") is None, reason="Open-source ccx absent")
@pytest.mark.parametrize("nonlinear", [False, True])
def test_native_membrane_patch(tmp_path, nonlinear):
    """Hooke law: E=1000, nu=0, delta/L=.01 -> sigma_x=10 MPa.

    This is an infinitesimal, uniaxial plane-stress patch. Numerical comparison
    relative tolerance 1e-4 covers shell expansion/serialization effects and is
    not a design criterion. The nonlinear case uses displacement .02 and plastic
    rows (10,0),(20,.1); kinematics give E_xx=((1+.02)^2-1)/2=.0202.
    That case verifies strain readback, not a constitutive benchmark.
    """
    source = Path(__file__).resolve().parents[3] / "examples/calculix/shell_patch.py"
    spec = importlib.util.spec_from_file_location("shell_patch", source)
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)
    path = example.build_patch(tmp_path, displacement=.02 if nonlinear else .01,
                               nlgeom=nonlinear,
                               plastic_rows=[(10., 0.), (20., .1)] if nonlinear else None)
    run_ccx(path)
    parser = CalculiXResultParser("patch", tmp_path)
    data = parser.parse_frd_file()
    assert data["stresses"]
    assert data["strains"]
    if nonlinear:
        assert data["equivalent_plastic_strain"]
        assert max(data["equivalent_plastic_strain"].values()) > 0
        assert max(row["exx"] for row in data["strains"].values()) == pytest.approx(.0202, rel=1e-4)
        status = parser.parse_sta_file()
        assert status["last_converged"]["step_time"] == pytest.approx(1.)
        assert status["collapse_status"] == "NOT_EVALUATED"
    else:
        for row in data["stresses"].values():
            assert row["sxx"] == pytest.approx(10, rel=1e-4)
        assert max(abs(row["principal"][0] - .01) for row in data["strains"].values()) < 1e-4


@pytest.mark.skipif(shutil.which("ccx") is None, reason="Open-source ccx absent")
def test_native_beam_buckling(tmp_path):
    """Pinned Euler column: Pcr=pi^2*EI/L^2; multiplier=Pcr/Pref.

    Synthetic E=1000, b=h=1, I=1/12, L=100 and Pref=.01 give 5*pi^2/6.
    The same Euler reference is used by structural/fe/elastic_buckling_workflow.py.
    A 20-element column is compared within 1 percent, an explicit test-only
    discretization tolerance, not an API 579 criterion or shell validation.
    """
    n = 20
    nodes = np.array([[100*i/n, 0., 0.] for i in range(n + 1)])
    writer = INPWriter(nodes, {"Line 2": {
        "connectivity": np.array([[i, i+1] for i in range(n)])}})
    writer.add_material("MAT", 1000., 0.)
    writer.add_beam_section("EBEAM_B31", "MAT", "RECT", (1., 1.))
    writer.add_node_set("LEFT", [0])
    writer.add_node_set("RIGHT", [n])
    writer.add_node_set("ALLN", list(range(n+1)))
    writer.add_step("buckle", modes=1)
    writer.add_boundary_condition("LEFT", 1)
    for name in ("LEFT", "RIGHT"):
        writer.add_boundary_condition(name, 2)
    for dof in (3, 4):
        writer.add_boundary_condition("ALLN", dof)
    writer.add_load("RIGHT", "cload", -.01, direction=(1, 0, 0))
    path = writer.write(tmp_path / "column.inp")
    run_ccx(path)
    factors = CalculiXResultParser("column", tmp_path).parse_dat_file()["buckling_factors"]
    assert factors
    assert factors[0]["factor"] == pytest.approx(5*math.pi**2/6, rel=.01)
    assert factors[0]["factor"] == pytest.approx(internal_buckling_factor(nodes), rel=.01)
