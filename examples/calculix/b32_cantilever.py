# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic rectangular cantilever using only the open-source ccx solver.

Assumed inputs in mm and N: length 100, width along z 2, height along y 4,
E=100000 N/mm^2, nu=0.3, tip Fy=+1 N, and twenty equal quadratic elements.
These inputs are selected for this test, not taken from a material database.
Euler-Bernoulli integration gives I_z=b*h^3/12 and v_tip=P*L^3/(3*E*I_z).
Force balance gives root Fy=-P. Small displacement, constant EI and perfect
clamping are assumed; shear flexibility is neglected by the reference.

Format references: https://www.dhondt.de/ccx_2.22.pdf, sections 6.2.33,
7.96 (*NODE FILE) and 7.98 (*NODE PRINT); Gmsh manual, Node ordering,
https://gmsh.info/doc/texinfo/gmsh.html#Node-ordering.
"""

import argparse
import json
from pathlib import Path

import numpy as np

from digitalmodel.solvers.calculix import FEMChain
from digitalmodel.solvers.calculix.fem_chain import is_calculix_available


# Synthetic inputs and numerical settings declared before any native solve.
LENGTH = 100.
WIDTH = 2.
HEIGHT = 4.
MODULUS = 100000.
POISSON = .3
FORCE = 1.
ELEMENTS = 20
DEFLECTION_RTOL = .01  # Test-only 1 percent discretization allowance.
FORCE_ATOL = 1e-5  # N; allowance for native text output rounding.


def analytical_reference():
    """Hand calculation independent of mesh serialization and solver output."""
    inertia = WIDTH * HEIGHT**3 / 12
    return {"tip_dy": FORCE * LENGTH**3 / (3 * MODULUS * inertia),
            "root_fy": -FORCE}


def build_cantilever(work_dir):
    """Write a B32 deck and request original-node U and root RF totals."""
    # Endpoints first, followed by midpoints: Gmsh Line3 connectivity.
    x = np.concatenate((np.linspace(0., LENGTH, ELEMENTS + 1),
                        (np.arange(ELEMENTS) + .5) * LENGTH / ELEMENTS))
    nodes = np.column_stack((x, np.zeros_like(x), np.zeros_like(x)))
    connectivity = np.array([[i, i + 1, ELEMENTS + 1 + i]
                             for i in range(ELEMENTS)])
    chain = FEMChain(work_dir)
    chain.set_mesh(nodes, {"Line 3": {"connectivity": connectivity, "dimension": 1}},
                   node_sets={"ROOT": [0], "TIP": [ELEMENTS]})
    path = chain.setup_analysis(
        material={"name": "MAT", "E": MODULUS, "nu": POISSON},
        loads=[{"type": "cload", "node_set": "TIP", "magnitude": FORCE,
                "direction": (0, 1, 0)}],
        boundary_conditions=[{"node_set": "ROOT", "dof_start": 1, "dof_end": 6}],
        beam_sections=[{"elset": "EBEAM_B32", "material": "MAT",
                        "section_type": "RECT", "dimensions": (WIDTH, HEIGHT),
                        "direction": (0., 0., 1.)}],
    )
    # Solver-specific output cards supplement the writer's default requests.
    # OUTPUT=2D retains original node IDs; RF at the unloaded root is reaction.
    deck = path.read_text().replace("*NODE FILE\n", "*NODE FILE, OUTPUT=2D\n")
    deck = deck.replace("*END STEP\n",
                        "*NODE PRINT, NSET=ROOT, TOTALS=ONLY\nRF\n*END STEP\n")
    path.write_text(deck)
    return chain


def main():
    """Build a deck; solve and enforce the declared tolerances with --solve."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", required=True, type=Path)
    parser.add_argument("--solve", action="store_true")
    args = parser.parse_args()
    available = is_calculix_available()
    print(f"ccx available: {available}")
    print("Predeclared tolerances: deflection 1 percent; force residual 1e-5 N")
    if args.solve and not available:
        parser.error("Native acceptance requires installed open-source ccx")
    chain = build_cantilever(args.work_dir)
    if not args.solve:
        print("Deck written; native response has not been checked")
        return
    status = chain.solve(n_threads=1)
    if not status["success"] or "*ERROR" in status["message"] + status["stderr"]:
        raise RuntimeError("ccx failed; inspect native output in the work directory")
    results = chain.extract_results()
    tip_dy = results["fields"]["displacements"][ELEMENTS + 1]["dy"]
    root = [row for row in results["dat"]["reactions"] if row["set"] == "ROOT"]
    if len(root) != 1:
        raise RuntimeError("Expected one root reaction total")
    reference = analytical_reference()
    if abs(tip_dy - reference["tip_dy"]) > DEFLECTION_RTOL * abs(reference["tip_dy"]):
        raise RuntimeError("Tip deflection exceeds the predeclared tolerance")
    reaction = root[0]
    if (abs(reaction["fy"] + FORCE) > FORCE_ATOL
            or abs(reaction["fx"]) > FORCE_ATOL or abs(reaction["fz"]) > FORCE_ATOL):
        raise RuntimeError("Root reaction exceeds the predeclared force residual")
    print(json.dumps({"tip_dy_mm": tip_dy, "root_fy_N": reaction["fy"]}))


if __name__ == "__main__":
    main()
