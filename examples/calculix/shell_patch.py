"""Generate a synthetic membrane patch; no solver is launched.

CalculiX 2.22 manual (2024), sections 7.117 and 7.122 specify the deck.
All numeric inputs are synthetic demonstration values, not design allowables.
The independent small-strain answer is sigma_x=E*delta/L=1000*.01/1=10,
epsilon_x=.01 and zero transverse strain for nu=0. Units: N, mm, MPa.
"""

import argparse
from pathlib import Path

import numpy as np

from digitalmodel.solvers.calculix import INPWriter


def build_patch(directory: Path, *, displacement=.01, nlgeom=False, plastic_rows=None) -> Path:
    """Write a unit square shell stretched by prescribed edge displacement."""
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    nodes = np.array([[0., 0., 0.], [1., 0., 0.], [1., 1., 0.], [0., 1., 0.]])
    writer = INPWriter(nodes, {"Quadrilateral 4": {
        "connectivity": np.array([[0, 1, 2, 3]])}})
    writer.add_material("ELASTIC", 1000., 0.)
    if plastic_rows is not None:
        writer.add_plasticity("ELASTIC", plastic_rows)
    writer.add_shell_section("ESHELL_S4", "ELASTIC", .1)
    writer.add_node_set("LEFT", [0, 3])
    writer.add_node_set("RIGHT", [1, 2])
    writer.add_node_set("BOTTOM", [0, 1])
    writer.add_node_set("ALLN", [0, 1, 2, 3])
    writer.add_step(nlgeom=nlgeom, increments=(.1, 1., .001, .1) if nlgeom else None)
    writer.add_boundary_condition("LEFT", 1)
    writer.add_boundary_condition("BOTTOM", 2)
    for dof in (3, 4, 5):
        writer.add_boundary_condition("ALLN", dof)
    writer.add_boundary_condition("RIGHT", 1, displacement)
    return writer.write(directory / "patch.inp")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="Destination outside the source checkout")
    args = parser.parse_args()
    build_patch(args.output)
