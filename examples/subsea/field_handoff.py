# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic prescribed-rate case; all losses are supplied, not predicted.

Run with the source package on PYTHONPATH. No solver or equipment qualification
is implied: arrivals are 11 and 9 MPa, choke differentials are 3 and 1 MPa,
and the proposed liquid inlet is 0.003 m3/s at 8 MPa.
"""

from digitalmodel.subsea.field import (
    FieldTopology,
    FlowlineEndpoint,
    JumperEdge,
    LiquidBasis,
    ManifoldNode,
    TreeNode,
    steady_handoff,
)
from digitalmodel.subsea.trees.flow_capacity import BoreCapacity


def main() -> None:
    """Print the proposal, conditional acceptance and individual diagnostics."""
    basis = LiquidBasis("oil", "synthetic common liquid condition", "common datum")
    trees = tuple(
        TreeNode(
            name,
            BoreCapacity(2, 0.01),
            rate,
            0,
            pressure,
            0,
            "synthetic supplied tree loss",
            basis,
        )
        for name, rate, pressure in (("tree-a", 0.001, 12e6), ("tree-b", 0.002, 11e6))
    )
    jumpers = tuple(
        JumperEdge(name, tree.id, "header", loss, "synthetic supplied loss", 4e6)
        for name, tree, loss in zip(("edge-a", "edge-b"), trees, (1e6, 2e6))
    )
    result = steady_handoff(
        FieldTopology(
            trees,
            jumpers,
            ManifoldNode("header", 8e6, 0.004, basis),
            FlowlineEndpoint("outlet", "header"),
        )
    )
    print("Proposed inlet:", result.proposed_inlet)
    print("Accepted under supplied inputs:", result.accepted_inlet)
    for branch in result.branches:
        print(branch.tree_id, branch.tree, branch.pressure)


if __name__ == "__main__":
    main()
