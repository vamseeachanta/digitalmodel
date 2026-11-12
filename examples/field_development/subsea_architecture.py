# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run an independently calculated, synthetic planar manifold example.

From the repository root: PYTHONPATH=src python examples/field_development/subsea_architecture.py

Basis: Euclidean distance sqrt(dx**2 + dy**2), with invented coordinates in m.
Wells are at (0, 0) and (6, 0); the explicit host is at (3, 8).
Candidate totals, calculated independently of the optimizer:
- centre (3, 4): sqrt(9+16) + sqrt(9+16) + 4 = 14 m;
- west (0, 4): 4 + sqrt(36+16) + sqrt(9+16) = 9+sqrt(52) m;
- at-host (3, 8): sqrt(9+64) + sqrt(9+64) + 0 = 2*sqrt(73) m.
Thus centre wins. These are planar lengths, not qualified installation routes.
"""

from digitalmodel.field_development.subsea_architecture import (
    ManifoldCandidate,
    PlanarPoint,
    optimize_manifold,
)


def main() -> None:
    """Print the winning candidate and every leg of the hand-calculated example."""
    wells = [PlanarPoint("well-a", (0, 0), "m"), PlanarPoint("well-b", (6, 0), "m")]
    host = PlanarPoint("host", (3, 8), "m")
    candidates = [
        ManifoldCandidate("west", (0, 4), "m", "synthetic west option"),
        ManifoldCandidate("centre", (3, 4), "m", "synthetic centre option"),
        ManifoldCandidate("at-host", (3, 8), "m", "synthetic host option"),
    ]
    result = optimize_manifold(wells, host, candidates)
    assert result.chosen_candidate.identifier == "centre"
    assert result.objective == 14  # Independent sum above: 5 + 5 + 4.
    chosen = result.chosen_candidate
    print(f"Chosen candidate: {chosen.identifier} at {chosen.coordinates} {result.units}")
    print(f"Provenance: {chosen.provenance}")
    for leg in (*result.well_legs, result.host_leg):
        print(f"{leg.source_id} -> {leg.target_id}: {leg.length:.1f} {result.units}")
    print(f"Total planar length: {result.objective:.1f} {result.units}")
    print("Planar length objective only; installation feasibility requires further design.")


if __name__ == "__main__":
    main()
