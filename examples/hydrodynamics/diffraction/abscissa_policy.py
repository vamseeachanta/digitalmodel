"""Demonstrate explicit numeric policy on synthetic frequency grids.

Run from the repository root with PYTHONPATH=src:
    python examples/hydrodynamics/diffraction/abscissa_policy.py

Hand calculation: the grids span [1, 2] rad/s, giving coverage 1. Their largest
relative gaps are (1.25 - 1)/1 = 0.25 and (1.125 - 1)/1 = 0.125. A declared
0.25 gap limit admits both; alignment returns the five coarse-grid points.
These synthetic inputs illustrate validation, not an engineering sampling rule.
"""

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaConfig,
    build_evaluation_grid,
)


def build_example_grid() -> np.ndarray:
    """Return the coarser grid under the hand-calculated illustrative policy."""
    # Basis: the hand calculation in the module docstring; no physical default
    # is inferred. Two points are the interval minimum in _validated_abscissa.
    coarse = np.array([1.0, 1.25, 1.5, 1.75, 2.0])
    dense = np.linspace(1.0, 2.0, 9)
    config = AbscissaConfig(
        min_samples=2,
        min_coverage=1.0,
        max_relative_gap=0.25,
        justification="Synthetic grids: coverage 1, largest relative gap 0.25.",
    )
    return build_evaluation_grid(coarse, dense, config)


def main() -> None:
    """Print the accepted grid and refusal of boolean threshold inputs."""
    print(f"Evaluation grid (rad/s): {build_example_grid().tolist()}")
    for field in ("min_coverage", "max_relative_gap"):
        try:
            AbscissaConfig(**{field: True}, justification="Invalid truth-value probe.")
        except ValueError:
            print(f"{field}: rejected boolean")
        else:
            raise AssertionError(f"{field} accepted a boolean threshold")


if __name__ == "__main__":
    main()
