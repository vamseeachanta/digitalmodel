# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent catenary validation and three-line equilibrium."""
from pathlib import Path

import numpy as np
import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]


def test_zero_offset_stiffness_preserves_reflection_parity():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import mooring_system, stiffness
    basis = yaml.safe_load((ROOT/"docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    matrix = stiffness(mooring_system(basis), np.zeros(6))
    parity = np.diag([1., -1., 1., -1., 1., -1.])
    assert abs(matrix-parity@matrix@parity).max()/abs(matrix).max() < 1e-8


def test_single_grounded_catenary_matches_closed_form():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import grounded_horizontal_tension
    from moorpy import catenary
    distance, rise, length, weight = 779.6, 186.0, 850.0, 5846.0
    expected = grounded_horizontal_tension(distance, rise, length, weight)
    solution = catenary(distance, rise, length, 1e16, weight, CB=0, Tol=1e-7)
    assert solution[0] == pytest.approx(expected, rel=0.005)


def test_three_line_equilibrium_and_zero_offset_stiffness():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import mooring_system, equilibrate, stiffness
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    system = mooring_system(basis)
    k = stiffness(system, np.zeros(6))
    assert k[0, 0] > 0 and k[5, 5] > 0
    assert k[0, 0] == pytest.approx(k[1, 1], rel=1e-4)
    equilibrium = equilibrate(system, basis, 0.0)
    assert equilibrium["offset"][0] > 0
    assert abs(equilibrium["offset"][1]) < 1e-4
    assert np.linalg.norm(equilibrium["residual"][:2]) < 100


def test_removed_line_re_equilibrates_remaining_two():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import mooring_system, equilibrate
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    damaged = mooring_system(basis, removed=0)
    result = equilibrate(damaged, basis, 0.0)
    assert len(damaged.lineList) == 2
    assert np.linalg.norm(result["residual"][:2]) < 100
    assert result["offset"][0] > 0
