# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Manual RAO input contracts using synthetic arrays and hand calculations.

The synthetic fixture has two frequencies, headings and DOFs so singleton axes
can expose unintended broadcasting. Its coefficients are arbitrary finite test
inputs, not measured data. Expected responses are derived explicitly below.
"""

from unittest.mock import Mock

import numpy as np
import pytest

from digitalmodel.hydrodynamics.capytaine.models import BEMResult
from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual


FIELDS = (
    "omegas",
    "added_mass",
    "radiation_damping",
    "excitation_force",
    "mass_matrix",
    "stiffness_matrix",
    "damping_extra",
)


def _inputs():
    """Return independent synthetic arrays for each test."""
    bem = BEMResult(
        omegas=np.array([1.0, 2.0]),
        added_mass=np.zeros((2, 2, 2)),
        radiation_damping=np.broadcast_to(np.eye(2), (2, 2, 2)).copy(),
        excitation_force=np.ones((2, 2, 2), dtype=complex),
        headings=np.array([0.0, 90.0]),
        dof_names=["first", "second"],
    )
    structural = dict(
        mass_matrix=np.eye(2),
        stiffness_matrix=10.0 * np.eye(2),
        damping_extra=np.eye(2),
    )
    return bem, structural


def _set_input(bem, structural, field, value):
    if field in structural:
        structural[field] = value
    else:
        setattr(bem, field, value)


def _assert_rejected_before_solve(monkeypatch, bem, structural, field):
    solve = Mock(side_effect=AssertionError("invalid input reached solve"))
    lstsq = Mock(side_effect=AssertionError("invalid input reached least squares"))
    monkeypatch.setattr(np.linalg, "solve", solve)
    monkeypatch.setattr(np.linalg, "lstsq", lstsq)
    with pytest.raises(ValueError, match=field):
        compute_rao_manual(bem, **structural)
    solve.assert_not_called()
    lstsq.assert_not_called()


@pytest.mark.parametrize(
    "field,shape",
    [
        ("omegas", (2, 1)),
        ("added_mass", (2, 2, 1)),
        ("radiation_damping", (2, 2, 1)),
        # An extra frequency slice used to be silently ignored.
        ("excitation_force", (3, 2, 2)),
        ("mass_matrix", (2, 1)),
        ("stiffness_matrix", (2, 1)),
        ("damping_extra", (2, 1)),
    ],
)
def test_manual_rao_rejects_broadcast_shapes(monkeypatch, field, shape):
    bem, structural = _inputs()
    _set_input(bem, structural, field, np.ones(shape))
    _assert_rejected_before_solve(monkeypatch, bem, structural, field)


@pytest.mark.parametrize("field", FIELDS)
@pytest.mark.parametrize("bad_value", [np.nan, np.inf, -np.inf])
def test_manual_rao_rejects_nonfinite_arrays(monkeypatch, field, bad_value):
    bem, structural = _inputs()
    values = structural[field] if field in structural else getattr(bem, field)
    # Put the defect in the last slice: no earlier valid slice may be solved.
    values.flat[-1] = bad_value
    _assert_rejected_before_solve(monkeypatch, bem, structural, field)


@pytest.mark.parametrize("bad_value", [complex(1.0, np.nan), complex(1.0, np.inf)])
def test_manual_rao_rejects_nonfinite_complex_excitation(monkeypatch, bad_value):
    bem, structural = _inputs()
    bem.excitation_force.flat[-1] = bad_value
    _assert_rejected_before_solve(monkeypatch, bem, structural, "excitation_force")


@pytest.mark.parametrize(
    "field,shape",
    [
        ("omegas", ()),
        ("omegas", (0,)),
        ("added_mass", (2, 2)),
        ("added_mass", (1, 2, 2)),
        ("added_mass", (2, 0, 0)),
        ("radiation_damping", (2, 2)),
        ("radiation_damping", (1, 2, 2)),
        ("excitation_force", (2, 2)),
        ("excitation_force", (1, 2, 2)),
        ("excitation_force", (2, 2, 1)),
        ("excitation_force", (2, 0, 2)),
        ("mass_matrix", ()),
        ("mass_matrix", (2,)),
        ("stiffness_matrix", ()),
        ("stiffness_matrix", (2,)),
        ("damping_extra", ()),
        ("damping_extra", (2,)),
    ],
)
def test_manual_rao_rejects_other_invalid_shapes(monkeypatch, field, shape):
    bem, structural = _inputs()
    _set_input(bem, structural, field, np.ones(shape))
    _assert_rejected_before_solve(monkeypatch, bem, structural, field)


@pytest.mark.parametrize("field", FIELDS[:-1])
def test_manual_rao_rejects_missing_arrays(monkeypatch, field):
    bem, structural = _inputs()
    _set_input(bem, structural, field, None)
    _assert_rejected_before_solve(monkeypatch, bem, structural, field)


def test_manual_rao_diagonal_hand_solution():
    # Independent hand calculation in exp(-i*w*t), with synthetic coefficients:
    # M=diag(2,3), A=diag(1,1), K=diag(10,20), B=diag(1,2),
    # B_ext=diag(1,1). Thus Z(w=1)=diag(7-2i,16-3i) and
    # Z(w=2)=diag(-2-4i,4-6i). Choose forces by multiplying each Z
    # by x=[1+i,-i] (first heading) and x=[-1+i,2] (second heading):
    # F(w=1)=[[9+5i,-3-16i],[-5+9i,32-6i]],
    # F(w=2)=[[2-6i,-6-4i],[6+2i,8-12i]].
    bem = BEMResult(
        omegas=np.array([1.0, 2.0]),
        added_mass=np.array([np.eye(2), np.eye(2)]),
        radiation_damping=np.array([np.diag([1.0, 2.0])] * 2),
        excitation_force=np.array([
            [[9.0 + 5.0j, -3.0 - 16.0j], [-5.0 + 9.0j, 32.0 - 6.0j]],
            [[2.0 - 6.0j, -6.0 - 4.0j], [6.0 + 2.0j, 8.0 - 12.0j]],
        ]),
    )
    result = compute_rao_manual(
        bem, np.diag([2.0, 3.0]), np.diag([10.0, 20.0]), np.eye(2)
    )
    expected = np.array([[[1.0 + 1.0j, -1.0j], [-1.0 + 1.0j, 2.0]]] * 2)
    amplitudes = np.array([[[np.sqrt(2.0), 1.0], [np.sqrt(2.0), 2.0]]] * 2)
    phases = np.array([[[45.0, -90.0], [135.0, 0.0]]] * 2)
    # The hand-derived values are exact; this tolerance permits rounding in
    # double-precision division and conversion from radians to degrees.
    np.testing.assert_allclose(result.rao, expected, rtol=0, atol=1e-12)
    np.testing.assert_allclose(result.rao_amplitude, amplitudes, rtol=0, atol=1e-12)
    np.testing.assert_allclose(result.rao_phase, phases, rtol=0, atol=1e-12)


def test_manual_rao_preserves_singular_least_squares():
    # Zero impedance and unit force give the minimum-norm solution x=0.
    bem = BEMResult(
        omegas=np.array([1.0]),
        added_mass=np.zeros((1, 1, 1)),
        radiation_damping=np.zeros((1, 1, 1)),
        excitation_force=np.ones((1, 1, 1), dtype=complex),
    )
    result = compute_rao_manual(bem, np.zeros((1, 1)), np.zeros((1, 1)))
    np.testing.assert_array_equal(result.rao, np.zeros((1, 1, 1)))


def test_manual_rao_preserves_zero_and_negative_frequencies():
    # Synthetic one-mode system M=1, A=0, B=1, K=2, F=1:
    # Z(0)=2, Z(-1)=1+i; hence x(0)=1/2 and x(-1)=(1-i)/2.
    bem = BEMResult(
        omegas=np.array([0.0, -1.0]),
        added_mass=np.zeros((2, 1, 1)),
        radiation_damping=np.ones((2, 1, 1)),
        excitation_force=np.ones((2, 1, 1), dtype=complex),
    )
    result = compute_rao_manual(bem, np.ones((1, 1)), 2.0 * np.ones((1, 1)))
    np.testing.assert_allclose(result.rao[:, 0, 0], [0.5, 0.5 - 0.5j], rtol=0, atol=1e-12)
