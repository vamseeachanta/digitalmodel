"""The damping term in compute_rao_manual follows Capytaine's time convention.

Capytaine writes every complex amplitude against exp(-i w t) and its excitation
force is passed through unchanged, so the equation of motion is

    [-w^2 (M + A) - i w (B + B_ext) + K] x = F

A +i w B operator is the other convention's. For an uncoupled mode the wrong
sign only conjugates the phase; in a coupled system it changes the magnitudes,
which is how it went unnoticed (#2147). Capytaine itself is not needed: the
BEM result is built directly.
"""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.capytaine.models import BEMResult
from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual


def _bem(A, B, F, omegas):
    return BEMResult(
        added_mass=np.asarray(A, dtype=float),
        radiation_damping=np.asarray(B, dtype=float),
        excitation_force=np.asarray(F, dtype=complex),
        omegas=np.asarray(omegas, dtype=float),
        headings=np.array([0.0]),
        dof_names=[f"dof{k}" for k in range(np.asarray(A).shape[1])],
    )


# Two coupled modes: w = 1, M = I, A = 0, B = I, K = [[2, 1], [1, 2]], F = [1, i].
# Solved by hand in exp(-i w t): x = [0.6 + 0.8i, -0.4 - 0.2i].
M2 = np.eye(2)
K2 = np.array([[2.0, 1.0], [1.0, 2.0]])
F2 = np.array([[[1.0, 1.0j]]])
EXPECTED = np.array([0.6 + 0.8j, -0.4 - 0.2j])


def test_coupled_response_uses_minus_i_omega_damping():
    bem = _bem(np.zeros((1, 2, 2)), np.eye(2)[None], F2, [1.0])
    x = compute_rao_manual(bem, M2, K2).rao[0, 0]
    np.testing.assert_allclose(x, EXPECTED, rtol=0, atol=1e-12)
    # The magnitudes are what the wrong sign changes: [1, 0.447] not [0.447, 1].
    np.testing.assert_allclose(np.abs(x), [1.0, np.sqrt(0.2)], atol=1e-12)


def test_extra_damping_takes_the_same_sign_as_radiation_damping():
    bem = _bem(np.zeros((1, 2, 2)), 0.25 * np.eye(2)[None], F2, [1.0])
    x = compute_rao_manual(bem, M2, K2, damping_extra=0.75 * np.eye(2)).rao[0, 0]
    np.testing.assert_allclose(x, EXPECTED, rtol=0, atol=1e-12)


@pytest.mark.parametrize("omega", [0.5, 1.3])
def test_uncoupled_phase_is_capytaines(omega):
    # One mode: x = F / (K - w^2 M - i w B). The magnitude is sign-blind; the
    # phase is not, and it must be the exp(-i w t) one.
    m, a, b, k, f = 2.0, 0.5, 0.3, 4.0, 1.0 + 0.0j
    bem = _bem([[[a]]], [[[b]]], [[[f]]], [omega])
    x = compute_rao_manual(bem, [[m]], [[k]]).rao[0, 0, 0]
    expected = f / (k - omega**2 * (m + a) - 1j * omega * b)
    assert x == pytest.approx(expected, abs=1e-12)
