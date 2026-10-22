# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rigid-body work-preserving reference transfer, independently calculated."""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.external_matrices import transfer_matrix


def test_hand_calculated_diagonal_example():
    """At CoG K=diag(2,3,5,7,11,13), with r=(0,0,2) m from origin.

    u_G=(x+2*pitch, y-2*roll, z). Substitution in twice the energy gives
    2(x+2*pitch)^2+3(y-2*roll)^2+5*z^2+7*roll^2+11*pitch^2+13*yaw^2.
    Thus K_O has diagonal (2,3,5,19,19,13), K15=K51=4,
    K24=K42=-6, and every other off-diagonal term is zero.
    Source: Handbook of Marine Craft Hydrodynamics and Motion Control,
    2nd ed. (2021), rigid-body kinematics; its public marine craft model
    gives S(a)b=a cross b. Congruence follows directly from this energy.
    """
    expected = np.diag([2., 3., 5., 19., 19., 13.])
    expected[0, 4] = expected[4, 0] = 4.
    expected[1, 3] = expected[3, 1] = -6.
    result = transfer_matrix(np.diag([2., 3., 5., 7., 11., 13.]), [0., 0., 2.])
    np.testing.assert_array_equal(result, expected)


def test_round_trip_and_virtual_work_for_general_offset():
    """Synthetic integer inputs yield exact reversible binary arithmetic.

    For r=(1,2,3), theta=(2,3,4), theta cross r=(1,-2,1).
    Hence u_target=(4,5,6) maps to u_source=(5,3,7). Virtual work is
    invariant, independently evaluated by the explicit cross product.
    """
    matrix = np.diag([2., 3., 5., 7., 11., 13.])
    result = transfer_matrix(matrix, [1., 2., 3.])
    np.testing.assert_array_equal(transfer_matrix(result, [-1., -2., -3.]), matrix)
    q_target = np.array([4., 5., 6., 2., 3., 4.])
    q_source = np.array([5., 3., 7., 2., 3., 4.])
    assert q_target @ result @ q_target == q_source @ matrix @ q_source


@pytest.mark.parametrize("offset", [[1., 2.], [0., 0., float("nan")],
                                    [0., 0., float("inf")]])
def test_invalid_offset_is_refused(offset):
    with pytest.raises(ValueError, match="offset"):
        transfer_matrix(np.eye(6), offset)
