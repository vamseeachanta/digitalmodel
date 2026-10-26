# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic complex and rigid-body hand calculations.

All numbers below are chosen arithmetic inputs, not measured response data or
acceptance limits. Each expected response is derived in the test's comments.
"""

from dataclasses import replace

import pytest

from digitalmodel.marine_ops.w2w_gangway import ResponseBasis, compute_relative_rao


@pytest.fixture
def basis():
    """Declare one synthetic frequency/heading sample and common wave reference."""
    return ResponseBasis(
        frequency=1.0,
        frequency_unit="rad/s",
        heading=30.0,
        heading_unit="deg",
        heading_convention="going-to, counterclockwise from +x",
        coordinate_convention="common right-handed x/y/z, z up",
        phase_convention="real(H * exp(+i * omega * t))",
        phase_reference="incident wave at shared reference origin",
        response_units={"surge": "m/m", "heave": "m/m", "pitch": "rad/m"},
    )


def test_equal_point_responses_cancel(basis):
    # Componentwise (a+bi) - (a+bi) = 0, including the rotational response.
    point = {"surge": 1 + 2j, "heave": -3 + 4j, "pitch": 0.1j}
    result = compute_relative_rao(point, point, access_basis=basis, receiving_basis=basis)
    assert result.responses == {"surge": 0j, "heave": 0j, "pitch": 0j}
    assert result.basis == basis


@pytest.mark.parametrize("stationary_access", [True, False])
def test_stationary_side_preserves_signed_complex_response(basis, stationary_access):
    # R - 0 = R; 0 - A = -A. Signs retain phase rather than amplitude alone.
    moving = {"surge": 1 + 2j, "heave": -3 + 4j, "pitch": 0.1j}
    still = dict.fromkeys(moving, 0j)
    access, receiving = (still, moving) if stationary_access else (moving, still)
    result = compute_relative_rao(access, receiving, access_basis=basis, receiving_basis=basis)
    expected = moving if stationary_access else {"surge": -1 - 2j, "heave": 3 - 4j, "pitch": -0.1j}
    assert result.responses == expected
    assert result.basis.response_units == basis.response_units


def test_opposed_phase_adds_magnitudes(basis):
    # Access = 3+4i (magnitude 5); receiving = -6-8i (magnitude 10).
    # Receiving - access = -9-12i, magnitude 15, phase in quadrant III.
    basis = replace(basis, response_units={"heave": "m/m"})
    result = compute_relative_rao(
        {"heave": 3 + 4j}, {"heave": -6 - 8j},
        access_basis=basis, receiving_basis=basis,
    )
    assert result.responses["heave"] == -9 - 12j
    assert abs(result.responses["heave"]) == 15


def test_nonzero_lever_arm_uses_pre_resolved_point_responses(basis):
    # Small-angle identity u_point = u_origin + theta cross r; in z:
    # z = heave - rx*pitch + ry*roll (angles rad, offsets m).
    # Access: h=1+2i, pitch=0.1+0.2i, roll=0.3-0.1i, r=(2,3,4).
    # z_A = (1+2i)-2(0.1+0.2i)+3(0.3-0.1i) = 1.7+1.3i.
    # Receiving: h=2-i, pitch=-0.2+0.1i, roll=0.1+0.2i, r=(-1,2,3).
    # z_R = (2-i)+(-0.2+0.1i)+2(0.1+0.2i) = 2-0.5i.
    # Pre-resolve before calling: z_R-z_A = 0.3-1.8i. No offset is inferred.
    basis = replace(basis, response_units={"heave": "m/m"})
    access_z = (1 + 2j) - 2 * (0.1 + 0.2j) + 3 * (0.3 - 0.1j)
    receiving_z = (2 - 1j) - (-1) * (-0.2 + 0.1j) + 2 * (0.1 + 0.2j)
    result = compute_relative_rao(
        {"heave": access_z}, {"heave": receiving_z},
        access_basis=basis, receiving_basis=basis,
    )
    assert access_z == pytest.approx(1.7 + 1.3j)
    assert receiving_z == pytest.approx(2 - 0.5j)
    assert result.responses["heave"] == pytest.approx(0.3 - 1.8j)


@pytest.mark.parametrize(("field", "different"), [
    ("frequency", 2.0),
    ("frequency_unit", "Hz"),
    ("heading", 60.0),
    ("heading_unit", "rad"),
    ("heading_convention", "coming-from"),
    ("coordinate_convention", "local axes, z down"),
    ("phase_convention", "real(H * exp(-i * omega * t))"),
    ("phase_reference", "different incident wave reference origin"),
    ("response_units", {"surge": "mm/m", "heave": "m/m", "pitch": "rad/m"}),
])
def test_relative_response_rejects_mismatched_basis(basis, field, different):
    point = {"surge": 1j, "heave": 2j, "pitch": 3j}
    with pytest.raises(ValueError, match="mismatched response basis"):
        compute_relative_rao(
            point, point, access_basis=basis,
            receiving_basis=replace(basis, **{field: different}),
        )


@pytest.mark.parametrize("side", ["access", "receiving"])
def test_component_keys_must_match_declared_units(basis, side):
    complete = {"surge": 1j, "heave": 2j, "pitch": 3j}
    incomplete = {"heave": 2j}
    access, receiving = (incomplete, complete) if side == "access" else (complete, incomplete)
    with pytest.raises(ValueError, match="components must match response_units"):
        compute_relative_rao(access, receiving, access_basis=basis, receiving_basis=basis)


@pytest.mark.parametrize("value", [complex("nan"), complex("inf"), complex(1, float("inf"))])
@pytest.mark.parametrize("side", ["access", "receiving"])
def test_nonfinite_response_is_rejected(basis, value, side):
    point = {"surge": 1j, "heave": 2j, "pitch": 3j}
    invalid = dict(point, heave=value)
    access, receiving = (invalid, point) if side == "access" else (point, invalid)
    with pytest.raises(ValueError, match="finite complex"):
        compute_relative_rao(access, receiving, access_basis=basis, receiving_basis=basis)


@pytest.mark.parametrize(("field", "value"), [
    ("frequency", float("nan")), ("heading", float("inf")),
    ("frequency_unit", ""), ("heading_unit", " "),
    ("heading_convention", ""), ("coordinate_convention", ""),
    ("phase_convention", ""), ("phase_reference", ""),
    ("response_units", {}), ("response_units", {"heave": ""}),
    ("response_units", {"": "m/m"}),
])
def test_basis_must_be_explicit_and_finite(basis, field, value):
    with pytest.raises(ValueError):
        replace(basis, **{field: value})
