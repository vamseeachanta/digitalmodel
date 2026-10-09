"""Exact identities for the approved open-ended cylinder; no native solver."""
from decimal import Decimal, Inexact, ROUND_DOWN, localcontext
from fractions import Fraction

import pytest

from digitalmodel.ansys.cylinder_reference import (
    reference, reference_exact, reference_text, symbols_for,
)


def test_boundary_tractions_and_open_end_branch():
    inner = reference_exact("10", "750", "120")
    outer = reference_exact("10", "810", "120")
    assert inner["sigma_r"] == -10
    assert outer["sigma_r"] == 0
    assert inner["sigma_theta"] - outer["sigma_theta"] == 10
    assert inner["sigma_z"] == inner["tau_rz"] == 0
    assert inner["u_z"] < 0 < inner["u_r"]


@pytest.mark.parametrize("radius", ["750", "780", "810"])
def test_zero_control_and_axial_identities(radius):
    assert all(v == 0 for v in reference("0", radius, "120").values())
    mid = reference_exact("10", radius, "120")
    for y, ratio in [("0", 0), ("60", Fraction(1, 2)), ("180", Fraction(3, 2))]:
        station = reference_exact("10", radius, y)
        assert station["u_z"] == ratio * mid["u_z"]
        for name in ["sigma_r", "sigma_theta", "sigma_z", "tau_rz", "u_r"]:
            assert station[name] == mid[name]


def test_hooke_law_equilibrium_and_branch_mutations():
    s = symbols_for("10", "780", "120")
    a, b, p, r, e, nu = [s[k] for k in ("a", "b", "p", "r", "E", "nu")]
    aa = p * a * a / (b * b - a * a)
    bb = aa * b * b
    q = reference_exact("10", "780", "120")
    eps_r = ((1 - nu) * aa - (1 + nu) * bb / r**2) / e
    eps_t = q["u_r"] / r
    eps_z = q["u_z"] / s["y"]
    lam = e * nu / ((1 + nu) * (1 - 2 * nu))
    mu = e / (2 * (1 + nu))
    assert lam * (eps_r + eps_t) + (lam + 2 * mu) * eps_z == 0
    assert 2 * bb / r**3 + (q["sigma_r"] - q["sigma_theta"]) / r == 0
    assert q['sigma_z'] == 0
    assert q['sigma_z'] != nu * (q['sigma_r'] + q['sigma_theta'])
    assert aa != q["sigma_z"]  # closed-end mutation
    assert q['u_z'] == -nu * (q['sigma_r'] + q['sigma_theta']) * s['y'] / e


def test_equivalent_stress_precision_and_canonical_text():
    q = reference("10", "750", "120")
    with localcontext() as ctx:
        ctx.prec = 50
        squared = (q["sigma_r"]**2 + q["sigma_theta"]**2
                   - q["sigma_r"] * q["sigma_theta"])
        assert abs(q["sigma_vm"]**2 - squared) < Decimal("1e-43")
    text = reference_text("1e1", "750.000", "120.0")
    assert text["sigma_r"] == "-10"
    assert text["sigma_z"] == "0"
    assert all("E" not in value for value in text.values())


@pytest.mark.parametrize("args", [(10, "750", "120"), ("-1", "750", "120"),
    ("5", "750", "120"), ("10", "749", "120"), ("10", "810.1", "120"),
    ("10", "750", "241"), ("NaN", "750", "120"), ("10", "inf", "120")])
def test_invalid_or_out_of_basis_inputs_refused(args):
    with pytest.raises(ValueError):
        reference(*args)


def test_reference_is_independent_of_ambient_decimal_settings():
    expected = reference("10", "780", "120")
    with localcontext() as ctx:
        ctx.rounding = ROUND_DOWN
        ctx.traps[Inexact] = True
        assert reference("10", "780", "120") == expected
