# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent force balances for synthetic single lines, without line tables.

Massless case: Hooke's law T=EA*(span/L-1) and endpoint action/reaction.
Weighted case: integrate dx/ds=H/sqrt(H²+V²)+H/EA and
dz/ds=V/sqrt(H²+V²)+V/EA, with V(s)=V0+w*s, over unstretched s.
These hand calculations prescribe endpoint coordinates from known forces;
the adapter must recover those forces using real MoorPy, without a stub.
All dimensional numbers below are chosen synthetic inputs, not material data.
"""

import builtins
import importlib
from math import asinh, sqrt

import pytest


def solve(**overrides):
    """Use a synthetic 10 m line with EA=1000 N and an 11 m span."""
    from digitalmodel.subsea.mooring_analysis.moorpy_adapter import solve_single_line

    inputs = dict(
        endpoint_a=(0.0, 0.0, 0.0),
        endpoint_b=(11.0, 0.0, 0.0),
        unstretched_length=10.0,
        ea=1000.0,
        submerged_weight_per_length=0.0,
    )
    return solve_single_line(**(inputs | overrides))


def test_missing_optional_dependency(monkeypatch):
    """Block MoorPy imports; package, catenary and adapter still import."""
    original_import = builtins.__import__

    def without_moorpy(name, *args, **kwargs):
        if name == "moorpy" or name.startswith("moorpy."):
            raise ModuleNotFoundError("No module named 'moorpy'", name="moorpy")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_moorpy)
    package = importlib.import_module("digitalmodel.subsea.mooring_analysis")
    importlib.reload(package)
    adapter = importlib.import_module(
        "digitalmodel.subsea.mooring_analysis.moorpy_adapter"
    )
    importlib.reload(adapter)
    assert package.CatenaryAnalyzer is not None
    with pytest.raises(ImportError, match=r"MoorPy.*pip install moorpy==1\.3\.0"):
        solve()


@pytest.mark.parametrize(
    "endpoint_b, expected_a",
    [
        ((11.0, 0.0, 0.0), (100.0, 0.0, 0.0)),
        ((6.6, 8.8, 0.0), (60.0, 80.0, 0.0)),
        ((0.0, 6.6, -8.8), (0.0, 60.0, -80.0)),
        ((0.0, 0.0, 11.0), (0.0, 0.0, 100.0)),
    ],
)
def test_massless_taut_line_hand_balance(endpoint_b, expected_a):
    """Span=11 m, L=10 m, EA=1000 N: T=100 N.

    The 3:4:5 direction gives components 60 and 80 N; vertical and
    downward cases establish the z sign. Absolute tolerance 1e-8 N
    allows floating-point rounding, far below the synthetic 100 N load.
    No importorskip: a missing library fails this acceptance check.
    """
    result = solve(endpoint_b=endpoint_b)
    assert result.force_a == pytest.approx(expected_a, abs=1e-8)
    assert result.force_b == pytest.approx(tuple(-f for f in expected_a), abs=1e-8)
    assert tuple(a + b for a, b in zip(result.force_a, result.force_b)) == (
        0.0, 0.0, 0.0,
    )


@pytest.mark.parametrize("span", [5.0, 10.0, 25.0])
def test_massless_line_is_tension_only(span):
    """At/below L: zero force; span=25 m: EA*(25/10-1)=1500 N.

    The large extension also checks that the EA curve is not clipped at
    a fixed strain. Synthetic mathematics only, not a material validity claim.
    """
    result = solve(endpoint_b=(span, 0.0, 0.0))
    expected = 1500.0 if span == 25.0 else 0.0
    assert result.force_a == pytest.approx((expected, 0.0, 0.0))
    assert result.force_b == pytest.approx((-expected, 0.0, 0.0))


@pytest.mark.parametrize("reverse", [False, True])
def test_weighted_line_hand_balance(reverse):
    """L=100 m, w=100 N/m, EA=1e6 N, H=10000 N, V0=-3000 N.

    V1=7000 N. Integrating the equations in the module docstring gives
    the coordinates below. End loads are (H,V0) and (-H,-V1).
    Rotate horizontally by the 3:4:5 direction and translate arbitrarily.
    Relative tolerance 1e-6 allows iterative error in recovered forces.
    """
    horizontal = 100.0 * (asinh(0.7) - asinh(-0.3)) + 1.0
    vertical = (sqrt(10000.0**2 + 7000.0**2)
                - sqrt(10000.0**2 + 3000.0**2)) / 100.0 + 0.2
    a = (2.0, -3.0, 25.0)
    b = (a[0] + 0.6 * horizontal, a[1] + 0.8 * horizontal, a[2] + vertical)
    expected_a = (6000.0, 8000.0, -3000.0)
    expected_b = (-6000.0, -8000.0, -7000.0)
    if reverse:
        a, b = b, a
        expected_a, expected_b = expected_b, expected_a
    result = solve(
        endpoint_a=a, endpoint_b=b, unstretched_length=100.0,
        ea=1e6, submerged_weight_per_length=100.0,
    )
    assert result.force_a == pytest.approx(expected_a, rel=1e-6)
    assert result.force_b == pytest.approx(expected_b, rel=1e-6)
    assert tuple(x + y for x, y in zip(result.force_a, result.force_b)) == (
        pytest.approx(0.0, abs=1e-8),
        pytest.approx(0.0, abs=1e-8),
        pytest.approx(-10000.0),  # -w*L, by whole-line equilibrium.
    )


@pytest.mark.parametrize("key", ["unstretched_length", "ea"])
@pytest.mark.parametrize("value", [0.0, -1.0, float("nan"), float("inf")])
def test_invalid_line_input(key, value):
    """Zero/negative/nonfinite length or stiffness has no elastic-line model."""
    with pytest.raises(ValueError, match=key):
        solve(**{key: value})


@pytest.mark.parametrize("key", ["endpoint_a", "endpoint_b"])
@pytest.mark.parametrize(
    "value", [(float("nan"), 0.0, 0.0), (0.0, float("inf"), 0.0),
              (0.0, 0.0, -float("inf")), (0.0, 0.0), (0.0,) * 4],
)
def test_invalid_endpoint(key, value):
    """Endpoints require exactly three finite Cartesian coordinates."""
    with pytest.raises(ValueError, match=key):
        solve(**{key: value})


@pytest.mark.parametrize("weight", [-1.0, float("nan"), float("inf")])
def test_invalid_submerged_weight(weight):
    """The initial adapter supports finite nonnegative submerged weight."""
    with pytest.raises(ValueError, match="submerged_weight_per_length"):
        solve(submerged_weight_per_length=weight)


def test_coincident_endpoints_rejected():
    """No unique line direction exists for coincident endpoints."""
    with pytest.raises(ValueError, match="distinct"):
        solve(endpoint_b=(0.0, 0.0, 0.0))
