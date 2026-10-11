"""Independent offline catenary reference for the input set in issue 2050."""

from math import asinh, atanh, cosh, hypot, sinh, sqrt, ulp
from pathlib import Path

import pytest
import yaml


REFERENCE_PATH = (
    Path(__file__).resolve().parents[3]
    / "fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml"
)
# Floating-point allowance is derived from binary64 resolution, not a force limit.
ROUND_OFF = sqrt(ulp(1.0))


@pytest.fixture
def reference():
    with REFERENCE_PATH.open(encoding="utf-8") as stream:
        return yaml.safe_load(stream)


def _bisect(function, lower, upper):
    """Bisect to floating-point exhaustion; no solver-specific tolerances."""
    lower_value = function(lower)
    assert lower_value * function(upper) < 0
    while True:
        midpoint = (lower + upper) / 2
        if midpoint in (lower, upper):
            return midpoint
        value = function(midpoint)
        if value == 0:
            return midpoint
        if lower_value * value > 0:
            lower, lower_value = midpoint, value
        else:
            upper = midpoint


def _geometry(horizontal, anchor_vertical, inputs, elastic=True):
    length, weight = inputs["length"], inputs["weight_per_length"]
    stiffness = inputs["ea_stiffness"]
    fairlead_vertical = anchor_vertical + weight * length
    anchor_tension = hypot(horizontal, anchor_vertical)
    fairlead_tension = hypot(horizontal, fairlead_vertical)
    x = (
        horizontal
        / weight
        * (asinh(fairlead_vertical / horizontal) - asinh(anchor_vertical / horizontal))
    )
    d = (fairlead_tension - anchor_tension) / weight
    if elastic:
        x += horizontal * length / stiffness
        d += (anchor_vertical + weight * length / 2) * length / stiffness
    return x, d


def _derive(inputs, elastic=True):
    """Integrate endpoint equilibrium; do not import the production solver."""
    length, weight = inputs["length"], inputs["weight_per_length"]

    def anchor_vertical(horizontal):
        return _bisect(
            lambda vertical: (
                _geometry(horizontal, vertical, inputs, elastic)[1]
                - inputs["vertical_span"]
            ),
            -weight * length,
            weight * length,
        )

    horizontal = _bisect(
        lambda horizontal: (
            _geometry(horizontal, anchor_vertical(horizontal), inputs, elastic)[0]
            - inputs["horizontal_span"]
        ),
        weight * inputs["horizontal_span"] / 4,
        weight * length,
    )
    anchor = anchor_vertical(horizontal)
    fairlead = anchor + weight * length

    def primitive(vertical):
        return (
            vertical * hypot(horizontal, vertical)
            + horizontal**2 * asinh(vertical / horizontal)
        ) / 2

    return {
        "horizontal_tension": horizontal,
        "vertical_tension_anchor": anchor,
        "vertical_tension_fairlead": fairlead,
        "total_tension_anchor": hypot(horizontal, anchor),
        "total_tension_fairlead": hypot(horizontal, fairlead),
        "catenary_parameter": horizontal / weight,
        "elongation": (
            (primitive(fairlead) - primitive(anchor))
            / (weight * inputs["ea_stiffness"])
            if elastic
            else 0.0
        ),
    }


def test_fixture_matches_independent_derivation(reference):
    r"""Derive from the original fixture inputs, independently of stored outputs.

    Let s be unstretched arclength, Va the signed internal vertical component
    at the anchor, H constant, V(s)=Va+w*s, and T(s)=hypot(H,V(s)).
    Hooke's law gives dx/ds=H/T+H/EA and dy/ds=V/T+V/EA. Integration gives
      X = (H/w)[asinh(Vf/H)-asinh(Va/H)] + H*L/EA,
      d = (Tf-Ta)/w + (Va+w*L/2)*L/EA,  Vf=Va+w*L.
    Nested bisection solves d for Va, then X for H; both brackets are
    dimensionally derived from this fixture and checked for opposite signs.
    Elongation is integral(T/EA ds)=[F(Vf)-F(Va)]/(w*EA), where
      F(V)=[V*hypot(H,V)+H**2*asinh(V/H)]/2.
    The public equation reference and retrieval date are recorded in YAML.

    In the inextensible limit, a=H/w, q=X/(2a), m=atanh(d/L):
      Va=H*sinh(m-q), Vf=H*sinh(m+q),
      sqrt(L**2-d**2)=2a*sinh(q).
    For finite EA this last identity uses Xg=X-H*L/EA and
    dg=d-(Va+w*L/2)*L/EA with S=L. The physical stretched curve does not
    obey the classical chord/arc identity. Both limits are asserted below.
    No expected quantity is obtained from the solver being tested.
    """
    example = reference["worked_examples"][0]
    inputs = {key: float(value) for key, value in example["inputs"].items()}
    expected = _derive(inputs)
    assert example["outputs"]["horizontal_tension"] == pytest.approx(
        expected["horizontal_tension"], rel=ROUND_OFF
    )
    (example,) = reference["worked_examples"]
    for output in reference["outputs"]:
        key = output["result_key"]
        tolerance = output["tolerance"]
        assert output["test_expected"] == pytest.approx(
            expected[key], rel=0, abs=tolerance
        )
        assert example["outputs"][key] == output["test_expected"]


def test_elastic_fixture_geometry_and_force_identities(reference):
    (example,) = reference["worked_examples"]
    inputs, outputs = example["inputs"], example["outputs"]
    length, weight = inputs["length"], inputs["weight_per_length"]
    horizontal = outputs["horizontal_tension"]
    anchor, fairlead = (
        outputs["vertical_tension_anchor"],
        outputs["vertical_tension_fairlead"],
    )
    x, d = _geometry(horizontal, anchor, inputs)
    assert x == pytest.approx(inputs["horizontal_span"], rel=ROUND_OFF)
    assert d == pytest.approx(inputs["vertical_span"], rel=ROUND_OFF)
    assert fairlead - anchor == pytest.approx(weight * length, rel=ROUND_OFF)
    for end, vertical in (("anchor", anchor), ("fairlead", fairlead)):
        assert outputs[f"total_tension_{end}"] == pytest.approx(
            hypot(horizontal, vertical), rel=ROUND_OFF
        )
    a = outputs["catenary_parameter"]
    assert a * weight == pytest.approx(horizontal, rel=ROUND_OFF)
    x_geometry = (
        inputs["horizontal_span"] - horizontal * length / inputs["ea_stiffness"]
    )
    d_geometry = (
        inputs["vertical_span"]
        - (anchor + weight * length / 2) * length / inputs["ea_stiffness"]
    )
    assert sqrt(length**2 - d_geometry**2) == pytest.approx(
        2 * a * sinh(x_geometry / (2 * a)), rel=ROUND_OFF
    )


def test_inextensible_chord_arc_and_end_forces(reference):
    inputs = reference["worked_examples"][0]["inputs"]
    outputs = _derive(inputs, elastic=False)
    assert outputs["elongation"] == 0  # Infinite stiffness implies zero strain.
    length, d = inputs["length"], inputs["vertical_span"]
    a, horizontal = outputs["catenary_parameter"], outputs["horizontal_tension"]
    q, m = inputs["horizontal_span"] / (2 * a), atanh(d / length)
    assert sqrt(length**2 - d**2) == pytest.approx(2 * a * sinh(q), rel=ROUND_OFF)
    anchor, fairlead = horizontal * sinh(m - q), horizontal * sinh(m + q)
    assert fairlead - anchor == pytest.approx(
        inputs["weight_per_length"] * length, rel=ROUND_OFF
    )
    for end, vertical in (("anchor", anchor), ("fairlead", fairlead)):
        assert outputs[f"vertical_tension_{end}"] == pytest.approx(
            vertical, rel=ROUND_OFF
        )
        assert outputs[f"total_tension_{end}"] == pytest.approx(
            hypot(horizontal, vertical), rel=ROUND_OFF
        )


def test_superseded_answers_fail_geometry_and_touchdown_diagnosis(reference):
    inputs = reference["worked_examples"][0]["inputs"]
    length, x, d, weight = (
        inputs[key]
        for key in ("length", "horizontal_span", "vertical_span", "weight_per_length")
    )
    for old in reference["superseded_answers"]:
        a = old["outputs"]["horizontal_tension"] / weight
        assert 2 * a * sinh(x / (2 * a)) != pytest.approx(
            sqrt(length**2 - d**2), rel=ROUND_OFF
        )
    old = reference["superseded_answers"][0]["outputs"]
    a = old["horizontal_tension"] / weight
    # A first-order stretch bound comes from the published endpoint tension.
    stretch_bound = old["total_tension_fairlead"] * length / inputs["ea_stiffness"]
    assert abs(a * sinh(x / a) - length) < stretch_bound
    assert (
        abs(old["vertical_tension_fairlead"] - weight * length) < weight * stretch_bound
    )
    assert a * (cosh(x / a) - 1) > d


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason=(
        "Issue 2050: solver enforces arc_length + H*L/EA = L; the independent "
        "elastic reference uses unstretched L and full tension-dependent strain."
    ),
)
def test_solver_against_reconciled_fixture(reference):
    from digitalmodel.marine_ops.marine_analysis.catenary import (
        CatenaryInput,
        CatenarySolver,
    )

    (example,) = reference["worked_examples"]
    result = CatenarySolver().solve(CatenaryInput(**example["inputs"]))
    if not result.converged:
        raise RuntimeError("Catenary solver did not converge")
    for output in reference["outputs"]:
        key = output["result_key"]
        if key == "vertical_tension_anchor":
            actual = (
                result.vertical_tension_fairlead
                - example["inputs"]["weight_per_length"] * example["inputs"]["length"]
            )
        else:
            actual = getattr(result, key)
        assert actual == pytest.approx(
            output["test_expected"], rel=0, abs=output["tolerance"]
        )


def test_elastic_elongation_by_independent_quadrature(reference):
    """Read back the elongation by integrating tension over reference length."""
    from scipy.integrate import quad

    (example,) = reference["worked_examples"]
    inputs, outputs = example["inputs"], example["outputs"]
    integral, error = quad(
        lambda s: (
            hypot(
                outputs["horizontal_tension"],
                outputs["vertical_tension_anchor"] + inputs["weight_per_length"] * s,
            )
            / inputs["ea_stiffness"]
        ),
        0,
        inputs["length"],
    )
    tolerance = next(
        output["tolerance"]
        for output in reference["outputs"]
        if output["result_key"] == "elongation"
    )
    assert outputs["elongation"] == pytest.approx(
        integral, rel=0, abs=tolerance + error
    )
    assert (
        outputs["elongation"]
        > outputs["horizontal_tension"] * inputs["length"] / inputs["ea_stiffness"]
    )
