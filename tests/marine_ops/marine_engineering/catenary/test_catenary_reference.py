"""Independent offline catenary reference for the input set in issue 2050."""

from math import asinh, atanh, cosh, hypot, log, sinh, sqrt, ulp
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


@pytest.mark.parametrize("elastic", [False, True], ids=["inextensible", "elastic"])
def test_independent_derivation_against_exact_hand_calculation(elastic):
    r"""Check the reference oracle against a manufactured, exactly soluble case.

    Source: NREL/TP-500-41958 (2007), Eqs. (2-35), (2-41), (2-44).
    All numbers below are constructed inputs or the following hand calculation,
    not measured properties or outputs of either numerical solver.

    Choose H=w=1, Va=-3/4, Vf=4/3 and EA=16 in consistent units.
    Then L=(Vf-Va)/w=25/12; Ta=5/4 and Tf=5/3 by the two
    Pythagorean triples. asinh(4/3)=ln(3), asinh(-3/4)=-ln(2), so
      Xg=ln(6), dg=5/12,
      Xe=HL/EA=25/(12*EA), de=(Va+Vf)*L/(2*EA)=175/(288*EA).
    Integrating T/EA over unstretched s gives
      dL=[V*T+H**2*asinh(V/H)]_Va^Vf/(2*w*EA)
        =[455/144+ln(6)]/(2*EA).
    The elastic and inextensible limits share the same forces; their prescribed
    endpoints differ by exactly (Xe,de). ROUND_OFF is the existing binary64
    allowance, not an engineering acceptance limit.
    """
    stiffness = 16
    x_elastic = 25 / (12 * stiffness) if elastic else 0
    d_elastic = 175 / (288 * stiffness) if elastic else 0
    inputs = {
        "length": 25 / 12,
        "horizontal_span": log(6) + x_elastic,
        "vertical_span": 5 / 12 + d_elastic,
        "weight_per_length": 1,
        "ea_stiffness": stiffness,
    }
    expected = {
        "horizontal_tension": 1,
        "vertical_tension_anchor": -3 / 4,
        "vertical_tension_fairlead": 4 / 3,
        "total_tension_anchor": 5 / 4,
        "total_tension_fairlead": 5 / 3,
        "catenary_parameter": 1,
        "elongation": (455 / 144 + log(6)) / (2 * stiffness) if elastic else 0,
    }
    actual = _derive(inputs, elastic=elastic)
    for key, value in expected.items():
        assert actual[key] == pytest.approx(value, rel=ROUND_OFF, abs=0), key


def test_fixture_input_provenance_and_rounding_contract(reference):
    """Keep a single reference, original inputs, and half-unit rounding limits.

    Source: fixture provenance.inputs and provenance.precision. Decimal-place
    counts are recorded in provenance.output_precision; half a last-place unit
    is 10**(-places)/2, by rounding to nearest in base ten. This is a storage
    precision contract, not a physical or solver accuracy specification.
    """
    (example,) = reference["worked_examples"]
    assert example["use_as_test"] is True
    former_analytical, former_spreadsheet = reference["superseded_answers"]
    assert former_analytical["use_as_test"] is False
    assert former_spreadsheet["use_as_test"] is False
    # Symbol mapping is the existing fixture's input naming convention.
    symbols = {
        "L": "length",
        "X": "horizontal_span",
        "Y": "vertical_span",
        "w": "weight_per_length",
        "EA": "ea_stiffness",
    }
    assert {
        symbols[item["symbol"]]: item["test_value"] for item in reference["inputs"]
    } == example["inputs"]
    precision = reference["provenance"]["output_precision"]
    # Counts are the existing fixture's provenance.precision declaration.
    assert precision == {"force_and_parameter": 6, "elongation": 12}
    for output in reference["outputs"]:
        key = output["result_key"]
        places = precision["elongation" if key == "elongation" else "force_and_parameter"]
        assert output["tolerance"] == 10.0 ** (-places) / 2, key
    source = reference["provenance"]["equations"]["technical_report"]
    assert source["url"].startswith("https://")
    assert source["equations"]


def test_elastic_coordinates_and_arc_by_independent_quadrature(reference):
    r"""Integrate the physical elastic curve, including its stretched arc length.

    Source: NREL/TP-500-41958 (2007), Eqs. (2-43a,b), (2-44).
    Differentiating the published coordinates gives
      x'(s)=H/T(s)+H/EA, y'(s)=V(s)/T(s)+V(s)/EA,
      V(s)=Va+w*s, T(s)=hypot(H,V(s)).
    Thus integral(hypot(x',y') ds)=L+integral(T/EA ds).
    Expected X, d and L are the original fixture inputs, not oracle outputs;
    expected elongation is independently checked by the preceding quadrature.

    Rounding allowance follows from the fixture's half-last-unit force limits:
    each derivative of H/T and V/T in H or V has magnitude <=1/H_min.
    Coordinate error is bounded by L*(dH+dVa)/H_min plus elastic rounding.
    The norm T is Lipschitz, so arc rounding is <=L*(dH+dVa)/EA.
    Add the integration error estimates and stored elongation rounding, without
    introducing an empirical engineering tolerance.
    """
    from scipy.integrate import quad

    (example,) = reference["worked_examples"]
    inputs, outputs = example["inputs"], example["outputs"]
    length, weight, stiffness = (
        inputs[key] for key in ("length", "weight_per_length", "ea_stiffness")
    )
    horizontal = outputs["horizontal_tension"]
    anchor = outputs["vertical_tension_anchor"]
    tolerances = {item["result_key"]: item["tolerance"] for item in reference["outputs"]}
    h_round = tolerances["horizontal_tension"]
    v_round = tolerances["vertical_tension_anchor"]
    coordinate_rounding = (
        length * (h_round + v_round) / (horizontal - h_round)
        + length * max(h_round, v_round) / stiffness
    )

    def tangent(s):
        vertical = anchor + weight * s
        tension = hypot(horizontal, vertical)
        return (
            horizontal / tension + horizontal / stiffness,
            vertical / tension + vertical / stiffness,
        )

    for component, key in enumerate(("horizontal_span", "vertical_span")):
        displacement, error = quad(lambda s: tangent(s)[component], 0, length)
        assert displacement == pytest.approx(
            inputs[key], rel=0, abs=coordinate_rounding + error
        ), key
    physical_arc, error = quad(lambda s: hypot(*tangent(s)), 0, length)
    arc_rounding = length * (h_round + v_round) / stiffness
    assert physical_arc == pytest.approx(
        length + outputs["elongation"],
        rel=0,
        abs=error + arc_rounding + tolerances["elongation"],
    )
    assert physical_arc > length
    # The physical elastic curve is not the classical inextensible catenary,
    # even if its stretched arc length is substituted in the classical identity.
    a = horizontal / weight
    assert sqrt(physical_arc**2 - inputs["vertical_span"] ** 2) != pytest.approx(
        2 * a * sinh(inputs["horizontal_span"] / (2 * a)), rel=ROUND_OFF
    )
