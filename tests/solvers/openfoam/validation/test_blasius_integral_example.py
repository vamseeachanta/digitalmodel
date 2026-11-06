# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent analytical checks; no CFD solution is exercised.

Basis: https://su2code.github.io/tutorials/Laminar_Flat_Plate/
The cited local law is Cf(x) = 0.664 / sqrt(U*x/nu). All flow inputs below
are synthetic arithmetic examples, not measured fluid properties.
Unit prefix: https://www.bipm.org/en/measurement-units/si-prefixes (centi).
"""

import math
import runpy
from pathlib import Path

import pytest
from scipy.integrate import quad

from digitalmodel.solvers.openfoam.validation.flat_plate import blasius_plate_cd


EXAMPLE = Path(__file__).resolve().parents[4] / "examples/openfoam/blasius_integral.py"


@pytest.fixture
def compare_coefficients():
    return runpy.run_path(str(EXAMPLE))["compare_coefficients"]


@pytest.mark.parametrize(
    ("velocity", "length", "nu"),
    [(1.0, 0.1, 1.0e-5), (0.25, 0.4, 1.0e-5), (0.02, 0.5, 1.0e-5)],
)
def test_blasius_integral_example(compare_coefficients, velocity, length, nu):
    # Substitute x=t**2, dx=2*t*dt to remove the leading-edge singularity.
    # Quad samples the regular transformed integrand inside the interval;
    # neither Cf(0) nor the analytical antiderivative is evaluated.
    def transformed_cf(t):
        x = t * t
        return 2.0 * t * (0.664 / math.sqrt(velocity * x / nu))

    integral, error = quad(transformed_cf, 0.0, math.sqrt(length))
    assert error < 1.0e-10
    mean_cf = integral / length
    cf_at_l = 0.664 / math.sqrt(velocity * length / nu)
    assert mean_cf == pytest.approx(2.0 * cf_at_l)

    result = compare_coefficients(velocity, length, nu)
    assert result["re_l"] == pytest.approx(velocity * length / nu)
    assert result["cf_at_l"] == pytest.approx(cf_at_l)
    assert result["plate_cd_integral"] == pytest.approx(mean_cf)
    assert result["plate_cd_helper"] == pytest.approx(mean_cf)
    # Check the public helper directly against the independent integral.
    assert blasius_plate_cd(velocity * length / nu) == pytest.approx(mean_cf)


@pytest.mark.parametrize("argument", ["velocity", "length", "kinematic_viscosity"])
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_invalid_inputs_rejected(compare_coefficients, argument, value):
    inputs = dict(velocity=1.0, length=0.1, kinematic_viscosity=1.0e-5)
    inputs[argument] = value
    with pytest.raises(ValueError, match=argument + " must be finite and positive"):
        compare_coefficients(**inputs)


@pytest.mark.parametrize("velocity, length", [(1.0e308, 1.0e308), (1.0e-300, 1.0e-300)])
def test_unrepresentable_reynolds_rejected(compare_coefficients, velocity, length):
    with pytest.raises(ValueError, match="re_l must be finite and positive"):
        compare_coefficients(velocity, length, 1.0e-5)


def test_reference_case_ceiling_accepted(compare_coefficients):
    # The repository's laminar reference case uses Re_L=10000.
    result = compare_coefficients(10000.0, 1.0, 1.0)
    assert result["re_l"] == 10000.0


@pytest.mark.parametrize("re_l", [10000.0 * (1.0 + 2.0e-12), 100000.0])
def test_above_reference_range_rejected(compare_coefficients, re_l):
    with pytest.raises(ValueError, match="re_l exceeds the example limit of 10000"):
        compare_coefficients(re_l, 1.0, 1.0)


def test_reference_boundary_accepts_roundoff(compare_coefficients):
    result = compare_coefficients(math.nextafter(10000.0, math.inf), 1.0, 1.0)
    assert result["re_l"] == math.nextafter(10000.0, math.inf)


def test_reference_boundary_in_feet(compare_coefficients):
    # Synthetic SI inputs converted consistently to feet; roundoff puts Re_L
    # just above 10000. Conversion inputs are explicit regression fixtures.
    feet = compare_coefficients(
        3.2808398950131235, 0.32808398950131235, 1.0763910416709722e-4
    )
    si = compare_coefficients(1.0, 0.1, 1.0e-5)
    assert feet == pytest.approx(si)


def test_blasius_example_units(compare_coefficients):
    # SI: U=1 m/s, L=0.1 m, nu=0.00001 m^2/s (synthetic inputs).
    # Re_L = (1 m/s)*(0.1 m)/(0.00001 m^2/s) = 10000, dimensionless.
    si = compare_coefficients(1.0, 0.1, 1.0e-5)
    # The same physical inputs in cm: 100 cm/s, 10 cm, 0.1 cm^2/s.
    # One metre is 100 centimetres; viscosity scales by its square.
    centimetres = compare_coefficients(100.0, 10.0, 0.1)
    assert si["re_l"] == pytest.approx(10000.0)
    # Hand calculation: sqrt(10000)=100; 0.664/100=0.00664;
    # the analytical plate mean is twice that, 0.01328.
    assert si["cf_at_l"] == pytest.approx(0.00664)
    assert si["plate_cd_integral"] == pytest.approx(0.01328)
    assert si["plate_cd_helper"] == pytest.approx(0.01328)
    assert centimetres == pytest.approx(si)


def test_saved_example_runs(capsys):
    runpy.run_path(str(EXAMPLE), run_name="__main__")
    output = capsys.readouterr().out
    assert "Formula comparison only; no CFD validation." in output
    assert "Re_L = 10000" in output
    assert "integral / Cf(L) = 2.0" in output
