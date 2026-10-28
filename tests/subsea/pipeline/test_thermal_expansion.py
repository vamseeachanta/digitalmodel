# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Free-growth checks based on independently integrated uniform strain.

All dimensions below are synthetic arithmetic inputs, not material data.
For L=1000 m, alpha=12e-6 /K and dT=50 K, uniform strain is
12e-6 * 50 = 0.0006, so its area over 1000 m is 0.6 m. Cooling
reverses the sign. A zero interval, strain or coefficient gives zero area.
The 1e-12 relative tolerance checks arithmetic, not engineering accuracy.
"""

import os
from fractions import Fraction
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest

from digitalmodel.subsea.pipeline.thermal_expansion import free_thermal_expansion


@pytest.mark.parametrize(
    "length, coefficient, temperature_change, expected",
    [
        pytest.param(1000.0, 12e-6, 50.0, 0.6, id="heating"),
        pytest.param(1000.0, 12e-6, -50.0, -0.6, id="cooling"),
        pytest.param(1000.0, 12e-6, 0.0, 0.0, id="zero-change"),
        # Even with extreme finite inputs, zero strain has zero integrated area.
        pytest.param(1e200, 1e200, 0.0, 0.0, id="zero-change-extreme"),
        pytest.param(0.0, 12e-6, 50.0, 0.0, id="zero-length"),
        pytest.param(1000.0, 0.0, 50.0, 0.0, id="zero-coefficient"),
        # Integer scalars: unit strain integrated over a unit interval is one.
        pytest.param(1, 1, 1, 1.0, id="integer-scalars"),
    ],
)
def test_free_expansion_matches_integrated_strain(
    length, coefficient, temperature_change, expected
):
    """Match the hand-calculated area under the uniform strain field."""
    growth = free_thermal_expansion(length, coefficient, temperature_change)
    assert growth == pytest.approx(expected, rel=1e-12, abs=0.0)


@pytest.mark.parametrize(
    "length, coefficient, temperature_change, expected",
    [
        # Exponents add to -610; the first pair alone would underflow.
        (2.0**-600, 2.0**-600, 2.0**590, 2.0**-610),
        (2.0**-600, 2.0**-600, -(2.0**590), -(2.0**-610)),
        # Exponents add to 610; the first pair alone would overflow.
        (2.0**600, 2.0**600, 2.0**-590, 2.0**610),
        (2.0**600, 2.0**600, -(2.0**-590), -(2.0**610)),
        # -500 - 500 - 74 = -1074, the smallest nonzero binary64 value.
        (2.0**-500, 2.0**-500, 2.0**-74, 2.0**-1074),
    ],
)
def test_free_expansion_preserves_representable_extreme_growth(
    length, coefficient, temperature_change, expected
):
    """Preserve finite nonzero growth using an independent rational oracle."""
    exact = Fraction(length) * Fraction(coefficient) * Fraction(temperature_change)
    assert float(exact) == expected
    growth = free_thermal_expansion(length, coefficient, temperature_change)
    assert growth == pytest.approx(expected, rel=1e-12, abs=0.0)


@pytest.mark.parametrize("temperature_change", [2.0**-100, -(2.0**-100)])
def test_free_expansion_rejects_underflowed_growth(temperature_change):
    """A nonzero product at magnitude 2**-1300 cannot represent growth."""
    with pytest.raises(ValueError, match="growth"):
        free_thermal_expansion(2.0**-600, 2.0**-600, temperature_change)


@pytest.mark.parametrize(
    "parameter", ["length", "thermal_expansion_coefficient", "temperature_change"]
)
@pytest.mark.parametrize(
    "invalid, error",
    [
        pytest.param(float("nan"), ValueError, id="nan"),
        pytest.param(float("inf"), ValueError, id="positive-infinity"),
        pytest.param(float("-inf"), ValueError, id="negative-infinity"),
        pytest.param(True, TypeError, id="true"),
        pytest.param(False, TypeError, id="false"),
        pytest.param("1", TypeError, id="string"),
        pytest.param(None, TypeError, id="none"),
        pytest.param([1], TypeError, id="sequence"),
        pytest.param(1 + 1j, TypeError, id="complex"),
    ],
)
def test_free_expansion_rejects_invalid_inputs(parameter, invalid, error):
    """Reject nonfinite values and values outside the real-scalar contract."""
    inputs = dict(
        length=1000.0, thermal_expansion_coefficient=12e-6, temperature_change=50.0
    )
    inputs[parameter] = invalid
    with pytest.raises(error, match=parameter):
        free_thermal_expansion(**inputs)


@pytest.mark.parametrize("parameter", ["length", "thermal_expansion_coefficient"])
def test_free_expansion_rejects_negative_length_or_coefficient(parameter):
    """Negative unity lies below the explicitly nonnegative input boundary."""
    inputs = dict(
        length=1000.0, thermal_expansion_coefficient=12e-6, temperature_change=50.0
    )
    inputs[parameter] = -1.0
    with pytest.raises(ValueError, match=parameter):
        free_thermal_expansion(**inputs)


@pytest.mark.parametrize("parameter", ["length", "thermal_expansion_coefficient"])
def test_free_expansion_rejects_negative_values_before_conversion(parameter):
    """A negative rational remains outside the domain when float rounds to zero."""
    inputs = dict(
        length=1.0, thermal_expansion_coefficient=1.0, temperature_change=1.0
    )
    inputs[parameter] = -Fraction(1, 10**400)
    with pytest.raises(ValueError, match=f"{parameter} must be nonnegative"):
        free_thermal_expansion(**inputs)


@pytest.mark.parametrize(
    "parameter, value",
    [
        ("length", Fraction(1, 10**400)),
        ("thermal_expansion_coefficient", Fraction(1, 10**400)),
        ("temperature_change", Fraction(1, 10**400)),
        ("temperature_change", -Fraction(1, 10**400)),
    ],
)
def test_free_expansion_rejects_nonzero_input_rounded_to_zero(parameter, value):
    """Reject conversion loss rather than treating nonzero input as zero strain."""
    inputs = dict(
        length=1.0, thermal_expansion_coefficient=1.0, temperature_change=1.0
    )
    inputs[parameter] = value
    with pytest.raises(ValueError, match=parameter):
        free_thermal_expansion(**inputs)


def test_free_expansion_requires_explicit_coefficient():
    """A material coefficient must be supplied by the caller."""
    with pytest.raises(TypeError):
        free_thermal_expansion(length=1000.0, temperature_change=50.0)


def test_free_expansion_rejects_unrepresentable_growth():
    """The hand product 1e200 * 1e200 * 1 exceeds binary64's finite range."""
    with pytest.raises(ValueError, match="growth"):
        free_thermal_expansion(1e200, 1e200, 1.0)


def test_import_has_no_solver_or_output_side_effects(tmp_path):
    """A fresh interpreter must import quietly without starting other work."""
    source = Path(__file__).resolve().parents[3] / "src"
    script = textwrap.dedent(
        """\
        import sys

        def guard(event, args):
            if event in {"subprocess.Popen", "os.system", "os.mkdir", "socket.connect"}:
                raise AssertionError("Import attempted external work")
            if event == "open":
                mode = args[1]
                if isinstance(mode, str) and any(flag in mode for flag in "wax+"):
                    raise AssertionError("Import attempted file output")

        sys.addaudithook(guard)
        import digitalmodel.subsea.pipeline.thermal_expansion

        forbidden = ("digitalmodel.engine", "digitalmodel.solvers", "assetutilities")
        assert not any(name.startswith(forbidden) for name in sys.modules)
        """
    )
    result = subprocess.run(
        [sys.executable, "-B", "-c", script],
        cwd=tmp_path,
        env={**os.environ, "PYTHONPATH": str(source), "PYTHONDONTWRITEBYTECODE": "1"},
        capture_output=True,
        text=True,
        check=True,
    )
    assert result.stdout == ""
    assert result.stderr == ""
    assert list(tmp_path.iterdir()) == []
