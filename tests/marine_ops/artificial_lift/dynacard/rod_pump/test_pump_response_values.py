# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic pump response checks from geometry, Hooke's law and NIST units.

Sources: OpenStax University Physics Volume 1 (2016), section 12.3;
NIST SP 811 (2008), appendix B. See rod-pump-hand-checks.md for links.
"""

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    analyse_rod_string,
    plunger_stroke,
    pump_displacement,
    volumetric_efficiency,
)


@pytest.mark.parametrize("overtravel,expected", [(0.0, 90.0), (5.0, 95.0)])
def test_plunger_stroke_subtracts_elastic_stretch(overtravel, expected):
    """Synthetic S=100 in, F=1000 lb, k=100 lb/in: stretch=10 in.

    Hooke's law (OpenStax section 12.3) gives Sp=90 in before an explicitly
    supplied overtravel; 5 in overtravel makes 95 in. Exact integer arithmetic
    needs no engineering tolerance. The dynamic overtravel model is not used.
    """
    assert plunger_stroke(100.0, 1000.0, 100.0, overtravel) == expected


def test_pump_displacement_from_volume_and_time_units():
    """NIST SP 811 appendix B: petroleum barrel=42 US gallons.

    From the US gallon and cubic-inch SI conversions, 1 gallon=231 in³
    (to the tabulated precision); 1 day=86400 s, 1 minute=60 s.
    Conversion=1440/(42*231)=0.148423005566 bbl/day per in³/min.
    Synthetic d=1 in, Sp=100 in, N=10/min: swept volume=250*pi in³/min,
    hence displacement=116.571155977 bbl/day. rel=0.0002 bounds rounding
    the conversion to four significant digits in the module; it is not a
    physical acceptance limit. This check detects use of diameter as area.
    """
    assert pump_displacement(1.0, 100.0, 10.0) == pytest.approx(
        116.571155977, rel=0.0002
    )


def test_single_string_to_pump_response():
    """Continue the single-string hand case in test_rod_string_values.

    Fo/k=(25*pi)/(250*pi)=0.1 in, so Sp=99.9 in from S=100 in.
    Displacement=116.571155977*(99.9/100)=116.454584821 bbl/day.
    Use the same 0.02% conversion-rounding bound as the preceding test.
    """
    string = analyse_rod_string(
        1.0,
        1000.0,
        100.0,
        1.0,
        0.0,
        0.0,
        tubing_pressure_psi=125.0,
        casing_pressure_psi=25.0,
        weight_lb_per_ft=1.0,
        modulus_psi=12_000_000,
    )
    stroke = plunger_stroke(100.0, string.fluid_load_lb, string.spring_rate_lb_per_in)
    assert stroke == pytest.approx(99.9, abs=1e-12)  # Binary arithmetic only.
    assert pump_displacement(1.0, stroke, 10.0) == pytest.approx(
        116.454584821, rel=0.0002
    )


@pytest.mark.parametrize(
    "rate,runtime,bo,expected",
    [
        (0.0, 24.0, 1.0, 0.0),
        (100.0, 24.0, 1.0, 1.0),
        (25.0, 12.0, 1.2, 0.6),
        (50.0, 24.0, 1.0, 0.5),
    ],
)
def test_volumetric_efficiency_with_explicit_runtime_and_volume_basis(
    rate, runtime, bo, expected
):
    """Volume balance: eta=(surface rate*Bo)/(PD*runtime/24).

    Synthetic PD=100 bbl/day: zero and equal production give 0 and 1.
    A 12 h run displaces 50 bbl; 25 surface bbl at Bo=1.2 is 30 reservoir
    bbl, giving 30/50=0.6. A full-day 50/100 balance gives 0.5.
    rel=1e-12 allows binary arithmetic only. See NIST appendix B for day/hour.
    This function currently does not enforce bounds outside these cases.
    """
    assert volumetric_efficiency(rate, 100.0, runtime, bo) == pytest.approx(
        expected, rel=1e-12, abs=1e-12
    )


@pytest.mark.parametrize(
    "displacement,runtime,bo",
    [
        (100.0, None, 1.0),
        (100.0, 24.0, None),
        (0.0, 24.0, 1.0),
        (-1.0, 24.0, 1.0),
        (100.0, 0.0, 1.0),
        (100.0, -1.0, 1.0),
    ],
)
def test_efficiency_undetermined_boundaries(displacement, runtime, bo):
    """Documented None boundaries; synthetic zero/negative denominators."""
    assert volumetric_efficiency(25.0, displacement, runtime, bo) is None
