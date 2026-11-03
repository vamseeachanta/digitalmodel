# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Surface PSS-78 checks from UNESCO Technical Paper 44 (1983), pp. 9, 13.

The standard conductivity is from TEOS-10, Notes on gsw_SP_from_C (2011),
p. 1. Decimal forward evaluation checks the inverse and CSV independently.
"""

import csv
import math
import re
from decimal import Decimal, localcontext
from pathlib import Path

import pytest

from digitalmodel.cathodic_protection.seawater_resistivity import (
    seawater_conductivity_s_m,
    seawater_resistivity_ohm_m,
)

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "docs/domains/cathodic_protection/data/seawater-resistivity.csv"
TEMPERATURES = (5, 10, 15, 20, 25, 30)  # Requested chart sampling, IPTS-68.

# UNESCO (1983), p. 13, surface-pressure rows, restricted to its valid domain.
# Salinity is printed to four decimals; conductivity ratio to seven decimals.
SURFACE_CHECKS = (
    (36.2864, 0, 0.6990725),
    (26.8609, 10, 0.6990725),
    (20.8085, 20, 0.6990725),
    (16.7100, 30, 0.6990725),
    (36.9676, 10, 0.9320967),
    (28.6080, 20, 0.9320967),
    (22.9533, 30, 0.9320967),
    (36.1195, 30, 1.3981451),
)


def forward_salinity(conductivity_s_m: float, temperature_c: float) -> Decimal:
    """Hand evaluation of UNESCO (1983), pp. 6-8, equations (1)-(3)."""
    with localcontext() as context:
        context.prec = 40  # Numerical checking precision, not a model constant.
        t = Decimal(str(temperature_c))
        # Independently transcribed published coefficients in ascending powers.
        a = list(map(Decimal, ("0.0080", "-0.1692", "25.3851", "14.0941",
                               "-7.0261", "2.7081")))
        b = list(map(Decimal, ("0.0005", "-0.0056", "-0.0066", "-0.0375",
                               "0.0636", "-0.0144")))
        c = list(map(Decimal, ("0.6766097", "0.0200564", "0.0001104259",
                               "-0.00000069698", "0.0000000010031")))
        # Horner evaluation avoids Decimal's undefined zero to power zero.
        rt = c[-1]
        for coefficient in reversed(c[:-1]):
            rt = rt * t + coefficient
        x = (Decimal(str(conductivity_s_m)) / Decimal("4.2914") / rt).sqrt()
        delta = (t - 15) / (1 + Decimal("0.0162") * (t - 15))
        coefficients = [ai + delta * bi for ai, bi in zip(a, b)]
        salinity = coefficients[-1]
        for coefficient in reversed(coefficients[:-1]):
            salinity = salinity * x + coefficient
        return +salinity


def test_standard_seawater_and_reciprocal_units():
    """UNESCO p. 9: R=1; TEOS-10 notes p. 1: 42.914 mS/cm=4.2914 S/m."""
    conductivity = seawater_conductivity_s_m(35, 15)
    # The printed r_t coefficients give 1.0000000019375 at t=15, not exact unity.
    assert conductivity == pytest.approx(4.2914, rel=0, abs=1e-8)
    # Hand conversion: rho [ohm.m] = 1 / sigma [S/m].
    assert seawater_resistivity_ohm_m(35, 15) == pytest.approx(
        1 / 4.2914, rel=0, abs=1e-9
    )


@pytest.mark.parametrize("salinity,temperature,ratio", SURFACE_CHECKS)
def test_published_surface_table_at_printed_precision(salinity, temperature, ratio):
    """Enclose each printed ratio using half the last printed salinity digit."""
    lower = seawater_conductivity_s_m(salinity - 0.00005, temperature) / 4.2914
    upper = seawater_conductivity_s_m(salinity + 0.00005, temperature) / 4.2914
    assert lower <= ratio + 0.00000005
    assert upper >= ratio - 0.00000005


@pytest.mark.parametrize(
    "salinity,temperature,rt,temperature_ratio",
    [(37.245628, 20, 1.0568875, 1.1164927),
     (27.995347, 5, 0.81705885, 0.77956585)],
)
def test_published_check_table_reduced_to_surface(
    salinity, temperature, rt, temperature_ratio
):
    """UNESCO p. 9: use tabulated R_t*r_t, removing the tabulated R_p factor.

    The original rows are at depth. Only their surface factors are checked;
    this API does not calculate conductivity at nonzero pressure. The absolute
    ratio allowance covers rounding of both printed factors (seven decimals
    or better) and the six-decimal salinity.
    """
    ratio = seawater_conductivity_s_m(salinity, temperature) / 4.2914
    assert ratio == pytest.approx(rt * temperature_ratio, rel=0, abs=2e-7)


@pytest.mark.parametrize("salinity", [2, 10, 25, 31, 35, 42])
@pytest.mark.parametrize("temperature", [-2, 0, 15, 25, 35])
def test_inverse_against_decimal_forward_equation(salinity, temperature):
    """UNESCO pp. 6-8; include all domain corners and hand-table salinities."""
    conductivity = seawater_conductivity_s_m(salinity, temperature)
    assert conductivity > 0
    assert float(forward_salinity(conductivity, temperature)) == pytest.approx(
        salinity, rel=0, abs=1e-11  # Numerical residual, not empirical accuracy.
    )
    assert seawater_resistivity_ohm_m(salinity, temperature) * conductivity == (
        pytest.approx(1, rel=0, abs=1e-15)
    )


@pytest.mark.parametrize("function", [seawater_conductivity_s_m,
                                      seawater_resistivity_ohm_m])
@pytest.mark.parametrize("bad", [1.999999, 42.000001, -1, math.nan,
                                math.inf, -math.inf])
def test_rejects_invalid_salinity(function, bad):
    """UNESCO p. 7: no extrapolation outside practical salinity 2 to 42."""
    with pytest.raises(ValueError, match="salinity_psu"):
        function(bad, 15)


@pytest.mark.parametrize("function", [seawater_conductivity_s_m,
                                      seawater_resistivity_ohm_m])
@pytest.mark.parametrize("bad", [-2.000001, 35.000001, math.nan,
                                math.inf, -math.inf])
def test_rejects_invalid_temperature(function, bad):
    """UNESCO pp. 7-8: no extrapolation outside -2 to 35 degrees IPTS-68."""
    with pytest.raises(ValueError, match="temperature_c"):
        function(35, bad)


def test_chart_csv_grid_and_every_value():
    """Requested sampling; every cell satisfies the cited forward equation."""
    with DATA.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        fields = [f"resistivity_{t}_c_ohm_m" for t in TEMPERATURES]
        assert reader.fieldnames == ["salinity_psu", *fields]
        rows = list(reader)
    assert [int(row["salinity_psu"]) for row in rows] == list(range(30, 41))
    for row in rows:
        for t, field in zip(TEMPERATURES, fields):
            rho = float(row[field])
            # Eight decimal places in rho propagate to less than 2e-6 in S
            # across this grid; this is a CSV rounding allowance only.
            assert float(forward_salinity(1 / rho, t)) == pytest.approx(
                float(row["salinity_psu"]), rel=0, abs=2e-6
            )
            # Independent SI identity: rho * sigma = 1. Propagate half the
            # last CSV digit rather than taking an expected value from the API.
            conductivity = seawater_conductivity_s_m(float(row["salinity_psu"]), t)
            assert abs(rho * conductivity - 1) <= 5e-9 * conductivity


def test_literature_hand_table_units_and_comparison():
    """literature.md, Design Data / Salinity: compare without assuming a match.

    Its temperature and salinity scale are unspecified. The valid overlapping
    numerical salinities are compared at an explicitly assumed IPTS-68
    temperature of 25 degrees C. The comparison is not validation data.
    """
    literature = (ROOT / "docs/domains/cathodic_protection/literature.md").read_text()
    entries = re.findall(
        r"Salinity (\d+(?:\.\d+)?)ppt\s*=\s*([\d.]+)\s*mS/cm"
        r"\s*=\s*([\d.]+)\s*Ohm.cm", literature
    )
    overlap = [(float(s), float(c), float(r)) for s, c, r in entries
               if 2 <= float(s) <= 42]
    assert [row[0] for row in overlap] == [10, 25, 31]
    for salinity, conductivity, rho_cm in overlap:
        # SI prefix hand conversion: rho [ohm.cm] = 1000 / C [mS/cm].
        assert rho_cm == pytest.approx(1000 / conductivity, rel=0, abs=0.02)
        rho = seawater_resistivity_ohm_m(salinity, 25)
        difference_pct = 100 * (rho / (rho_cm / 100) - 1)
        rounded_difference = round(difference_pct, 1)
        print(f"S={salinity:g}, assumed t68=25: "
              f"resistivity difference={rounded_difference or 0.0:+.1f}%")
