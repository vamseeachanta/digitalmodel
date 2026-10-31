# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent integrals and eigenfunction for the synthetic report basis.

All dimensional inputs are synthetic. The rectangle integrals and column
eigenfunction below supply the expected values, independently of production code.
"""

import math
import sys

import pytest

from digitalmodel.structural.structural_analysis import (
    ColumnBucklingAnalyzer,
    MaterialProperties,
    PlateBucklingAnalyzer,
    PlateGeometry,
    StressCalculator,
)

# A small number of correctly rounded products/divisions and pi evaluation is
# allowed 32 binary64 epsilons; this is roundoff, not an engineering tolerance.
ROUNDING = 32 * sys.float_info.epsilon


@pytest.fixture
def material():
    """Synthetic N/mm² properties; only E enters the worked column."""
    return MaterialProperties(250, 400, 200000, 0.3, 7850, "Synthetic")


@pytest.mark.parametrize("sign, expected_mpa", [(1, 16), (-1, 4)])
def test_rectangle_integrals_and_signed_stress(material, sign, expected_mpa):
    """Integral z² dA = b h³/12; integral y² dA = h b³/12."""
    width, height = 0.2, 0.4  # Synthetic dimensions, m.
    area = width * height
    i_y = width * ((height / 2) ** 3 - (-height / 2) ** 3) / 3
    i_z = height * ((width / 2) ** 3 - (-width / 2) ** 3) / 3
    assert area == pytest.approx(0.08, rel=ROUNDING, abs=0)
    assert i_y == pytest.approx(width * height**3 / 12, rel=ROUNDING, abs=0)
    assert i_z == pytest.approx(height * width**3 / 12, rel=ROUNDING, abs=0)
    actual = StressCalculator(material).beam_stress(
        800000, 16000, 8000, area, i_y, i_z, sign * 0.1, sign * 0.2,
    )
    # N/A = 10 MPa; My*z/Iy = 3 MPa; Mz*y/Iz = 3 MPa.
    assert actual == pytest.approx(expected_mpa, rel=ROUNDING, abs=0)


def test_beam_components_and_independent_unit_conversion(material):
    """Recompute N, N mm, mm², mm⁴ directly; N/mm² equals MPa."""
    calculator = StressCalculator(material)
    area_mm2 = 200 * 400  # Synthetic rectangle expressed independently in mm.
    iy_mm4, iz_mm4 = 200 * 400**3 / 12, 400 * 200**3 / 12
    # SI prefix definitions: one metre = 1000 mm, one MPa = 10**6 Pa.
    area_m2 = area_mm2 / 1000**2
    iy_m4, iz_m4 = iy_mm4 / 1000**4, iz_mm4 / 1000**4
    reference = (800000 / area_mm2,
                 (16000 * 1000) * 200 / iy_mm4,
                 (8000 * 1000) * 100 / iz_mm4)
    for loads, expected, hand in zip(
        [(800000, 0, 0), (0, 16000, 0), (0, 0, 8000)],
        [10, 3, 3], reference,
    ):
        actual = calculator.beam_stress(
            *loads, area_m2, iy_m4, iz_m4, 0.1, 0.2,
        )
        assert hand == pytest.approx(expected, rel=ROUNDING, abs=0)
        assert actual == pytest.approx(hand, rel=ROUNDING, abs=0)


@pytest.mark.parametrize("length_mm, divisor", [(2000, 1), (4000, 4)])
def test_pin_ended_column_eigenfunction(material, length_mm, divisor):
    """EI v'''' + P v''=0 and v=sin(pi*x/L) give P=pi² EI/L²."""
    # Synthetic EI/L² = 200000*1000000/2000² = 50000 N.
    expected = 50000 * math.pi**2 / divisor
    actual = ColumnBucklingAnalyzer(material).euler_buckling_load(1000000, length_mm)
    assert actual == pytest.approx(expected, rel=ROUNDING, abs=0)
    # Separate SI calculation: E in N/m², I in m⁴, length in m.
    si_value = math.pi**2 * (200000 * 10**6) * (1000000 / 1000**4)
    si_value /= (length_mm / 1000) ** 2
    assert actual == pytest.approx(si_value, rel=ROUNDING, abs=0)


def test_plate_rejects_unimplemented_transverse_stress(material):
    """An unused load component cannot silently receive a design disposition."""
    plate = PlateGeometry(length=1000, width=1000, thickness=10)
    with pytest.raises(ValueError, match="sigma_y"):
        PlateBucklingAnalyzer(material).check_plate_buckling(plate, 0, sigma_y=1)
