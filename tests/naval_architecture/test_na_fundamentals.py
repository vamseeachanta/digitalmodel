# ABOUTME: Skeleton tests for naval_architecture fundamentals module
# ABOUTME: Covers unit conversions, buoyancy, water density interpolation

import json
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.fundamentals import (
    G,
    GAMMA_SW,
    GAMMA_FW,
    LT_TO_LB,
    mass_to_weight,
    displaced_volume_to_weight_lt,
    interpolate_water_density,
)
from digitalmodel.naval_architecture.hull_form import (
    block_coefficient,
    displacement_from_cb,
    midship_coefficient,
    prismatic_coefficient,
    waterplane_coefficient,
)


@pytest.fixture
def rectangular_prism():
    """Load the submerged prism from a cited idealized teaching example."""
    path = Path(__file__).with_name("synthetic_rectangular_prism.json")
    return json.loads(path.read_text(encoding="utf-8"))


class TestConstants:
    def test_gravity(self):
        assert G == pytest.approx(32.17, rel=1e-4)

    def test_saltwater_gamma(self):
        assert GAMMA_SW == pytest.approx(64.0, rel=1e-4)

    def test_freshwater_gamma(self):
        assert GAMMA_FW == pytest.approx(62.4, rel=1e-4)

    def test_lt_to_lb(self):
        assert LT_TO_LB == pytest.approx(2240.0, rel=1e-6)


class TestMassToWeight:
    def test_one_slug(self):
        w = mass_to_weight(1.0)
        assert w == pytest.approx(G, rel=1e-6)

    def test_zero_mass(self):
        assert mass_to_weight(0.0) == pytest.approx(0.0, abs=1e-9)

    def test_proportional(self):
        w1 = mass_to_weight(2.0)
        w2 = mass_to_weight(4.0)
        assert w2 == pytest.approx(2 * w1, rel=1e-6)


class TestDisplacedVolumeToWeight:
    def test_saltwater(self):
        v = 2240.0 / 64.0  # volume that displaces exactly 1 LT in saltwater
        w_lt = displaced_volume_to_weight_lt(v, water="saltwater")
        assert w_lt == pytest.approx(1.0, rel=1e-4)

    def test_freshwater_heavier_volume_needed(self):
        # freshwater less dense, so same volume -> less buoyancy
        v = 1000.0
        w_sw = displaced_volume_to_weight_lt(v, water="saltwater")
        w_fw = displaced_volume_to_weight_lt(v, water="freshwater")
        assert w_sw > w_fw

    def test_zero_volume(self):
        assert displaced_volume_to_weight_lt(0.0) == pytest.approx(0.0, abs=1e-9)


class TestInterpolateWaterDensity:
    def test_at_exact_table_point_60f(self):
        rho = interpolate_water_density(60.0, water="freshwater")
        assert rho == pytest.approx(1.9383, rel=1e-4)

    def test_interpolation_between_points(self):
        rho = interpolate_water_density(62.5, water="freshwater")
        # Between 62 (1.9379) and 63 (1.9377)
        assert 1.9377 <= rho <= 1.9379

    def test_below_range_returns_first_value(self):
        rho = interpolate_water_density(40.0, water="freshwater")
        assert rho == pytest.approx(1.9400, rel=1e-4)

    def test_above_range_returns_last_value(self):
        rho = interpolate_water_density(100.0, water="freshwater")
        assert rho == pytest.approx(1.9333, rel=1e-4)

    def test_saltwater_raises(self):
        with pytest.raises(NotImplementedError):
            interpolate_water_density(60.0, water="saltwater")


class TestSyntheticPrismGeometry:
    """Independent hand calculations for submerged rectangular prisms.

    Inputs: CK-12 Physics FlexBook 2.0, Archimedes' Principle, Example
    10.2.2, as reproduced in LibreTexts section 13.2 (2024-09-04 revision).
    Coefficients: EN400 Principles of Ship Performance, July 2020 edition,
    Chapter 2, Hull Form and Geometry. Similarity follows from the products
    of three lengths for volume and two lengths for sectional areas.
    Mass unit: BIPM SI Brochure, 9th edition, version 4.01 (2026),
    Chapter 4, Non-SI units, Table 8: one tonne is exactly 1000 kg.
    Source links and their input coverage are recorded in the fixture.
    """

    def test_rectangular_prism_coefficients_and_mass(self, rectangular_prism):
        """L=1, B=1, T=0.600 m give V=0.600 m³, Am=0.600 m², Awp=1 m².

        The cited cube has side 1.00 m and mass 600 kg in water with
        density 1000 kg/m³; T=600/(1000*1*1)=0.600 m is its draft.
        Hand calculation: Cb=0.6/(1*1*0.6)=1, Cm=0.6/(1*0.6)=1,
        Cp=0.6/(0.6*1)=1, Cwp=1/(1*1)=1; hence Cb=Cp*Cm.
        Mass=0.6*1000=600 kg=0.600 tonnes, using Table 8 cited above.
        """
        length = rectangular_prism["length_m"]
        beam = rectangular_prism["beam_m"]
        draft = rectangular_prism["draft_m"]
        density = rectangular_prism["density_kg_m3"]
        volume = length * beam * draft
        midship_area = beam * draft
        waterplane_area = length * beam
        assert (volume, midship_area, waterplane_area) == pytest.approx((0.6, 0.6, 1.0))

        cb = block_coefficient(volume, length, beam, draft)
        cm = midship_coefficient(midship_area, beam, draft)
        cp = prismatic_coefficient(cb, cm)
        cwp = waterplane_coefficient(waterplane_area, length, beam)
        assert (cb, cm, cp, cwp) == pytest.approx((1.0, 1.0, 1.0, 1.0))
        assert cp == pytest.approx(volume / (midship_area * length))
        assert cb == pytest.approx(cp * cm)
        mass_tonnes = displacement_from_cb(cb, length, beam, draft, rho=density)
        assert mass_tonnes == pytest.approx(0.6)
        assert mass_tonnes * 1000.0 == pytest.approx(volume * density)

    @pytest.mark.parametrize(
        "scale, expected",
        [(0.5, (0.075, 0.15, 0.25, 0.075)), (2.0, (4.8, 2.4, 4.0, 4.8))],
        ids=["half", "double"],
    )
    def test_uniform_scaling_preserves_coefficients(self, rectangular_prism, scale, expected):
        """Scale the cited 1*1*0.600 m prism in the same 1000 kg/m³ fluid.

        Hand calculations: s=0.5 gives 0.5*0.5*0.3 m, V=0.075 m³,
        Am=0.15 m², Awp=0.25 m², mass=0.075*1000/1000=0.075 tonnes;
        s=2 gives 2*2*1.2 m, V=4.8 m³, Am=2.4 m², Awp=4 m²,
        mass=4.8*1000/1000=4.8 tonnes. These are synthetic similarity
        transformations, not additional published examples.
        V/V_base=s³, Am/Am_base=Awp/Awp_base=s².
        Each coefficient remains unity, since numerator and denominator
        scale by the same power. The mass conversion uses Table 8 above.
        """
        base_length = rectangular_prism["length_m"]
        base_beam = rectangular_prism["beam_m"]
        base_draft = rectangular_prism["draft_m"]
        density = rectangular_prism["density_kg_m3"]
        length, beam, draft = (
            dimension * scale for dimension in (base_length, base_beam, base_draft)
        )
        volume = length * beam * draft
        midship_area = beam * draft
        waterplane_area = length * beam
        assert (volume, midship_area, waterplane_area) == pytest.approx(expected[:3])
        assert volume == pytest.approx(0.6 * scale**3)
        assert midship_area == pytest.approx(0.6 * scale**2)
        assert waterplane_area == pytest.approx(1.0 * scale**2)

        cb = block_coefficient(volume, length, beam, draft)
        cm = midship_coefficient(midship_area, beam, draft)
        cp = prismatic_coefficient(cb, cm)
        cwp = waterplane_coefficient(waterplane_area, length, beam)
        base_coefficients = (
            block_coefficient(0.6, base_length, base_beam, base_draft),
            midship_coefficient(0.6, base_beam, base_draft),
            prismatic_coefficient(cb=1.0, cm=1.0),
            waterplane_coefficient(1.0, base_length, base_beam),
        )
        assert (cb, cm, cp, cwp) == pytest.approx((1.0, 1.0, 1.0, 1.0))
        assert (cb, cm, cp, cwp) == pytest.approx(base_coefficients)
        assert cp == pytest.approx(volume / (midship_area * length))
        assert cb == pytest.approx(cp * cm)
        mass_tonnes = displacement_from_cb(cb, length, beam, draft, rho=density)
        assert mass_tonnes == pytest.approx(expected[3])
        assert mass_tonnes * 1000.0 == pytest.approx(volume * density)
