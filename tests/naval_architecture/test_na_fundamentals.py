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
    """Load invented dimensions and density; no measured source data is used."""
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
    """Independent hand calculations for submerged rectangular prisms."""

    def test_rectangular_prism_coefficients_and_mass(self, rectangular_prism):
        """L=12, B=4, T=2 m give V=96 m³, Am=8 m², Awp=48 m².

        Hand calculation: Cb=96/(12*4*2)=1, Cm=8/(4*2)=1,
        Cp=96/(8*12)=1, Cwp=48/(12*4)=1; hence Cb=Cp*Cm.
        The invented fluid density is 997 kg/m³, deliberately supplied:
        mass=96*997=95712 kg=95.712 tonnes, using 1000 kg per tonne.
        """
        length = rectangular_prism["length_m"]
        beam = rectangular_prism["beam_m"]
        draft = rectangular_prism["draft_m"]
        density = rectangular_prism["density_kg_m3"]
        volume = length * beam * draft
        midship_area = beam * draft
        waterplane_area = length * beam
        assert (volume, midship_area, waterplane_area) == (96.0, 8.0, 48.0)

        cb = block_coefficient(volume, length, beam, draft)
        cm = midship_coefficient(midship_area, beam, draft)
        cp = prismatic_coefficient(cb, cm)
        cwp = waterplane_coefficient(waterplane_area, length, beam)
        assert (cb, cm, cp, cwp) == pytest.approx((1.0, 1.0, 1.0, 1.0))
        assert cp == pytest.approx(volume / (midship_area * length))
        assert cb == pytest.approx(cp * cm)
        mass_tonnes = displacement_from_cb(cb, length, beam, draft, rho=density)
        assert mass_tonnes == pytest.approx(95.712)
        assert mass_tonnes * 1000.0 == pytest.approx(volume * density)

    @pytest.mark.parametrize("scale", [0.5, 2.0], ids=["half", "double"])
    def test_uniform_scaling_preserves_coefficients(self, rectangular_prism, scale):
        """Scale the 12*4*2 m prism uniformly, with the same 997 kg/m³ fluid.

        Hand calculations: s=0.5 gives 6*2*1 m, V=12 m³, Am=2 m²,
        Awp=12 m²; s=2 gives 24*8*4 m, V=768 m³, Am=32 m²,
        Awp=192 m². V/V_base=s³, Am/Am_base=Awp/Awp_base=s².
        Each coefficient remains unity, since numerator and denominator
        scale by the same power. Mass in tonnes is V*997/1000, where
        1000 kg per tonne converts the supplied SI density to tonnes.
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
        assert volume == pytest.approx(96.0 * scale**3)
        assert midship_area == pytest.approx(8.0 * scale**2)
        assert waterplane_area == pytest.approx(48.0 * scale**2)

        cb = block_coefficient(volume, length, beam, draft)
        cm = midship_coefficient(midship_area, beam, draft)
        cp = prismatic_coefficient(cb, cm)
        cwp = waterplane_coefficient(waterplane_area, length, beam)
        base_coefficients = (
            block_coefficient(96.0, base_length, base_beam, base_draft),
            midship_coefficient(8.0, base_beam, base_draft),
            prismatic_coefficient(cb=1.0, cm=1.0),
            waterplane_coefficient(48.0, base_length, base_beam),
        )
        assert (cb, cm, cp, cwp) == pytest.approx((1.0, 1.0, 1.0, 1.0))
        assert (cb, cm, cp, cwp) == pytest.approx(base_coefficients)
        assert cp == pytest.approx(volume / (midship_area * length))
        assert cb == pytest.approx(cp * cm)
        mass_tonnes = displacement_from_cb(cb, length, beam, draft, rho=density)
        assert mass_tonnes == pytest.approx(96.0 * scale**3 * density / 1000.0)
        assert mass_tonnes * 1000.0 == pytest.approx(volume * density)
