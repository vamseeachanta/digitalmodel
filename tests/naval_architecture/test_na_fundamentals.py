# ABOUTME: Skeleton tests for naval_architecture fundamentals module
# ABOUTME: Covers unit conversions, buoyancy, water density interpolation

import re
from math import ulp
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
from digitalmodel.naval_architecture.hydrostatics import (
    vertical_cg_after_weight_change,
)


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


class TestSyntheticWeightMomentBalance:
    """Independent arithmetic in docs/examples/hydrostatic-weight-moment-balance.md.

    Synthetic starting components: 800 LT at 10 ft and 200 LT at 30 ft.
    Their total is 1000 LT, moment 14000 LT ft, and KG 14 ft. All new physical
    constants below come from that instructional example, not source data.
    Four ulps allow rounding in the short multiply/add/divide sequences while
    checking agreement at floating-point precision; relative tolerance is zero.
    """

    @pytest.mark.parametrize(
        "weight_lt, expected_weight_lt, expected_moment_lt_ft",
        [
            pytest.param(100.0, 1100.0, 17000.0, id="add"),
            pytest.param(-200.0, 800.0, 8000.0, id="remove"),
        ],
    )
    def test_weight_change_matches_component_moments(
        self, weight_lt, expected_weight_lt, expected_moment_lt_ft
    ):
        # Add: 800*10 + (200+100)*30 = 17000 LT ft over 1100 LT.
        # Remove: 800*10 = 8000 LT ft over 800 LT.
        new_weight_lt = 1000.0 + weight_lt
        expected_kg_ft = expected_moment_lt_ft / expected_weight_lt
        new_kg_ft = vertical_cg_after_weight_change(1000.0, 14.0, weight_lt, 30.0)

        assert new_weight_lt == expected_weight_lt
        assert new_kg_ft == pytest.approx(
            expected_kg_ft, rel=0.0, abs=4 * ulp(expected_kg_ft)
        )
        assert new_weight_lt * new_kg_ft == pytest.approx(
            expected_moment_lt_ft, rel=0.0, abs=4 * ulp(expected_moment_lt_ft)
        )

    def test_vertical_shift_matches_component_moments(self):
        # Shift the existing 200 LT from 30 ft to 5 ft. Treat the move as
        # removal then addition: 800*10 + 200*5 = 9000 LT ft over 1000 LT.
        remaining_weight_lt = 1000.0 - 200.0
        remaining_kg_ft = vertical_cg_after_weight_change(
            1000.0, 14.0, -200.0, 30.0
        )
        shifted_weight_lt = remaining_weight_lt + 200.0
        shifted_kg_ft = vertical_cg_after_weight_change(
            remaining_weight_lt, remaining_kg_ft, 200.0, 5.0
        )

        assert shifted_weight_lt == 1000.0
        assert shifted_kg_ft == pytest.approx(9.0, rel=0.0, abs=4 * ulp(9.0))
        assert shifted_weight_lt * shifted_kg_ft == pytest.approx(
            9000.0, rel=0.0, abs=4 * ulp(9000.0)
        )

    @pytest.mark.parametrize(
        "weight_lt",
        [
            pytest.param(100.0, id="add-then-remove"),
            pytest.param(-200.0, id="remove-then-add"),
        ],
    )
    def test_inverse_weight_change(self, weight_lt):
        # Undo at the same 30 ft height: the moment returns to
        # 800*10 + 200*30 = 14000 LT ft, and the total returns to 1000 LT.
        changed_weight_lt = 1000.0 + weight_lt
        changed_kg_ft = vertical_cg_after_weight_change(
            1000.0, 14.0, weight_lt, 30.0
        )
        recovered_weight_lt = changed_weight_lt - weight_lt
        recovered_kg_ft = vertical_cg_after_weight_change(
            changed_weight_lt, changed_kg_ft, -weight_lt, 30.0
        )

        assert recovered_weight_lt == 1000.0
        assert recovered_kg_ft == pytest.approx(14.0, rel=0.0, abs=4 * ulp(14.0))

    def test_documented_snippet(self):
        example = (
            Path(__file__).resolve().parents[2]
            / "docs/examples/hydrostatic-weight-moment-balance.md"
        )
        snippets = re.findall(
            r"```python\n(.*?)```", example.read_text(encoding="utf-8"), re.DOTALL
        )
        assert len(snippets) == 1
        namespace = {}
        exec(compile(snippets[0], str(example), "exec"), namespace)

        # Independent results from the component moment sums above.
        expected = {
            "added": (1100.0, 17000.0 / 1100.0),
            "removed": (800.0, 10.0),
            "shifted": (1000.0, 9.0),
            "recovered_after_add": (1000.0, 14.0),
            "recovered_after_remove": (1000.0, 14.0),
        }
        for name, (weight_lt, kg_ft) in expected.items():
            assert namespace[f"{name}_weight_lt"] == weight_lt
            assert namespace[f"{name}_kg_ft"] == pytest.approx(
                kg_ft, rel=0.0, abs=4 * ulp(kg_ft)
            )
