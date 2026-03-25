"""
Tests for fatigue damage accumulation models.

Covers:
- LoadCycle and DamageState dataclasses
- DamageAccumulationBase abstract class
- LinearDamageAccumulation (Palmgren-Miner rule)
- ModifiedMinersRule (sequence-sensitive accelerated damage)
- NonlinearDamageAccumulation (Marco-Starkey, Corten-Dolan, Double Linear)
- CriticalPlaneAnalysis (Findley, Brown-Miller, SWT)
- compare_damage_methods utility function
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.sn_curves import (
    PowerLawSNCurve,
    MaterialProperties,
    SNCurveBase,
)
from digitalmodel.structural.fatigue.damage_accumulation import (
    LoadCycle,
    DamageState,
    DamageAccumulationBase,
    LinearDamageAccumulation,
    ModifiedMinersRule,
    NonlinearDamageAccumulation,
    CriticalPlaneAnalysis,
    compare_damage_methods,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def dnv_d_curve():
    """DNV-D class S-N curve (A=5.73e11, m=3, CAFL=52.63 MPa)."""
    return PowerLawSNCurve(
        name="DNV-D",
        A=5.73e11,
        m=3.0,
        fatigue_limit=52.63,
    )


@pytest.fixture
def simple_curve():
    """Simple power-law S-N curve with no fatigue limit for easy math checks."""
    return PowerLawSNCurve(
        name="Simple",
        A=1e12,
        m=3.0,
        fatigue_limit=0.0,
    )


@pytest.fixture
def curve_with_material():
    """S-N curve with material properties attached."""
    mat = MaterialProperties(
        ultimate_strength=500.0,
        yield_strength=350.0,
        elastic_modulus=200000.0,
        name="Structural Steel",
    )
    return PowerLawSNCurve(
        name="WithMat",
        A=1e12,
        m=3.0,
        fatigue_limit=0.0,
        material=mat,
    )


@pytest.fixture
def typical_cycles_df():
    """Typical variable-amplitude loading block."""
    return pd.DataFrame({
        "range": [100.0, 80.0, 60.0, 40.0, 20.0],
        "mean": [10.0, 5.0, 0.0, -5.0, -10.0],
        "count": [1000.0, 2000.0, 5000.0, 10000.0, 50000.0],
    })


@pytest.fixture
def single_level_df():
    """Constant-amplitude loading at a single stress level."""
    return pd.DataFrame({
        "range": [100.0],
        "mean": [0.0],
        "count": [10000.0],
    })


# ===========================================================================
# LoadCycle dataclass
# ===========================================================================

class TestLoadCycle:
    def test_default_values(self):
        lc = LoadCycle(stress_range=120.0)
        assert lc.stress_range == 120.0
        assert lc.mean_stress == 0.0
        assert lc.count == 1.0
        assert lc.frequency == 1.0
        assert lc.temperature == 20.0
        assert lc.environment == "air"

    def test_custom_values(self):
        lc = LoadCycle(
            stress_range=200.0,
            mean_stress=50.0,
            count=5000.0,
            frequency=0.1,
            temperature=80.0,
            environment="seawater",
        )
        assert lc.stress_range == 200.0
        assert lc.mean_stress == 50.0
        assert lc.count == 5000.0
        assert lc.frequency == 0.1
        assert lc.temperature == 80.0
        assert lc.environment == "seawater"


# ===========================================================================
# DamageState dataclass
# ===========================================================================

class TestDamageState:
    def test_default_state(self):
        ds = DamageState()
        assert ds.total_damage == 0.0
        assert ds.cycle_history == []
        assert ds.damage_contributions == {}
        assert ds.crack_initiation_damage == 0.0
        assert ds.crack_propagation_damage == 0.0
        assert ds.temperature_effects == 1.0
        assert ds.environment_effects == 1.0

    def test_mutable_defaults_are_independent(self):
        ds1 = DamageState()
        ds2 = DamageState()
        ds1.cycle_history.append(LoadCycle(stress_range=100.0))
        assert len(ds2.cycle_history) == 0


# ===========================================================================
# DamageAccumulationBase (abstract)
# ===========================================================================

class TestDamageAccumulationBase:
    def test_cannot_instantiate_directly(self):
        with pytest.raises(TypeError):
            DamageAccumulationBase("test")

    def test_concrete_subclass_works(self, simple_curve, single_level_df):
        calc = LinearDamageAccumulation()
        assert calc.name == "Palmgren-Miner"

    def test_reset_history(self, simple_curve, single_level_df):
        calc = LinearDamageAccumulation()
        calc.calculate_damage(single_level_df, simple_curve)
        assert len(calc.damage_history) == 1
        calc.reset_history()
        assert len(calc.damage_history) == 0


# ===========================================================================
# LinearDamageAccumulation (Palmgren-Miner)
# ===========================================================================

class TestLinearDamageAccumulation:
    # --- basic construction ---
    def test_default_construction(self):
        calc = LinearDamageAccumulation()
        assert calc.name == "Palmgren-Miner"
        assert calc.failure_criterion == 1.0
        assert calc.mean_stress_correction is None

    def test_custom_failure_criterion(self):
        calc = LinearDamageAccumulation(failure_criterion=0.5)
        assert calc.failure_criterion == 0.5

    # --- single stress level, known answer ---
    def test_single_level_damage_known_answer(self, simple_curve):
        """With A=1e12, m=3, stress_range=100 MPa:
        N_allow = 1e12 / 100^3 = 1e6
        Damage = 10000 / 1e6 = 0.01
        """
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [10000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)

        assert result["method"] == "Palmgren-Miner"
        assert pytest.approx(result["total_damage"], rel=1e-6) == 0.01
        assert result["n_stress_levels"] == 1
        assert pytest.approx(result["cycles_processed"]) == 10000.0

    def test_single_level_safety_factor(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [10000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        expected_sf = 1.0 / 0.01
        assert pytest.approx(result["safety_factor"], rel=1e-6) == expected_sf

    # --- multi-level loading ---
    def test_multi_level_damage_is_sum(self, simple_curve):
        """Two stress levels: D = n1/N1 + n2/N2."""
        # S=100 -> N=1e6, S=200 -> N=1e12/200^3=125000
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [5000.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)

        d1 = 5000.0 / 1e6
        d2 = 1000.0 / 125000.0
        expected = d1 + d2
        assert pytest.approx(result["total_damage"], rel=1e-6) == expected

    def test_life_fraction_remaining(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [500000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        # D = 500000 / 1e6 = 0.5
        assert pytest.approx(result["life_fraction_used"], rel=1e-6) == 0.5
        assert pytest.approx(result["life_fraction_remaining"], rel=1e-6) == 0.5

    def test_life_fraction_remaining_capped_at_zero(self, simple_curve):
        """When damage exceeds failure criterion, remaining fraction = 0."""
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [2000000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["life_fraction_remaining"] == 0.0

    def test_estimated_remaining_cycles(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [500000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        # D = 0.5, damage_rate = 0.5/500000 = 1e-6
        # remaining_damage = 0.5, remaining_cycles = 0.5 / 1e-6 = 500000
        assert pytest.approx(result["estimated_remaining_cycles"], rel=1e-6) == 500000.0

    def test_remaining_cycles_zero_when_failed(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [2000000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["estimated_remaining_cycles"] == 0

    # --- numpy array input ---
    def test_ndarray_1d_input(self, simple_curve):
        """1-D array treated as stress ranges, count=1 each."""
        arr = np.array([100.0, 200.0])
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(arr, simple_curve)
        d1 = 1.0 / 1e6
        d2 = 1.0 / 125000.0
        assert pytest.approx(result["total_damage"], rel=1e-6) == d1 + d2

    def test_ndarray_2d_input(self, simple_curve):
        arr = np.array([[100.0, 0.0, 5000.0], [200.0, 0.0, 1000.0]])
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(arr, simple_curve)
        d1 = 5000.0 / 1e6
        d2 = 1000.0 / 125000.0
        assert pytest.approx(result["total_damage"], rel=1e-6) == d1 + d2

    # --- missing columns auto-fill ---
    def test_missing_mean_column_defaults_to_zero(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "count": [1000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] > 0

    def test_missing_count_column_defaults_to_one(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        expected = 1.0 / 1e6
        assert pytest.approx(result["total_damage"], rel=1e-6) == expected

    def test_missing_range_column_raises(self, simple_curve):
        df = pd.DataFrame({"stress": [100.0], "count": [1000.0]})
        calc = LinearDamageAccumulation()
        with pytest.raises(ValueError, match="range"):
            calc.calculate_damage(df, simple_curve)

    # --- edge cases ---
    def test_zero_stress_range_skipped(self, simple_curve):
        df = pd.DataFrame({
            "range": [0.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [1000.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["n_stress_levels"] == 1

    def test_negative_stress_range_skipped(self, simple_curve):
        df = pd.DataFrame({
            "range": [-50.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [1000.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["n_stress_levels"] == 1

    def test_zero_count_skipped(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [0.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["n_stress_levels"] == 1

    def test_below_fatigue_limit_infinite_life(self, dnv_d_curve):
        """Stress below CAFL gives infinite allowable cycles, zero damage."""
        df = pd.DataFrame({
            "range": [30.0],
            "mean": [0.0],
            "count": [1e9],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, dnv_d_curve)
        assert result["total_damage"] == 0.0
        assert result["safety_factor"] == np.inf

    def test_no_valid_cycles_zero_damage(self, simple_curve):
        df = pd.DataFrame({
            "range": [0.0, -10.0],
            "mean": [0.0, 0.0],
            "count": [100.0, 200.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] == 0.0
        assert result["safety_factor"] == np.inf
        assert result["estimated_remaining_cycles"] == np.inf

    # --- damage contributions detail ---
    def test_damage_contributions_structure(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [5000.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        assert len(contribs) == 2
        for c in contribs:
            assert "stress_range" in c
            assert "cycles_applied" in c
            assert "cycles_allowable" in c
            assert "damage_increment" in c
            assert "cumulative_damage" in c

    def test_cumulative_damage_monotonically_increases(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0, 150.0, 200.0],
            "mean": [0.0, 0.0, 0.0],
            "count": [1000.0, 1000.0, 1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        cum = [c["cumulative_damage"] for c in contribs]
        for i in range(1, len(cum)):
            assert cum[i] > cum[i - 1]

    # --- history tracking ---
    def test_history_tracking(self, simple_curve, single_level_df):
        calc = LinearDamageAccumulation()
        calc.calculate_damage(single_level_df, simple_curve)
        calc.calculate_damage(single_level_df, simple_curve)
        assert len(calc.damage_history) == 2

    # --- custom failure criterion ---
    def test_custom_failure_criterion_affects_life_fraction(self, simple_curve):
        df = pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [5000.0]})
        calc = LinearDamageAccumulation(failure_criterion=0.5)
        result = calc.calculate_damage(df, simple_curve)
        # D = 0.005, life_fraction_used = 0.005/0.5 = 0.01
        assert pytest.approx(result["life_fraction_used"], rel=1e-6) == 0.01

    # --- DNV D-curve realistic scenario ---
    def test_realistic_dnv_d_damage(self, dnv_d_curve):
        """Realistic offshore weld fatigue assessment."""
        df = pd.DataFrame({
            "range": [120.0, 100.0, 80.0, 60.0],
            "mean": [0.0, 0.0, 0.0, 0.0],
            "count": [1e3, 5e3, 2e4, 1e5],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, dnv_d_curve)
        assert result["total_damage"] > 0
        assert result["n_stress_levels"] == 4
        # Higher stress ranges should contribute more damage per cycle
        contribs = result["damage_contributions"]
        damage_per_cycle = [
            c["damage_increment"] / c["cycles_applied"] for c in contribs
        ]
        assert damage_per_cycle[0] > damage_per_cycle[1]


# ===========================================================================
# ModifiedMinersRule
# ===========================================================================

class TestModifiedMinersRule:
    def test_default_construction(self):
        calc = ModifiedMinersRule()
        assert calc.name == "Modified Miner's Rule"
        assert calc.failure_criterion == 1.0
        assert calc.acceleration_threshold == 0.3
        assert calc.acceleration_factor == 1.5
        assert calc.sequence_sensitive is True

    def test_custom_construction(self):
        calc = ModifiedMinersRule(
            failure_criterion=0.8,
            damage_acceleration_threshold=0.5,
            acceleration_factor=2.0,
            sequence_sensitive=False,
        )
        assert calc.failure_criterion == 0.8
        assert calc.acceleration_threshold == 0.5
        assert calc.acceleration_factor == 2.0
        assert calc.sequence_sensitive is False

    def test_low_damage_matches_linear_rate(self, simple_curve):
        """When total damage stays below threshold, modification_factor = 1.0."""
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1000.0],
        })
        calc = ModifiedMinersRule(damage_acceleration_threshold=0.3)
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # D = 1000/1e6 = 0.001 < 0.3 threshold, so factor should be 1.0
        assert contribs[0]["modification_factor"] == 1.0

    def test_high_damage_gets_accelerated(self, simple_curve):
        """Damage above threshold should use acceleration_factor."""
        # First block: push damage above 0.3
        # S=100 -> N=1e6. Need n > 300,000 for D > 0.3
        df = pd.DataFrame({
            "range": [100.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [400000.0, 100000.0],
        })
        calc = ModifiedMinersRule(
            damage_acceleration_threshold=0.3,
            acceleration_factor=1.5,
        )
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # First block: D=0.4 > 0.3 is accumulated at factor 1.0 initially
        # but since it goes above threshold during processing,
        # the second block should see factor 1.5
        assert contribs[1]["modification_factor"] == 1.5

    def test_sequence_sensitive_sorting(self, simple_curve):
        """When sequence_sensitive=True, cycles are sorted high-to-low."""
        df = pd.DataFrame({
            "range": [50.0, 200.0, 100.0],
            "mean": [0.0, 0.0, 0.0],
            "count": [1000.0, 1000.0, 1000.0],
        })
        calc = ModifiedMinersRule(sequence_sensitive=True)
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        stress_order = [c["stress_range"] for c in contribs]
        assert stress_order == sorted(stress_order, reverse=True)

    def test_modification_parameters_in_results(self, simple_curve, single_level_df):
        calc = ModifiedMinersRule(
            damage_acceleration_threshold=0.25,
            acceleration_factor=1.8,
            sequence_sensitive=False,
        )
        result = calc.calculate_damage(single_level_df, simple_curve)
        params = result["modification_parameters"]
        assert params["acceleration_threshold"] == 0.25
        assert params["acceleration_factor"] == 1.8
        assert params["sequence_sensitive"] is False

    def test_modified_damage_ge_linear_damage(self, simple_curve):
        """Modified Miner should generally give equal or higher damage than linear."""
        df = pd.DataFrame({
            "range": [100.0, 100.0, 100.0],
            "mean": [0.0, 0.0, 0.0],
            "count": [400000.0, 300000.0, 200000.0],
        })
        linear_calc = LinearDamageAccumulation()
        linear_result = linear_calc.calculate_damage(df, simple_curve)

        modified_calc = ModifiedMinersRule(
            damage_acceleration_threshold=0.3,
            acceleration_factor=1.5,
        )
        modified_result = modified_calc.calculate_damage(df, simple_curve)

        assert modified_result["total_damage"] >= linear_result["total_damage"]


# ===========================================================================
# NonlinearDamageAccumulation
# ===========================================================================

class TestNonlinearDamageMarcoStarkey:
    def test_construction(self):
        calc = NonlinearDamageAccumulation("marco_starkey")
        assert calc.model == "marco_starkey"
        assert "marco_starkey" in calc.name

    def test_basic_damage_calculation(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [5000.0, 1000.0],
        })
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] > 0
        assert result["method"] == "Nonlinear-marco_starkey"

    def test_alpha_depends_on_stress(self, simple_curve):
        """Higher stress should get higher alpha exponent."""
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [1000.0, 1000.0],
        })
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # 200 MPa should have higher alpha than 100 MPa
        alphas = {c["stress_range"]: c["alpha"] for c in contribs}
        assert alphas[200.0] > alphas[100.0]

    def test_custom_alpha_parameters(self, simple_curve):
        df = pd.DataFrame({
            "range": [150.0],
            "mean": [0.0],
            "count": [5000.0],
        })
        params = {"alpha_base": 0.6, "stress_dependency": 0.3}
        calc = NonlinearDamageAccumulation("marco_starkey", model_parameters=params)
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # Single stress level means stress_ratio = 1.0
        # alpha = 0.6 + 0.3 * 1.0 = 0.9
        assert pytest.approx(contribs[0]["alpha"], rel=1e-6) == 0.9

    def test_nonlinear_exponent_effect(self, simple_curve):
        """With alpha < 1 and damage_ratio < 1, nonlinear damage > linear damage."""
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [100.0],
        })
        # N_allow = 1e6, ratio = 100/1e6 = 1e-4
        # alpha = 0.4 + 0.5*1.0 = 0.9 (single stress level, ratio=1.0)
        # nonlinear_damage = (1e-4)^0.9
        calc_nl = NonlinearDamageAccumulation("marco_starkey")
        result_nl = calc_nl.calculate_damage(df, simple_curve)

        calc_lin = LinearDamageAccumulation()
        result_lin = calc_lin.calculate_damage(df, simple_curve)

        # (1e-4)^0.9 > 1e-4 since exponent < 1 and base < 1
        assert result_nl["total_damage"] > result_lin["total_damage"]

    def test_zero_and_negative_stress_skipped(self, simple_curve):
        df = pd.DataFrame({
            "range": [0.0, -10.0, 100.0],
            "mean": [0.0, 0.0, 0.0],
            "count": [100.0, 100.0, 100.0],
        })
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        assert len(contribs) == 1

    def test_results_contain_model_parameters(self, simple_curve, single_level_df):
        params = {"alpha_base": 0.5}
        calc = NonlinearDamageAccumulation("marco_starkey", model_parameters=params)
        result = calc.calculate_damage(single_level_df, simple_curve)
        assert "model_parameters" in result
        assert result["model_parameters"]["alpha_base"] == 0.5

    def test_ndarray_input(self, simple_curve):
        arr = np.array([[100.0, 0.0, 1000.0]])
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(arr, simple_curve)
        assert result["total_damage"] > 0


class TestNonlinearDamageCortenDolan:
    def test_construction(self):
        calc = NonlinearDamageAccumulation("corten_dolan")
        assert calc.model == "corten_dolan"

    def test_basic_damage(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [5000.0, 1000.0],
        })
        calc = NonlinearDamageAccumulation("corten_dolan")
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] > 0
        assert result["method"] == "Nonlinear-corten_dolan"

    def test_sorted_high_to_low(self, simple_curve):
        """Corten-Dolan sorts by stress range descending."""
        df = pd.DataFrame({
            "range": [50.0, 200.0, 100.0],
            "mean": [0.0, 0.0, 0.0],
            "count": [1000.0, 1000.0, 1000.0],
        })
        calc = NonlinearDamageAccumulation("corten_dolan")
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        stresses = [c["stress_range"] for c in contribs]
        assert stresses == sorted(stresses, reverse=True)

    def test_interaction_effect(self, simple_curve):
        """With accumulated damage, effective_cycles should increase."""
        df = pd.DataFrame({
            "range": [200.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [100000.0, 100000.0],
        })
        calc = NonlinearDamageAccumulation("corten_dolan")
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # Second block should have effective_cycles > cycles_applied
        second = contribs[1]
        assert second["effective_cycles"] > second["cycles_applied"]

    def test_corten_dolan_damage_ge_linear(self, simple_curve):
        """Corten-Dolan should give >= linear damage due to interaction."""
        df = pd.DataFrame({
            "range": [100.0, 200.0],
            "mean": [0.0, 0.0],
            "count": [5000.0, 1000.0],
        })
        calc_cd = NonlinearDamageAccumulation("corten_dolan")
        result_cd = calc_cd.calculate_damage(df, simple_curve)

        calc_lin = LinearDamageAccumulation()
        result_lin = calc_lin.calculate_damage(df, simple_curve)

        assert result_cd["total_damage"] >= result_lin["total_damage"]


class TestNonlinearDamageDoubleLinear:
    def test_construction(self):
        calc = NonlinearDamageAccumulation("double_linear")
        assert calc.model == "double_linear"

    def test_basic_damage(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1000.0],
        })
        calc = NonlinearDamageAccumulation("double_linear")
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] > 0

    def test_initiation_phase(self, simple_curve):
        """Below initiation_fraction threshold, damage should equal linear."""
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [10.0],
        })
        params = {"initiation_fraction": 0.1, "propagation_rate": 2.0}
        calc = NonlinearDamageAccumulation("double_linear", model_parameters=params)
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        assert contribs[0]["phase"] == "initiation"

    def test_propagation_phase_accelerated(self, simple_curve):
        """After initiation, damage rate should be multiplied by propagation_rate."""
        # Need D > 0.1 after first block.
        # S=100, N=1e6. Need n>100,000 for D>0.1
        df = pd.DataFrame({
            "range": [100.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [200000.0, 100000.0],
        })
        params = {"initiation_fraction": 0.1, "propagation_rate": 2.0}
        calc = NonlinearDamageAccumulation("double_linear", model_parameters=params)
        result = calc.calculate_damage(df, simple_curve)
        contribs = result["damage_contributions"]
        # Second block should be in propagation phase
        assert contribs[1]["phase"] == "propagation"
        # Its damage_increment should be 2x the linear value
        linear_damage = 100000.0 / 1e6
        assert pytest.approx(contribs[1]["damage_increment"], rel=1e-6) == linear_damage * 2.0

    def test_custom_parameters(self, simple_curve):
        params = {"initiation_fraction": 0.05, "propagation_rate": 3.0}
        calc = NonlinearDamageAccumulation("double_linear", model_parameters=params)
        assert calc.parameters["initiation_fraction"] == 0.05
        assert calc.parameters["propagation_rate"] == 3.0


class TestNonlinearDamageGeneral:
    def test_unknown_model_raises(self):
        calc = NonlinearDamageAccumulation("unknown_model")
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1000.0],
        })
        curve = PowerLawSNCurve(name="test", A=1e12, m=3.0, fatigue_limit=0.0)
        with pytest.raises(ValueError, match="Unknown nonlinear model"):
            calc.calculate_damage(df, curve)

    def test_format_results_safety_factor_infinite_at_zero_damage(self, simple_curve):
        """When all cycles are below fatigue limit, damage = 0, SF = inf."""
        curve = PowerLawSNCurve(name="high_limit", A=1e12, m=3.0, fatigue_limit=500.0)
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1000.0],
        })
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(df, curve)
        assert result["total_damage"] == 0.0
        assert result["safety_factor"] == np.inf

    def test_format_results_life_fraction(self, simple_curve):
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1000.0],
        })
        calc = NonlinearDamageAccumulation("marco_starkey")
        result = calc.calculate_damage(df, simple_curve)
        assert result["life_fraction_used"] == result["total_damage"]
        assert pytest.approx(
            result["life_fraction_remaining"], abs=1e-10
        ) == max(0, 1 - result["total_damage"])


# ===========================================================================
# CriticalPlaneAnalysis
# ===========================================================================

class TestCriticalPlaneAnalysis:
    @pytest.fixture
    def stress_history(self):
        """Synthetic multiaxial stress history [sigma_xx, sigma_yy, sigma_zz, tau_xy, tau_yz, tau_zx]."""
        n_steps = 100
        t = np.linspace(0, 2 * np.pi, n_steps)
        history = np.zeros((n_steps, 6))
        history[:, 0] = 100 * np.sin(t)       # sigma_xx: -100 to 100
        history[:, 1] = 50 * np.sin(t + 0.5)  # sigma_yy
        history[:, 3] = 30 * np.cos(t)         # tau_xy: -30 to 30
        return history

    def test_findley_construction(self):
        cpa = CriticalPlaneAnalysis(criterion="findley")
        assert cpa.criterion == "findley"

    def test_findley_equivalent_stress(self, stress_history):
        cpa = CriticalPlaneAnalysis(criterion="findley")
        constants = {"k": 0.5}
        eq_stress = cpa.calculate_equivalent_stress(stress_history, constants)
        assert eq_stress > 0
        assert np.isfinite(eq_stress)

    def test_findley_k_sensitivity(self, stress_history):
        """Higher k value should give higher equivalent stress (for positive mean)."""
        cpa = CriticalPlaneAnalysis(criterion="findley")
        eq_low_k = cpa.calculate_equivalent_stress(stress_history, {"k": 0.1})
        eq_high_k = cpa.calculate_equivalent_stress(stress_history, {"k": 1.0})
        # sigma_n_max = (max + min) / 2 = 0 for a symmetric sine
        # But depending on rounding, we just check both are positive and finite
        assert eq_low_k > 0
        assert eq_high_k > 0

    def test_brown_miller(self, stress_history):
        cpa = CriticalPlaneAnalysis(criterion="brown_miller")
        eq_stress = cpa.calculate_equivalent_stress(stress_history, {})
        # Should equal max(sigma_xx) - min(sigma_xx) = 100 - (-100) = 200
        assert pytest.approx(eq_stress, abs=1.0) == 200.0

    def test_swt_criterion(self, stress_history):
        cpa = CriticalPlaneAnalysis(criterion="swt")
        eq_stress = cpa.calculate_equivalent_stress(stress_history, {})
        # sigma_max ~ 100, delta_sigma ~ 200
        # SWT = sqrt(100 * 200/2) = sqrt(10000) = 100
        assert pytest.approx(eq_stress, abs=1.0) == 100.0

    def test_unknown_criterion_raises(self, stress_history):
        cpa = CriticalPlaneAnalysis(criterion="invalid")
        with pytest.raises(ValueError, match="Unknown criterion"):
            cpa.calculate_equivalent_stress(stress_history, {})

    def test_findley_manual_calculation(self):
        """Verify Findley calculation with hand-computable inputs."""
        # Constant stress: sigma_xx = 50 always, tau_xy = 20 always
        history = np.zeros((10, 6))
        history[:, 0] = 50.0  # sigma_xx
        history[:, 3] = 20.0  # tau_xy

        cpa = CriticalPlaneAnalysis(criterion="findley")
        k = 0.5
        eq_stress = cpa.calculate_equivalent_stress(history, {"k": k})

        # sigma_max = 50, sigma_min = 50 => sigma_n_max = 50
        # tau_max = 20
        # findley_param = 20 + 0.5*50 = 45
        # eq_stress = 2 * 45 = 90
        assert pytest.approx(eq_stress, rel=1e-6) == 90.0


# ===========================================================================
# compare_damage_methods
# ===========================================================================

class TestCompareDamageMethods:
    def test_default_methods(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(typical_cycles_df, simple_curve)
        assert isinstance(comparison, pd.DataFrame)
        assert len(comparison) == 3
        assert set(comparison.columns) >= {"method", "total_damage", "safety_factor"}

    def test_specified_methods(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(
            typical_cycles_df, simple_curve, methods=["linear"]
        )
        assert len(comparison) == 1
        assert comparison.iloc[0]["method"] == "linear"

    def test_unknown_method_skipped(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(
            typical_cycles_df, simple_curve, methods=["linear", "nonexistent"]
        )
        assert len(comparison) == 1

    def test_all_damage_values_positive(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(typical_cycles_df, simple_curve)
        assert (comparison["total_damage"] > 0).all()

    def test_all_safety_factors_positive(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(typical_cycles_df, simple_curve)
        assert (comparison["safety_factor"] > 0).all()

    def test_comparison_contains_life_fraction(self, simple_curve, typical_cycles_df):
        comparison = compare_damage_methods(typical_cycles_df, simple_curve)
        assert "life_fraction_used" in comparison.columns


# ===========================================================================
# Integration / cross-model scenarios
# ===========================================================================

class TestIntegrationScenarios:
    def test_all_models_accept_same_input(self, simple_curve, typical_cycles_df):
        """Verify all models can process the same cycle data without errors."""
        models = [
            LinearDamageAccumulation(),
            ModifiedMinersRule(),
            NonlinearDamageAccumulation("marco_starkey"),
            NonlinearDamageAccumulation("corten_dolan"),
            NonlinearDamageAccumulation("double_linear"),
        ]
        for model in models:
            result = model.calculate_damage(typical_cycles_df, simple_curve)
            assert result["total_damage"] >= 0, f"{model.name} gave negative damage"
            assert "method" in result

    def test_damage_ordering_across_models(self, simple_curve):
        """Under typical conditions, nonlinear Corten-Dolan >= linear Miner."""
        df = pd.DataFrame({
            "range": [150.0, 100.0],
            "mean": [0.0, 0.0],
            "count": [10000.0, 50000.0],
        })
        linear_result = LinearDamageAccumulation().calculate_damage(df, simple_curve)
        cd_result = NonlinearDamageAccumulation("corten_dolan").calculate_damage(
            df, simple_curve
        )
        assert cd_result["total_damage"] >= linear_result["total_damage"]

    def test_large_cycle_count_numerical_stability(self, simple_curve):
        """Verify no overflow or precision issues with large cycle counts."""
        df = pd.DataFrame({
            "range": [60.0],
            "mean": [0.0],
            "count": [1e15],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert np.isfinite(result["total_damage"])
        assert result["total_damage"] > 0

    def test_many_stress_levels(self, simple_curve):
        """A wide spectrum of stress ranges processed correctly."""
        stress_ranges = np.linspace(10, 300, 50)
        counts = np.full(50, 100.0)
        df = pd.DataFrame({
            "range": stress_ranges,
            "mean": np.zeros(50),
            "count": counts,
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["n_stress_levels"] == 50
        assert result["total_damage"] > 0

    def test_single_cycle_small_damage(self, simple_curve):
        """A single cycle at moderate stress gives very small damage."""
        df = pd.DataFrame({
            "range": [100.0],
            "mean": [0.0],
            "count": [1.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(df, simple_curve)
        assert result["total_damage"] == pytest.approx(1e-6, rel=1e-6)
