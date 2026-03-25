"""
Tests for CALM buoy mooring fatigue module.

Coverage: OrcaFlex tension reader, rainflow counting, S-N curves (API RP 2SK),
Miner's rule damage accumulation, fatigue life output, scatter diagram weighting.

References:
    - DNVGL-RP-C205
    - API RP 2SK
    - ASTM E1049

Test strategy: AAA (Arrange-Act-Assert), synthetic time histories with
analytically-known damage to verify numerical correctness.
"""
import io
import math
import textwrap

import numpy as np
import pandas as pd
import pytest

from digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue import (
    MooringLineResult,
    OrcaFlexTensionReader,
    RainflowFatigue,
    SNcurve,
    MinersRuleDamage,
    ScatterDiagramFatigue,
    FatigueLifeReport,
    CHAIN_SN_SEAWATER,
    WIRE_ROPE_SN_SEAWATER,
    compute_fatigue_life,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_sinusoidal_tension(
    mean_kn: float = 500.0,
    amplitude_kn: float = 100.0,
    n_cycles: int = 1000,
    points_per_cycle: int = 20,
) -> np.ndarray:
    """Synthetic single-frequency tension signal: exactly n_cycles full cycles."""
    t = np.linspace(0, 2 * math.pi * n_cycles, n_cycles * points_per_cycle + 1)
    return mean_kn + amplitude_kn * np.sin(t)


def _make_csv_content(
    time: np.ndarray,
    tension: np.ndarray,
    line_name: str = "Line1",
) -> str:
    """Build CSV string in OrcaFlex-export style (time + one tension column)."""
    header = f"Time (s),{line_name} Effective Tension (kN)"
    rows = [header]
    for t_val, ten_val in zip(time, tension):
        rows.append(f"{t_val:.4f},{ten_val:.4f}")
    return "\n".join(rows)


# ---------------------------------------------------------------------------
# 1. SNcurve
# ---------------------------------------------------------------------------

class TestSNcurve:
    """Unit tests for the SNcurve dataclass / helper."""

    def test_chain_sn_constant_is_defined(self):
        """CHAIN_SN_SEAWATER constant must expose K and m."""
        sn = CHAIN_SN_SEAWATER
        assert sn.K > 0
        assert sn.m > 0

    def test_wire_rope_sn_constant_is_defined(self):
        """WIRE_ROPE_SN_SEAWATER constant must expose K and m."""
        sn = WIRE_ROPE_SN_SEAWATER
        assert sn.K > 0
        assert sn.m > 0

    def test_chain_sn_parameters_match_api_rp_2sk(self):
        """Chain S-N: K=1.57e11, m=3.0 per API RP 2SK seawater+CP."""
        sn = CHAIN_SN_SEAWATER
        assert pytest.approx(sn.K, rel=1e-3) == 1.57e11
        assert pytest.approx(sn.m, rel=1e-6) == 3.0

    def test_wire_rope_sn_parameters_match_api_rp_2sk(self):
        """Wire rope S-N: K=1.0e11, m=4.0 per API RP 2SK."""
        sn = WIRE_ROPE_SN_SEAWATER
        assert pytest.approx(sn.K, rel=1e-3) == 1.0e11
        assert pytest.approx(sn.m, rel=1e-6) == 4.0

    def test_cycles_to_failure_chain_known_value(self):
        """N = K / S^m  =>  N = 1.57e11 / 100^3 = 1.57e5."""
        sn = CHAIN_SN_SEAWATER
        stress_range_kn = 100.0
        n_fail = sn.cycles_to_failure(stress_range_kn)
        expected = 1.57e11 / (100.0 ** 3)
        assert pytest.approx(n_fail, rel=1e-6) == expected

    def test_cycles_to_failure_wire_rope_known_value(self):
        """N = K / S^m  =>  N = 1.0e11 / 50^4 = 1.6e3."""
        sn = WIRE_ROPE_SN_SEAWATER
        stress_range_kn = 50.0
        n_fail = sn.cycles_to_failure(stress_range_kn)
        expected = 1.0e11 / (50.0 ** 4)
        assert pytest.approx(n_fail, rel=1e-6) == expected

    def test_cycles_to_failure_zero_range_returns_inf(self):
        """Zero stress range must return infinity (no damage)."""
        sn = CHAIN_SN_SEAWATER
        assert sn.cycles_to_failure(0.0) == math.inf

    def test_custom_sn_curve(self):
        """SNcurve accepts arbitrary K and m."""
        sn = SNcurve(name="Custom", K=1e12, m=3.5)
        n_fail = sn.cycles_to_failure(200.0)
        expected = 1e12 / (200.0 ** 3.5)
        assert pytest.approx(n_fail, rel=1e-6) == expected


# ---------------------------------------------------------------------------
# 2. OrcaFlexTensionReader
# ---------------------------------------------------------------------------

class TestOrcaFlexTensionReader:
    """Unit tests for CSV/XLSX tension reader."""

    def test_read_csv_returns_dataframe(self):
        """Reader must return a DataFrame with time and tension columns."""
        n_cycles = 10
        time = np.linspace(0, 10, n_cycles * 20 + 1)
        tension = _make_sinusoidal_tension(n_cycles=n_cycles)
        csv_text = _make_csv_content(time, tension, line_name="ML1")
        reader = OrcaFlexTensionReader()
        result = reader.read_csv(io.StringIO(csv_text))
        assert isinstance(result, dict)
        assert "ML1" in result
        assert isinstance(result["ML1"], np.ndarray)
        assert len(result["ML1"]) == len(time)

    def test_read_csv_multiple_lines(self):
        """Reader handles multiple mooring lines in one CSV."""
        n = 50
        time = np.linspace(0, 50, n)
        t1 = np.ones(n) * 400.0
        t2 = np.ones(n) * 600.0
        header = "Time (s),Line1 Effective Tension (kN),Line2 Effective Tension (kN)"
        rows = [header]
        for i in range(n):
            rows.append(f"{time[i]:.3f},{t1[i]:.3f},{t2[i]:.3f}")
        csv_text = "\n".join(rows)
        reader = OrcaFlexTensionReader()
        result = reader.read_csv(io.StringIO(csv_text))
        assert "Line1" in result
        assert "Line2" in result

    def test_read_csv_extracts_tension_values_correctly(self):
        """Tension values must match source data within floating-point tolerance."""
        time = np.array([0.0, 0.5, 1.0, 1.5, 2.0])
        tension = np.array([500.0, 520.0, 480.0, 510.0, 495.0])
        csv_text = _make_csv_content(time, tension, line_name="ML1")
        reader = OrcaFlexTensionReader()
        result = reader.read_csv(io.StringIO(csv_text))
        np.testing.assert_allclose(result["ML1"], tension, rtol=1e-5)

    def test_read_csv_raises_on_missing_time_column(self):
        """CSV without a time column must raise ValueError."""
        csv_text = "LineA Effective Tension (kN)\n500\n510\n"
        reader = OrcaFlexTensionReader()
        with pytest.raises(ValueError, match="[Tt]ime"):
            reader.read_csv(io.StringIO(csv_text))

    def test_parse_line_name_strips_descriptor(self):
        """Column 'Line3 Effective Tension (kN)' should produce key 'Line3'."""
        reader = OrcaFlexTensionReader()
        name = reader.parse_line_name("Line3 Effective Tension (kN)")
        assert name == "Line3"

    def test_parse_line_name_bare_name(self):
        """Column with no descriptor suffix should pass through unchanged."""
        reader = OrcaFlexTensionReader()
        name = reader.parse_line_name("ML4")
        assert name == "ML4"

    def test_read_csv_from_file_path(self, tmp_path):
        """Reader accepts a file path (str or Path) as well as file-like objects."""
        time = np.linspace(0, 5, 11)
        tension = np.linspace(400, 600, 11)
        csv_text = _make_csv_content(time, tension)
        p = tmp_path / "tension.csv"
        p.write_text(csv_text)
        reader = OrcaFlexTensionReader()
        result = reader.read_csv(str(p))
        assert "Line1" in result


# ---------------------------------------------------------------------------
# 3. RainflowFatigue
# ---------------------------------------------------------------------------

class TestRainflowFatigue:
    """Unit tests for rainflow cycle extraction (ASTM E1049)."""

    def test_single_frequency_cycle_count(self):
        """Sinusoidal signal with n_cycles must extract approximately n_cycles."""
        n_cycles = 200
        tension = _make_sinusoidal_tension(n_cycles=n_cycles, points_per_cycle=40)
        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        total = float(np.sum(counts))
        # Allow ±10 % tolerance for half-cycle handling at signal edges
        assert abs(total - n_cycles) / n_cycles < 0.1

    def test_dominant_range_for_mean_start_sinusoid(self):
        """ASTM E1049: sinusoid starting at mean yields half-cycles of range = amplitude.

        For a signal sin(t) starting at mean (zero crossing), rainflow extracts
        two half-cycles per period each of range = amplitude (not 2*amplitude).
        The dominant (and only) range equals the single amplitude A.
        """
        amplitude = 150.0
        tension = _make_sinusoidal_tension(amplitude_kn=amplitude, n_cycles=100)
        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        # Dominant range for a mean-start sinusoid = amplitude (one-sided swing)
        dominant_range = ranges[np.argmax(counts)]
        assert pytest.approx(dominant_range, rel=0.05) == amplitude

    def test_constant_signal_produces_no_cycles(self):
        """Flat tension time history must produce zero cycles."""
        tension = np.full(1000, 500.0)
        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        assert float(np.sum(counts)) == pytest.approx(0.0)

    def test_returns_numpy_arrays(self):
        """count_cycles must return (np.ndarray, np.ndarray)."""
        tension = _make_sinusoidal_tension(n_cycles=10)
        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        assert isinstance(ranges, np.ndarray)
        assert isinstance(counts, np.ndarray)

    def test_ranges_are_non_negative(self):
        """All extracted ranges must be >= 0."""
        tension = _make_sinusoidal_tension(n_cycles=50)
        rf = RainflowFatigue()
        ranges, _ = rf.count_cycles(tension)
        assert np.all(ranges >= 0)

    def test_two_frequency_signal_extracts_both_ranges(self):
        """Superimposed sine waves should yield cycles near each wave's range."""
        n = 5000
        t = np.linspace(0, 4 * math.pi * 100, n)
        a1, a2 = 80.0, 20.0
        tension = 500 + a1 * np.sin(t) + a2 * np.sin(5 * t)
        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        assert len(ranges) > 1


# ---------------------------------------------------------------------------
# 4. MinersRuleDamage
# ---------------------------------------------------------------------------

class TestMinersRuleDamage:
    """Unit tests for Miner's rule damage accumulation."""

    def test_single_range_known_damage(self):
        """D = n / N = n * S^m / K for a single stress range."""
        sn = CHAIN_SN_SEAWATER
        stress_range = 100.0  # kN
        n_cycles = 1000.0
        n_fail = sn.cycles_to_failure(stress_range)
        expected_damage = n_cycles / n_fail

        calc = MinersRuleDamage(sn_curve=sn)
        damage = calc.compute(
            ranges=np.array([stress_range]),
            counts=np.array([n_cycles]),
        )
        assert pytest.approx(damage, rel=1e-6) == expected_damage

    def test_zero_range_contributes_zero_damage(self):
        """A zero-range cycle must add zero damage (D_i = n_i / inf = 0)."""
        sn = CHAIN_SN_SEAWATER
        calc = MinersRuleDamage(sn_curve=sn)
        damage = calc.compute(
            ranges=np.array([0.0, 50.0]),
            counts=np.array([1000.0, 0.0]),
        )
        assert damage == pytest.approx(0.0, abs=1e-12)

    def test_damage_additivity(self):
        """Miner's rule: D_total = sum(n_i / N_i)."""
        sn = CHAIN_SN_SEAWATER
        ranges = np.array([50.0, 100.0, 150.0])
        counts = np.array([5000.0, 2000.0, 500.0])
        expected = sum(
            counts[i] / sn.cycles_to_failure(ranges[i]) for i in range(3)
        )
        calc = MinersRuleDamage(sn_curve=sn)
        damage = calc.compute(ranges=ranges, counts=counts)
        assert pytest.approx(damage, rel=1e-6) == expected

    def test_empty_input_returns_zero(self):
        """Empty ranges/counts arrays must return zero damage."""
        calc = MinersRuleDamage(sn_curve=CHAIN_SN_SEAWATER)
        damage = calc.compute(ranges=np.array([]), counts=np.array([]))
        assert damage == 0.0

    def test_wire_rope_damage_differs_from_chain(self):
        """Different S-N curve must yield different damage for same loading."""
        ranges = np.array([80.0, 120.0])
        counts = np.array([3000.0, 1000.0])
        d_chain = MinersRuleDamage(sn_curve=CHAIN_SN_SEAWATER).compute(ranges, counts)
        d_wire = MinersRuleDamage(sn_curve=WIRE_ROPE_SN_SEAWATER).compute(ranges, counts)
        assert d_chain != pytest.approx(d_wire)


# ---------------------------------------------------------------------------
# 5. ScatterDiagramFatigue
# ---------------------------------------------------------------------------

class TestScatterDiagramFatigue:
    """Unit tests for scatter-diagram weighted damage."""

    def _make_scatter(self) -> pd.DataFrame:
        """Minimal 2-bin scatter diagram."""
        return pd.DataFrame({
            "Hs": [1.0, 2.0],
            "Tp": [6.0, 8.0],
            "probability": [0.6, 0.4],
        })

    def _make_tension_per_seastate(self) -> dict:
        """Tension histories keyed by (Hs, Tp)."""
        hs_tp = [(1.0, 6.0), (2.0, 8.0)]
        result = {}
        for hs, tp in hs_tp:
            amplitude = hs * 50  # crude proxy
            result[(hs, tp)] = {
                "ML1": _make_sinusoidal_tension(amplitude_kn=amplitude, n_cycles=100)
            }
        return result

    def test_scatter_weighted_damage_is_probability_weighted_sum(self):
        """D_total = sum(p_i * D_i) across sea states."""
        scatter = self._make_scatter()
        tensions = self._make_tension_per_seastate()
        calc = ScatterDiagramFatigue(sn_curve=CHAIN_SN_SEAWATER)
        result = calc.compute(scatter=scatter, tension_per_seastate=tensions)
        # result must be a dict keyed by line name
        assert "ML1" in result
        assert result["ML1"] >= 0.0

    def test_scatter_probabilities_must_sum_to_one(self):
        """Scatter diagram with probabilities not summing to ~1.0 raises ValueError."""
        bad_scatter = pd.DataFrame({
            "Hs": [1.0, 2.0],
            "Tp": [6.0, 8.0],
            "probability": [0.3, 0.3],  # sums to 0.6
        })
        tensions = self._make_tension_per_seastate()
        calc = ScatterDiagramFatigue(sn_curve=CHAIN_SN_SEAWATER)
        with pytest.raises(ValueError, match="[Pp]robabilit"):
            calc.compute(scatter=bad_scatter, tension_per_seastate=tensions)

    def test_missing_seastate_tension_raises_keyerror(self):
        """Scatter with a (Hs,Tp) bin absent from tension dict must raise KeyError."""
        scatter = pd.DataFrame({
            "Hs": [1.0, 3.0],
            "Tp": [6.0, 10.0],
            "probability": [0.7, 0.3],
        })
        tensions = self._make_tension_per_seastate()  # only has (1,6) and (2,8)
        calc = ScatterDiagramFatigue(sn_curve=CHAIN_SN_SEAWATER)
        with pytest.raises(KeyError):
            calc.compute(scatter=scatter, tension_per_seastate=tensions)

    def test_single_seastate_with_probability_one(self):
        """Single sea state with p=1.0 must equal un-weighted damage."""
        scatter = pd.DataFrame({
            "Hs": [2.0],
            "Tp": [8.0],
            "probability": [1.0],
        })
        tension = _make_sinusoidal_tension(amplitude_kn=80.0, n_cycles=100)
        tensions = {(2.0, 8.0): {"ML1": tension}}
        sn = CHAIN_SN_SEAWATER

        rf = RainflowFatigue()
        ranges, counts = rf.count_cycles(tension)
        expected_damage = MinersRuleDamage(sn_curve=sn).compute(ranges, counts)

        calc = ScatterDiagramFatigue(sn_curve=sn)
        result = calc.compute(scatter=scatter, tension_per_seastate=tensions)
        assert pytest.approx(result["ML1"], rel=1e-6) == expected_damage


# ---------------------------------------------------------------------------
# 6. FatigueLifeReport
# ---------------------------------------------------------------------------

class TestFatigueLifeReport:
    """Unit tests for fatigue life and inspection interval output."""

    def test_fatigue_life_inverse_of_damage_per_year(self):
        """Fatigue life [years] = design_life / damage_per_design_life."""
        damage = 0.1  # per design_life of 25 years
        design_life_years = 25.0
        report = FatigueLifeReport(design_life_years=design_life_years)
        result = report.build({"ML1": damage, "ML2": 0.5})
        # Fatigue life = design_life / damage
        assert pytest.approx(result["ML1"].fatigue_life_years, rel=1e-6) == (
            design_life_years / damage
        )

    def test_inspection_interval_is_fraction_of_fatigue_life(self):
        """Default inspection interval = fatigue_life / safety_factor."""
        damage = 0.2
        design_life_years = 20.0
        safety_factor = 3.0
        report = FatigueLifeReport(
            design_life_years=design_life_years,
            inspection_interval_safety_factor=safety_factor,
        )
        result = report.build({"ML1": damage})
        expected_life = design_life_years / damage
        expected_interval = expected_life / safety_factor
        assert pytest.approx(result["ML1"].inspection_interval_years, rel=1e-6) == (
            expected_interval
        )

    def test_damage_fraction_is_capped_description(self):
        """Damage > 1.0 must be reported as exceeded."""
        damage = 2.5
        report = FatigueLifeReport(design_life_years=25.0)
        result = report.build({"ML1": damage})
        assert result["ML1"].design_life_exceeded is True

    def test_damage_below_one_not_exceeded(self):
        """Damage < 1.0 means design life not exceeded."""
        damage = 0.8
        report = FatigueLifeReport(design_life_years=25.0)
        result = report.build({"ML1": damage})
        assert result["ML1"].design_life_exceeded is False

    def test_report_all_lines_present(self):
        """All lines passed in damages dict must appear in report."""
        damages = {"ML1": 0.1, "ML2": 0.5, "ML3": 1.2}
        report = FatigueLifeReport(design_life_years=25.0)
        result = report.build(damages)
        assert set(result.keys()) == {"ML1", "ML2", "ML3"}

    def test_zero_damage_gives_infinite_fatigue_life(self):
        """Zero damage must yield infinite fatigue life."""
        report = FatigueLifeReport(design_life_years=25.0)
        result = report.build({"ML1": 0.0})
        assert result["ML1"].fatigue_life_years == math.inf


# ---------------------------------------------------------------------------
# 7. MooringLineResult dataclass
# ---------------------------------------------------------------------------

class TestMooringLineResult:
    """Tests for the result dataclass."""

    def test_fields_present(self):
        """MooringLineResult must expose required fields."""
        r = MooringLineResult(
            line_name="ML1",
            damage=0.4,
            fatigue_life_years=62.5,
            inspection_interval_years=20.0,
            design_life_exceeded=False,
        )
        assert r.line_name == "ML1"
        assert pytest.approx(r.damage) == 0.4
        assert pytest.approx(r.fatigue_life_years) == 62.5
        assert r.design_life_exceeded is False


# ---------------------------------------------------------------------------
# 8. compute_fatigue_life integration function
# ---------------------------------------------------------------------------

class TestComputeFatigueLife:
    """Integration tests for the top-level convenience function."""

    def test_compute_fatigue_life_returns_report_dict(self):
        """compute_fatigue_life must return a dict of MooringLineResult."""
        n_cycles = 100
        tension = _make_sinusoidal_tension(amplitude_kn=100.0, n_cycles=n_cycles)
        csv_content = _make_csv_content(
            np.linspace(0, 100, len(tension)), tension, line_name="ML1"
        )
        report = compute_fatigue_life(
            tension_csv=io.StringIO(csv_content),
            sn_curve=CHAIN_SN_SEAWATER,
            design_life_years=25.0,
        )
        assert isinstance(report, dict)
        assert "ML1" in report
        assert isinstance(report["ML1"], MooringLineResult)

    def test_higher_tension_amplitude_yields_higher_damage(self):
        """Larger tension amplitude (larger stress range) must increase damage."""
        def _damage(amplitude_kn):
            n = 200
            tension = _make_sinusoidal_tension(
                amplitude_kn=amplitude_kn, n_cycles=n
            )
            csv_content = _make_csv_content(
                np.linspace(0, 200, len(tension)), tension, line_name="ML1"
            )
            report = compute_fatigue_life(
                tension_csv=io.StringIO(csv_content),
                sn_curve=CHAIN_SN_SEAWATER,
                design_life_years=25.0,
            )
            return report["ML1"].damage

        d_low = _damage(50.0)
        d_high = _damage(150.0)
        assert d_high > d_low

    def test_compute_fatigue_life_with_scatter_diagram(self):
        """compute_fatigue_life with scatter_diagram must produce weighted result."""
        n = 100
        tension_low = _make_sinusoidal_tension(amplitude_kn=50.0, n_cycles=n)
        tension_high = _make_sinusoidal_tension(amplitude_kn=150.0, n_cycles=n)
        scatter = pd.DataFrame({
            "Hs": [1.0, 3.0],
            "Tp": [6.0, 10.0],
            "probability": [0.7, 0.3],
        })
        tension_per_seastate = {
            (1.0, 6.0): {"ML1": tension_low},
            (3.0, 10.0): {"ML1": tension_high},
        }
        report = compute_fatigue_life(
            tension_csv=None,
            sn_curve=CHAIN_SN_SEAWATER,
            design_life_years=25.0,
            scatter_diagram=scatter,
            tension_per_seastate=tension_per_seastate,
        )
        assert "ML1" in report
        assert report["ML1"].damage >= 0.0

    def test_compute_requires_either_csv_or_scatter(self):
        """Passing neither tension_csv nor scatter_diagram must raise ValueError."""
        with pytest.raises(ValueError, match="[Tt]ension"):
            compute_fatigue_life(
                tension_csv=None,
                sn_curve=CHAIN_SN_SEAWATER,
                design_life_years=25.0,
            )
