# ABOUTME: Tests for FatiguePostprocessor — S-N curves, Miner's damage, stress ranges
# ABOUTME: Verifies fatigue calculations and APDL extraction commands

"""Tests for fatigue_postprocessor — FatiguePostprocessor methods."""

import math

import pytest
import pandas as pd

from digitalmodel.ansys.fatigue_postprocessor import (
    FatigueConfig,
    FatigueLoadCase,
    FatiguePostprocessor,
    FatigueResult,
    SNcurve,
)


def _fp() -> FatiguePostprocessor:
    return FatiguePostprocessor()


# ---------------------------------------------------------------------------
# S-N curve creation
# ---------------------------------------------------------------------------

class TestSNcurve:
    def test_from_dnv_d_curve(self):
        sn = SNcurve.from_dnv("D")
        assert sn.name == "D"
        assert sn.m1 == 3.0

    def test_from_dnv_f_curve(self):
        sn = SNcurve.from_dnv("F")
        assert sn.name == "F"
        assert sn.log_a1 == pytest.approx(11.855)

    def test_from_dnv_case_insensitive(self):
        sn = SNcurve.from_dnv("b1")
        assert sn.name == "B1"

    def test_unknown_curve_raises(self):
        with pytest.raises(KeyError):
            SNcurve.from_dnv("INVALID")


# ---------------------------------------------------------------------------
# Cycles to failure
# ---------------------------------------------------------------------------

class TestCyclesToFailure:
    def test_d_curve_at_100mpa(self):
        """DNV D curve at 100 MPa: log N = 12.164 - 3*log(100) = 12.164 - 6 = 6.164
        N = 10^6.164 ≈ 1.46e6"""
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(100.0, sn)
        assert 1e6 < N < 2e6

    def test_zero_stress_returns_inf(self):
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(0.0, sn)
        assert N == float("inf")

    def test_higher_stress_fewer_cycles(self):
        sn = SNcurve.from_dnv("D")
        N_low = _fp().calculate_cycles_to_failure(50.0, sn)
        N_high = _fp().calculate_cycles_to_failure(150.0, sn)
        assert N_high < N_low

    def test_negative_stress_returns_inf(self):
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(-10.0, sn)
        assert N == float("inf")


# ---------------------------------------------------------------------------
# Thickness correction
# ---------------------------------------------------------------------------

class TestThicknessCorrection:
    def test_no_correction_below_reference(self):
        """Thickness <= 25mm should have no correction."""
        S = _fp().calculate_thickness_correction(100.0, 20.0, 25.0, 0.2)
        assert S == pytest.approx(100.0)

    def test_correction_increases_stress(self):
        """Thicker plates get higher effective stress range."""
        S = _fp().calculate_thickness_correction(100.0, 50.0, 25.0, 0.2)
        assert S > 100.0

    def test_known_correction(self):
        """50mm plate: factor = (50/25)^0.2 = 2^0.2 ≈ 1.149"""
        S = _fp().calculate_thickness_correction(100.0, 50.0, 25.0, 0.2)
        expected = 100.0 * (50.0 / 25.0) ** 0.2
        assert S == pytest.approx(expected)


# ---------------------------------------------------------------------------
# Fatigue damage (Miner's rule)
# ---------------------------------------------------------------------------

class TestFatigueDamage:
    def test_single_load_case_damage(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert result.total_damage > 0
        assert result.total_damage < 1.0  # 1e5 cycles at 100 MPa on D curve

    def test_damage_accumulates_across_cases(self):
        lc1 = FatigueLoadCase(load_case_id="LC1", stress_range_mpa=100.0, num_cycles=1e5)
        lc2 = FatigueLoadCase(load_case_id="LC2", stress_range_mpa=80.0, num_cycles=2e5)
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[lc1, lc2],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert len(result.damage_per_case) == 2
        total = sum(result.damage_per_case.values())
        assert result.total_damage == pytest.approx(total)

    def test_dff_affects_utilization(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=3.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        # With DFF=3, allowable damage = 1/3 = 0.333
        assert result.allowable_damage == pytest.approx(1.0 / 3.0)
        # Utilization = damage / allowable
        assert result.utilization > result.total_damage

    def test_scf_increases_damage(self):
        config_no_scf = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
            scf=1.0,
        )
        config_with_scf = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
            scf=1.5,
        )
        d1 = _fp().calculate_fatigue_damage(config_no_scf).total_damage
        d2 = _fp().calculate_fatigue_damage(config_with_scf).total_damage
        assert d2 > d1

    def test_fatigue_life_years_computed(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert result.fatigue_life_years > 0
        assert result.fatigue_life_years < float("inf")


# ---------------------------------------------------------------------------
# APDL stress range extraction
# ---------------------------------------------------------------------------

class TestStressRangeExtraction:
    def test_generates_post1(self):
        text = _fp().generate_stress_range_extraction()
        assert "/POST1" in text

    def test_generates_set_commands(self):
        text = _fp().generate_stress_range_extraction(
            load_step_pairs=[(2, 1)]
        )
        assert "SET,2" in text
        assert "SET,1" in text

    def test_generates_sadd_for_range(self):
        text = _fp().generate_stress_range_extraction(
            load_step_pairs=[(3, 1)]
        )
        assert "SADD," in text


# ---------------------------------------------------------------------------
# S-N curve APDL
# ---------------------------------------------------------------------------

class TestSNCurveAPDL:
    def test_generates_log_a_parameter(self):
        sn = SNcurve.from_dnv("D")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "*SET,LOG_A1," in text

    def test_generates_slope_parameter(self):
        sn = SNcurve.from_dnv("D")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "*SET,M1," in text

    def test_includes_curve_name(self):
        sn = SNcurve.from_dnv("F")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "F" in text


# ---------------------------------------------------------------------------
# Damage summary DataFrame
# ---------------------------------------------------------------------------

class TestDamageSummaryDF:
    def test_returns_dataframe(self):
        results = [FatigueResult(location_id="LOC1", total_damage=0.5)]
        df = _fp().create_damage_summary_df(results)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 1

    def test_status_column(self):
        results = [
            FatigueResult(location_id="LOC1", utilization=0.5),
            FatigueResult(location_id="LOC2", utilization=1.5),
        ]
        df = _fp().create_damage_summary_df(results)
        statuses = df["status"].tolist()
        assert statuses[0] == "PASS"
        assert statuses[1] == "FAIL"

    def test_empty_results(self):
        df = _fp().create_damage_summary_df([])
        assert len(df) == 0


def test_module_docstring_states_caller_input_boundary():
    from digitalmodel.ansys import fatigue_postprocessor

    docstring = fatigue_postprocessor.__doc__
    assert "rainflow" not in docstring.lower()
    assert "supplied by the caller" in docstring


def test_regression_pin_load_case_exposes_caller_input_fields():
    """Regression pin: stress ranges and cycle counts remain input fields."""
    from dataclasses import fields

    input_fields = {item.name for item in fields(FatigueLoadCase) if item.init}
    assert {"stress_range_mpa", "num_cycles"} <= input_fields


@pytest.mark.parametrize("claim", ["ASME VIII Div 2", "contour data generation"])
def test_module_does_not_advertise_absent_capabilities(claim):
    from digitalmodel.ansys import fatigue_postprocessor as module

    assert claim not in module.__doc__


def test_documented_dnv_classes_equal_stored_classes():
    import re
    from digitalmodel.ansys import fatigue_postprocessor as module

    match = re.search(r"DNV-RP-C203 curves: ([^\n]+)", module.__doc__)
    assert match is not None
    assert {name.strip() for name in match[1].split(",")} == set(module._DNV_SN_CURVES)


def test_unknown_curve_error_names_valid_classes():
    from digitalmodel.ansys import fatigue_postprocessor as module

    with pytest.raises(KeyError) as error:
        SNcurve.from_dnv("unknown")
    assert "UNKNOWN" in str(error.value)
    assert "Valid classes:" in str(error.value)
    assert ", ".join(module._DNV_SN_CURVES) in str(error.value)


# Numerical transcription verified on the opened PDF page, not an OCR export.
_VERIFIED_AIR_ROWS = [
    ("B1", 15.117, 4.0, 17.146, 5.0, 1e7, 106.97),
    ("B2", 14.885, 4.0, 16.856, 5.0, 1e7, 93.59),
    ("C", 12.592, 3.0, 16.320, 5.0, 1e7, 73.10),
    ("C1", 12.449, 3.0, 16.081, 5.0, 1e7, 65.50),
    ("D", 12.164, 3.0, 15.606, 5.0, 1e7, 52.63),
    ("E", 12.010, 3.0, 15.350, 5.0, 1e7, 46.78),
    ("F", 11.855, 3.0, 15.091, 5.0, 1e7, 41.52),
    ("F1", 11.699, 3.0, 14.832, 5.0, 1e7, 36.84),
    ("F3", 11.546, 3.0, 14.576, 5.0, 1e7, 32.75),
    ("W1", 11.261, 3.0, 14.101, 5.0, 1e7, 26.32),
    ("W3", 10.970, 3.0, 13.617, 5.0, 1e7, 21.05),
]


@pytest.mark.parametrize("row", _VERIFIED_AIR_ROWS, ids=lambda row: row[0])
def test_dnv_stored_constants_match_opened_air_table(row):
    """DNV-RP-C203, October 2011, clause 2.4.4, Table 2-1, page 18.

    Fields: class, log_a1, m1, log_a2, m2, transition cycles, limit (MPa).
    Intercepts use stress ranges in MPa and cycles as dimensionless counts.
    """
    name, *expected = row
    sn = SNcurve.from_dnv(name)
    actual = (sn.log_a1, sn.m1, sn.log_a2, sn.m2, sn.N_transition, sn.threshold_mpa)
    assert actual == tuple(expected)


def test_default_environment_identifies_verified_air_values():
    """DNV-RP-C203, October 2011, clause 2.4.4, Table 2-1, page 18."""
    assert SNcurve().environment == "air"
    assert SNcurve.from_dnv("D").environment == "air"
    assert SNcurve(environment="seawater_cp").environment == "seawater_cp"


def test_below_threshold_documentation_matches_finite_extrapolation():
    sn = SNcurve.from_dnv("D")
    cycles = _fp().calculate_cycles_to_failure(sn.threshold_mpa / 2, sn)
    config = FatigueConfig(
        sn_curve=sn,
        load_cases=[FatigueLoadCase(stress_range_mpa=sn.threshold_mpa / 2,
                                   num_cycles=1000)],
        thickness_correction=False,
    )
    assert math.isfinite(cycles)
    assert _fp().calculate_fatigue_damage(config).total_damage > 0
    doc = FatiguePostprocessor.calculate_cycles_to_failure.__doc__
    assert "non-positive" in doc
    assert "threshold_mpa is not used as a cut-off" in doc


def test_equivalent_stress_difference_limitation_and_counterexample():
    """Arithmetic from ansys/cylinder_results.py:117–120, zero shear case."""
    def equivalent(diagonal):
        x, y, z = diagonal
        return math.sqrt(((x - y)**2 + (y - z)**2 + (z - x)**2) / 2)

    maximum = (100.0, 0.0, 0.0)
    minimum = (-100.0, 0.0, 0.0)
    scalar_difference = equivalent(maximum) - equivalent(minimum)
    component_difference = tuple(a - b for a, b in zip(maximum, minimum))
    assert scalar_difference == 0.0
    assert equivalent(component_difference) == 200.0
    assert equivalent((50.0, 0.0, 0.0)) - equivalent(maximum) == -50.0
    doc = FatiguePostprocessor.generate_stress_range_extraction.__doc__
    output = _fp().generate_stress_range_extraction()
    for text in (doc, output):
        assert "not the von Mises of the stress difference" in text
        assert "underestimate" in text
        assert "negative" in text


@pytest.mark.parametrize("pairs", [None, [(3, 1), (5, 2)]])
def test_extraction_comments_identify_scalar_differences(pairs):
    """SET pairs are synthetic indices; SADD subtracts equivalent scalars.

    Source: generate_stress_range_extraction emits S,EQV tables and SADD,
    not stress-component differences (the counterexample above applies).
    """
    text = _fp().generate_stress_range_extraction(load_step_pairs=pairs)
    comments = [line for line in text.splitlines() if line.startswith("!")]
    assert "! --- Signed Equivalent-Stress Differences ---" in comments
    assert not any("stress range" in line.lower() for line in comments)
    assert any("Signed scalar difference" in line for line in comments)


def test_sn_export_states_parameter_only_limitations():
    """Source: generate_sn_curve_apdl emits parameters, no life calculation.

    The Python evaluator ignores threshold and environment metadata.
    """
    text = _fp().generate_sn_curve_apdl(SNcurve.from_dnv("D"))
    assert "! Parameter definitions only; no fatigue evaluation is performed." in text
    assert "! S_THRESH is metadata; the Python evaluator applies no cut-off." in text
    assert "! Positive stress ranges use finite S-N extrapolation in Python." in text
    assert "! Environment is metadata only; it does not select curve values." in text


def test_w1_life_matches_hand_calculation():
    """Hand calculation using the existing _VERIFIED_AIR_ROWS W1 record.

    Source for that record: DNV-RP-C203 (October 2011), clause 2.4.4,
    Table 2-1. At the synthetic range 100 MPa, log10(S) = 2. Substitution
    of the stored first-segment intercept and slope gives log10(N) = 5.261,
    below the stored transition; hence N = 10**5.261 cycles.
    No curve parameter is inferred or re-transcribed here.
    """
    actual = _fp().calculate_cycles_to_failure(100.0, SNcurve.from_dnv("W1"))
    assert actual == pytest.approx(10**5.261)


def test_below_threshold_life_and_damage_match_hand_calculation():
    """Hand calculation using the existing _VERIFIED_AIR_ROWS D record.

    Source for that record: DNV-RP-C203 (October 2011), clause 2.4.4,
    Table 2-1. Half its stored limit gives the synthetic range 26.315 MPa.
    Substitution into log10(N) = log_a2 - m2*log10(S) gives N rounded
    to the nearest cycle = 319877084. For a synthetic 1000 applied cycles,
    Miner damage = 1000/319877084 (within rounding of the cycle count).
    Thickness correction is off, SCF and DFF are unity by construction.
    This pins existing extrapolation, not a standard's acceptance criterion.
    """
    expected_cycles = 319_877_084  # Rounded hand result derived above.
    sn = SNcurve.from_dnv("D")
    stress = sn.threshold_mpa / 2  # Synthetic half-limit case derived above.
    config = FatigueConfig(
        sn_curve=sn,
        load_cases=[FatigueLoadCase(stress_range_mpa=stress, num_cycles=1000)],
        thickness_correction=False,
        scf=1.0,
        design_fatigue_factor=1.0,
    )
    assert _fp().calculate_cycles_to_failure(stress, sn) == pytest.approx(
        expected_cycles
    )
    assert _fp().calculate_fatigue_damage(config).total_damage == pytest.approx(
        1000 / expected_cycles
    )
