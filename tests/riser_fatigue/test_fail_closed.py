"""Regression probes for incomplete and invalid fatigue evidence."""

from dataclasses import replace
import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.fatigue.damage import miner_damage, thickness_correction
from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.riser_fatigue.touchdown import (
    RiserSection, TouchdownFatigueInput, assess_touchdown_fatigue, report_markdown,
)
from digitalmodel.riser_fatigue.workflow import (
    _curve_settings, _evaluate_segment, _fraction, _fatigue_life_years,
    _safe_divide, router,
)


@pytest.fixture
def touch():
    return TouchdownFatigueInput(
        RiserSection(300, 20), [100], [1e6], sn_class="F1",
        environment="seawater_cp", scf=1, design_life_years=25, dff=10,
        histogram_period_years=1,
        scf_basis="Synthetic hot-spot stress includes concentration",
        sn_basis="Synthetic F1 detail with specified protection",
        histogram_basis={"period_years": 1.0, "evidence": "Synthetic annual cycle counts"},
    )


@pytest.fixture
def segment():
    return {
        "outer_diameter_mm": 300, "wall_thickness_mm": 20,
        "wave": {"stress_ranges_MPa": [100], "cycles": [1], "scf": 1,
                 "histogram_period_years": 1,
                 "scf_basis": "Synthetic hot-spot stress includes concentration",
                 "histogram_basis": {"period_years": 1.0, "evidence": "Synthetic annual cycle counts"}},
        "viv_cases": [{"stress_range_MPa": 100, "frequency_hz": 1,
                       "exposure_fraction": 1}],
    }


def evaluate(segment):
    return _evaluate_segment(segment, 25, 10, "F1", "seawater_cp")


@pytest.mark.parametrize("value", ["missing", None])
def test_RF01_missing_wave_rejected(segment, value):
    segment["viv_cases"][0].update(stress_range_MPa=1, exposure_fraction=.001)
    segment["wave"]["cycles"] = [1e6]
    assert evaluate(segment)[1]["passes_dff"] is False
    if value == "missing":
        segment.pop("wave")
    else:
        segment["wave"] = value
    with pytest.raises(ValueError, match="wave"):
        evaluate(segment)


@pytest.mark.parametrize("value", ["missing", None, [], 0])
def test_RF02_missing_viv_rejected(segment, value):
    if value == "missing":
        segment.pop("viv_cases")
    else:
        segment["viv_cases"] = value
    with pytest.raises(ValueError, match="VIV|viv_cases"):
        evaluate(segment)


@pytest.mark.parametrize("stress,cycles", [
    ([math.nan], [1e6]), ([None], [1e6]), ([100], [math.nan]),
    ([100], [None]), ([-100], [1e6]), ([100], [-1e6]),
    ([0], [0]), ([0], [1e6]), ([100], [0]),
])
def test_RF03_invalid_histogram_rejected(touch, segment, stress, cycles):
    with pytest.raises(ValueError):
        assess_touchdown_fatigue(replace(touch, stress_ranges_mpa=stress, cycles=cycles))
    with pytest.raises(ValueError):
        miner_damage(pd.DataFrame({"stress_range": stress, "cycles": cycles}),
                     get_sn_curve("F1", "seawater_cp"))
    segment["wave"].update(stress_ranges_MPa=stress, cycles=cycles)
    with pytest.raises(ValueError):
        evaluate(segment)


def test_RF04_empty_histogram_rejected(touch):
    with pytest.raises(ValueError, match="non-empty"):
        assess_touchdown_fatigue(replace(touch, stress_ranges_mpa=[], cycles=[]))


@pytest.mark.parametrize("scf", [0, -1, math.nan, math.inf, None])
def test_RF05_scf_requires_positive_finite_value(touch, scf):
    with pytest.raises(ValueError, match="scf"):
        assess_touchdown_fatigue(replace(touch, scf=scf))


@pytest.mark.parametrize("period", [math.inf, math.nan, 0, -1, None])
def test_RF06_histogram_period_requires_finite_basis(touch, period):
    with pytest.raises(ValueError, match="histogram_period_years"):
        assess_touchdown_fatigue(replace(touch, histogram_period_years=period))


@pytest.mark.parametrize("life", [0, -1, math.nan, math.inf, None])
def test_RF07_direct_design_life_rejected(touch, life):
    with pytest.raises(ValueError, match="design_life_years"):
        assess_touchdown_fatigue(replace(touch, design_life_years=life))


@pytest.mark.parametrize("name,value", [
    ("design_life_years", math.nan), ("design_life_years", math.inf),
    ("dff", math.nan), ("dff", math.inf),
])
def test_RF08_nonfinite_controls_rejected(segment, tmp_path, name, value):
    segment.pop("wave")
    segment["wave_exclusion"] = {"reason": "Isolated VIV probe", "evidence": "Synthetic scope"}
    segment["viv_cases"][0]["stress_range_MPa"] = math.nan
    settings = {"design_life_years": 25, "dff": 10,
                "sn_curve": {"curve": "F1", "environment": "seawater_cp",
                             "basis": "Synthetic F1 detail with specified protection"},
                "output_dir": str(tmp_path), "segments": [segment]}
    settings[name] = value
    with pytest.raises(ValueError, match=name):
        router({"riser_fatigue": settings})


@pytest.mark.parametrize("name", ["stress_range_MPa", "frequency_hz"])
@pytest.mark.parametrize("value", [math.nan, math.inf, None])
def test_RF09_unknown_viv_bin_rejected(segment, name, value):
    segment["viv_cases"][0][name] = value
    with pytest.raises(ValueError):
        evaluate(segment)


@pytest.mark.parametrize("overrides", [
    {"section": RiserSection(300, math.nan)},
    {"section": RiserSection(300, 0)}, {"section": RiserSection(300, -1)},
    {"thickness_exponent": math.nan}, {"thickness_exponent": -1},
    {"thickness_ref_mm": math.inf}, {"thickness_ref_mm": 0},
])
def test_RF10_thickness_inputs_rejected(touch, overrides):
    with pytest.raises(ValueError):
        assess_touchdown_fatigue(replace(touch, **overrides))


@pytest.mark.parametrize("actual,ref,k", [
    (0, 25, .25), (-1, 25, .25), (math.nan, 25, .25),
    (20, math.inf, .25), (20, 0, .25), (50, 25, -1), (20, 25, math.nan),
])
def test_RF10_core_thickness_inputs_rejected(actual, ref, k):
    with pytest.raises(ValueError):
        thickness_correction([100], actual, ref, k)


@pytest.mark.parametrize("source", [{}, {"sn_curve": "free_corrosion"},
                                     {"sn_curve": {}},
                                     {"sn_curve": {"curve": "F1"}}])
def test_RF11_curve_selection_requires_evidence(source):
    with pytest.raises(ValueError, match="sn_curve"):
        _curve_settings(source)


def test_RF12_package_controls_unset():
    path = (Path(__file__).resolve().parents[2] / "src" / "digitalmodel" /
            "base_configs" / "modules" / "riser_fatigue" / "riser_fatigue.yml")
    settings = yaml.safe_load(path.read_text(encoding="utf-8"))["riser_fatigue"]
    assert settings.get("design_life_years") is None
    assert settings.get("dff") is None
    assert settings.get("sn_curve") is None
    with pytest.raises(ValueError, match="design_life_years"):
        router({"riser_fatigue": settings})


@pytest.mark.parametrize("aliases", [
    {"exposure_fraction": .001, "exposure_hours_per_year": 8766},
    {"stress_range_MPa": 1, "stress_range": 100},
    {"frequency_hz": .01, "frequency": 1},
])
def test_RF13_conflicting_aliases_rejected(segment, aliases):
    segment["viv_cases"][0].update(aliases)
    with pytest.raises(ValueError, match="exactly one"):
        evaluate(segment)


@pytest.mark.parametrize("diameter", [0, -1, math.nan, math.inf, 40])
def test_RF14_geometry_rejected(touch, diameter):
    with pytest.raises(ValueError, match="diameter|geometry"):
        assess_touchdown_fatigue(replace(touch, section=RiserSection(diameter, 20)))


@pytest.mark.parametrize("contribution,key", [("wave", "wave"), ("viv", "viv_cases")])
def test_explicit_exclusion_retains_basis(segment, contribution, key):
    segment.pop(key)
    evidence = {"reason": "Isolated contribution study", "evidence": "Synthetic fixture scope"}
    segment[contribution + "_exclusion"] = evidence
    _, summary = evaluate(segment)
    assert summary[contribution + "_status"] == "not_applicable"
    assert summary[contribution + "_exclusion_reason"] == evidence["reason"]
    assert summary[contribution + "_exclusion_evidence"] == evidence["evidence"]
    assert summary[contribution + "_damage"] == 0
    assert summary["total_damage"] > 0


@pytest.mark.parametrize("exclusion", [None, {}, {"reason": ""},
                                     {"reason": "No loading", "evidence": " "}])
def test_RF01_exclusion_requires_substantiation(segment, exclusion):
    segment.pop("wave")
    segment["wave_exclusion"] = exclusion
    with pytest.raises(ValueError, match="wave_exclusion"):
        evaluate(segment)


def test_RF02_exclusion_cannot_override_loading(segment):
    segment["viv_exclusion"] = {"reason": "Excluded", "evidence": "Synthetic scope"}
    with pytest.raises(ValueError, match="cannot both"):
        evaluate(segment)


def test_no_assessment_from_two_exclusions(segment):
    for key, contribution in [("wave", "wave"), ("viv_cases", "viv")]:
        segment.pop(key)
        segment[contribution + "_exclusion"] = {"reason": "Excluded", "evidence": "Synthetic scope"}
    with pytest.raises(ValueError, match="neither"):
        evaluate(segment)


@pytest.mark.parametrize("stress,cycles", [([100, math.nan], [1, 1]),
                                          ([100, 100], [1, None])])
def test_RF03_unknown_bin_cannot_disappear(touch, stress, cycles):
    with pytest.raises(ValueError, match="finite"):
        assess_touchdown_fatigue(replace(touch, stress_ranges_mpa=stress, cycles=cycles))


def test_known_empty_bin_preserves_loaded_bin_damage(touch):
    base = assess_touchdown_fatigue(touch)
    combined = assess_touchdown_fatigue(replace(touch, stress_ranges_mpa=[0, 100], cycles=[0, 1e6]))
    assert combined.period_damage == base.period_damage


def test_RF05_missing_scf_and_unity_basis_rejected(touch, segment):
    with pytest.raises(ValueError, match="scf_basis"):
        assess_touchdown_fatigue(replace(touch, scf_basis=None))
    segment["wave"].pop("scf")
    with pytest.raises(ValueError, match="scf"):
        evaluate(segment)


def test_RF06_duration_requires_evidence(touch, segment):
    with pytest.raises(ValueError, match="histogram_basis"):
        assess_touchdown_fatigue(replace(touch, histogram_basis=None))
    segment["wave"]["histogram_period_years"] = math.inf
    with pytest.raises(ValueError, match="histogram_period_years"):
        evaluate(segment)


@pytest.mark.parametrize("dff", [math.nan, math.inf, 0, -1, None])
def test_RF08_direct_dff_rejected(touch, dff):
    with pytest.raises(ValueError, match="dff"):
        assess_touchdown_fatigue(replace(touch, dff=dff))


@pytest.mark.parametrize("damage", [0, -1, math.nan, math.inf])
def test_RF08_unqualified_damage_cannot_imply_infinite_life(damage):
    with pytest.raises(ValueError):
        _fatigue_life_years(25, damage)


def test_RF08_required_life_validated_before_division():
    with pytest.raises(ValueError, match="fatigue_life_years|required_life_years"):
        _safe_divide(math.inf, math.nan)


def test_RF08_required_life_product_overflow_rejected(segment):
    with pytest.raises(ValueError, match="required_life_years"):
        _evaluate_segment(segment, 1e308, 1e308, "F1", "seawater_cp")


def test_RF09_cycle_overflow_rejected(segment):
    segment["viv_cases"][0]["frequency_hz"] = 1e308
    with pytest.raises(ValueError, match="VIV cycles"):
        evaluate(segment)


@pytest.mark.parametrize("changes", [{"thickness_exponent": .1},
                                     {"thickness_exponent": .3},
                                     {"thickness_ref_mm": 50}])
def test_RF10_penalty_override_requires_basis(touch, changes):
    with pytest.raises(ValueError, match="thickness"):
        assess_touchdown_fatigue(replace(touch, **changes))


def test_RF10_workflow_override_is_validated(segment):
    segment["wave"]["thickness_exponent"] = -1
    with pytest.raises(ValueError, match="thickness_exponent"):
        evaluate(segment)


@pytest.mark.parametrize("changes", [{"sn_class": None}, {"environment": None},
                                     {"sn_basis": None}])
def test_RF11_direct_selection_requires_basis(touch, changes):
    with pytest.raises(ValueError, match="sn_class|environment|sn_basis"):
        assess_touchdown_fatigue(replace(touch, **changes))


def test_RF11_protection_is_not_inferred(touch):
    histogram = replace(touch, stress_ranges_mpa=[20], cycles=[1e5])
    protected = assess_touchdown_fatigue(histogram)
    unprotected = assess_touchdown_fatigue(replace(histogram, environment="free_corrosion"))
    assert protected.pass_fail == "PASS"
    assert unprotected.pass_fail == "FAIL"
    with pytest.raises(ValueError, match="basis"):
        _curve_settings({"sn_curve": {"curve": "F1", "environment": "seawater_cp"}})


def test_RF12_null_dff_cannot_resolve_to_default():
    from digitalmodel.riser_fatigue.workflow import resolve_dff

    with pytest.raises(ValueError, match="dff"):
        resolve_dff({"dff": None})


def test_RF13_duplicate_equivalent_aliases_also_rejected():
    with pytest.raises(ValueError, match="exactly one"):
        _fraction({"exposure_fraction": 1, "exposure_hours_per_year": 8766})


def test_RF14_diameter_is_informational_for_supplied_stress(touch):
    result = assess_touchdown_fatigue(touch)
    larger = assess_touchdown_fatigue(replace(touch, section=RiserSection(400, 20)))
    assert result.period_damage == larger.period_damage
    report = report_markdown(touch, result)
    assert "Outer diameter is informational" in report
    assert "PASS requires design-life damage × DFF ≤ 1" in report
    assert report.index("## Summary") < report.index("## Revision history") < report.index("## Inputs")


@pytest.mark.parametrize("allowable", [math.nan, -1, 0, math.inf])
def test_RF03_invalid_allowable_cannot_hide_a_bin(allowable):
    class InvalidCurve:
        def cycles(self, stress):
            return [1, allowable]

    with pytest.raises(ValueError, match="allowable"):
        miner_damage(pd.DataFrame({"stress_range": [100, 100], "cycles": [1, 1]}), InvalidCurve())


@pytest.mark.parametrize("stress", [True, 1j])
def test_RF03_nonphysical_scalar_not_coerced(touch, stress):
    with pytest.raises(ValueError, match="histogram"):
        assess_touchdown_fatigue(replace(touch, stress_ranges_mpa=[stress]))


@pytest.mark.parametrize("basis", [
    {"period_years": 2, "evidence": "Two-year cycle counts"},
    {"period_years": math.inf, "evidence": "Unknown duration"},
    {"period_years": 1, "evidence": ""}, "Annual", {},
])
def test_RF06_histogram_duration_must_match_basis(touch, segment, basis):
    with pytest.raises(ValueError, match="histogram"):
        assess_touchdown_fatigue(replace(touch, histogram_basis=basis))
    segment["wave"]["histogram_basis"] = basis
    with pytest.raises(ValueError, match="histogram"):
        evaluate(segment)


def test_RF05_unity_is_an_evidenced_choice(touch):
    histogram = replace(touch, cycles=[100])
    assert assess_touchdown_fatigue(histogram).pass_fail == "PASS"
    assert assess_touchdown_fatigue(replace(histogram, scf=2)).pass_fail == "FAIL"
    with pytest.raises(ValueError, match="scf"):
        assess_touchdown_fatigue(replace(histogram, scf=None))


def test_RF10_reversed_thickness_penalty_cannot_pass(touch):
    histogram = replace(touch, section=RiserSection(300, 50), cycles=[1000])
    assert assess_touchdown_fatigue(histogram).pass_fail == "FAIL"
    with pytest.raises(ValueError, match="thickness_exponent"):
        assess_touchdown_fatigue(replace(histogram, thickness_exponent=-1))
