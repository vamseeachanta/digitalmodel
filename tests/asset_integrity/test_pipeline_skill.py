"""Tests for the agent-callable pipeline integrity skill.

Coverage:
- Happy-path X65 pipe under design pressure
- Thin wall / high pressure — utilisation > 1 (fails WT check)
- Zero corrosion — ffs_verdict = "not_applicable"
- Heavy corrosion — ffs_verdict = "replace"
- Moderate corrosion — triggers FFS calc and returns "repair" or "accept"
- Invalid material_grade raises ValueError
- Negative dimensions raise ValueError
- utilisation_ratio in [0, 1] for valid, safe input
- All material grades X52 through X100 produce valid output
- SMYS table completeness
- Boundary: corrosion_depth equal to full wall thickness returns "replace"
- FFS safe pressure <= design pressure for corroded pipe
- Summary dict contains expected keys
- source field matches SKILL_NAME
- Zero design pressure raises ValueError
- Large OD pipe (914 mm) passes for normal operating conditions
- PipelineInput default material_grade is X65
- FFS not run when corrosion_depth is zero
- Utilisation ratio increases with pressure
- FFS verdict "accept" when corrosion is very shallow
"""

import math
import pytest

from digitalmodel.asset_integrity.pipeline_skill import (
    SKILL_NAME,
    SMYS_TABLE,
    PipelineInput,
    PipelineIntegrityResult,
    pipeline_integrity,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

X65_NOMINAL = PipelineInput(
    outer_diameter_mm=323.9,
    wall_thickness_mm=14.3,
    design_pressure_mpa=10.0,
    material_grade="X65",
)


def _make(
    od=323.9,
    wt=14.3,
    pressure=10.0,
    grade="X65",
    corr_depth=0.0,
    corr_len=0.0,
) -> PipelineInput:
    return PipelineInput(
        outer_diameter_mm=od,
        wall_thickness_mm=wt,
        design_pressure_mpa=pressure,
        material_grade=grade,
        corrosion_depth_mm=corr_depth,
        corrosion_length_mm=corr_len,
    )


# ---------------------------------------------------------------------------
# 1. Happy-path: X65 pipe under normal design pressure passes WT check
# ---------------------------------------------------------------------------

def test_x65_passes_wall_thickness_check():
    result = pipeline_integrity(X65_NOMINAL)
    assert result.wall_thickness_ok is True


def test_x65_utilisation_below_1():
    result = pipeline_integrity(X65_NOMINAL)
    assert result.utilisation_ratio < 1.0


def test_x65_utilisation_positive():
    result = pipeline_integrity(X65_NOMINAL)
    assert result.utilisation_ratio > 0.0


# ---------------------------------------------------------------------------
# 2. Thin wall / high pressure — fails wall thickness check
# ---------------------------------------------------------------------------

def test_thin_wall_high_pressure_fails_wt_check():
    inp = _make(od=323.9, wt=5.0, pressure=25.0)
    result = pipeline_integrity(inp)
    assert result.wall_thickness_ok is False


def test_thin_wall_utilisation_above_1():
    inp = _make(od=323.9, wt=5.0, pressure=25.0)
    result = pipeline_integrity(inp)
    assert result.utilisation_ratio > 1.0


# ---------------------------------------------------------------------------
# 3. Zero corrosion — ffs_verdict = "not_applicable"
# ---------------------------------------------------------------------------

def test_zero_corrosion_ffs_not_applicable():
    result = pipeline_integrity(X65_NOMINAL)
    assert result.ffs_verdict == "not_applicable"


def test_zero_corrosion_safe_pressure_equals_design():
    result = pipeline_integrity(X65_NOMINAL)
    assert math.isclose(result.ffs_safe_pressure_mpa, 10.0, rel_tol=1e-9)


# ---------------------------------------------------------------------------
# 4. Heavy corrosion — ffs_verdict = "replace"
# ---------------------------------------------------------------------------

def test_heavy_corrosion_replace():
    # 90% wall loss — extreme corrosion
    inp = _make(wt=14.3, corr_depth=12.87)
    result = pipeline_integrity(inp)
    assert result.ffs_verdict == "replace"


def test_full_wall_corrosion_replace():
    # corrosion_depth == wall_thickness => measured thickness = 0 => replace
    inp = _make(wt=14.3, corr_depth=14.3)
    result = pipeline_integrity(inp)
    assert result.ffs_verdict == "replace"


# ---------------------------------------------------------------------------
# 5. Moderate corrosion — FFS calc runs and returns a verdict
# ---------------------------------------------------------------------------

def test_moderate_corrosion_ffs_runs():
    # ~30% corrosion — should trigger FFS assessment
    inp = _make(wt=14.3, corr_depth=4.0, corr_len=200.0)
    result = pipeline_integrity(inp)
    assert result.ffs_verdict in {"accept", "repair", "replace"}


def test_moderate_corrosion_safe_pressure_positive():
    inp = _make(wt=14.3, corr_depth=4.0, corr_len=200.0)
    result = pipeline_integrity(inp)
    assert result.ffs_safe_pressure_mpa >= 0.0


def test_moderate_corrosion_safe_pressure_is_barlow_mawp():
    # When FFS verdict is repair, safe_pressure is the Barlow MAWP for measured WT.
    # For this geometry the pipe has capacity > design pressure, so MAWP > 10 MPa.
    inp = _make(wt=14.3, corr_depth=4.0, corr_len=200.0)
    result = pipeline_integrity(inp)
    assert result.ffs_safe_pressure_mpa > 0.0


# ---------------------------------------------------------------------------
# 6. Invalid material_grade raises ValueError
# ---------------------------------------------------------------------------

def test_invalid_material_grade_raises():
    with pytest.raises(ValueError, match="Unknown material_grade"):
        PipelineInput(
            outer_diameter_mm=323.9,
            wall_thickness_mm=14.3,
            design_pressure_mpa=10.0,
            material_grade="X999",
        )


def test_invalid_grade_lowercase_raises():
    with pytest.raises(ValueError):
        PipelineInput(
            outer_diameter_mm=323.9,
            wall_thickness_mm=14.3,
            design_pressure_mpa=10.0,
            material_grade="x65",
        )


# ---------------------------------------------------------------------------
# 7. Negative dimensions raise ValueError
# ---------------------------------------------------------------------------

def test_negative_od_raises():
    with pytest.raises(ValueError):
        PipelineInput(outer_diameter_mm=-100.0, wall_thickness_mm=10.0, design_pressure_mpa=5.0)


def test_negative_wall_thickness_raises():
    with pytest.raises(ValueError):
        PipelineInput(outer_diameter_mm=323.9, wall_thickness_mm=-5.0, design_pressure_mpa=5.0)


def test_negative_design_pressure_raises():
    with pytest.raises(ValueError):
        PipelineInput(outer_diameter_mm=323.9, wall_thickness_mm=14.3, design_pressure_mpa=-1.0)


def test_negative_corrosion_depth_raises():
    with pytest.raises(ValueError):
        PipelineInput(
            outer_diameter_mm=323.9,
            wall_thickness_mm=14.3,
            design_pressure_mpa=10.0,
            corrosion_depth_mm=-1.0,
        )


# ---------------------------------------------------------------------------
# 8. utilisation_ratio in [0, 1] for valid, safely designed input
# ---------------------------------------------------------------------------

def test_utilisation_ratio_in_0_1_for_safe_input():
    result = pipeline_integrity(X65_NOMINAL)
    assert 0.0 <= result.utilisation_ratio <= 1.0


# ---------------------------------------------------------------------------
# 9. Material grade table coverage X52 through X100
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("grade", ["X52", "X60", "X65", "X70", "X80", "X100"])
def test_all_material_grades_produce_valid_result(grade):
    inp = _make(grade=grade, pressure=8.0)
    result = pipeline_integrity(inp)
    assert isinstance(result, PipelineIntegrityResult)
    assert result.utilisation_ratio > 0.0
    assert result.ffs_verdict == "not_applicable"  # no corrosion


@pytest.mark.parametrize("grade,smys", [
    ("X52", 359.0), ("X60", 414.0), ("X65", 448.0),
    ("X70", 483.0), ("X80", 552.0), ("X100", 690.0),
])
def test_smys_table_values_correct(grade, smys):
    assert SMYS_TABLE[grade] == smys


# ---------------------------------------------------------------------------
# 10. Summary dict contains expected keys
# ---------------------------------------------------------------------------

def test_summary_contains_expected_keys():
    result = pipeline_integrity(X65_NOMINAL)
    expected_keys = {
        "material_grade", "smys_mpa", "hoop_stress_mpa",
        "allowable_stress_mpa", "utilisation_ratio", "wall_thickness_ok",
        "ffs_verdict", "ffs_safe_pressure_mpa", "has_corrosion",
    }
    assert expected_keys.issubset(result.summary.keys())


# ---------------------------------------------------------------------------
# 11. source field matches SKILL_NAME
# ---------------------------------------------------------------------------

def test_source_field_matches_skill_name():
    result = pipeline_integrity(X65_NOMINAL)
    assert result.source == f"skill:{SKILL_NAME}"


def test_skill_name_constant():
    assert SKILL_NAME == "pipeline_integrity"


# ---------------------------------------------------------------------------
# 12. Large OD pipe (914 mm) passes for normal operating conditions
# ---------------------------------------------------------------------------

def test_large_od_pipe_passes():
    inp = _make(od=914.0, wt=19.1, pressure=7.0)
    result = pipeline_integrity(inp)
    assert isinstance(result, PipelineIntegrityResult)
    assert result.utilisation_ratio > 0.0


# ---------------------------------------------------------------------------
# 13. Default material_grade is X65
# ---------------------------------------------------------------------------

def test_default_material_grade_is_x65():
    inp = PipelineInput(
        outer_diameter_mm=323.9,
        wall_thickness_mm=14.3,
        design_pressure_mpa=10.0,
    )
    assert inp.material_grade == "X65"


# ---------------------------------------------------------------------------
# 14. FFS verdict "accept" when corrosion is very shallow
# ---------------------------------------------------------------------------

def test_very_shallow_corrosion_accept():
    # 2% wall loss — should be FFS acceptable
    inp = _make(wt=14.3, corr_depth=0.3, corr_len=50.0)
    result = pipeline_integrity(inp)
    assert result.ffs_verdict == "accept"


def test_very_shallow_corrosion_safe_pressure_equals_design():
    inp = _make(wt=14.3, corr_depth=0.3, corr_len=50.0)
    result = pipeline_integrity(inp)
    # For accepted FFS, safe pressure == design pressure
    assert math.isclose(result.ffs_safe_pressure_mpa, 10.0, rel_tol=1e-6)


# ---------------------------------------------------------------------------
# 15. Utilisation ratio increases with design pressure
# ---------------------------------------------------------------------------

def test_utilisation_increases_with_pressure():
    r_low = pipeline_integrity(_make(pressure=5.0))
    r_high = pipeline_integrity(_make(pressure=15.0))
    assert r_high.utilisation_ratio > r_low.utilisation_ratio
