"""Hand-calculated geometry checks; no API acceptance limits are assumed."""
import copy
import json
import math
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

from digitalmodel.asset_integrity.tank_settlement import (
    reduce_shell_survey, reduce_bottom_profile, reduce_edge_settlement,
)
from digitalmodel.asset_integrity.tank_settlement_workflow import assess, run_workflow


@pytest.fixture
def survey():
    """z=10+3*cos(theta)+4*sin(theta)+2*cos(2*theta), eight stations.

    Discrete orthogonality gives a=10,b=3,c=4, amplitude=5 mm;
    residuals=[2,0,-2,0,2,0,-2,0]. Sum residual squares=16;
    total sum squares=116, hence R2=100/116. Diameter=10 m gives
    slope=(2*5)/(10*1000)=0.001, tilt=atan(0.001).
    All dimensions are synthetic choices, not standard limits.
    """
    angles = list(range(0, 360, 45))
    return angles, [10 + 3*math.cos(math.radians(t))
                    + 4*math.sin(math.radians(t))
                    + 2*math.cos(2*math.radians(t)) for t in angles]


@pytest.fixture
def block(survey):
    angles, elevations = survey
    return {
        "tank": {"diameter_m": 10, "height_m": 12, "fill_height_m": 8,
                 "course_thicknesses_mm": [8, 6], "material_grade": "unspecified"},
        "shell_survey": {"angles_deg": angles, "elevations_mm": elevations},
        "edge": {"radial_extent_m": 2, "shell_elevation_mm": -12,
                 "reference_elevation_mm": -2},
        "bottom_profile": {"distances_m": [0, 1, 3, 4],
                           "elevations_mm": [0, -4, 1, 4]},
    }


def test_harmonic_reduction(survey):
    angles, elevations = survey
    result = reduce_shell_survey(10, angles, elevations)
    assert result["coefficients_mm"] == pytest.approx([10, 3, 4])
    assert result["residuals_mm"] == pytest.approx([2, 0, -2, 0, 2, 0, -2, 0], abs=1e-12)
    assert result["amplitude_mm"] == pytest.approx(5)
    assert result["diametral_tilt_mm"] == pytest.approx(10)
    assert result["tilt_slope"] == pytest.approx(0.001)
    assert result["r_squared"] == pytest.approx(100/116)
    assert result["high_point_azimuth_deg"] == pytest.approx(math.degrees(math.atan2(4, 3)))


def test_shift_permutation_and_rotation(survey):
    """A constant datum shift changes a only; angle rotation preserves amplitude."""
    angles, elevations = survey
    result = reduce_shell_survey(10, list(reversed(angles)),
                                 [z+100 for z in reversed(elevations)])
    assert result["coefficients_mm"] == pytest.approx([110, 3, 4])
    rotated = reduce_shell_survey(10, [(a+17) % 360 for a in angles], elevations)
    assert rotated["amplitude_mm"] == pytest.approx(5)


def test_irregular_angles():
    """Exact z=7+2cos(theta)-sin(theta) at uneven angles has zero residual."""
    angles = [0, 20, 110, 190, 310]
    z = [7+2*math.cos(math.radians(t))-math.sin(math.radians(t)) for t in angles]
    result = reduce_shell_survey(10, angles, z)
    assert result["coefficients_mm"] == pytest.approx([7, 2, -1])
    assert result["residuals_mm"] == pytest.approx([0]*5, abs=1e-12)


def test_uniform_survey():
    """Equal elevations carry no variation; R2 and high-point azimuth are undefined."""
    result = reduce_shell_survey(10, [0, 90, 180, 270], [25]*4)
    assert result["amplitude_mm"] == pytest.approx(0, abs=1e-12)
    assert result["r_squared"] is None
    assert result["high_point_azimuth_deg"] is None


def test_edge_and_bottom_geometry():
    """Edge: (-2)-(-12)=10 mm over 2 m -> 0.005 slope.

    Bottom chord through (0,0),(4,4) is z=x in mm for x in m;
    z-chord=[0,-5,-2,0] -> maximum depression=5, bulge=0 mm.
    """
    edge = reduce_edge_settlement(2, -12, -2)
    assert edge["drop_mm"] == 10
    assert edge["slope"] == pytest.approx(0.005)
    bottom = reduce_bottom_profile([0, 1, 3, 4], [0, -4, 1, 4])
    assert bottom["chord_deviations_mm"] == pytest.approx([0, -5, -2, 0])
    assert bottom["maximum_depression_mm"] == 5
    assert bottom["maximum_bulge_mm"] == 0


@pytest.mark.parametrize("angles,z", [([], []), ([0, 90, 180], [1, 2, 3]),
    ([0, 90, 180, 360], [1]*4), ([0, 0, 90, 180], [1]*4),
    ([0, 90, 180, 270], [1, 2]), ([0, 90, 180, float('nan')], [1]*4),
    ([0, 90, 180, 270], [True, 1, 2, 3]), ([0, 90, 180, 270], [1, 2, 3, float('inf')])])
def test_invalid_shell(angles, z):
    with pytest.raises(ValueError):
        reduce_shell_survey(10, angles, z)


@pytest.mark.parametrize("diameter", [0, -1, True, float('inf')])
def test_invalid_diameter(diameter):
    with pytest.raises(ValueError):
        reduce_shell_survey(diameter, [0, 90, 180, 270], [0]*4)


@pytest.mark.parametrize("x,z", [([0, 1, 1], [0]*3), ([0, 2, 1], [0]*3),
    ([0, 1], [0, 1]), ([0, 1, 2], [0, float('nan'), 0])])
def test_invalid_bottom(x, z):
    with pytest.raises(ValueError):
        reduce_bottom_profile(x, z)


def test_fail_closed_assessment(block):
    result = assess(block)
    assert set(result["checks"]) == {"planar_tilt", "out_of_plane", "edge", "bottom"}
    assert all(c["status"] == "NOT_EVALUATED" for c in result["checks"].values())
    assert all(c["allowable"] is None for c in result["checks"].values())
    assert result["decision"]["verdict"] == "ESCALATE"
    assert not result["decision"]["applicability"]["ok"]
    assert result["action"] == "ESCALATE-TO-LEVEL-3"
    json.dumps(result, allow_nan=False)


def test_missing_profiles_not_zero(block):
    del block["edge"]
    del block["bottom_profile"]
    result = assess(block)
    assert result["geometry"]["edge"] is None
    assert "MISSING_EDGE_SURVEY" in result["decision"]["applicability"]["flags"]
    assert "MISSING_BOTTOM_SURVEY" in result["decision"]["applicability"]["flags"]


@pytest.mark.parametrize("field,value", [("height_m", 0), ("fill_height_m", 13),
    ("course_thicknesses_mm", []), ("course_thicknesses_mm", [True]),
    ("material_grade", " ")])
def test_invalid_context(block, field, value):
    block["tank"][field] = value
    with pytest.raises(ValueError):
        assess(block)


def test_report_and_workflow(block, tmp_path):
    block["component_id"] = "<tank>"
    cfg = {"basename": "tank_settlement_screen", "tank_settlement": block}
    result = run_workflow(copy.deepcopy(cfg), tmp_path)
    saved = json.loads((tmp_path / "settlement-summary.json").read_text())
    assert saved == result
    report = (tmp_path / "settlement-report.html").read_text()
    assert "&lt;tank&gt;" in report and "<tank>" not in report
    assert "Revision history" in report
    assert "NOT_EVALUATED" in report
    assert "Settlement screening analysis" in report


def test_workflow_cli_and_registry(tmp_path):
    root = Path(__file__).resolve().parents[2]
    registry = yaml.safe_load((root / "docs/registry/workflows.yaml").read_text())
    row = next(r for r in registry["workflows"] if r["id"] == "api653-tank-settlement-screen")
    completed = subprocess.run(
        [sys.executable, "-m", "digitalmodel.asset_integrity.tank_settlement_workflow",
         str(root / row["input"]), "--output-dir", str(tmp_path)],
        capture_output=True, text=True, check=False,
    )
    assert completed.returncode == 0, completed.stderr
    result = json.loads((tmp_path / "settlement-summary.json").read_text())
    assert result["geometry"]["shell"]["coefficients_mm"] == pytest.approx([10, 3, 4])
    assert result["decision"]["verdict"] == "ESCALATE"
    assert (tmp_path / "settlement-report.html").exists()


@pytest.mark.parametrize("extent,shell,reference", [(0, 1, 2), (-1, 1, 2),
    (True, 1, 2), (1, float('nan'), 0), (1, 0, float('inf'))])
def test_invalid_edge(extent, shell, reference):
    with pytest.raises(ValueError):
        reduce_edge_settlement(extent, shell, reference)


def test_bulge_and_upward_edge():
    """Chord is flat; a +3 mm midpoint is a 3 mm bulge, not depression.

    Shell at +4 mm above zero reference gives drop=-4 mm, not abs(4).
    """
    result = reduce_bottom_profile([0, 1, 2], [0, 3, 0])
    assert result["maximum_bulge_mm"] == 3
    assert result["maximum_depression_mm"] == 0
    assert reduce_edge_settlement(1, 4, 0)["drop_mm"] == -4


def test_preserve_profile_evidence(block):
    result = assess(block)
    assert result["edge_survey"] == block["edge"]
    assert result["bottom_profile"] == block["bottom_profile"]
    assert all(s["url"].startswith("https://") for s in result["sources"])
