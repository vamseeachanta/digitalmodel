# ABOUTME: Tests the issue #1915 wall-thickness, tension, and moment data sweep.
# ABOUTME: Verifies tidy rows, capacity-derived axes, and JSON-only output.

import importlib.util
import math
import sys

import pytest
from pathlib import Path

from digitalmodel.structural.analysis.wall_thickness import DesignCode


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "scripts" / "capabilities" / "build_wall_thickness_3d.py"


def _load_module():
    spec = importlib.util.spec_from_file_location("build_wall_thickness_3d", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_single_grid_point_emits_one_flat_row_per_design_code():
    generator = _load_module()

    study = generator.build_study(
        wall_thickness_mm=(18.0,),
        effective_tension_n=(0.0,),
        bending_moment_nm=(0.0,),
    )

    assert len(study["rows"]) == len(DesignCode) == 9
    assert {row["code"] for row in study["rows"]} == {
        code.value for code in DesignCode
    }
    for row in study["rows"]:
        assert row["wall_thickness_mm"] == 18.0
        assert row["effective_tension_n"] == 0.0
        assert row["bending_moment_nm"] == 0.0
        assert row["governing_check"]
        assert isinstance(row["utilisation"], float)
        assert all(not isinstance(value, (dict, list)) for value in row.values())


def test_default_metadata_records_capacity_derived_ranges_and_grid():
    generator = _load_module()

    meta = generator.build_metadata()
    basis = meta["load_range_basis"]

    expected_area_m2 = math.pi / 4 * (0.3239**2 - (0.3239 - 2 * 0.018) ** 2)
    expected_yield_tension_n = 448.0e6 * expected_area_m2
    expected_yield_moment_nm = (
        math.pi / 4 * 448.0e6 * (0.3239 - 0.018) ** 2 * 0.018
    )
    assert basis["reference_wall_thickness_mm"] == 18.0
    assert basis["reference_area_m2"] == expected_area_m2
    assert basis["tension"]["yield_capacity_n"] == expected_yield_tension_n
    assert basis["tension"]["maximum_n"] == 4_649_766
    assert basis["bending_moment"]["yield_capacity_nm"] == expected_yield_moment_nm
    assert basis["bending_moment"]["maximum_nm"] == 355_591

    grid = meta["grid"]
    assert grid["wall_thickness_mm"]["values"] == [
        8.0 + 0.5 * index for index in range(41)
    ]
    assert grid["effective_tension_n"]["values"][0] == 0.0
    assert grid["effective_tension_n"]["values"][-1] == 4_649_766
    assert grid["bending_moment_nm"]["values"][0] == 0.0
    assert grid["bending_moment_nm"]["values"][-1] == 355_591
    assert grid["row_count"] == 41 * 31 * 31 * 9 == 354_609
    assert meta["units"]["wall_thickness"] == "mm"


@pytest.mark.parametrize(
    "axis,values",
    [
        ("wall_thickness_mm", ()),
        ("wall_thickness_mm", (0.0,)),
        ("wall_thickness_mm", (200.0,)),
        ("wall_thickness_mm", (18.0, 18.0)),
        ("effective_tension_n", (float("nan"),)),
        ("effective_tension_n", (-1.0,)),
        ("effective_tension_n", (2.0, 1.0)),
        ("bending_moment_nm", (float("inf"),)),
        ("bending_moment_nm", (True,)),
    ],
)
def test_invalid_axes_are_rejected_before_analysis(axis, values, monkeypatch):
    """Synthetic invalid inputs; no engineering acceptance values are used."""
    generator = _load_module()

    def unexpected_analysis(*args, **kwargs):
        pytest.fail("axis validation must precede any analysis")

    monkeypatch.setattr(generator, "_analyze_point", unexpected_analysis)
    with pytest.raises(ValueError, match=axis):
        generator.build_study(**{axis: values})


@pytest.mark.parametrize("codes", [(), ("API-STD-2RD",), (DesignCode.API_STD_2RD,) * 2])
def test_invalid_code_axes_are_rejected(codes):
    generator = _load_module()
    with pytest.raises(ValueError, match="codes"):
        generator.build_metadata(codes=codes)


def test_study_carries_sources_and_explicit_input_qualification():
    meta = _load_module().build_metadata()
    assert meta["qualification"]["status"] == "NOT_EVALUATED"
    assert "assumed, based on previous experience" in meta["qualification"]["inputs"]
    assert meta["sources"]["interaction"]["edition"] == "2013"
    assert "api_std_2rd.py" in meta["sources"]["interaction"]["implementation"]
    assert "sampling choice" in meta["load_range_basis"]["qualification"]


@pytest.mark.parametrize(
    "tension,moment,expected,governing",
    [
        (0.0, 0.0, 0.0, "collapse"),
        (4_649_766.0, 0.0, 0.7727464724920794, "combined"),
        (0.0, 355_591.0, 0.7727467879224368, "combined"),
        (4_649_766.0, 355_591.0, 1.545493260414516, "combined"),
    ],
)
def test_combined_corners_against_independent_hand_calculation(
    tension, moment, expected, governing
):
    """Independent arithmetic for the inherited Method 1 implementation.

    Source: API STD 2RD (2013), Method 1, as documented in repository
    src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py,
    _check_combined, _check_burst and _check_collapse. This compares the
    implementation, not compliance with an independently opened standard.
    All geometry, material and pressure inputs are assumed, based on previous
    experience, and match build_wall_thickness_explorer.py's reference case.

    Annular area by integration: A = pi*t*(D-t) = 0.017298237469196117 m2.
    Ty = A*448e6 = 7749610.386199861 N.
    My = pi/4*448e6*(0.3239-0.018)^2*0.018 = 592651.4542846344 Nm.
    pb = 0.45*(448e6+530.9e6)*ln(0.3239/0.2879) = 51901016.488449834 Pa.
    Fc = sqrt(0.8^2-(10e6/pb)^2) = 0.7764512572463445.
    u = (T/Ty + M/My)/Fc gives the four tabulated expectations above.
    Collapse u = 10e6/(0.6*pc) = 0.396987384587297, where
    pel = 2*207e9*(0.018/0.3239)^3/(1-0.3^2), py = 2*448e6*0.018/0.3239,
    pc = pel*py/sqrt(pel^2+py^2). Burst u = 10e6/(0.8*pb) = 0.2408430672.
    The maximum is collapse at zero loads, combined at every loaded corner.
    Rounding tolerance is half the generator's six-decimal output quantum.
    """
    row = _load_module().build_study(
        wall_thickness_mm=(18.0,),
        effective_tension_n=(tension,),
        bending_moment_nm=(moment,),
        codes=(DesignCode.API_STD_2RD,),
    )["rows"][0]
    assert row["combined_utilisation"] == pytest.approx(expected, abs=0.5e-6)
    assert row["governing_check"] == governing
    assert row["utilisation"] == pytest.approx(max(expected, 0.396987384587297), abs=0.5e-6)


def test_generator_cli_writes_to_the_requested_directory(tmp_path, monkeypatch):
    generator = _load_module()
    study = generator.build_study((18.0,), (0.0,), (0.0,), (DesignCode.API_STD_2RD,))
    monkeypatch.setattr(generator, "build_study", lambda: study)
    monkeypatch.setattr(generator, "__file__", str(tmp_path / "scripts/capabilities/build.py"))
    destination = tmp_path / "study.json"
    generator.main(["--output", str(destination)])
    import json
    assert json.loads(destination.read_text(encoding="utf-8")) == study
