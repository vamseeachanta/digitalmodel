# ABOUTME: FFS showcase regression checks for Part B pressures and publication limits.
# ABOUTME: Exercises the negative control, generated grids and published HTML.
"""Test the FFS showcase driver's data and publication contract.

Imports ``build_showcase_data`` from the demo driver and checks that it returns
non-empty corroded-pipe grids with finite safe-pressure values for at least one
preset+method combination. Part B pressures, the retained notice and bounded
qualification claims are checked in both generated data and published HTML.
"""

import importlib.util
import json
import math
import re
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.corroded_pipe import (
    ASME_B318_CLASS1_DESIGN_FACTOR,
    SMYS_PSI,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import SMTS_PSI, dnv_f101_single_defect

_DRIVER = (
    Path(__file__).resolve().parents[2]
    / "examples" / "demos" / "asset_integrity" / "ffs_showcase.py"
)


def _load_driver():
    spec = importlib.util.spec_from_file_location("ffs_showcase_driver", _DRIVER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_build_showcase_data_returns_finite_grids():
    data = _load_driver().build_showcase_data()
    cor = data["corroded"]

    assert cor["grids"], "expected at least one (preset, method) grid"
    assert cor["presets"] and cor["methods"]

    # At least one preset+method grid must have finite, positive safe pressures.
    finite_grid_found = False
    for combo, grid in cor["grids"].items():
        safe = grid["safe"]
        assert safe, f"empty safe-pressure grid for {combo}"
        flat = [v for row in safe for v in row]
        assert all(math.isfinite(v) for v in flat), f"non-finite safe pressure in {combo}"
        if any(v > 0 for v in flat):
            finite_grid_found = True
            assert grid["maop"] > 0
            assert grid["ratio"] and len(grid["ratio"]) == len(safe)
    assert finite_grid_found, "no grid had positive finite safe pressures"


@pytest.fixture(scope="module")
def showcase_data():
    return _load_driver().build_showcase_data()


def _part_b_reference(preset):
    _label, diameter, thickness, grade = preset
    # Reuse the assessment's design-factor constant and the engine's Part B
    # modelling-factor resolution; never duplicate a total usage-factor value.
    return dnv_f101_single_defect(
        diameter, thickness, thickness * 0.4, 8.0, SMTS_PSI[grade],
        operational_usage_factor=ASME_B318_CLASS1_DESIGN_FACTOR,
    )


def _assert_showcase_part_b(driver, preset):
    _label, diameter, thickness, grade = preset
    reference = _part_b_reference(preset)
    actual = driver._safe_pressure(
        "DNV-RP-F101", diameter, thickness, thickness * 0.4, 8.0,
        SMYS_PSI[grade], SMTS_PSI[grade],
    )
    expected = reference.capacity_pressure_psi * reference.details["total_usage_factor"]
    assert actual == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("preset_index", range(3))
def test_showcase_dnv_uses_corrected_part_b_factor(preset_index):
    driver = _load_driver()
    _assert_showcase_part_b(driver, driver.PRESETS[preset_index])


def test_showcase_part_b_matches_hand_calculation(showcase_data):
    driver = _load_driver()
    preset = driver.PRESETS[1]
    # For D=30, t=0.375, d=0.150, L=8 in and SMTS=66,700 psi:
    # Q=sqrt(1+0.31*8**2/(30*0.375))=1.6624;
    # capacity=(2*0.375*66700/29.625)*0.6/(1-0.4/Q)=1334.19 psi.
    # Part B safe pressure=0.648*capacity=864.56 psi.
    assert _part_b_reference(preset).capacity_pressure_psi == pytest.approx(
        1334.19, abs=0.05
    )
    safe = driver._safe_pressure(
        "DNV-RP-F101", 30.0, 0.375, 0.150, 8.0, 52_000.0, 66_700.0,
    )
    assert safe == pytest.approx(864.56, abs=0.05)
    corroded = showcase_data["corroded"]
    row = corroded["depths_pct"].index(40.0)
    column = corroded["lengths"].index(8.0)
    grid = corroded["grids"][preset[0] + "|DNV-RP-F101"]
    assert grid["safe"][row][column] == pytest.approx(864.6, abs=0.05)


def test_part_b_regression_rejects_deprecated_total_factor(monkeypatch):
    driver = _load_driver()

    def legacy_total_factor(diameter, thickness, depth, length, smts, **_kwargs):
        return dnv_f101_single_defect(
            diameter, thickness, depth, length, smts,
            usage_factor=ASME_B318_CLASS1_DESIGN_FACTOR,
        )

    monkeypatch.setattr(driver, "dnv_f101_single_defect", legacy_total_factor)
    with pytest.warns(DeprecationWarning, match="deprecated total F"):
        with pytest.raises(AssertionError):
            _assert_showcase_part_b(driver, driver.PRESETS[0])


@pytest.mark.parametrize("preset_index", range(3))
def test_embedded_dnv_grid_uses_corrected_part_b_factor(showcase_data, preset_index):
    driver = _load_driver()
    preset = driver.PRESETS[preset_index]
    corroded = showcase_data["corroded"]
    grid = corroded["grids"][preset[0] + "|DNV-RP-F101"]
    row = corroded["depths_pct"].index(40.0)
    column = corroded["lengths"].index(8.0)
    expected = _part_b_reference(preset).allowable_pressure_psi
    assert grid["safe"][row][column] == round(expected, 1)
    assert grid["ratio"][row][column] == round(expected / grid["maop"], 4)


@pytest.mark.parametrize("source", ["generator", "published"])
def test_showcase_notice_and_qualification_limits(showcase_data, monkeypatch, source):
    driver = _load_driver()
    if source == "generator":
        monkeypatch.setattr(driver, "get_plotlyjs", lambda: "/* test renderer */")
        html = driver._render_html(showcase_data)
    else:
        html = (_DRIVER.parents[3] / "docs/api/ffs/ffs-showcase.html").read_text(
            encoding="utf-8"
        )
    notice = (
        "DNV-RP-F101 safe pressures were corrected on 2026-10-20 to apply the "
        "Part B modelling factor (F = F1·F2); independent verification is "
        "pending; do not use them for decisions."
    )
    assert html.index("<body>") < html.index(notice) < html.index("<header>")
    assert 'role="alert"' in html
    assert "Representative pipe preset" in html
    assert "Representative AH36 plate" in html
    assert "not edition-matched to API 579 Part 5" in html
    assert "Validated (API 579-1)" not in html
    assert "validated FFS modules" not in html
    assert "validated digitalmodel modules" not in html


def test_published_showcase_contains_current_pressure_grid(showcase_data):
    html = (_DRIVER.parents[3] / "docs/api/ffs/ffs-showcase.html").read_text(
        encoding="utf-8"
    )
    embedded = json.loads(re.search(r"const COR = (.+);", html).group(1))
    assert embedded == showcase_data["corroded"]
