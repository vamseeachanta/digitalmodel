"""Regression checks for the second-provider drive-off review.

The inputs are illustrative registry parameters, not measured propeller data.
"""
import csv
import math
import runpy
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import propeller_rudder as pr

ROOT = Path(__file__).resolve().parents[2]
VESSEL = pr.VesselPropulsion(4.0, (0.35, -0.30), 0.18, 0.25, 0.50)
RUDDER = pr.RudderGeometry(18.0, 1.8, 50.0)


@pytest.fixture
def example():
    return runpy.run_path(str(ROOT / "examples/propeller_rudder_driveoff.py"))


def test_example_omits_unqualified_yaw_columns(example, tmp_path):
    """The CSV shall not export the existing aft-rudder yaw sign defect."""
    manifest = example["main"](output_dir=tmp_path)
    with manifest["csv"].open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        assert not any("F_yaw" in field for field in reader.fieldnames)
        rows = list(reader)
    assert len(rows) == 9
    assert all("yaw omitted" in row["yaw_status"].lower() for row in rows)


def test_braking_extrapolation_is_omitted_from_csv_and_plot(example, tmp_path, monkeypatch):
    """An unsourced polynomial shall not supply reported braking loads."""
    from matplotlib.figure import Figure

    plotted = {}
    savefig = Figure.savefig

    def capture(figure, filename, *args, **kwargs):
        if Path(filename).stem == "driveoff_sway":
            plotted.update({line.get_label(): line for line in figure.axes[0].lines})
        return savefig(figure, filename, *args, **kwargs)

    monkeypatch.setattr(Figure, "savefig", capture)
    manifest = example["main"](output_dir=tmp_path)
    with manifest["csv"].open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    for row in rows[1:4]:
        assert row["regime"] == "braking"
        assert float(row["J"]) > 1.5
        for field in ("K_T", "T", "C_th_primary", "C_T_fallback", "primary_clamped",
                      "primary_F_surge_N", "primary_F_sway_N"):
            assert row[field] == "", field
        assert "fitted J range unknown" in row["label"]
        assert "four-quadrant data required" in row["label"]
        assert "omitted" in row["label"]
        assert float(row["fallback_F_sway_N"]) == pytest.approx(-205448.8651, rel=1e-8)
    primary = plotted["Interaction increment"]
    for n, force in zip(primary.get_xdata(), primary.get_ydata()):
        if 0.0 < n < 1.0:
            assert math.isnan(force)
    assert primary.get_ydata()[-1] == pytest.approx(-43875.27328, rel=1e-8)
    assert "fitted J range unknown" in example["CAPTION"]


@pytest.mark.parametrize("advance", [0.0, 1e-13, 1e-170, 1e-12, 1e-6])
def test_near_zero_inflow_uses_fallback_bollard_threshold(advance):
    """Sub-threshold inflow shall avoid division by an underflowed square."""
    result = pr.driveoff_sweep(advance / 0.75, [2.0], 20.0, VESSEL, RUDDER)[0]
    if advance < 1e-12:
        assert result.regime == "bollard"
        assert result.fallback is None
        assert result.C_th_primary == result.C_T_fallback == math.inf
        assert result.slipstream_axial == pytest.approx(6.446511447, rel=1e-8)
    else:
        assert result.regime == "ahead"
        assert result.fallback is not None
        assert math.isfinite(result.fallback.F_sway)
    assert math.isfinite(result.primary.F_sway)


@pytest.mark.parametrize("angle", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("shaft_speeds", [[0.0], [2.0], []])
def test_nonfinite_deflections_are_rejected(angle, shaft_speeds):
    """Validation shall precede force computation, including an empty sweep."""
    with pytest.raises(ValueError, match="delta_deg must be finite"):
        pr.driveoff_sweep(6.173328, shaft_speeds, angle, VESSEL, RUDDER)


def test_bollard_label_discloses_unvalidated_primary_limit(example):
    """A mathematical limit shall not be labelled as validated bollard loading."""
    point = pr.driveoff_sweep(0.0, [2.0], 20.0, VESSEL, RUDDER)[0]
    row = example["_csv_row"](point)
    assert "primary mathematical limit" in row["label"]
    assert "not validated" in row["label"]
    assert "fallback not evaluated" in row["label"]
    assert row["primary_F_sway_N"] == pytest.approx(-51514.25, rel=1e-6)
    assert row["fallback_F_sway_N"] is None
    assert "primary mathematical limit" in example["CAPTION"]


@pytest.mark.parametrize("speed,shaft_speed,regime,statuses", [
    (6.173328, 2.0, "ahead", (
        "illustrative_arithmetic", "illustrative_arithmetic",
        "illustrative_arithmetic", "illustrative_arithmetic",
    )),
    (6.173328, 0.0, "engine_off", (
        "model_assumed_zero", "model_assumed_zero",
        "freestream_only_approximation", "freestream_only_approximation",
    )),
    (6.173328, 0.75, "braking", (
        "omitted_missing_four_quadrant_basis", "omitted_missing_four_quadrant_basis",
        "freestream_only_approximation", "freestream_only_approximation",
    )),
    (0.0, 2.0, "bollard", (
        "illustrative_arithmetic", "unvalidated_mathematical_limit",
        "not_evaluated", "unvalidated_mathematical_limit",
    )),
])
def test_csv_statuses_distinguish_omitted_approximate_and_zero(
    example, speed, shaft_speed, regime, statuses,
):
    """Each reported quantity shall carry its evaluation basis per regime."""
    point = pr.driveoff_sweep(speed, [shaft_speed], 20.0, VESSEL, RUDDER)[0]
    row = example["_csv_row"](point)
    assert row["regime"] == regime
    fields = ("thrust_status", "primary_status", "fallback_status", "slipstream_status")
    assert tuple(row[field] for field in fields) == statuses
    if regime == "engine_off":
        assert row["T"] == row["primary_F_sway_N"] == 0.0
    elif regime == "braking":
        assert row["T"] is row["primary_F_sway_N"] is None
    elif regime == "bollard":
        assert row["fallback_F_sway_N"] is None
        assert row["primary_F_sway_N"] != 0.0


def test_statuses_survive_csv_export_and_are_disclosed_on_plots(
    example, tmp_path, monkeypatch,
):
    """CSV consumers and both plot captions shall receive the evaluation basis."""
    from matplotlib.figure import Figure

    captions = []
    savefig = Figure.savefig

    def capture(figure, filename, *args, **kwargs):
        captions.append("\n".join(text.get_text() for text in figure.texts))
        return savefig(figure, filename, *args, **kwargs)

    monkeypatch.setattr(Figure, "savefig", capture)
    manifest = example["main"](output_dir=tmp_path)
    with manifest["csv"].open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert rows[0]["thrust_status"] == "model_assumed_zero"
    assert rows[0]["T"] == "0.0"
    for row in rows[1:4]:
        assert row["thrust_status"] == "omitted_missing_four_quadrant_basis"
        assert row["primary_status"] == "omitted_missing_four_quadrant_basis"
        assert row["T"] == row["primary_F_sway_N"] == ""
    assert rows[-1]["slipstream_status"] == "illustrative_arithmetic"
    assert len(captions) == 2
    for caption in captions:
        for phrase in ("CSV statuses", "model-assumed zero", "freestream-only",
                       "illustrative arithmetic", "slipstream limit unvalidated"):
            assert phrase in caption


def test_example_acceptance_matrix_matches_status_columns(example):
    """The documented regime matrix shall match the exported status contract."""
    text = (ROOT / "docs/domains/hydrodynamics/propeller-rudder-method-selection.md").read_text(
        encoding="utf-8",
    )
    assert "### Example acceptance matrix" in text
    for speed, shaft_speed in ((6.173328, 2.0), (6.173328, 0.0),
                               (6.173328, 0.75), (0.0, 2.0)):
        point = pr.driveoff_sweep(speed, [shaft_speed], 20.0, VESSEL, RUDDER)[0]
        row = example["_csv_row"](point)
        matrix_row = next(line for line in text.splitlines()
                          if line.startswith(f"| `{point.regime}` |"))
        cells = [cell.strip().strip("`") for cell in matrix_row.split("|")[1:-1]]
        assert cells == [point.regime, row["thrust_status"], row["primary_status"],
                         row["fallback_status"], row["slipstream_status"]]


def test_method_selection_indexes_current_driveoff_tests_and_limits():
    text = (ROOT / "docs/domains/hydrodynamics/propeller-rudder-method-selection.md").read_text(
        encoding="utf-8",
    )
    for name in ("tests/test_propeller_rudder_interaction.py",
                 "tests/hydrodynamics/test_propeller_rudder_driveoff.py",
                 "tests/hydrodynamics/test_propeller_rudder_review.py"):
        assert name in text
        assert (ROOT / name).is_file()
    assert "36 tests pass" not in text and "36 pass" not in text
    assert "primary mathematical limit" in text
    assert "fitted J range unknown" in text
    assert "yaw columns are omitted" in text
