"""Offline screening tests; no solver output is generated or corrected.

The cutoff 0.5 rad/s is the case-specific recommendation in issue 1548
(2026), Recommendation section, not a universal hydrodynamic constant.
All other finite frequencies below are synthetic boundary-test inputs.
Expected labels follow direct comparisons, written out in each test.
"""

import csv

import pytest

from digitalmodel.hydrodynamics.rao_tabulation.reliability import (
    ScreeningPolicy,
    annotate_rows,
    main,
)

BASIS = "https://github.com/vamseeachanta/digitalmodel/issues/1548"
# Source: issue 1548 (2026), Recommendation, frequency screening range.
CUTOFF = 0.5


def test_boundary_and_all_headings_preserve_values():
    """Hand comparison: 0.35 < 0.5, 0.5 = 0.5, 0.8 > 0.5 rad/s.

    The issue's frequency exclusion applies across headings, including beam.
    Synthetic amplitudes are opaque payloads, not expected physical results.
    """
    rows = [
        {"frequency_rad_s": f, "heading_deg": h, "heave": "unchanged"}
        for f in [0.35, CUTOFF, 0.8]
        for h in [0, 90, 180]
    ]
    original = [row.copy() for row in rows]
    result = annotate_rows(rows, policy=ScreeningPolicy(CUTOFF, BASIS))
    assert [r["reliability"] for r in result] == (
        ["low_freq_artifact"] * 3 + [""] * 6
    )
    assert [r["frequency_screening_status"] for r in result] == (
        ["low_freq_artifact"] * 3 + ["screening_range"] * 6
    )
    assert rows == original
    for source, target in zip(rows, result):
        assert all(target[key] == value for key, value in source.items())
        assert target["reliability_basis"] == (
            BASIS if source["frequency_rad_s"] < CUTOFF else ""
        )
        assert target["frequency_screening_basis"] == BASIS


def test_missing_policy_is_not_evaluated():
    """No documented criterion means no eligibility conclusion, at any frequency."""
    result = annotate_rows([{"frequency_rad_s": 0.8}])
    assert result[0]["reliability"] == "NOT_EVALUATED"
    assert result[0]["reliability_basis"] == ""


def test_existing_flags_are_retained():
    """A frequency comparison cannot clear an earlier independent defect flag."""
    result = annotate_rows(
        [
            {"frequency_rad_s": 0.8, "reliability": "mesh_defect"},
            {"frequency_rad_s": 0.35, "reliability": "mesh_defect"},
        ],
        policy=ScreeningPolicy(CUTOFF, BASIS),
    )
    assert [row["reliability"] for row in result] == [
        "mesh_defect",
        "mesh_defect;low_freq_artifact",
    ]
    assert [row["reliability_basis"] for row in result] == ["", BASIS]


@pytest.mark.parametrize(
    "frequency,flag", [(CUTOFF, "low_freq_artifact"), (0.8, "mesh_defect")]
)
@pytest.mark.parametrize("previous_basis", [None, "independent source"])
def test_in_range_screen_retains_flag_provenance(frequency, flag, previous_basis):
    """An in-range bound provides no evidence for an earlier defect label."""
    row = {"frequency_rad_s": frequency, "reliability": flag}
    if previous_basis is not None:
        row["reliability_basis"] = previous_basis
    original = row.copy()
    result = annotate_rows([row], policy=ScreeningPolicy(CUTOFF, BASIS))[0]
    assert result["reliability"] == flag
    assert result["reliability_basis"] == (previous_basis or "")
    assert result["frequency_screening_status"] == "screening_range"
    assert result["frequency_screening_basis"] == BASIS
    assert result["minimum_screening_frequency_rad_s"] == CUTOFF
    assert row == original


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True])
def test_invalid_policy_cutoff(value):
    with pytest.raises(ValueError, match="positive finite"):
        ScreeningPolicy(value, BASIS)


@pytest.mark.parametrize("basis", ["", "   ", None])
def test_missing_policy_basis(basis):
    with pytest.raises(ValueError, match="basis"):
        ScreeningPolicy(CUTOFF, basis)


@pytest.mark.parametrize("value", [None, "", "Hz", 0, -1, "nan", "inf", True])
def test_invalid_frequency_is_rejected(value):
    with pytest.raises(ValueError, match="frequency"):
        annotate_rows([{"frequency_rad_s": value}])


def test_missing_frequency_column_is_rejected():
    with pytest.raises(ValueError, match="frequency"):
        annotate_rows([{"period_s": "20"}])


@pytest.mark.parametrize("column", ["frequency_hz", "frequency_HZ", "period_s", "Period"])
def test_non_angular_frequency_column_is_rejected(column):
    with pytest.raises(ValueError, match="rad/s"):
        annotate_rows(
            [{column: 0.35}],
            policy=ScreeningPolicy(CUTOFF, BASIS),
            frequency_column=column,
        )


@pytest.mark.parametrize("column", ["frequency_Hz", "wave_period_s"])
def test_cli_rejects_non_angular_frequency_column_without_output(tmp_path, column):
    source = tmp_path / "input.csv"
    output = tmp_path / "annotated.csv"
    raw = f"{column},heave\n0.35,8.0\n"
    source.write_text(raw)
    with pytest.raises(ValueError, match="rad/s"):
        main(
            [
                str(source),
                str(output),
                "--frequency-column",
                column,
                "--minimum-frequency-rad-s",
                str(CUTOFF),
                "--basis",
                BASIS,
            ]
        )
    assert source.read_text() == raw
    assert not output.exists()


def test_cli_retains_every_row_and_source_bytes(tmp_path):
    """Independent comparisons: 0.35 < 0.5; 0.5 = 0.5. No row is dropped."""
    source = tmp_path / "input.csv"
    output = tmp_path / "annotated.csv"
    raw = "omega,heave,phase\n0.35,8.0,-10\n0.5,1.0,0\n"
    source.write_text(raw)
    assert (
        main(
            [
                str(source),
                str(output),
                "--frequency-column",
                "omega",
                "--minimum-frequency-rad-s",
                str(CUTOFF),
                "--basis",
                BASIS,
            ]
        )
        == 0
    )
    assert source.read_text() == raw
    with output.open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert [row["reliability"] for row in rows] == [
        "low_freq_artifact",
        "",
    ]
    assert [row["reliability_basis"] for row in rows] == [BASIS, ""]
    assert [row["frequency_screening_status"] for row in rows] == [
        "low_freq_artifact",
        "screening_range",
    ]
    assert [row["heave"] for row in rows] == ["8.0", "1.0"]
    assert [row["phase"] for row in rows] == ["-10", "0"]


def test_cli_refuses_existing_output(tmp_path):
    source = tmp_path / "input.csv"
    source.write_text("frequency_rad_s\n0.5\n")
    with pytest.raises(FileExistsError):
        main([str(source), str(source)])


def test_cli_invalid_input_does_not_create_output(tmp_path):
    source = tmp_path / "input.csv"
    output = tmp_path / "annotated.csv"
    source.write_text("frequency_rad_s\nNaN\n")
    with pytest.raises(ValueError, match="frequency"):
        main([str(source), str(output)])
    assert not output.exists()


def test_existing_provenance_and_flag_are_not_erased():
    result = annotate_rows(
        [
            {
                "frequency_rad_s": 0.35,
                "reliability": "low_freq_artifact",
                "reliability_basis": "independent source",
            }
        ],
        policy=ScreeningPolicy(CUTOFF, BASIS),
    )[0]
    assert result["reliability"] == "low_freq_artifact"
    assert result["reliability_basis"] == f"independent source;{BASIS}"
    assert result["frequency_screening_status"] == "low_freq_artifact"


@pytest.mark.parametrize(
    "options", [["--basis", BASIS], ["--minimum-frequency-rad-s", str(CUTOFF)]]
)
def test_cli_requires_criterion_and_source_together(options):
    with pytest.raises(SystemExit) as exc:
        main(["unused-input.csv", "unused-output.csv", *options])
    assert exc.value.code == 2


@pytest.mark.parametrize(
    "raw",
    [
        "frequency_rad_s\n",
        "frequency_rad_s,frequency_rad_s\n0.5,0.5\n",
        "frequency_rad_s,\n0.5,payload\n",
        "frequency_rad_s,heave\n0.5\n",
        "frequency_rad_s\n0.5,extra\n",
    ],
)
def test_malformed_csv_creates_no_output(tmp_path, raw):
    source = tmp_path / "input.csv"
    output = tmp_path / "annotated.csv"
    source.write_text(raw)
    with pytest.raises(ValueError, match="CSV"):
        main([str(source), str(output)])
    assert not output.exists()


def test_cli_without_policy_exposes_missing_criterion(tmp_path):
    source = tmp_path / "input.csv"
    output = tmp_path / "annotated.csv"
    source.write_text("frequency_rad_s,heave\n0.8,1.0\n")
    assert main([str(source), str(output)]) == 0
    with output.open(newline="") as stream:
        row = next(csv.DictReader(stream))
    assert row["frequency_screening_status"] == "NOT_EVALUATED"
    assert row["minimum_screening_frequency_rad_s"] == ""
