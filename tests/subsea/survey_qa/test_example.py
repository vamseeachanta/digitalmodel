"""Slice 6: issue #2955 prescribed biases; independent inverse-transform check.

The chosen regression tolerances (0.1 deg and 0.01 s) are synthetic test criteria,
not hydrographic accuracy limits. Depth noise is a chosen 0.002 m standard
deviation. The expected biases are inputs, never outputs of another estimator.
"""

from dataclasses import asdict, replace
from math import dist, sqrt

import pytest

from digitalmodel.subsea.survey_qa import Offsets, apply_corrections
from digitalmodel.subsea.survey_qa.calibration import PatchTestLines, estimate_patch
from digitalmodel.subsea.survey_qa.example import make_example


@pytest.mark.parametrize("noise", [0.0, 0.002])
def test_combined_parameter_recovery_without_truth(noise):
    survey = make_example(noise_std_m=noise, seed=7)
    # Remove every truth coordinate before estimation, including noisy cases.
    blind = PatchTestLines(
        **{
            name: tuple(replace(s, truth_xyz_m=()) for s in pair)
            for name, pair in vars(survey).items()
        }
    )
    got = estimate_patch(blind)
    for name, expected in asdict(Offsets(0.5, 1.0, 1.5, 0.08)).items():
        tolerance = 0.01 if name == "latency_s" else 0.1
        assert getattr(got, name) == pytest.approx(expected, abs=tolerance)
    if noise == 0:
        for pair in vars(survey).values():
            for line in pair:
                corrected = apply_corrections(line, got)
                rms = sqrt(
                    sum(
                        dist(p, q) ** 2
                        for p, q in zip(corrected.xyz_m, line.truth_xyz_m)
                    )
                    / len(line.xyz_m)
                )
                assert rms < 1e-10


def test_wrong_estimation_order_biases_yaw():
    """Unremoved pitch and roll couple beam X/Y; raw yaw cannot be isolated.
    The sequential estimator removes these terms before fitting the target.
    """
    from digitalmodel.subsea.survey_qa import estimate_yaw

    lines = make_example(noise_std_m=0, seed=7)
    correct = estimate_patch(lines).yaw_deg
    premature = estimate_yaw(*lines.yaw)
    assert abs(premature - 1.5) > abs(correct - 1.5)


def test_report_pack_renders_example(tmp_path):
    """The house pack must contain the computed results and no absolute paths."""
    from digitalmodel.subsea.survey_qa.report import write_example_report

    result = write_example_report(tmp_path)
    html = (tmp_path / "patch_test_report.html").read_text()
    markdown = (tmp_path / "patch_test_report.md").read_text()
    assert "Synthetic MBES patch-test analysis" in html
    assert "NOT_EVALUATED" in html
    assert "WITHIN_SCREEN" not in html
    assert "450.7" not in html
    assert "eligible citation evidence is unavailable" in html
    assert "0.08" in html
    assert "1.5" in html
    assert "Revision history" in markdown
    assert str(tmp_path) not in html
    assert str(tmp_path) not in markdown
    assert result["report_pack"]["pdf_status"] == "pdf rendering disabled (pdf: off)"
    # Chosen metadata and compute environment must remain portable.
    import json

    manifest = json.loads((tmp_path / "report-layer-manifest.json").read_text())
    assert manifest["privacy_classification"] == "public-synthetic"
    assert manifest["compute_environment"] == "not-recorded"
    sidecar = json.loads((tmp_path / "patch_test_citations.json").read_text())
    assert sidecar == {"citations": []}
    import csv

    with (tmp_path / "overlap.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert rows[0]["Screen"] == "NOT_EVALUATED"
    assert rows[0]["Min TVU ceiling mm"] == "NOT_EVALUATED"
    assert rows[0]["Worst utilisation percent"] == "NOT_EVALUATED"


def test_checked_in_report_reproduces(tmp_path):
    """The committed md/html must equal a fresh deterministic house-pack render."""
    from pathlib import Path
    from digitalmodel.subsea.survey_qa.report import write_example_report

    write_example_report(tmp_path)
    root = Path(__file__).resolve().parents[3]
    for suffix in ("html", "md"):
        expected = root / "docs/domains/subsea" / f"survey-qa-patch-test.{suffix}"
        assert (
            expected.read_bytes()
            == (tmp_path / f"patch_test_report.{suffix}").read_bytes()
        )
