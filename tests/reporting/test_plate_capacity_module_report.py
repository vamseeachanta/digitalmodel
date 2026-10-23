# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reproducibility and independent report checks; stresses MPa, lengths mm."""

import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/plate_capacity"


@pytest.fixture
def builder():
    """Load the report builder; all example inputs use N, mm and MPa."""
    spec = importlib.util.spec_from_file_location(
        "plate_capacity_report",
        ROOT / "scripts/reporting/build_plate_capacity_report.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_worked_example_independent_values(builder):
    """MIT (2013), Lecture 11 Eqs. 11.15-17, Lecture 4 Eqs. 4.25/4.60.

    Synthetic E=120000 MPa, nu=0.3, t=2 mm, b=200 mm, a=400 mm:
    m=2, k=4; sigma=4*pi²*120000/[12*0.91]*(2/200)²
    =43.38287648830487 MPa. b/t=100; lambda²=250/sigma
    =5.762642319657961; utilisation=20/sigma=0.4610113855726369.
    The bar hand calculation is documented in test_axial_bar (N and mm).
    """
    record = builder.calculate()
    assert record["plate"]["critical_stress"] == pytest.approx(43.38287648830487)
    assert record["plate"]["geometric_slenderness"] == 100
    assert record["plate"]["normalised_slenderness"] ** 2 == pytest.approx(
        5.762642319657961
    )
    assert record["plate"]["utilisation"] == pytest.approx(0.4610113855726369)
    assert record["bar"]["one"]["equivalent_stiffness"] == 20000
    assert record["bar"]["one"]["displacements"] == [0, 0.5]
    assert record["bar"]["two"]["equivalent_stiffness"] == pytest.approx(
        6666.666666666667
    )
    assert record["bar"]["two"]["displacements"] == [0, 0.5, 1.5]


def test_rebuild_matches_committed_report(builder, tmp_path):
    """Run the one-command build in scratch; generated N/mm results are stable."""
    subprocess.run(
        [
            sys.executable,
            str(ROOT / "scripts/reporting/build_plate_capacity_report.py"),
            "--output",
            str(tmp_path),
        ],
        check=True,
    )
    for name in ("index.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (OUTPUT / name).read_bytes()
    builder.build(tmp_path)
    assert (tmp_path / "index.html").read_bytes() == (
        OUTPUT / "index.html"
    ).read_bytes()


def test_report_structure_and_rounding(builder, tmp_path):
    """The report declares MPa/mm/N, source limits and revision provenance."""
    builder.build(tmp_path)
    text = (tmp_path / "index.html").read_text()
    for item in (
        "Summary",
        "Revision history",
        "Inputs",
        "Method",
        "Worked example",
        "Limits of use",
        "NOT_EVALUATED",
        "Verification appendix",
        "Sources",
        "43.4 MPa",
        "100.0",
        "2.4",
        "46.1 %",
        "11.15",
        "4.60",
        "4.25",
        "20000.0 N/mm",
        "6666.7 N/mm",
        "0.5 mm",
        "1.5 mm",
    ):
        assert item in text
    assert text.index("Revision history") < text.index('id="inputs"')
    assert "<svg" in text and "<title>" in text
    assert "<script src=" not in text and "<link " not in text
    changes = json.loads((tmp_path / "changes.json").read_text())
    assert changes["revision"] == 0
    assert changes["changes"]


def test_figure_points_come_from_calculations(builder):
    """Each dimensionless k and MPa stress sample uses a freshly computed case."""
    from digitalmodel.structural.plate_capacity.elastic_buckling import (
        plate_buckling_coefficient,
    )

    record = builder.calculate()
    text = builder.render(record)
    for sample in record["samples"]:
        assert sample["coefficient"] == plate_buckling_coefficient(
            length=record["inputs"]["width"] * sample["ratio"],
            width=record["inputs"]["width"],
        )
        assert (
            f'data-ratio="{sample["ratio"]:.1f}" data-k="{sample["coefficient"]:.1f}"'
            in text
        )
    changed = builder.calculate()
    changed["samples"][0]["coefficient"] += 1
    assert builder.render(changed) != text
    changed = builder.calculate()
    changed["plate"]["critical_stress"] *= 2
    assert builder.render(changed) != text


def test_report_files_are_visible():
    """Durable report paths must not be ignored; numerical units are inapplicable."""
    result = subprocess.run(
        [
            "git",
            "check-ignore",
            "--",
            str(OUTPUT / "index.html"),
            str(OUTPUT / "changes.json"),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 1 and result.stdout == ""
