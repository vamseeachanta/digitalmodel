# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independently check every worked number and exact report rebuild bytes."""

import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_structural_analysis_module_report.py"
REPORT = ROOT / "docs/reports/modules/structural_analysis"
FILES = {"index.html", "changes.json", "manifest.json", "README.md"}
# Roundoff allowance for the closed-form binary64 arithmetic, not model error.
ROUNDING = 32 * sys.float_info.epsilon


def builder():
    """Load the standalone builder without importing shared report machinery."""
    assert SCRIPT.is_file(), "The structural module report builder is missing"
    spec = importlib.util.spec_from_file_location("structural_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def calculation(page):
    """Read the exact payload embedded in the public page."""
    match = re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        page, re.S,
    )
    assert match
    return json.loads(match[1])


def test_beam_payload_matches_hand_equilibrium(tmp_path):
    """The integrated rectangle and N/mm² arithmetic are independent oracles."""
    build = builder()
    page = build.render(tmp_path)
    payload = calculation(page)
    assert payload["basis"] == "SYNTHETIC"
    inputs = payload["inputs"]["beam"]
    expected_inputs = {
        "width_m": 0.2, "height_m": 0.4, "axial_force_n": 800000,
        "moment_y_nm": 16000, "moment_z_nm": 8000, "y_m": 0.1, "z_m": 0.2,
    }
    assert inputs == expected_inputs
    expected = {
        "area_m2": 0.08, "i_y_m4": 0.2 * 0.4**3 / 12,
        "i_z_m4": 0.4 * 0.2**3 / 12,
        "axial_mpa": 10, "bending_y_mpa": 3, "bending_z_mpa": 3,
        "positive_corner_mpa": 16, "negative_corner_mpa": 4,
    }
    assert payload["beam"] == pytest.approx(expected, rel=ROUNDING, abs=0)
    assert "16.0 MPa at (+y, +z) and 4.0 MPa at (−y, −z)" in page
    assert payload["units"]["beam"] == {
        "area_m2": "m²", "i_y_m4": "m⁴", "i_z_m4": "m⁴",
        **{key: "MPa" for key in expected if key.endswith("mpa")},
    }
    # The visible rows are checked independently of the template's formatter.
    for key, value in expected.items():
        display = (f"{value:.1e}" if key.startswith("i_") else
                   f"{value:.1f}" if key.endswith("mpa") else f"{value:.2f}")
        assert f'data-value="beam.{key}">{display}</td>' in page
    assert "∫" in page and "N/mm² = MPa" in page


def test_column_payload_matches_eigenfunction(tmp_path):
    """Pin-ended sine mode: EI*pi⁴/L⁴ - P*pi²/L² = 0."""
    build = builder()
    page = build.render(tmp_path)
    payload = calculation(page)
    assert payload["inputs"]["column"] == {
        "youngs_modulus_mpa": 200000, "i_mm4": 1000000,
        "effective_length_mm": 2000,
    }
    expected = {"load_n": 50000 * math.pi**2,
                "double_length_load_n": 12500 * math.pi**2, "length_load_ratio": 0.25}
    assert payload["column"] == pytest.approx(expected, rel=ROUNDING, abs=0)
    assert f'ideal pin-ended critical load is {expected["load_n"]:.1f} N' in page
    assert payload["units"]["column"] == {
        "load_n": "N", "double_length_load_n": "N", "length_load_ratio": "1",
    }
    for key, value in expected.items():
        display = f"{value * 100:.1f}" if key.endswith("ratio") else f"{value:.1f}"
        assert f'data-value="column.{key}">{display}</td>' in page
    assert "EI v'''' + P v'' = 0" in page
    assert "32" in page and "roundoff" in page


def test_rebuild_is_deterministic(tmp_path):
    """Independent directories and delivered files must match exactly."""
    build = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    build.build(tmp_path / "scratch-a", first)
    build.build(tmp_path / "scratch-b", second)
    assert {path.name for path in first.iterdir()} == FILES
    for name in FILES:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes()


def test_cli_manifest_matches_written_files(tmp_path):
    """Check source bindings and all nonrecursive artifact digests from disk."""
    build = builder()
    destination = tmp_path / "cli-output"
    environment = os.environ.copy()
    # Explicit cwd must not inherit unrelated repository bindings.
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(tmp_path / "scratch"),
         "--output-dir", str(destination)], cwd=ROOT, env=environment,
        check=True, capture_output=True, text=True,
    )
    manifest = json.loads((destination / "manifest.json").read_text())
    assert manifest["schema_version"] == 1
    assert manifest["basis"] == "SYNTHETIC"
    assert manifest["criteria_revision"] == "A"
    assert set(manifest["artifacts"]) == FILES - {"manifest.json"}
    assert set(manifest["sources"]) == set(build.SOURCES)
    assert SCRIPT.relative_to(ROOT).as_posix() in manifest["sources"]
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((destination / name).read_bytes()).hexdigest() == digest
    for name, digest in manifest["sources"].items():
        assert not Path(name).is_absolute()
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
    assert json.loads((destination / "changes.json").read_text()) == build.CHANGES


def test_sections_inventory_sources_and_qualification(tmp_path):
    """Unworked methods cannot inherit qualification from the two examples."""
    build = builder()
    page = build.render(tmp_path)
    headings = re.findall(r'<h2 id="s\d+">(.*?)</h2>', page)
    assert headings == [
        "1. Executive Summary", "2. Introduction", "3. Design Data and Basis",
        "4. Analysis Methodology", "5. Detailed Results", "6. Key Findings",
        "7. Limitations and Not Covered", "8. Recommendations",
        "9. References and Appendices", "10. Acronyms and Review Notes",
    ]
    summary_end = page.index("<h3>Revision history</h3>")
    assert page.index('id="s1"') < summary_end < page.index('id="s2"')
    assert 'id="changes-panel"' in page
    assert "simplified linear" in page and "NOT_EVALUATED" in page
    assert "IDEAL_MECHANICS_CHECKED" in page
    expected_methods = {
        "StressState": ["von_mises", "principal_stresses", "max_shear"],
        "StressCalculator": ["beam_stress", "shear_stress", "torsional_stress",
                             "hoop_stress", "longitudinal_stress"],
        "PlateBucklingAnalyzer": ["elastic_buckling_stress", "reduced_slenderness",
                                  "johnson_ostenfeld", "check_plate_buckling"],
        "ColumnBucklingAnalyzer": ["euler_buckling_load", "slenderness_ratio",
                                   "reduction_factor", "check_column_buckling"],
        "MemberCapacityChecker": ["check_tension_member", "check_combined_loading"],
    }
    inventory = calculation(page)["methods"]
    expected_names = {f"{cls}.{name}" for cls, names in expected_methods.items() for name in names}
    assert {row["method"] for row in inventory} == expected_names
    for row in inventory:
        status = ("IDEAL_MECHANICS_CHECKED" if row["method"] in {
            "StressCalculator.beam_stress", "ColumnBucklingAnalyzer.euler_buckling_load",
        } else "NOT_EVALUATED")
        assert row["qualification"] == status
        assert row["units"] and row["limits"] and row["source"]
        assert f'<code>{row["method"]}</code>' in page
    # Every result appears once with its unit and source, not just in hidden JSON.
    assert len(re.findall('data-value="', page)) == 11
    assert "unknown curve" in page and "sigma_y" in page
    assert "source-unqualified" in page


def test_checkout_local_scratch_is_rejected_without_writes(tmp_path):
    """The CLI must validate scratch before creating its output directory."""
    build = builder()
    destination = tmp_path / "never-written"
    before = {name: (REPORT / name).read_bytes() for name in FILES}
    with pytest.raises(ValueError, match="outside the checkout"):
        build.build(ROOT / "forbidden-scratch", destination)
    assert not destination.exists()
    assert not (ROOT / "forbidden-scratch").exists()
    assert {name: (REPORT / name).read_bytes() for name in FILES} == before
