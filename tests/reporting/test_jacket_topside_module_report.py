# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report oracles use independent annulus integrals and direct joint ratios.

No expected allowable or nonzero capacity is obtained from production methods.
Floating-point tolerance follows the existing documented annulus example.
"""

import hashlib
import html
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
SCRIPT = ROOT / "scripts/reports/build_jacket_topside_module_report.py"
REPORT = ROOT / "docs/reports/modules/jacket_topside"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}
REBUILD = (
    'PYTHONPATH=src python scripts/reports/build_jacket_topside_module_report.py '
    '--scratch "$TMPDIR/calculation" --output-dir docs/reports/modules/jacket_topside'
)


def builder():
    spec = importlib.util.spec_from_file_location("jacket_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def payload(page):
    match = re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S
    )
    assert match is not None
    return json.loads(match[1])


@pytest.mark.parametrize("note", ["", '<marker>& "quoted" </pre><script>example</script>'])
def test_disclosure_contains_visible_escaped_payload(tmp_path, monkeypatch, note):
    build = builder()
    original = build.calculate

    def changed(scratch):
        data = original(scratch)
        data["assessments"]["display_note"] = note
        return data

    monkeypatch.setattr(build, "calculate", changed)
    page = build.render(tmp_path / "scratch")
    disclosure = re.search(
        r'<details><summary>Unrounded calculation payload</summary>(.*?)</details>',
        page, re.S,
    )
    assert disclosure is not None
    visible = re.search(r'<pre>(.*?)</pre>', disclosure[1], re.S)
    assert visible is not None, "Expanded calculation disclosure must show JSON"
    assert json.loads(html.unescape(visible[1])) == payload(page)
    if note:
        assert "&lt;marker&gt;&amp;" in visible[1]
        assert "<script>" not in visible[1]


@pytest.mark.parametrize("group,key,value,expected", [
    ("section", "wall_thickness", 0.5, "0.500"),
    ("joint", "chord_wt", 0.5, "0.500"),
    ("joint", "brace_wts", [0.25], "[0.250]"),
    ("section", "wall_thickness", 0.0254, "0.0254"),
])
def test_thickness_display_preserves_input_precision(
    tmp_path, monkeypatch, group, key, value, expected,
):
    build = builder()
    original = build.calculate

    def changed(scratch):
        data = original(scratch)
        # Display-only mutation; no new geometry or capacity claim is made.
        data["inputs"][group][key]["value"] = value
        return data

    monkeypatch.setattr(build, "calculate", changed)
    page = build.render(tmp_path / "scratch")
    assert f'<td>{group}/{key}</td><td>{expected}</td><td>in</td>' in page
    assert payload(page)["inputs"][group][key]["value"] == pytest.approx(value)


def test_annulus_payload_matches_independent_integrals(tmp_path):
    data = builder().calculate(tmp_path / "scratch")
    # Independent documented integrals with R=6 in and a=11/2 in.
    expected = {
        "inner_diameter": (11.0, "in"),
        "cross_sectional_area": (23 * math.pi / 4, "in^2"),
        "moment_of_inertia": (6095 * math.pi / 64, "in^4"),
        "section_modulus": (6095 * math.pi / 384, "in^3"),
        "radius_of_gyration": (math.sqrt(265) / 4, "in"),
    }
    assert set(data["annulus"]) == set(expected)
    for key, (value, unit) in expected.items():
        actual = data["annulus"][key]
        assert actual == {"value": pytest.approx(value, rel=1e-12, abs=0),
                          "unit": unit, "source": "[1, 2, 3]"}
    assert data["inputs"]["section"] == {
        "outer_diameter": {"value": 12.0, "unit": "in"},
        "wall_thickness": {"value": 0.5, "unit": "in"},
        "yield_strength": {"value": 40.0, "unit": "ksi"},
        "elastic_modulus": {"value": 28000.0, "unit": "ksi"},
        "effective_length": {"value": 120.0, "unit": "in"},
    }


def test_joint_geometry_payload_matches_hand_ratios(tmp_path):
    data = builder().calculate(tmp_path / "scratch")
    assert data["joint_geometry"] == {
        "gamma": {"value": 12.0, "unit": "1", "source": "[4, 9]"},
        "beta": {"value": [0.5], "unit": "1", "source": "[4, 9]"},
        "tau": {"value": [0.5], "unit": "1", "source": "[4, 9]"},
        "brace_count": {"value": 1, "unit": "1", "source": "[4, 9]"},
        "classification": {"value": "T/Y", "unit": "label", "source": "[4, 9]"},
    }
    assert data["inputs"]["joint"] == {
        "chord_od": {"value": 12.0, "unit": "in"},
        "chord_wt": {"value": 0.5, "unit": "in"},
        "brace_ods": {"value": [6.0], "unit": "in"},
        "brace_wts": {"value": [0.25], "unit": "in"},
        "brace_angles": {"value": [90.0], "unit": "deg"},
        "gap": {"value": 0.0, "unit": "in"},
        "braces_on_same_side": {"value": True, "unit": "boolean"},
        "chord_fy": {"value": 40.0, "unit": "ksi"},
        "brace_index": {"value": 0, "unit": "index"},
        "chord_utilization": {"value": 0.0, "unit": "1"},
    }


def test_unqualified_capacity_is_not_presented_as_validated(tmp_path):
    build = builder()
    page = build.render(tmp_path / "scratch")
    data = payload(page)
    assert set(data) == {
        "inputs", "annulus", "joint_geometry", "zero_demand", "assessments"
    }
    assert data["inputs"]["member_loads"] == {
        "fa": {"value": 0.0, "unit": "ksi"},
        "fb": {"value": 0.0, "unit": "ksi"},
        "Cm": {"value": 1.0, "unit": "1"},
        "is_tension": {"value": [False, True], "unit": "boolean"},
    }
    assert data["inputs"]["joint_loads"] == {
        "axial_load": {"value": 0.0, "unit": "kip"},
        "ipb_moment": {"value": 0.0, "unit": "kip-in"},
        "opb_moment": {"value": 0.0, "unit": "kip-in"},
    }
    assert data["assessments"] == {
        "member_nonzero_capacity": "NOT_EVALUATED",
        "joint_nonzero_capacity": "NOT_EVALUATED",
        "damaged_member_acceptance": "NOT_EVALUATED",
        "standards_compliance": "NOT_EVALUATED",
    }
    expected_names = {
        "compression/axial_compression", "compression/bending",
        "compression/combined", "compression/governing",
        "tension/axial_tension", "tension/bending", "tension/combined",
        "tension/governing", "joint/punching_shear",
    }
    assert set(data["zero_demand"]) == expected_names
    for row in data["zero_demand"].values():
        assert row == {
            "applied_demand": {"value": 0.0, "unit": "ksi"},
            "utilisation": {"value": 0.0, "unit": "1"},
            "finite_positive_capacity": True,
            "status": "ZERO_DEMAND_ARITHMETIC_ONLY",
            "capacity_status": "NOT_EVALUATED",
            "source": "[3, 4, 9]",
        }
    # No numeric allowables, standard coefficients, details or PASS verdicts leak.
    assert not re.search(r'"(?:allowable\w*|details|passes)"', page)
    assert "Simplified bending treatment" in page
    assert "No design compliance is certified" in page
    assert "Independent geometry" in page
    assert "Existing implementation tests" in page
    assert "Unqualified standard allowables" in page
    assert "changes-panel" in page and "changes.json" in page
    assert re.findall(r'<h2 id="s\d+">(.*?)</h2>', page) == [
        "1. Executive Summary", "2. Introduction", "3. Design Data and Basis",
        "4. Analysis Methodology", "5. Detailed Results", "6. Key Findings",
        "7. Limitations and Not Covered", "8. Recommendations",
        "9. References and Appendices", "10. Conclusions",
    ]
    summary_end = page.index('<h2 id="s2">')
    assert page.index("Revision history") < summary_end
    for text in ("23*pi/4", "6095*pi/64", "6095*pi/384", "sqrt(265)/4"):
        assert text in page
    for key, row in data["annulus"].items():
        assert f'{row["value"]:.1f}' in page
    for item in build.SOURCES:
        assert item in page
    assert ("/" + "home/") not in page and ("/" + "mnt/") not in page
    assert data == build.calculate(tmp_path / "second-scratch")


def run_cli(directory):
    output = directory / "output"
    environment = os.environ.copy()
    environment.update(PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(directory / "scratch"),
         "--output-dir", str(output)], cwd=directory.parent,
        env=environment, check=True, capture_output=True, text=True,
        encoding="utf-8", timeout=30,
    )
    return output


def test_rebuild_is_deterministic(tmp_path):
    first = run_cli(tmp_path / "first")
    second = run_cli(tmp_path / "second")
    assert {p.name for p in first.iterdir()} == DELIVERABLES
    assert {p.name for p in second.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Retained delivery differs; rebuild with: {REBUILD}"
        )


def test_cli_manifest_matches_written_files(tmp_path):
    output = run_cli(tmp_path / "cli")
    manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest) == {"algorithm", "sources", "artifacts", "manifest_policy"}
    assert manifest["algorithm"] == "sha256"
    assert "excludes itself" in manifest["manifest_policy"]
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    assert set(manifest["sources"]) == set(builder().SOURCES)
    for name, digest in manifest["artifacts"].items():
        assert digest == hashlib.sha256((output / name).read_bytes()).hexdigest(), (
            f"Artifact digest differs; rebuild with: {REBUILD}"
        )
    for name, digest in manifest["sources"].items():
        assert digest == hashlib.sha256((ROOT / name).read_bytes()).hexdigest(), (
            f"Source digest differs; rebuild with: {REBUILD}"
        )
    assert json.loads((output / "changes.json").read_text(encoding="utf-8")) == builder().CHANGES
    data = payload((output / "index.html").read_text(encoding="utf-8"))
    assert json.loads(
        (tmp_path / "cli/scratch/calculation.json").read_text(encoding="utf-8")
    ) == data


def test_scratch_inside_checkout_is_rejected():
    # The builder must reject before creating this path or writing intermediates.
    with pytest.raises(ValueError, match="outside the checkout"):
        builder().calculate(ROOT / "report-scratch-must-not-exist")


def test_display_tracks_computed_results(tmp_path, monkeypatch):
    build = builder()
    original = build.calculate

    def changed(scratch):
        data = original(scratch)
        # Synthetic mutation, not an engineering benchmark or capacity value.
        data["annulus"]["cross_sectional_area"]["value"] = 1234.5
        return data

    monkeypatch.setattr(build, "calculate", changed)
    page = build.render(tmp_path / "scratch")
    assert "1234.5" in page
    assert payload(page)["annulus"]["cross_sectional_area"]["value"] == 1234.5
