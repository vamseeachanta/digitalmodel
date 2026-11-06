# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline comparison tests; all numbers below are synthetic arithmetic inputs.

No number is a material property, site datum or standards acceptance limit.
"""

from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.subsea.flexible_pipes.configuration_comparison import (
    Case,
    Criterion,
    CriteriaSet,
    Evidence,
    Measurement,
    case_table,
    compare,
    inventory,
    mesh_change,
    render_report,
)

ROOT = Path(__file__).resolve().parents[3]


def measurement(value, unit):
    return Measurement(value, unit, "Synthetic hand calculation in this test")


def evidence(case=None, **kwargs):
    return Evidence(
        case=case or case_table()[0],
        solver_version="synthetic-only",
        basis_digest="a" * 64,
        input_digest="b" * 64,
        output_digest="c" * 64,
        criteria_revision="synthetic-v1",
        kind="synthetic",
        converged=True,
        tension=measurement(80, "kN"),
        curvature=measurement(0.2, "1/m"),
        clearance=measurement(-0.1, "m"),
        **kwargs,
    )


def criteria(**overrides):
    return CriteriaSet(
        "synthetic-v1",
        {
            "tension": Criterion(100, "kN", "Synthetic test limit", "maximum"),
            "curvature": Criterion(0.25, "1/m", "Synthetic test limit", "maximum"),
            "clearance": Criterion(0, "m", "Synthetic test limit", "minimum"),
        }
        | overrides,
    )


def test_symbolic_case_table():
    """Issue scope independently specifies four shapes x three offsets x two seas."""
    cases = case_table()
    assert len(cases) == len(set(cases)) == 24
    assert {c.configuration for c in cases} == {
        "catenary",
        "lazy_wave",
        "steep_wave",
        "pliant_wave",
    }
    assert {c.offset for c in cases} == {"near", "far", "cross"}
    assert {c.sea_state for c in cases} == {"sea_1", "sea_2"}
    with pytest.raises(ValueError):
        Case("unknown", "near", "sea_1")


def test_source_inventory_and_missing_source(tmp_path):
    records = inventory(ROOT)
    assert len(records) == 8  # Two issue-named libraries x four configurations.
    assert all(len(r["sha256"]) == 64 for r in records)
    assert all((ROOT / r["path"]).is_file() for r in records)
    with pytest.raises(FileNotFoundError):
        inventory(tmp_path)


def test_independent_threshold_arithmetic():
    """Hand calculation: 80 <= 100; 0.2 <= 0.25; -0.1 < 0 fails clearance.

    Dimensionless tension ratio = 80/100 = 0.8; no design-code claim follows.
    """
    rows = compare(evidence(), criteria())
    assert [r["status"] for r in rows] == ["PASS", "PASS", "FAIL"]
    assert rows[0]["value"] == 80
    assert rows[0]["limit"] == 100
    assert rows[2]["source"] == "Synthetic test limit"


def test_missing_criteria_response_and_convergence():
    assert all(r["status"] == "NOT_EVALUATED" for r in compare(evidence(), {}))
    partial = replace(evidence(), tension=None)
    assert compare(partial, criteria())[0]["status"] == "NOT_EVALUATED"
    unconverged = replace(evidence(), converged=False)
    assert all(r["status"] == "NOT_EVALUATED" for r in compare(unconverged, criteria()))


def test_threshold_direction_units_and_boundary():
    rows = compare(replace(evidence(), clearance=measurement(0, "m")), criteria())
    assert rows[2]["status"] == "PASS"
    bad = criteria(tension=Criterion(100, "N", "Synthetic", "maximum"))
    with pytest.raises(ValueError, match="unit"):
        compare(evidence(), bad)
    bad = criteria(clearance=Criterion(0, "m", "Synthetic", "maximum"))
    with pytest.raises(ValueError, match="direction"):
        compare(evidence(), bad)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), True])
def test_reject_nonfinite_or_boolean_numbers(value):
    with pytest.raises(ValueError):
        measurement(value, "kN")


def test_evidence_validation():
    with pytest.raises(ValueError, match="source"):
        Measurement(1, "kN", " ")
    with pytest.raises(ValueError, match="digest"):
        replace(evidence(), output_digest="not-a-digest")
    with pytest.raises(ValueError, match="curvature"):
        replace(evidence(), curvature=measurement(-1, "1/m"))
    with pytest.raises(ValueError):
        replace(evidence(), converged="yes")


def test_mesh_hand_calculation_and_zero_reference():
    """Hand calc: coarse 105, fine 100 -> |105-100|/|100| x 100 = 5 %.

    Zero fine result cannot define a relative change; zero/zero is also undefined.
    """
    assert mesh_change(measurement(105, "kN"), measurement(100, "kN")) == 5
    assert mesh_change(measurement(1, "m"), measurement(0, "m")) is None
    with pytest.raises(ValueError, match="unit"):
        mesh_change(measurement(1, "m"), measurement(1, "kN"))


def test_report_empty_and_partial_are_not_complete():
    report = render_report([], {}, revision="1", date="2026-10-20")
    assert "0 / 24" in report
    assert report.count("data-case=") == 24
    assert "NOT_EVALUATED" in report
    assert (
        report.index("Summary")
        < report.index("Revision history")
        < report.index("Case table")
    )
    partial = render_report([evidence()], criteria(), revision="1", date="2026-10-20")
    assert "1 / 24" in partial
    assert "synthetic" in partial
    assert "native readback is not established" in partial
    assert "-0.1" in partial


def test_report_rejects_mixed_campaigns_and_duplicates():
    first = evidence()
    second = replace(first, case=case_table()[1])
    for field, value in [
        ("solver_version", "different"),
        ("basis_digest", "d" * 64),
        ("criteria_revision", "different"),
        ("kind", "stored"),
    ]:
        with pytest.raises(ValueError, match="common"):
            render_report(
                [first, replace(second, **{field: value})],
                criteria(),
                revision="1",
                date="2026-10-20",
            )
    with pytest.raises(ValueError, match="duplicate"):
        render_report([first, first], criteria(), revision="1", date="2026-10-20")


def test_html_escapes_external_text():
    hostile = replace(evidence(), solver_version="<script>alert(1)</script>")
    report = render_report(
        [hostile], criteria(), revision="<unsafe>", date="2026-10-20"
    )
    assert "<script>" not in report
    assert "&lt;script&gt;" in report


def test_paired_mesh_report_and_refinement_validation():
    """Synthetic mesh halves segment length 2 -> 1 m; tension 105 -> 100 kN.

    Independent change = 5 kN and 5 %. Clearance fine reference is zero, so its
    percentage is undefined; no mesh tolerance or convergence verdict is inferred.
    """
    from digitalmodel.subsea.flexible_pipes.configuration_comparison import MeshPair

    coarse = replace(evidence(), tension=measurement(105, "kN"))
    fine = replace(
        coarse,
        tension=measurement(100, "kN"),
        clearance=measurement(0, "m"),
        input_digest="d" * 64,
        output_digest="e" * 64,
    )
    pair = MeshPair(coarse, fine, (measurement(2, "m"),), (measurement(1, "m"),))
    report = render_report(
        [coarse], criteria(), revision="1", date="2026-10-20", meshes=[pair]
    )
    assert "5.0 %" in report
    assert "two-level change" in report
    assert "undefined (zero reference)" in report
    with pytest.raises(ValueError, match="refine"):
        MeshPair(coarse, fine, (measurement(1, "m"),), (measurement(2, "m"),))
    with pytest.raises(ValueError, match="common"):
        MeshPair(
            coarse,
            replace(fine, case=case_table()[1]),
            (measurement(2, "m"),),
            (measurement(1, "m"),),
        )
    with pytest.raises(ValueError, match="input"):
        MeshPair(
            coarse,
            replace(fine, input_digest=coarse.input_digest),
            (measurement(2, "m"),),
            (measurement(1, "m"),),
        )
    with pytest.raises(ValueError, match="baseline"):
        render_report([], criteria(), revision="1", date="2026-10-20", meshes=[pair])


def test_preserve_revision_history():
    first = ("1", "2026-10-19", "Initial offline basis")
    second = ("2", "2026-10-20", "Added stored evidence")
    report = render_report(
        [], {}, revision="2", date="2026-10-20", history=[first, second]
    )
    assert "Initial offline basis" in report
    assert "Added stored evidence" in report
    with pytest.raises(ValueError, match="history"):
        render_report([], {}, revision="3", date="2026-10-20", history=[first, second])


def test_cli_cannot_import_licensed_interface(tmp_path):
    """A fresh interpreter denies licensed modules before importing the feature."""
    import os
    import subprocess
    import sys

    output = tmp_path / "comparison.html"
    script = """
import importlib.abc
import sys
class DenyLicensed(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0].lower() in {"orcfxapi", "orcawave"}:
            raise AssertionError("licensed interface import attempted")
sys.meta_path.insert(0, DenyLicensed())
from digitalmodel.subsea.flexible_pipes.configuration_comparison import main
main()
"""
    env = {
        k: v
        for k, v in os.environ.items()
        if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }
    env["PYTHONPATH"] = str(ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            script,
            "--output",
            str(output),
            "--revision",
            "1",
            "--date",
            "2026-10-20",
        ],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert output.read_text().count("data-case=") == 24


def test_report_keeps_sources_and_limits_visible_without_raw_float_repr():
    record = replace(
        evidence(), tension=Measurement(80.123456789, "kN", "response-ref")
    )
    report = render_report([record], criteria(), revision="1", date="2026-10-20")
    assert "response-ref" in report
    assert "Synthetic test limit" in report
    assert "80.123456789" not in report


def test_mesh_derived_overflow_is_rejected():
    with pytest.raises(ValueError, match="finite"):
        mesh_change(measurement(1e308, "m"), measurement(-1e308, "m"))


@pytest.mark.parametrize("render", [False, True])
def test_supplied_criteria_revision_must_match_evidence(render):
    revised = CriteriaSet(
        "synthetic-v2",
        dict(criteria())
        | {"tension": Criterion(70, "kN", "Revised synthetic limit", "maximum")},
    )
    with pytest.raises(ValueError, match="criteria revision"):
        if render:
            render_report([evidence()], revised, revision="1", date="2026-10-20")
        else:
            compare(evidence(), revised)
    current = replace(evidence(), criteria_revision="synthetic-v2")
    assert compare(current, revised)[0]["status"] == "FAIL"
    report = render_report([current], revised, revision="1", date="2026-10-20")
    assert "Supplied criteria revision: synthetic-v2" in report


def test_nonempty_criteria_require_revision_binding():
    limits = {"tension": Criterion(70, "kN", "Synthetic", "maximum")}
    with pytest.raises(ValueError, match="revision-bound"):
        compare(evidence(), limits)
    with pytest.raises(ValueError, match="revision-bound"):
        render_report([], limits, revision="1", date="2026-10-20")


@pytest.mark.parametrize("metric,unit", [("curvature", "1/m"), ("clearance", "m")])
def test_small_mesh_changes_never_display_as_zero(metric, unit):
    from digitalmodel.subsea.flexible_pipes.configuration_comparison import MeshPair

    coarse = replace(evidence(), **{metric: measurement(0.20004, unit)})
    fine = replace(
        coarse,
        **{metric: measurement(0.2, unit)},
        input_digest="d" * 64,
        output_digest="e" * 64,
    )
    pair = MeshPair(coarse, fine, (measurement(2, "m"),), (measurement(1, "m"),))
    report = render_report(
        [coarse], criteria(), revision="1", date="2026-10-20", meshes=[pair]
    )
    row = report.split(f"<td>{metric} ({unit})</td>")[1].split("</tr>")[0]
    assert row == "<td>&lt;0.1 (nonzero)</td><td>&lt;0.1 (nonzero) %</td>"


def test_tension_display_preserves_the_verdict_distinction():
    record = replace(evidence(), tension=measurement(1004, "kN"))
    limits = criteria(tension=Criterion(1000, "kN", "Synthetic", "maximum"))
    report = render_report([record], limits, revision="1", date="2026-10-20")
    assert "1004.0; 1000.0; FAIL" in report


def test_inventory_is_an_escaped_path_and_digest_table():
    sources = [
        {
            "configuration": "<shape>",
            "group": "<group>",
            "path": "relative/<spec>.yml",
            "sha256": "a" * 64,
        }
    ]
    report = render_report([], {}, revision="1", date="2026-10-20", sources=sources)
    section = report.split("<h2>Source inventory</h2>")[1].split("<h2>")[0]
    assert "<th>Path</th><th>SHA-256</th>" in section
    assert "<td>relative/&lt;spec&gt;.yml</td>" in section
    assert "<td>" + "a" * 64 + "</td>" in section
    assert "<shape>" not in section
    assert "{'configuration'" not in section


def test_criteria_set_snapshots_limits_and_requires_nonempty_revision():
    limits = dict(criteria())
    supplied = CriteriaSet("synthetic-v1", limits)
    limits["tension"] = Criterion(70, "kN", "Revised synthetic limit", "maximum")
    assert compare(evidence(), supplied)[0]["status"] == "PASS"
    with pytest.raises(TypeError):
        supplied.limits["tension"] = limits["tension"]
    with pytest.raises(ValueError, match="criteria revision"):
        CriteriaSet(" ", limits)


@pytest.mark.parametrize(
    "metric,unit,limit",
    [("tension", "kN", 100), ("curvature", "1/m", 0.25), ("clearance", "m", 0)],
)
def test_scalar_rounding_never_hides_unequal_values(metric, unit, limit):
    # Maximum criteria fail just above the limit; minimum criteria fail just below.
    value = limit + 1e-8 if metric != "clearance" else limit - 1e-8
    record = replace(evidence(), **{metric: measurement(value, unit)})
    report = render_report([record], criteria(), revision="1", date="2026-10-20")
    relation = "above" if metric != "clearance" else "below"
    assert f"({relation} limit; difference below display resolution)" in report
    index = list(criteria()).index(metric)
    assert compare(record, criteria())[index]["status"] == "FAIL"
    boundary = replace(record, **{metric: measurement(limit, unit)})
    assert compare(boundary, criteria())[index]["status"] == "PASS"


def test_exact_zero_mesh_change_remains_zero():
    from digitalmodel.subsea.flexible_pipes.configuration_comparison import MeshPair

    coarse = evidence()
    fine = replace(coarse, input_digest="d" * 64, output_digest="e" * 64)
    pair = MeshPair(coarse, fine, (measurement(2, "m"),), (measurement(1, "m"),))
    report = render_report(
        [coarse], criteria(), revision="1", date="2026-10-20", meshes=[pair]
    )
    assert report.count("<td>0.0</td><td>0.0 %</td>") == 3
    assert "<td>&lt;0.1 (nonzero)" not in report
