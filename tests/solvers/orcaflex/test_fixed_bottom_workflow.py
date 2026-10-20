# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free contract tests with independently calculated synthetic answers.

Fixture values are arbitrary arithmetic inputs, not turbine predictions or
engineering coefficients. For times 0, 1, 2 s, Fx = -3, 0, 6 kN:
mean = (-3+0+6)/3 = 1 kN; min=-3, max=6, absolute peak=6 kN.
For Mx = -12, 0, 24 kN.m, mean=4 and absolute peak=24 kN.m.
Soft/baseline/stiff synthetic tower frequencies are 1, 2, 3 Hz:
relative changes (1-2)/2=-0.5 and (3-2)/2=0.5. These are definitions,
not values obtained from the implementation. Source of the sample mean:
NIST/SEMATECH e-Handbook, section 1.3.5.1, Measures of Location, online
edition read 2026-10-20. Synthetic soil factors 0.5, 1, 2 are arbitrary
arithmetic inputs. The second modal label has twice the tower frequency:
2, 4, 6 Hz, so it has the same relative changes. All other channels are zero;
their zero baselines have undefined relative changes, represented by None.
"""

import copy
import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "src/digitalmodel/solvers/orcaflex/fixed_bottom_workflow.py"
EXAMPLE = ROOT / "tests/solvers/orcaflex/fixtures/fixed_bottom/synthetic_exports.json"


@pytest.fixture
def workflow():
    spec = importlib.util.spec_from_file_location("fixed_bottom_workflow", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def exports():
    return json.loads(EXAMPLE.read_text())


def test_inventory_and_basis(workflow):
    inventory = workflow.inventory(ROOT)
    by_name = {Path(row["path"]).name: row for row in inventory}
    assert by_name["libdiscon.dll"]["held"] is True
    assert len(by_name["libdiscon.dll"]["sha256"]) == 64
    assert by_name["wind.bts"]["held"] is False
    assert by_name["wind.bts"]["sha256"] is None
    basis = workflow.read_basis(ROOT)
    # Independently transcribed: raw K02 L120,139,140; template L64; inp L40.
    assert {k: v["value"] for k, v in basis.items()} == {
        "depth_m": 30,
        "hs_m": 5,
        "tz_s": 6.51155,
        "steady_speed_m_s": 11.4,
        "turbulent_speed_m_s": 12,
    }
    assert all(v["source"] for v in basis.values())


def test_cases_keep_unselected_factors_missing(workflow):
    cases = workflow.prepare_cases(ROOT)
    assert len(cases) == 6
    assert len({c["case_id"] for c in cases}) == 6
    assert all(c["status"] == "NOT_EVALUATED" for c in cases)
    assert all(c["soil_factor"] is None for c in cases if c["soil"] != "baseline")
    assert all(c["soil_factor"] == 1 for c in cases if c["soil"] == "baseline")
    assert all(
        "wind.bts" in " ".join(c["blockers"]) for c in cases if c["wind"] == "turbulent"
    )


@pytest.mark.parametrize(
    "soft,stiff", [(0, 2), (1, 2), (0.5, 1), (-1, 2), (0.5, float("nan")), (True, 2)]
)
def test_invalid_factors(workflow, soft, stiff):
    with pytest.raises(ValueError):
        workflow.prepare_cases(ROOT, soft, stiff)


def test_reduction_and_sensitivity(workflow, exports):
    cases = workflow.prepare_cases(ROOT, 0.5, 2)
    report = workflow.analyse(cases, exports)
    baseline = report["results"]["steady_baseline"]
    assert baseline["loads"]["fx"]["sample_mean"] == 1
    assert baseline["loads"]["fx"]["minimum"] == -3
    assert baseline["loads"]["fx"]["maximum"] == 6
    assert baseline["loads"]["fx"]["absolute_peak"] == 6
    assert baseline["loads"]["mx"]["sample_mean"] == 4
    assert baseline["loads"]["mx"]["absolute_peak"] == 24
    assert (
        report["sensitivity"]["steady_softened"]["frequency_relative_change"][
            "tower_fa"
        ]
        == -0.5
    )
    assert (
        report["sensitivity"]["steady_stiffened"]["frequency_relative_change"][
            "tower_fa"
        ]
        == 0.5
    )
    assert report["results"]["turbulent_baseline"]["status"] == "NOT_EVALUATED"
    assert report["engineering_acceptance"] == "NOT_EVALUATED"
    assert baseline["synthetic"] is True


@pytest.mark.parametrize(
    "mutation",
    [
        lambda r: r.update(synthetic="false"),
        lambda r: r.update(solver_version=""),
        lambda r: r.update(input_sha256="bad"),
        lambda r: r.update(case_id="unknown"),
        lambda r: r.update(soil_factor=2),
        lambda r: r.update(representation="raw"),
        lambda r: r.update(frame="local"),
        lambda r: r.update(origin="tower base"),
        lambda r: r.update(force_unit="N"),
        lambda r: r.update(moment_unit="N.m"),
        lambda r: r.update(load_convention=""),
        lambda r: r.update(samples=[]),
        lambda r: r["samples"][1].update(time_s=0),
        lambda r: r["samples"][0].update(fx=float("inf")),
        lambda r: r["samples"][0].update(fz=True),
        lambda r: r["modes"][0].update(frequency_hz=0),
        lambda r: r["modes"][0].update(shape_evidence=""),
        lambda r: r["modes"].append(copy.deepcopy(r["modes"][0])),
        lambda r: r.update(window_s=[4, 5]),
        lambda r: r.update(window_s=[2, 1]),
    ],
)
def test_reject_ambiguous_or_invalid_exports(workflow, exports, mutation):
    mutation(exports[0])
    with pytest.raises(ValueError):
        workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)


def test_duplicate_and_mixed_results_rejected(workflow, exports):
    cases = workflow.prepare_cases(ROOT, 0.5, 2)
    with pytest.raises(ValueError):
        workflow.analyse(cases, exports + [exports[0]])
    exports[1]["synthetic"] = False
    with pytest.raises(ValueError):
        workflow.analyse(cases, exports)


def test_incompatible_comparisons_not_evaluated(workflow, exports):
    exports[1]["window_s"] = [1, 2]
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)
    assert report["sensitivity"]["steady_softened"]["status"] == "NOT_EVALUATED"


def test_window_includes_endpoints(workflow, exports):
    exports[0]["window_s"] = [1, 2]
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports[:1])
    assert report["results"]["steady_baseline"]["loads"]["fx"]["sample_mean"] == 3


def test_empty_report_and_html_escaping(workflow):
    cases = workflow.prepare_cases(ROOT)
    data = workflow.analyse(cases, [])
    data["note"] = "<script>alert(1)</script>"
    html = workflow.render_report(
        workflow.inventory(ROOT), workflow.read_basis(ROOT), cases, data
    )
    assert "<script>" not in html
    assert "&lt;script&gt;" in html
    assert "Revision history" in html
    assert "NOT_EVALUATED" in html
    assert "Synthetic" not in html or "synthetic" in html.lower()


def test_fresh_process_never_loads_solver():
    code = (
        "import importlib.util,sys;"
        f"s=importlib.util.spec_from_file_location('workflow',{str(SCRIPT)!r});"
        "m=importlib.util.module_from_spec(s);s.loader.exec_module(m);"
        "assert not any(n.startswith(('OrcFxAPI','digitalmodel')) for n in sys.modules)"
    )
    completed = subprocess.run(
        [sys.executable, "-c", code], capture_output=True, text=True
    )
    assert completed.returncode == 0, completed.stderr


def test_cli_end_to_end_and_no_overwrite(tmp_path):
    output = tmp_path / "report.html"
    args = [
        sys.executable,
        str(SCRIPT),
        "--root",
        str(ROOT),
        "--output",
        str(output),
        "--exports",
        str(EXAMPLE),
        "--soft-factor",
        ".5",
        "--stiff-factor",
        "2",
    ]
    result = subprocess.run(args, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    assert "SYNTHETIC" in output.read_text()
    before = output.read_bytes()
    assert subprocess.run(args, capture_output=True).returncode != 0
    assert output.read_bytes() == before


def test_missing_inventory_never_hashes_absent_inputs(workflow, tmp_path):
    assert all(
        not row["held"] and row["sha256"] is None
        for row in workflow.inventory(tmp_path)
    )


@pytest.mark.parametrize("field", ["comparison_basis_sha256", "soil_factor_source"])
def test_required_comparison_provenance(workflow, exports, field):
    exports[0].pop(field, None)
    with pytest.raises(ValueError):
        workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)


@pytest.mark.parametrize(
    "field,value",
    [
        ("comparison_basis_sha256", "1" * 64),
        ("load_convention", "Opposite sign convention"),
        ("window_s", [1, 2]),
    ],
)
def test_changed_non_soil_basis_prevents_sensitivity(workflow, exports, field, value):
    exports[1][field] = value
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)
    assert report["sensitivity"]["steady_softened"]["status"] == "NOT_EVALUATED"
    assert report["sensitivity"]["steady_softened"]["reason"]


def test_zero_load_baseline_not_zero_relative_change(workflow, exports):
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)
    assert (
        report["sensitivity"]["steady_softened"]["load_peak_relative_change"]["fy"]
        is None
    )


def test_shape_provenance_retained(workflow, exports):
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)
    assert report["results"]["steady_baseline"]["shape_evidence"]["tower_fa"]
    assert report["results"]["steady_baseline"]["soil_factor_source"]


def test_mismatched_mode_sets_not_compared(workflow, exports):
    exports[1]["modes"][0]["mode_key"] = "unmatched_shape"
    report = workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)
    assert report["sensitivity"]["steady_softened"]["status"] == "NOT_EVALUATED"


def test_mixed_solver_versions_rejected(workflow, exports):
    exports[1]["solver_version"] = "different"
    with pytest.raises(ValueError):
        workflow.analyse(workflow.prepare_cases(ROOT, 0.5, 2), exports)


def test_no_export_for_unselected_factor(workflow, exports):
    with pytest.raises(ValueError):
        workflow.analyse(workflow.prepare_cases(ROOT), exports)


@pytest.mark.parametrize(
    "filename,synthetic", [("report.html", False), ("synthetic-example.html", True)]
)
def test_committed_report_is_reproducible(workflow, exports, filename, synthetic):
    folder = ROOT / "docs/domains/orcaflex/wind/fixed-bottom-worked-example"
    cases = workflow.prepare_cases(
        ROOT, 0.5 if synthetic else None, 2 if synthetic else None
    )
    data = workflow.analyse(cases, exports if synthetic else [])
    assert (folder / filename).read_text() == workflow.render_report(
        workflow.inventory(ROOT), workflow.read_basis(ROOT), cases, data
    )


def test_document_links_resolve():
    from html.parser import HTMLParser
    from urllib.parse import unquote, urlparse

    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.targets = []

        def handle_starttag(self, tag, attrs):
            if tag == "a":
                self.targets.extend(v for k, v in attrs if k == "href")

    folder = ROOT / "docs/domains/orcaflex/wind/fixed-bottom-worked-example"
    for path in folder.glob("*.html"):
        parser = Links()
        parser.feed(path.read_text())
        for target in parser.targets:
            if not urlparse(target).scheme and not target.startswith("#"):
                assert (path.parent / unquote(target)).is_file(), target
