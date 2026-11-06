# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Report arithmetic, display wiring and exact-byte delivery for issue 3308."""

import copy
import hashlib
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from types import SimpleNamespace

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_offshore_resilience_module_report.py"
REPORT = ROOT / "docs/reports/modules/offshore_resilience"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}
MODULES = ("minimum_facility", "installation_checklist", "lifecycle_planning", "structural_health")
# Absolute roundoff allowance for a single binary64 division; not a design limit.
RATIO_ABS_TOLERANCE = 1e-15
REBUILD_COMMAND = (
    "PYTHONPATH=src uv run --no-sync python "
    "scripts/reports/build_offshore_resilience_module_report.py"
)
EXPECTED_INTERFACES = {
    "minimum_facility": {
        "InstallationMethod", "BseePlatformClass", "MinimumFacilityPlatform",
        "MinimumFacilityPlatform.__post_init__", "MinimumFacilityPlatform.total_weight_tonnes",
        "MinimumFacilityPlatform.crane_installable", "MinimumFacilityPlatform.water_depth_midpoint_ft",
        "ModularCostComparison", "ModularCostComparison.__post_init__",
        "ModularCostComparison.net_savings_usd", "ModularCostComparison.savings_fraction",
        "classify_bsee_platform", "lifecycle_value_index",
    },
    "installation_checklist": {
        "ChecklistStatus", "HammerType", "FoundationType", "ChecklistItem", "ChecklistItem.set_value",
        "InstallationChecklist", "InstallationChecklist.pass_count", "InstallationChecklist.fail_count",
        "InstallationChecklist.pending_count", "InstallationChecklist.completion_ratio",
        "InstallationChecklist.is_complete", "InstallationChecklist.summary",
        "build_structure_checklist", "build_wind_foundation_checklist",
    },
    "lifecycle_planning": {
        "DecomFlagSeverity", "PipelineLayout", "DecomFlag", "decom_flags_for_design_choices",
        "score_tieback_accommodation", "marginal_field_lifecycle_score",
    },
    "structural_health": {
        "AlertSeverity", "MeasurementType", "AssetType", "StructuralHealthRecord",
        "StructuralHealthRecord.__post_init__", "SensorConfig", "SensorConfig.__post_init__",
        "SensorTemplate", "build_sensor_template", "AlertThresholdConfig", "build_alert_thresholds",
    },
}
EXPECTED_HAND_CHECKED = {
    "minimum_facility.MinimumFacilityPlatform.total_weight_tonnes",
    "minimum_facility.MinimumFacilityPlatform.crane_installable",
    "minimum_facility.MinimumFacilityPlatform.water_depth_midpoint_ft",
    "installation_checklist.ChecklistItem.set_value",
    "installation_checklist.InstallationChecklist.pass_count",
    "installation_checklist.InstallationChecklist.fail_count",
    "installation_checklist.InstallationChecklist.pending_count",
    "installation_checklist.InstallationChecklist.completion_ratio",
    "installation_checklist.InstallationChecklist.is_complete",
    "installation_checklist.InstallationChecklist.summary",
}


def builder():
    spec = importlib.util.spec_from_file_location("offshore_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class TableCells(HTMLParser):
    """Read visible result cells by stable field identifiers."""

    def __init__(self, page):
        super().__init__()
        self.cells = {}
        self.active = None
        self.feed(page)

    def handle_starttag(self, tag, attrs):
        if tag == "td":
            self.active = dict(attrs).get("id")
            if self.active:
                self.cells[self.active] = ""

    def handle_data(self, data):
        if self.active:
            self.cells[self.active] += data

    def handle_endtag(self, tag):
        if tag == "td":
            self.active = None


def test_facility_payload_matches_weight_and_midpoint():
    """Expected values are independent addition, averaging and comparison."""
    build = builder()
    data = build.calculate()
    facility = data["facility"]
    assert facility["inputs"] == {
        "platform_type": "wellhead", "topsides_weight_tonnes": 120,
        "jacket_weight_tonnes": 80, "water_depth_range_ft": [100, 300],
        "installation_method": "crane_barge", "max_crane_capacity_tonnes": 200,
        "well_slots": 1, "design_life_years": 1, "expansion_capability": False,
    }
    assert facility["total_weight_tonnes"] == 200
    assert facility["water_depth_midpoint_ft"] == 200
    assert facility["crane_cases"] == [
        {"capacity_tonnes": 200, "crane_installable": True},
        {"capacity_tonnes": 199, "crane_installable": False},
    ]
    cells = TableCells(build.render(data)).cells
    assert cells["total-weight"] == "200"
    assert cells["depth-midpoint"] == "200"
    assert cells["crane-200"] == "PASS"
    assert cells["crane-199"] == "FAIL"
    for field, expected in {
        "topsides_weight_tonnes": "120", "jacket_weight_tonnes": "80",
        "water_depth_range_ft": "100, 300", "max_crane_capacity_tonnes": "200",
        "well_slots": "1", "design_life_years": "1", "expansion_capability": "False",
        "platform_type": "wellhead", "installation_method": "crane_barge",
    }.items():
        assert cells[f"input-{field}"] == expected


def test_checklist_payload_matches_enumerated_states():
    """One passed, one failed, one pending; then three passed, by enumeration."""
    build = builder()
    data = build.calculate()
    assert data["checklist"]["allowed_interval_m"] == [2, 4]
    states = data["checklist"]["states"]
    assert [state["name"] for state in states] == ["mixed", "all_pass"]
    expected = [
        ({"total": 3, "pass": 1, "fail": 1, "pending": 1}, [2, 5, None],
         ["pass", "fail", "pending"], 1 / 3, False),
        ({"total": 3, "pass": 3, "fail": 0, "pending": 0}, [2, 3, 4],
         ["pass", "pass", "pass"], 1, True),
    ]
    cells = TableCells(build.render(data)).cells
    for state, (summary, values, statuses, ratio, complete) in zip(states, expected, strict=True):
        assert state["summary"] == summary
        assert state["completion_ratio"] == pytest.approx(ratio, rel=0, abs=RATIO_ABS_TOLERANCE)
        assert state["is_complete"] is complete
        assert state["items"] == [
            {"parameter": key, "actual_value": value, "status": status}
            for key, value, status in zip(
                ("check_a", "check_b", "check_c"), values, statuses, strict=True,
            )
        ]
        for key, value in summary.items():
            assert cells[f'{state["name"]}-{key}'] == str(value)
        assert cells[f'{state["name"]}-is-complete'] == str(complete)
        assert cells[f'{state["name"]}-ratio'] == f'{summary["pass"]}/{summary["total"]} ({ratio:.1%})'
        for item in state["items"]:
            prefix = f'{state["name"]}-{item["parameter"]}'
            assert cells[f"{prefix}-value"] == (
                "NOT_ENTERED" if item["actual_value"] is None else str(item["actual_value"])
            )
            assert cells[f"{prefix}-status"] == item["status"]


def test_rebuild_is_deterministic(tmp_path):
    build = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    build.build(tmp_path / "scratch-first", first)
    build.build(tmp_path / "scratch-second", second)
    for output in (first, second):
        assert {path.name for path in output.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (first / name).read_bytes() == (second / name).read_bytes(), REBUILD_COMMAND
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), REBUILD_COMMAND


def test_cli_manifest_matches_written_files(tmp_path):
    """Manifest hashes cover sources and the three non-self-referential artifacts."""
    output = tmp_path / "cli-output"
    environment = {key: value for key, value in os.environ.items() if key not in {
        "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR",
    }}
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(tmp_path / "cli-scratch"),
         "--output-dir", str(output)], cwd=ROOT, env=environment, check=True,
        capture_output=True, text=True, encoding="utf-8",
    )
    manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    required = {SCRIPT.relative_to(ROOT).as_posix(),
                "tests/reporting/test_offshore_resilience_module_report.py",
                "tests/structural/offshore_resilience/test_module_report_hand_values.py"}
    required.update(f"src/digitalmodel/structural/offshore_resilience/{name}.py" for name in MODULES)
    assert set(manifest["sources"]) == required
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((output / name).read_bytes()).hexdigest() == digest, REBUILD_COMMAND
    for name, digest in manifest["sources"].items():
        assert not Path(name).is_absolute()
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest, REBUILD_COMMAND
    assert (output / "manifest.json").read_bytes() == (REPORT / "manifest.json").read_bytes(), REBUILD_COMMAND


def test_report_sections_inventory_and_limitations():
    build = builder()
    page = build.render(build.calculate())
    assert re.findall(r'<h2 id="s\d+">([^<]+)</h2>', page) == [
        "1. Executive Summary", "2. Scope and Objectives", "3. Design Data",
        "4. Methodology", "5. Detailed Results", "6. Key Findings",
        "7. Limitations and Not Covered", "8. Recommendations",
        "9. References and Appendices", "10. Acronyms and Review Notes",
    ]
    assert page.index('id="revision-history"') < page.index('id="s2"')
    assert page.index('id="s1"') < page.index('id="revision-history"')
    assert 'id="changes-panel"' in page
    assert "passed checks, not the fraction entered" in page
    for phrase in ("regulatory", "cost", "sensor", "lifecycle", "operational approval"):
        assert phrase in page.lower()
    assert "NOT_EVALUATED" in page and "Synthetic constructor control" in page
    assert "allowed interval [2, 4] m" in page
    assert "1e-15" in page
    inventory = {row["method"]: row for row in build.method_inventory()}
    assert set(inventory) == {
        f"{module}.{name}" for module, names in EXPECTED_INTERFACES.items() for name in names
    }
    for start, end in (("s2", "s3"), ("s6", "s7")):
        section = page.split(f'<h2 id="{start}">', 1)[1].split(f'<h2 id="{end}">', 1)[0]
        assert "Module-level constants are excluded" in section
    assert any(row["status"] == "NOT_EVALUATED" for row in inventory.values())
    assert {name for name, row in inventory.items()
            if row["status"] == "HAND_CHECKED_SYNTHETIC"} == EXPECTED_HAND_CHECKED
    assert all(row["status"] == "NOT_EVALUATED"
               for name, row in inventory.items() if name not in EXPECTED_HAND_CHECKED)
    for row in inventory.values():
        assert row["method"] in page
        assert row["status"] in page
        assert row["limitation"]


def test_display_uses_returned_values():
    """Mutation probes establish wiring; their numbers are synthetic controls."""
    build = builder()
    data = copy.deepcopy(build.calculate())
    data["facility"]["total_weight_tonnes"] = 201
    data["facility"]["water_depth_midpoint_ft"] = 202
    data["facility"]["crane_cases"][0]["crane_installable"] = False
    data["checklist"]["states"][0]["summary"]["pass"] = 2
    data["checklist"]["states"][0]["completion_ratio"] = 2 / 3
    cells = TableCells(build.render(data)).cells
    assert cells["total-weight"] == "201"
    assert cells["depth-midpoint"] == "202"
    assert cells["crane-200"] == "FAIL"
    assert cells["mixed-pass"] == "2"
    assert cells["mixed-ratio"] == "2/3 (66.7%)"


def test_summary_binds_returned_results_to_supplied_criteria():
    build = builder()
    page = build.render(build.calculate())
    summary = page.split('<h2 id="s1">', 1)[1].split('<h3 id="revision-history">', 1)[0]
    assert "combined mass is 200 t" in summary
    assert "depth midpoint is 200 ft" in summary
    assert "200 t: PASS; 199 t: FAIL" in summary
    assert "mixed passed fraction is 1/3 (33.3%)" in summary
    assert "final passed fraction is 3/3 (100.0%)" in summary


@pytest.mark.parametrize("provide_scratch", [False, True])
def test_cli_needs_no_scratch_files(tmp_path, provide_scratch):
    """Rebuild without scratch; a legacy option remains inert for compatibility."""
    output = tmp_path / "output"
    scratch = tmp_path / "unused-scratch"
    command = [sys.executable, str(SCRIPT), "--output-dir", str(output)]
    if provide_scratch:
        command.extend(["--scratch", str(scratch)])
    environment = {key: value for key, value in os.environ.items() if key not in {
        "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR",
    }}
    subprocess.run(command, cwd=ROOT, env=environment, check=True,
                   capture_output=True, text=True, encoding="utf-8")
    assert not scratch.exists()
    assert {path.name for path in output.iterdir()} == DELIVERABLES
    assert "--scratch" not in (output / "README.md").read_text(encoding="utf-8")


def test_input_basis_identifies_local_controls_pending_acceptance():
    build = builder()
    page = build.render(build.calculate())
    reference = page.split("<p>[S1] ", 1)[1].split("</p>", 1)[0]
    assert "Locally selected illustrative controls" in reference
    assert "defined in" in reference and "build_offshore_resilience_module_report.py" in reference
    assert "pending maintainer acceptance" in reference
    assert "Issue 3308 synthetic arithmetic specification" not in page
    assert "locally selected illustrative controls" in build.readme()


def test_hand_qualification_is_bound_to_the_source_module():
    """A same-named interface in another module shall not inherit qualification."""
    build = builder()
    assert build.method_assessment("installation_checklist", "InstallationChecklist.summary")[0] == "HAND_CHECKED_SYNTHETIC"
    assert build.method_assessment("lifecycle_planning", "InstallationChecklist.summary")[0] == "NOT_EVALUATED"


def test_review_note_download_defers_object_url_cleanup():
    """Execute the rendered handler; cleanup shall occur after the click task."""
    build = builder()
    script = build.render(build.calculate()).split("<script>", 1)[1].split("</script>", 1)[0]
    harness = """
const assert = require('node:assert/strict');
const tasks = [], events = [], notes = {value: 'Synthetic review note'}, status = {};
let handler;
global.document = {
  getElementById(id) {
    if (id === 'save-notes') return {addEventListener: (_, fn) => {handler = fn;}};
    return id === 'notes' ? notes : status;
  },
  createElement: () => ({click() {events.push('click');}})
};
global.URL = {createObjectURL: () => 'blob:synthetic',
              revokeObjectURL: () => {events.push('revoke');}};
global.setTimeout = (fn) => {tasks.push(fn);};
"""
    checks = """
handler();
assert.deepEqual(events, ['click']);
assert.equal(tasks.length, 1);
tasks[0]();
assert.deepEqual(events, ['click', 'revoke']);
assert.match(status.textContent, /Download requested/);
"""
    subprocess.run(["node", "-"], input=harness + script + checks, check=True,
                   capture_output=True, text=True, encoding="utf-8")


def test_output_cannot_overwrite_unrelated_checkout_files(tmp_path, monkeypatch):
    build = builder()
    disposable_root = tmp_path / "checkout"
    disposable_root.mkdir()
    before = b"Preserved fixture content\n"
    (disposable_root / "README.md").write_bytes(before)
    monkeypatch.setattr(build, "ROOT", disposable_root)
    with pytest.raises(ValueError, match="named report directory"):
        build.build(tmp_path / "scratch", disposable_root)
    link = tmp_path / "output-link"
    link.symlink_to(disposable_root, target_is_directory=True)
    with pytest.raises(ValueError, match="named report directory"):
        build.build(tmp_path / "scratch", link)
    assert (disposable_root / "README.md").read_bytes() == before
    assert {path.name for path in disposable_root.iterdir()} == {"README.md"}


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
@pytest.mark.parametrize("dangling", [False, True], ids=["existing", "dangling"])
@pytest.mark.parametrize("inside_checkout", [False, True], ids=["external", "report"])
def test_output_file_symlinks_are_rejected_before_any_write(
    tmp_path, monkeypatch, name, dangling, inside_checkout,
):
    """All targets are preflighted; unrelated files and artifacts are preserved."""
    build = builder()
    checkout = tmp_path / "checkout"
    for source in build.SOURCES:
        destination = checkout / source
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / source, destination)
    report = checkout / "docs/reports/modules/offshore_resilience"
    output = report if inside_checkout else tmp_path / "output"
    output.mkdir(parents=True)
    unrelated = tmp_path / "unrelated.txt"
    preserved = b"Unrelated fixture content\n"
    if not dangling:
        unrelated.write_bytes(preserved)
    for artifact in DELIVERABLES - {name}:
        (output / artifact).write_bytes(b"Existing artifact\n")
    (output / name).symlink_to(unrelated)
    before = {artifact: hashlib.sha256((output / artifact).read_bytes()).hexdigest()
              for artifact in DELIVERABLES - {name}}
    sources_before = {source: hashlib.sha256((checkout / source).read_bytes()).hexdigest()
                      for source in build.SOURCES}
    monkeypatch.setattr(build, "ROOT", checkout)
    monkeypatch.setattr(build, "DESTINATION", report)
    with pytest.raises(ValueError, match="Output targets must not be symlinks"):
        build.build(tmp_path / "scratch", output)
    assert {path.name for path in output.iterdir()} == DELIVERABLES
    assert (output / name).is_symlink()
    assert (output / name).readlink() == unrelated
    assert unrelated.exists() is not dangling
    if not dangling:
        assert unrelated.read_bytes() == preserved
    assert before == {
        artifact: hashlib.sha256((output / artifact).read_bytes()).hexdigest()
        for artifact in DELIVERABLES - {name}
    }
    assert sources_before == {
        source: hashlib.sha256((checkout / source).read_bytes()).hexdigest()
        for source in build.SOURCES
    }


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
def test_output_reparse_points_are_rejected_before_any_write(tmp_path, monkeypatch, name):
    """Synthetic Windows stat attributes exercise junction/reparse rejection."""
    build = builder()
    output = tmp_path / "output"
    output.mkdir()
    for artifact in DELIVERABLES:
        (output / artifact).write_bytes(b"Preserved artifact\n")
    before = {artifact: (output / artifact).read_bytes() for artifact in DELIVERABLES}
    original_lstat = Path.lstat

    def reparse_lstat(path, *args, **kwargs):
        result = original_lstat(path, *args, **kwargs)
        if path == output / name:
            return SimpleNamespace(st_mode=result.st_mode, st_file_attributes=0x400,
                                   st_nlink=result.st_nlink)
        return result

    monkeypatch.setattr(Path, "lstat", reparse_lstat)
    with pytest.raises(ValueError, match="symlinks or reparse points"):
        build.build(tmp_path / "scratch", output)
    assert before == {artifact: (output / artifact).read_bytes() for artifact in DELIVERABLES}


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
@pytest.mark.parametrize("kind", ["hardlink", "directory"])
def test_output_aliases_and_non_regular_targets_are_rejected(tmp_path, name, kind):
    """A pre-existing target shall neither alias another file nor be a directory."""
    build = builder()
    output = tmp_path / "output"
    output.mkdir()
    unrelated = tmp_path / "unrelated.txt"
    unrelated.write_bytes(b"Preserved unrelated content\n")
    for artifact in DELIVERABLES - {name}:
        (output / artifact).write_bytes(b"Preserved artifact\n")
    if kind == "hardlink":
        (output / name).hardlink_to(unrelated)
    else:
        (output / name).mkdir()
    before = {artifact: (output / artifact).read_bytes() for artifact in DELIVERABLES - {name}}
    with pytest.raises(ValueError, match="single-link regular files"):
        build.build(tmp_path / "scratch", output)
    assert unrelated.read_bytes() == b"Preserved unrelated content\n"
    assert before == {artifact: (output / artifact).read_bytes()
                      for artifact in DELIVERABLES - {name}}
