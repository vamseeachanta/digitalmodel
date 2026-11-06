# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Bind the complete pipelay report to hand arithmetic and committed bytes.

Numeric oracle source: independent hand derivations for locally selected illustrative
controls, documented below; no external input specification is identified. Tolerance is
32 binary64 epsilons for fewer than ten positive elementary operations.
"""

import ast
import hashlib
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_installation_pipelay_module_report.py"
REPORT = ROOT / "docs/reports/modules/installation_pipelay"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}
REBUILD = (
    'PYTHONPATH=src uv run --no-project python '
    'scripts/reports/build_installation_pipelay_module_report.py '
    '--scratch "$TMPDIR/pipelay-report"'
)
SECTIONS = [
    "Executive Summary", "Introduction", "Design Data and Basis",
    "Analysis Methodology", "Detailed Results", "Key Findings",
    "Limitations and Not Covered", "Recommendations", "References and Appendices",
    "Acronyms and Review Notes",
]
ROUND_OFF = 32 * sys.float_info.epsilon


def builder():
    spec = importlib.util.spec_from_file_location("pipelay_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def payload(page):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        page, re.S,
    )[1])


def test_worked_values_match_hand_calculation(tmp_path):
    """(200/2)/100000=1/1000; E*strain=200; F/A=20; sum=220.

    400*(3/4)=300; 220/300=11/15; 220/400*100=55; 220<=300.
    Independent route equilibrium: 100000/((1/2)*1000)=200 m.
    Stress units are N/mm², geometry is mm, and the separate route uses N/m.
    """
    build = builder()
    page = build.render(tmp_path / "calculation")
    data = payload(page)
    assert set(data) == {"inputs", "input_units", "results", "qualification", "methods"}
    assert data["inputs"] == {
        "stress": {"outer_diameter": 200, "bend_radius": 100000,
                   "youngs_modulus": 200000, "axial_force": 20000,
                   "steel_area": 1000, "smys": 400, "allowable_fraction": 3 / 4},
        "route": {"bottom_tension": 100000, "lateral_friction": 1 / 2,
                  "submerged_weight_per_length": 1000},
    }
    assert data["input_units"] == {
        "stress": {"outer_diameter": "mm", "bend_radius": "mm",
                   "youngs_modulus": "N/mm²", "axial_force": "N",
                   "steel_area": "mm²", "smys": "N/mm²", "allowable_fraction": "1"},
        "route": {"bottom_tension": "N", "lateral_friction": "1",
                  "submerged_weight_per_length": "N/m"},
    }
    expected = {
        "bending_strain": (1 / 1000, "1", "S1; S3"),
        "bending_stress": (200, "N/mm²", "S1; S2"),
        "axial_stress": (20, "N/mm²", "S1; S2"),
        "longitudinal_stress": (220, "N/mm²", "S1; S2"),
        "allowable_stress": (300, "N/mm²", "S1; S2"),
        "utilisation": (11 / 15, "1", "S1; S2"),
        "percent_smys": (55, "%", "S1; S2"),
        "passes": (True, "boolean", "S1; S2"),
        "route_radius": (200, "m", "S1; S3"),
    }
    assert set(data["results"]) == set(expected)
    for name, (value, unit, source) in expected.items():
        row = data["results"][name]
        assert set(row) == {"value", "unit", "source"}
        if isinstance(value, bool):
            assert row["value"] is value
        else:
            assert row["value"] == pytest.approx(value, rel=ROUND_OFF, abs=0)
        assert row["unit"] == unit
        assert row["source"] == source
        assert build.result_row(name, row) in page
    assert data["qualification"] == "ARITHMETIC_ONLY"
    assert re.findall(r'<h2 id="s\d+">\d+\. (.*?)</h2>', page) == SECTIONS
    assert re.search(
        r'<h2 id="s1">[^<]+</h2>\s*<p>[^<]+</p>\s*<h3>Revision history</h3>',
        page, re.S,
    )
    assert "changes-panel" in page and "showSaveFilePicker" in page
    assert "NOT_EVALUATED" in page and 'class="hs-doc"' in page
    methods = data["methods"]
    actual_functions = set()
    for module in ("stress", "integrity"):
        path = ROOT / f"src/digitalmodel/installation/pipelay/{module}.py"
        actual_functions.update(
            f"{module}.{node.name}" for node in ast.parse(path.read_text(encoding="utf-8")).body
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_")
        )
    assert {row["name"] for row in methods} == actual_functions
    assert all(row["review"] == "DIMENSIONALLY_REVIEWED" for row in methods)
    for row in methods:
        assert row["limits"] and row["evidence_needed"]
        if "concrete" in row["name"] or "weld_repair" in row["name"]:
            assert row["qualification"] == "NOT_EVALUATED"


def test_rebuild_is_deterministic(tmp_path):
    build = builder()
    outputs = [tmp_path / "first", tmp_path / "second"]
    for output in outputs:
        build.build(output, tmp_path / (output.name + "-scratch"))
        assert {path.name for path in output.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (outputs[0] / name).read_bytes() == (outputs[1] / name).read_bytes(), REBUILD
        assert (outputs[0] / name).read_bytes() == (REPORT / name).read_bytes(), REBUILD


def test_input_basis_is_local_and_pending_acceptance(tmp_path):
    """Reject invented input authority or an implied accepted design basis."""
    build = builder()
    output = tmp_path / "report"
    manifest = build.build(output, tmp_path / "scratch")
    page = (output / "index.html").read_text(encoding="utf-8")
    reference = re.search(r'<li id="S1">(.*?)</li>', page, re.S)[1]
    for text in (reference, (output / "README.md").read_text(encoding="utf-8")):
        text = " ".join(text.lower().split())
        assert "locally selected illustrative arithmetic controls" in text
        assert "pending maintainer acceptance" in text
        assert "not qualified design inputs" in text
        assert "worked-example specification" not in text
    assert manifest["input_basis"] == (
        "Locally selected illustrative arithmetic controls; not qualified design inputs"
    )
    assert manifest["input_basis_review"] == "PENDING_MAINTAINER_ACCEPTANCE"


def test_cli_writes_matching_manifest(tmp_path):
    output = tmp_path / "published"
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    subprocess.run(
        [sys.executable, str(SCRIPT), "--output-dir", str(output),
         "--scratch", str(tmp_path / "scratch")], cwd=ROOT, env=env,
        check=True, capture_output=True, text=True, encoding="utf-8",
    )
    manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
    expected_sources = {
        "scripts/reports/build_installation_pipelay_module_report.py",
        "src/digitalmodel/installation/pipelay/stress.py",
        "src/digitalmodel/installation/pipelay/integrity.py",
        "tests/installation/test_pipelay_report_hand_values.py",
        "tests/installation/test_pipelay_stress.py",
        "tests/installation/test_pipelay_stress_finite_inputs.py",
        "tests/installation/test_pipelay_integrity.py",
        "tests/reporting/test_installation_pipelay_module_report.py",
        "docs/api/_assets/brand.css",
    }
    assert set(manifest["sources"]) == expected_sources
    for path, digest in manifest["sources"].items():
        assert digest == hashlib.sha256((ROOT / path).read_bytes()).hexdigest(), REBUILD
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    for path, digest in manifest["artifacts"].items():
        assert digest == hashlib.sha256((output / path).read_bytes()).hexdigest(), REBUILD
    assert (output / "manifest.json").read_bytes() == (REPORT / "manifest.json").read_bytes(), REBUILD
    assert json.loads((output / "changes.json").read_text(encoding="utf-8")) == builder().CHANGES


def test_result_display_follows_calculation(tmp_path, monkeypatch):
    """A sentinel result will expose a typed-in result in the rendered table."""
    build = builder()
    calculate = build.calculate

    def changed(scratch):
        data = calculate(scratch)
        # Synthetic display sentinel, unrelated to an engineering criterion.
        data["results"]["longitudinal_stress"]["value"] = 221
        return data

    monkeypatch.setattr(build, "calculate", changed)
    page = build.render(tmp_path / "scratch")
    row = payload(page)["results"]["longitudinal_stress"]
    assert row["value"] == 221
    assert build.result_row("longitudinal_stress", row) in page


def test_checkout_scratch_is_rejected_before_writes(tmp_path):
    """The checkout and resolved symlink targets will be protected from staging."""
    build = builder()
    before = {name: hashlib.sha256((REPORT / name).read_bytes()).hexdigest()
              for name in DELIVERABLES}
    link = tmp_path / "checkout-link"
    link.symlink_to(ROOT, target_is_directory=True)
    for scratch in (ROOT, ROOT / "report-scratch", link / "report-scratch"):
        with pytest.raises(ValueError, match="outside the checkout"):
            build.build(tmp_path / "output", scratch)
    assert not (ROOT / "report-scratch").exists()
    assert not (tmp_path / "output").exists()
    assert before == {name: hashlib.sha256((REPORT / name).read_bytes()).hexdigest()
                      for name in DELIVERABLES}


@pytest.fixture
def disposable_builder(tmp_path):
    """Exercise output protection without risking the real checkout."""
    checkout = tmp_path / "checkout"
    for path in builder().SOURCES:
        target = checkout / path
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / path, target)
    spec = importlib.util.spec_from_file_location("disposable_report", checkout / SCRIPT.relative_to(ROOT))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("location", ["root", "source", "alias"])
def test_checkout_output_is_rejected_before_calculation(disposable_builder, tmp_path, monkeypatch, location):
    build = disposable_builder
    protected = build.ROOT / "README.md"
    protected.write_bytes(b"unrelated checkout content\n")
    alias = tmp_path / "checkout-alias"
    alias.symlink_to(build.ROOT, target_is_directory=True)
    output = {"root": build.ROOT, "source": build.ROOT / "src/digitalmodel/installation/pipelay",
              "alias": alias}[location]
    calculated = []
    monkeypatch.setattr(build, "render", lambda _: calculated.append(True) or "unexpected")
    with pytest.raises(ValueError, match="output inside checkout"):
        build.build(output, tmp_path / "scratch")
    assert protected.read_bytes() == b"unrelated checkout content\n"
    assert not calculated
    assert not (output / "index.html").exists()
    assert not (tmp_path / "scratch").exists()


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
@pytest.mark.parametrize("existing", [True, False])
def test_output_symlink_is_rejected_before_any_write(disposable_builder, tmp_path, monkeypatch, name, existing):
    build = disposable_builder
    output = tmp_path / "report"
    output.mkdir()
    protected = tmp_path / "unrelated.txt"
    if existing:
        protected.write_bytes(b"unrelated content\n")
    for other in DELIVERABLES - {name}:
        (output / other).write_bytes(b"previous report\n")
    (output / name).symlink_to(protected)
    calculated = []
    monkeypatch.setattr(build, "render", lambda _: calculated.append(True) or "unexpected")
    with pytest.raises(ValueError, match="symlink"):
        build.build(output, tmp_path / "scratch")
    assert (output / name).is_symlink()
    if existing:
        assert protected.read_bytes() == b"unrelated content\n"
    else:
        assert not protected.exists()
    assert all((output / other).read_bytes() == b"previous report\n"
               for other in DELIVERABLES - {name})
    assert not calculated
    assert not (tmp_path / "scratch").exists()


def test_named_checkout_report_destination_is_allowed(disposable_builder, tmp_path):
    build = disposable_builder
    build.build(build.DESTINATION, tmp_path / "scratch")
    assert {path.name for path in build.DESTINATION.iterdir()} == DELIVERABLES


def test_parent_workspace_output_preserves_unrelated_readme(disposable_builder, tmp_path, monkeypatch):
    build = disposable_builder
    output = build.ROOT.parent
    protected = output / "README.md"
    protected.write_bytes(b"unrelated workspace content\n")
    calculated = []
    monkeypatch.setattr(build, "render", lambda _: calculated.append(True) or "unexpected")
    with pytest.raises(ValueError, match="external report directory"):
        build.build(output, tmp_path / "scratch")
    assert protected.read_bytes() == b"unrelated workspace content\n"
    assert not calculated
    assert not (output / "index.html").exists()
    assert not (tmp_path / "scratch").exists()


@pytest.mark.parametrize("case", [
    "readme-only", "invalid-json", "unknown-schema", "missing-schema", "list", "null",
    "extra-file", "extra-directory", "deliverable-directory", "output-file",
])
def test_unrecognised_external_output_is_rejected(disposable_builder, tmp_path, monkeypatch, case):
    build = disposable_builder
    output = tmp_path / "external-report"
    if case == "output-file":
        output.write_bytes(b"unrelated file\n")
        before = output.read_bytes()
    else:
        output.mkdir()
        (output / "README.md").write_bytes(b"unrelated content\n")
        manifest = output / "manifest.json"
        manifest.write_text('{"schema": "installation-pipelay-report/1"}',
                            encoding="utf-8", newline="\n")
        if case == "readme-only":
            # A missing manifest must not authorize replacing an existing README.
            manifest.rename(tmp_path / "saved-manifest.json")
        elif case in {"invalid-json", "unknown-schema", "missing-schema", "list", "null"}:
            manifest.write_text({
                "invalid-json": "{", "unknown-schema": '{"schema": "other-report/1"}',
                "missing-schema": "{}", "list": "[]", "null": "null",
            }[case], encoding="utf-8", newline="\n")
        elif case == "extra-file":
            (output / "comments.txt").write_bytes(b"unrelated comments\n")
        elif case == "extra-directory":
            (output / "notes").mkdir()
        elif case == "deliverable-directory":
            (output / "index.html").mkdir()
        before = {path.name: path.read_bytes() if path.is_file() else None
                  for path in output.iterdir()}
    calculated = []
    monkeypatch.setattr(build, "render", lambda _: calculated.append(True) or "unexpected")
    with pytest.raises(ValueError, match="external report directory"):
        build.build(output, tmp_path / "scratch")
    if output.is_file():
        assert output.read_bytes() == before
    else:
        assert {path.name: path.read_bytes() if path.is_file() else None
                for path in output.iterdir()} == before
    assert not calculated
    assert not (tmp_path / "scratch").exists()


@pytest.mark.parametrize("state", ["absent", "empty", "recognised"])
def test_external_output_can_be_created_and_rebuilt(disposable_builder, tmp_path, state):
    build = disposable_builder
    output = tmp_path / "report"
    if state != "absent":
        output.mkdir()
    if state == "recognised":
        (output / "manifest.json").write_text(
            '{"schema": "installation-pipelay-report/1"}', encoding="utf-8", newline="\n",
        )
    build.build(output, tmp_path / "scratch")
    before = {path.name: path.read_bytes() for path in output.iterdir()}
    assert set(before) == DELIVERABLES
    build.build(output, tmp_path / "scratch")
    assert {path.name: path.read_bytes() for path in output.iterdir()} == before


@pytest.mark.parametrize("state", ["absent", "empty", "recognised"])
@pytest.mark.parametrize("location", ["equal", "nested", "alias"])
def test_scratch_inside_output_is_rejected_before_writes(
    disposable_builder, tmp_path, monkeypatch, state, location,
):
    """Overlapping staging must preserve output bytes and create no directories."""
    build = disposable_builder
    output = tmp_path / "report"
    if state != "absent":
        output.mkdir()
    if state == "recognised":
        (output / "manifest.json").write_bytes(
            b'{"schema": "installation-pipelay-report/1"}\n',
        )
    before = {path.name: path.read_bytes() for path in output.iterdir()} if output.exists() else None
    scratch = output if location == "equal" else output / "scratch"
    if location == "alias":
        alias = tmp_path / "report-alias"
        alias.symlink_to(output, target_is_directory=True)
        scratch = alias / "scratch"
    rendered = []
    render = build.render

    def record_render(path):
        rendered.append(True)
        return render(path)

    monkeypatch.setattr(build, "render", record_render)
    with pytest.raises(ValueError, match="scratch.*output"):
        build.build(output, scratch)
    assert not rendered
    if before is None:
        assert not output.exists()
    else:
        assert {path.name: path.read_bytes() for path in output.iterdir()} == before


def test_deliverable_junction_is_rejected_before_calculation(
    disposable_builder, tmp_path, monkeypatch,
):
    """Resolve directory links even when the symlink predicate does not identify them."""
    build = disposable_builder
    output = build.DESTINATION
    output.mkdir(parents=True)
    junction = output / "index.html"
    target = tmp_path / "protected-content"
    target.mkdir()
    junction.symlink_to(target, target_is_directory=True)
    protected = target / "retained.txt"
    protected.write_bytes(b"unrelated content\n")
    # Junctions resolve to their target without being reported as symlinks.
    is_symlink = Path.is_symlink
    monkeypatch.setattr(Path, "is_symlink", lambda path: path != junction and is_symlink(path))
    with pytest.raises(ValueError, match="junction"):
        build.build(output, tmp_path / "scratch")
    assert protected.read_bytes() == b"unrelated content\n"
    assert {path.name for path in output.iterdir()} == {"index.html"}
    assert not (tmp_path / "scratch").exists()


def test_external_output_is_revalidated_before_publication(disposable_builder, tmp_path, monkeypatch):
    build = disposable_builder
    output = tmp_path / "report"
    output.mkdir()
    render = build.render

    def create_unrelated_file(scratch):
        page = render(scratch)
        (output / "README.md").write_bytes(b"new unrelated content\n")
        return page

    monkeypatch.setattr(build, "render", create_unrelated_file)
    with pytest.raises(ValueError, match="external report directory"):
        build.build(output, tmp_path / "scratch")
    assert {path.name for path in output.iterdir()} == {"README.md"}
    assert (output / "README.md").read_bytes() == b"new unrelated content\n"


def test_constant_inventory_scope_and_download_cleanup(tmp_path):
    page = builder().render(tmp_path / "scratch")
    assert "Module constants are outside this function inventory" in page
    assert "setTimeout(() => URL.revokeObjectURL(url), 1000)" in page
    assert "link.click(); URL.revokeObjectURL(url)" not in page


@pytest.fixture(params=["plain", "global-excludes", "foreign-bindings"])
def retention_environment(request, tmp_path, monkeypatch):
    """Exercise retention rules independently of caller Git settings."""
    if request.param == "global-excludes":
        excludes = tmp_path / "global-excludes"
        excludes.write_text("*.txt\n*.json\n*.html\n*.md\n*.py\n", encoding="utf-8", newline="\n")
        config = tmp_path / "global-config"
        config.write_text(f'[core]\n\texcludesFile = "{excludes.as_posix()}"\n',
                          encoding="utf-8", newline="\n")
        monkeypatch.setenv("GIT_CONFIG_GLOBAL", str(config))
    elif request.param == "foreign-bindings":
        for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
            monkeypatch.setenv(key, str(tmp_path / "foreign-checkout"))


def repository_ignore_pattern(path):
    """Require the repository rule, rather than a caller's exclusion source."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    result = subprocess.run(
        ["git", "-c", "core.excludesFile=", "check-ignore", "-v", "--no-index", "--stdin"],
        cwd=ROOT, env=env, input=path + "\n", capture_output=True, text=True,
        encoding="utf-8", check=False,
    )
    assert result.returncode == 0, result.stderr
    source, reported_path = result.stdout.rstrip("\n").split("\t")
    source_name, line, pattern = source.split(":", 2)
    assert source_name == ".gitignore"
    assert line.isdecimal()
    assert reported_path == path
    return pattern


@pytest.mark.parametrize("name", ["comments.txt", "scratch.json", "nested/comments.txt"])
def test_unpublished_report_files_stay_ignored(name, retention_environment):
    """Keep exported comments and scratch files out of the published report."""
    path = (REPORT / name).relative_to(ROOT).as_posix()
    assert repository_ignore_pattern(path) == "/docs/reports/modules/installation_pipelay/*"


@pytest.mark.parametrize("path", [
    *(f"docs/reports/modules/installation_pipelay/{name}" for name in sorted(DELIVERABLES)),
    "scripts/reports/build_installation_pipelay_module_report.py",
])
def test_published_report_files_remain_trackable(path, retention_environment):
    """Retain exactly the named report deliverables and their builder."""
    assert repository_ignore_pattern(path) == "!/" + path
