# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise static evidence classification without importing report builders."""

import copy
import importlib.util
import json
from pathlib import Path

import pytest

SPEC = importlib.util.spec_from_file_location(
    "report_evidence_audit",
    Path(__file__).resolve().parents[2] / "scripts/reporting/report_evidence_audit.py",
)
audit = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(audit)


@pytest.fixture
def tree(tmp_path):
    files = {
        "docs/reports/modules/alpha/index.html": "<html>Draft for maintainer review</html>\n",
        "scripts/reporting/build_alpha_report.py": "# docs/reports/modules/alpha/index.html\n",
        "tests/reporting/test_alpha_report.py": (
            '# scripts/reporting/build_alpha_report.py\n'
            '# docs/reports/modules/alpha/index.html\n'
            'def test_independent_hand_worked():\n    assert 42 == 42\n'
        ),
        ".claude/quality-gates.yaml": 'command: "python -m pytest tests/reporting/test_alpha_report.py -q"\n',
    }
    for path, text in files.items():
        target = tmp_path / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8", newline="\n")
    return tmp_path, list(files)


def run(tree, document=None):
    root, paths = tree
    return audit.build_audit(root, paths, "a" * 40, document)


def record(document):
    return document["reports"][0]


def config(tree, text):
    (tree[0] / ".claude/quality-gates.yaml").write_text(text, encoding="utf-8", newline="\n")


@pytest.mark.parametrize("command, expected", [
    ("python -m pytest tests/reporting/test_alpha_report.py -q", "EXPLICIT"),
    ("pytest tests/reporting/", "EXPLICIT"),
    ("uv run --no-project python -m pytest tests/reporting", "EXPLICIT"),
    ("pytest tests/other/test_beta.py", "MISSING"),
    ("uv run --no-project --with pytest python -m pytest tests/reporting -q", "EXPLICIT"),
    ("uv run --directory elsewhere pytest tests/reporting", "UNKNOWN"),
    ("pytest", "UNKNOWN"),
    ("pytest $TEST_PATHS", "UNKNOWN"),
    ("pytest tests/${GROUP}/", "UNKNOWN"),
    ("pytest tests/reporting/*.py", "UNKNOWN"),
    ("pytest tests/reporting/test_alpha_report.py::test_one", "UNKNOWN"),
    ("pytest tests/reporting --ignore=tests/reporting/test_alpha_report.py", "UNKNOWN"),
    ("pytest tests/reporting -k alpha", "UNKNOWN"),
    ("pytest tests/reporting -m unit", "UNKNOWN"),
    ("pytest tests/reporting --deselect=test_one", "UNKNOWN"),
    ("pytest tests/reporting --collect-only", "UNKNOWN"),
    ("pytest tests/reporting --pyargs", "UNKNOWN"),
    ("echo pytest tests/reporting", "UNKNOWN"),
    ("pytest tests/reporting && true", "UNKNOWN"),
    ("cd elsewhere; pytest tests/reporting", "UNKNOWN"),
    ("pytest tests/reporting --unknown-option value", "UNKNOWN"),
    ("pytest ../tests/reporting", "UNKNOWN"),
])
def test_selection_is_literal_and_conservative(tree, command, expected):
    config(tree, "command: " + json.dumps(command) + "\n")
    report = record(run(tree))
    assert report["selection_status"] == expected
    assert report["execution_evidence"]["status"] == "NOT_RUN"
    assert run(tree)["commands"]


@pytest.mark.parametrize("text", [
    'jobs:\n  alpha:\n    if: success()\n    run: pytest tests/reporting\n',
    'enabled: false\ncommand: pytest tests/reporting\n',
    'command: "pytest tests/reporting\n',
    'run: |\n  pytest tests/reporting \\\n    -q\n',
    'command: [pytest, tests/reporting]\n',
    'command: &shared pytest tests/reporting\n',
    'command: *shared\n',
    'working-directory: other\nrun: pytest tests/reporting\n',
    'command: pytest tests/reporting\nunknown_syntax: !include other\n',
])
def test_conditional_or_unparsed_configuration_is_unknown(tree, text):
    config(tree, text)
    assert record(run(tree))["selection_status"] == "UNKNOWN"
    assert run(tree)["commands"][0]["status"] == "UNKNOWN"


def test_literal_block_command_and_quoted_path(tree):
    config(tree, 'run: |\n  python -m pytest "tests/reporting/" -q\n')
    assert record(run(tree))["selection_status"] == "EXPLICIT"


def test_configuration_comments_do_not_become_commands(tree):
    config(tree, '# command: pytest tests/reporting\ncommand: pytest tests/other\n')
    assert record(run(tree))["selection_status"] == "MISSING"


def test_directory_boundary_is_not_a_prefix_match(tree):
    config(tree, "command: pytest tests/report\n")
    assert record(run(tree))["selection_status"] == "MISSING"


def test_multiple_tests_require_selection_of_every_path(tree):
    root, paths = tree
    path = "tests/reporting/test_second_report.py"
    (root / path).write_text("# scripts/reporting/build_alpha_report.py\n", encoding="utf-8")
    paths.append(path)
    assert record(run(tree))["selection_status"] == "MISSING"


@pytest.mark.parametrize("name, text", [
    ("pytest.ini", "[pytest]\nnorecursedirs = reporting\n"),
    ("pytest.ini", "[pytest]\npython_files = check_*.py\n"),
    ("pytest.ini", "[pytest]\naddopts = --ignore=tests/reporting\n"),
    ("tests/reporting/conftest.py", 'collect_ignore = ["test_alpha_report.py"]\n'),
    ("tests/reporting/conftest.py", 'def pytest_collection_modifyitems(items):\n    items.clear()\n'),
])
def test_disabled_or_custom_collection_is_not_explicit(tree, name, text):
    (tree[0] / name).write_text(text, encoding="utf-8")
    tree[1].append(name)
    assert record(run(tree))["selection_status"] == "UNKNOWN"


def test_unrelated_conftest_does_not_invalidate_selection(tree):
    path = "tests/other/conftest.py"
    (tree[0] / path).parent.mkdir()
    (tree[0] / path).write_text("collect_ignore = ['anything']\n", encoding="utf-8")
    tree[1].append(path)
    assert record(run(tree))["selection_status"] == "EXPLICIT"


def test_missing_components_are_recorded(tree):
    tree[1].remove("scripts/reporting/build_alpha_report.py")
    report = record(run(tree))
    assert report["generator_path"] is None
    assert "generator mapping" in report["missing_inputs"]
    assert report["selection_status"] == "EXPLICIT"


def test_no_report_test_pair_is_an_error(tree):
    tree[1].remove("tests/reporting/test_alpha_report.py")
    with pytest.raises(ValueError, match="report/test pair"):
        run(tree)


def test_observations_round_trip_and_execution_is_never_inferred(tree):
    first = run(tree)
    observation = first["observations"][0]
    observation["observation_time"] = "2026-10-24T00:00:00Z"
    observation["observation_basis"] = "Manual source inspection; no engineering result qualified."
    observation["rule_findings"] = [{
        "rule": "L3", "status": "UNESTABLISHED",
        "detail": "The label and literal do not establish a first-principles derivation.",
        "locators": [{"path": "tests/reporting/test_alpha_report.py", "line": 2}],
    }]
    result = run(tree, first)
    assert result["observations"] == first["observations"]
    assert run(tree, result) == result
    assert record(result)["execution_evidence"]["status"] == "NOT_RUN"
    assert record(result)["rule_findings"][2]["status"] == "UNESTABLISHED"


def test_changed_digest_invalidates_human_findings(tree):
    old = run(tree)
    old["observations"][0]["rule_findings"] = [{
        "rule": "L1", "status": "OBSERVED_GAP", "detail": "Manual read.",
        "locators": [{"path": "scripts/reporting/build_alpha_report.py", "line": 1}],
    }]
    (tree[0] / "scripts/reporting/build_alpha_report.py").write_text("# changed\n", encoding="utf-8")
    new = record(run(tree, old))
    assert new["observation_status"] == "STALE"
    assert all(item["status"] == "UNESTABLISHED" for item in new["rule_findings"][:7])
    assert "stale manual observations" in new["missing_inputs"]


def test_changed_configuration_invalidates_observation(tree):
    old = run(tree)
    config(tree, "command: pytest tests/other\n")
    assert record(run(tree, old))["observation_status"] == "STALE"


def test_misleading_anchor_label_remains_unestablished(tree):
    finding = record(run(tree))["rule_findings"][2]
    assert finding["rule"] == "L3"
    assert finding["status"] == "UNESTABLISHED"


@pytest.mark.parametrize("field, value", [
    ("report_path", "../outside.html"),
    ("generator_path", "../outside.py"),
    ("test_paths", ["../outside.py"]),
    ("support_paths", ["../outside.json"]),
])
def test_observation_paths_cannot_escape(tree, field, value):
    document = run(tree)
    document["observations"][0][field] = value
    with pytest.raises(ValueError, match="path|source"):
        run(tree, document)


def test_absolute_and_symlink_paths_are_rejected(tree, tmp_path):
    with pytest.raises(ValueError, match="path"):
        audit.build_audit(tree[0], [str(tmp_path / "outside")], "a" * 40)
    link = tree[0] / "docs/reports/escape.html"
    link.symlink_to(tree[0].parent / "outside.html")
    with pytest.raises(ValueError, match="path"):
        audit.build_audit(tree[0], tree[1] + ["docs/reports/escape.html"], "a" * 40)


def test_missing_report_is_preserved_from_frozen_inventory(tree):
    old = run(tree)
    (tree[0] / "docs/reports/modules/alpha/index.html").unlink()
    report = record(run(tree, old))
    assert "report file" in report["missing_inputs"]
    assert report["source_digests"][report["report_path"]] is None


def test_unknown_or_untracked_observation_source_is_rejected(tree):
    old = run(tree)
    old["observations"][0]["support_paths"] = ["docs/unknown.json"]
    with pytest.raises(ValueError, match="source"):
        run(tree, old)


def test_output_is_stable_escaped_and_read_before_overwrite(tree, tmp_path):
    document = run(tree)
    document["observations"][0]["observation_basis"] = "<script>alert(1)</script>"
    result = run(tree, document)
    output = tmp_path / "output"
    audit.write_outputs(result, output)
    before = [(output / name).read_bytes() for name in audit.OUTPUT_NAMES]
    loaded = json.loads((output / audit.OUTPUT_NAMES[0]).read_text(encoding="utf-8"))
    audit.write_outputs(run(tree, loaded), output)
    assert before == [(output / name).read_bytes() for name in audit.OUTPUT_NAMES]
    html = before[1].decode("utf-8")
    assert "&lt;script&gt;" in html
    assert "<script>alert" not in html
    assert html.index("Summary") < html.index("Revision history") < html.index("Rule basis")
    assert "NOT_RUN" in html


def test_outputs_are_excluded_from_sources(tree):
    tree[1].extend("docs/audits/" + name for name in audit.OUTPUT_NAMES)
    result = run(tree)
    assert not any("docs/audits/" in path for path in result["source_files"])


def test_invalid_observation_status_cannot_qualify_a_report(tree):
    old = run(tree)
    old["observations"][0]["rule_findings"] = [{
        "rule": "L3", "status": "VERIFIED", "detail": "Independent anchor label.",
        "locators": [{"path": "tests/reporting/test_alpha_report.py", "line": 2}],
    }]
    with pytest.raises(ValueError, match="status"):
        run(tree, old)


def test_missing_digests_cannot_authenticate_observations(tree):
    old = run(tree)
    old["observations"][0]["source_digests"] = {}
    assert record(run(tree, old))["observation_status"] == "STALE"


def test_findings_must_name_a_pinned_source_and_valid_line(tree):
    old = run(tree)
    old["observations"][0]["rule_findings"] = [{
        "rule": "L1", "status": "OBSERVED_GAP", "detail": "Read needs encoding.",
        "locators": [{"path": "elsewhere.py", "line": 1}],
    }]
    with pytest.raises(ValueError, match="locator"):
        run(tree, old)


def test_duplicate_report_ids_are_rejected(tree):
    old = run(tree)
    old["observations"].append(copy.deepcopy(old["observations"][0]))
    with pytest.raises(ValueError, match="duplicate"):
        run(tree, old)


def test_stale_config_observations_cannot_become_current_on_round_trip(tree):
    old = run(tree)
    config(tree, "command: pytest tests/other\n")
    changed = run(tree, old)
    assert record(changed)["observation_status"] == "STALE"
    assert record(run(tree, changed))["observation_status"] == "STALE"


def test_frozen_inventory_cannot_drop_a_report_candidate(tree):
    old = run(tree)
    old["source_files"].remove("docs/reports/modules/alpha/index.html")
    with pytest.raises(ValueError, match="incomplete frozen"):
        run(tree, old)


def test_frozen_inventory_cannot_drop_check_configuration(tree):
    old = run(tree)
    old["source_files"].remove(".claude/quality-gates.yaml")
    with pytest.raises(ValueError, match="incomplete frozen"):
        run(tree, old)


def test_marker_descriptions_are_not_collection_options(tree):
    path = "pytest.ini"
    (tree[0] / path).write_text(
        "[pytest]\nmarkers =\n    slow: deselect with -m quick\naddopts = --import-mode=importlib\n",
        encoding="utf-8", newline="\n",
    )
    tree[1].append(path)
    assert record(run(tree))["selection_status"] == "EXPLICIT"


def test_conditional_literal_coverage_does_not_imply_selection(tree):
    config(tree, "if: success()\ncommand: pytest tests/reporting\n")
    report = record(run(tree))
    assert report["literal_whole_file_coverage"] is True
    assert report["selection_status"] == "UNKNOWN"
    assert report["execution_evidence"]["status"] == "NOT_RUN"


def test_inventory_digest_entries_are_structured_path_hash_pairs(tree):
    import hashlib

    document = run(tree)
    digests = {entry["path"]: entry["sha256"] for entry in document["source_digests"]}
    assert set(digests) == set(document["source_files"])
    assert all(value == hashlib.sha256((tree[0] / path).read_bytes()).hexdigest()
               for path, value in digests.items())


def test_static_collection_paths_with_parent_escape_remain_unknown(tree):
    path = "tests/reporting/conftest.py"
    (tree[0] / path).write_text('collect_ignore = ["../reporting/test_alpha_report.py"]\n', encoding="utf-8")
    tree[1].append(path)
    assert record(run(tree))["selection_status"] == "UNKNOWN"


def test_html_distinguishes_content_basis_claims_and_selection(tree):
    html = audit.render_html(run(tree))
    assert "<h3>Report content</h3>" in html
    assert "<h3>Engineering basis</h3>" in html
    assert "<h3>Claims and execution evidence</h3>" in html
    assert "<h3>Test selection</h3>" in html
