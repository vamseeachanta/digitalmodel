"""Exercise the stdlib report provenance ratchet against tracked fixture trees."""

import hashlib
import os
import subprocess
import sys
from pathlib import Path

import pytest

CHECKER = (
    Path(__file__).resolve().parents[2]
    / "scripts/enforcement/check-report-provenance.py"
)
ALLOWLIST = "scripts/enforcement/report-provenance-allowlist.txt"


def git_environment():
    return {
        key: value
        for key, value in os.environ.items()
        if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}
    }


@pytest.fixture
def tree(tmp_path):
    subprocess.run(
        ["git", "init", "-q", str(tmp_path)], check=True, env=git_environment()
    )
    return tmp_path


def write(tree, name, content, tracked=True):
    path = tree / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    if tracked:
        subprocess.run(
            ["git", "-C", str(tree), "add", name], check=True, env=git_environment()
        )


def lint(tree):
    return subprocess.run(
        [sys.executable, str(CHECKER), "--root", str(tree)],
        text=True,
        capture_output=True,
    )


@pytest.mark.parametrize("symbol", ["assemble_report", "ReportSkeleton", "Provenance"])
@pytest.mark.parametrize(
    "module", ["digitalmodel.reporting", "digitalmodel.reporting.provenance"]
)
def test_shared_provenance_import_is_compliant(tree, symbol, module):
    write(
        tree,
        "src/digitalmodel/report.py",
        f"from {module} import {symbol} as shared\ndef generate_report():\n    pass\n",
    )
    assert lint(tree).returncode == 0


@pytest.mark.parametrize(
    "content",
    [
        "def generate_demo_report():\n    return 'literal results'\n",
        "def render_html():\n    return '<p>results</p>'\n",
        "from pathlib import Path\nPath('result.html').write_text('results')\n",
        "with open('result.html', 'w') as f:\n    f.write('results')\n",
        "figure.to_html('result.html')\n",
        "output = 'result.html'\noutput.write_text('results')\n",
    ],
)
def test_uncontracted_html_writers_fail(tree, content):
    write(tree, "src/digitalmodel/domain/report_generator.py", content)
    result = lint(tree)
    assert result.returncode == 1
    assert (
        "src/digitalmodel/domain/report_generator.py" in result.stdout + result.stderr
    )


def test_allowlisted_legacy_report_is_frozen(tree):
    name = "src/digitalmodel/ansys/report_generator.py"
    write(tree, name, "def render_html():\n    pass\n")
    write(tree, ALLOWLIST, f"# RATCHET\n{name} # legacy\n")
    assert lint(tree).returncode == 0


def test_line_escape_applies_only_to_annotated_evidence(tree):
    name = "src/digitalmodel/report.py"
    write(tree, name, "def render_html():  # report-lint: allowed\n    pass\n")
    assert lint(tree).returncode == 0
    write(
        tree,
        name,
        "def render_html():  # report-lint: allowed\n    pass\ndef generate_other_report():\n    pass\n",
    )
    assert lint(tree).returncode == 1


def test_comment_or_string_cannot_impersonate_import_or_escape(tree):
    write(
        tree,
        "src/digitalmodel/report.py",
        '# from digitalmodel.reporting import Provenance\ndef render_html():\n    return "# report-lint: allowed"\n',
    )
    assert lint(tree).returncode == 1


def test_ignores_tests_scripts_untracked_and_nonreport_modules(tree):
    for name in (
        "tests/report.py",
        "scripts/report.py",
        "src/digitalmodel/calculator.py",
    ):
        write(tree, name, "def render_html():\n    pass\n")
    write(
        tree,
        "src/digitalmodel/report_data.py",
        "class ReportData:\n    pass\n",
    )
    write(
        tree,
        "src/digitalmodel/untracked_report.py",
        "def render_html():\n    pass\n",
        tracked=False,
    )
    assert lint(tree).returncode == 0


def test_shared_backbone_alone_is_not_provenance(tree):
    write(
        tree,
        "src/digitalmodel/report.py",
        "from digitalmodel.reporting import ReportBackbone\ndef render_html():\n    pass\n",
    )
    assert lint(tree).returncode == 1


def test_new_report_cannot_be_exempted_by_growing_allowlist(tree):
    name = "src/digitalmodel/new/report.py"
    write(tree, name, "def render_html():\n    pass\n")
    write(tree, ALLOWLIST, name + "\n")
    result = lint(tree)
    assert result.returncode == 1
    assert name in result.stdout
    assert "not in frozen legacy roster" in result.stdout


@pytest.mark.parametrize(
    "content",
    [
        None,
        "from digitalmodel.reporting import Provenance\ndef render_html():\n    pass\n",
        "class ReportData:\n    pass\n",
        "def render_html():  # report-lint: allowed\n    pass\n",
    ],
)
def test_stale_legacy_exemptions_must_be_removed(tree, content):
    name = "src/digitalmodel/ansys/report_generator.py"
    if content is not None:
        write(tree, name, content)
    write(tree, ALLOWLIST, name + "\n")
    result = lint(tree)
    assert result.returncode == 1
    assert name in result.stdout
    assert "stale allowlist entry" in result.stdout


def test_scan_uses_explicit_root_despite_inherited_git_bindings(
    tree, tmp_path, monkeypatch
):
    other = tmp_path / "caller"
    subprocess.run(["git", "init", "-q", str(other)], check=True, env=git_environment())
    write(other, "src/digitalmodel/report.py", "def render_html():\n    pass\n")
    tracked = other / "src/digitalmodel/report.py"
    before = {
        p: hashlib.sha256(p.read_bytes()).hexdigest()
        for p in [tracked, other / ".git/index", other / ".git/HEAD"]
    }
    monkeypatch.setenv("GIT_DIR", str(other / ".git"))
    monkeypatch.setenv("GIT_WORK_TREE", str(other))
    monkeypatch.setenv("GIT_COMMON_DIR", str(other / ".git"))
    monkeypatch.setenv("GIT_INDEX_FILE", str(other / ".git/index"))
    assert lint(tree).returncode == 0
    assert {p: hashlib.sha256(p.read_bytes()).hexdigest() for p in before} == before
