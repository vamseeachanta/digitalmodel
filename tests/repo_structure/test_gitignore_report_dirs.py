# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Module reports stay visible while local build products remain ignored."""

import os
from pathlib import Path
import runpy
import subprocess

import pytest


ROOT = Path(__file__).resolve().parents[2]


def test_report_ignore_regressions_select_contracts_domain():
    detector = runpy.run_path(str(ROOT / "scripts/ci/detect_touched_domains.py"))
    domains = detector["parse_domains"](ROOT / "tests/DOMAINS.md")
    relative = Path(__file__).resolve().relative_to(ROOT).as_posix()
    selected = detector["touched_domains"]([relative], domains)
    assert [domain.name for domain in selected] == ["contracts"]


@pytest.fixture(scope="module")
def report_repo(tmp_path_factory):
    """Evaluate the real rules in a disposable repository without user config."""
    root = tmp_path_factory.mktemp("report-ignore")
    env = {
        key: value
        for key, value in os.environ.items()
        if not key.startswith("GIT_")
    }
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    env["GIT_CONFIG_NOSYSTEM"] = "1"

    def git(*args):
        return subprocess.run(
            ["git", "-c", f"core.excludesFile={os.devnull}", *args],
            cwd=root, env=env, capture_output=True, text=True, encoding="utf-8",
        )

    result = git("init", "--quiet", "--template=")
    assert result.returncode == 0, result.stderr
    (root / ".gitignore").write_bytes((ROOT / ".gitignore").read_bytes())
    return root, git


@pytest.mark.parametrize("module", ["alpha", "beta", "new-module"])
@pytest.mark.parametrize(
    "name",
    [
        "report.md", "report.html", "changes.json", "figure.svg", "report.pdf",
        "build_report.py", "assets/reports/figure.svg", ".env.template",
        "assets/reports/nested/reports/figure.svg", "env/case.yml",
        "assets/.env.template",
    ],
)
def test_module_report_is_visible(report_repo, module, name):
    root, git = report_repo
    relative = f"docs/reports/modules/{module}/{name}"
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    result = git("check-ignore", "--no-index", "--quiet", "--", relative)
    assert result.returncode == 1, relative + "\n" + result.stderr


@pytest.mark.parametrize(
    "name",
    [
        "__pycache__/build_report.cpython-311.pyc", "__pycache__/README.md",
        "build_report.pyc", "build_report.pyo", "build_report.pyd",
        ".pytest_cache/README.md", "build/report.html", "dist/report.pdf",
        "_build/report.html", "assets/__pycache__/README.md",
        "assets/.pytest_cache/README.md", "assets/build/report.html",
    ],
)
def test_module_build_product_is_ignored(report_repo, name):
    root, git = report_repo
    relative = f"docs/reports/modules/alpha/{name}"
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    result = git("check-ignore", "--no-index", "--quiet", "--", relative)
    assert result.returncode == 0, relative + "\n" + result.stderr


@pytest.mark.parametrize("prefix", ["", "assets/", "assets/reports/"])
@pytest.mark.parametrize(
    "name",
    [
        ".env", ".env.local", "assets/.env", ".venv/pyvenv.cfg",
        "node_modules/package/index.js", "config.local", "venv/pyvenv.cfg",
        "ENV/pyvenv.cfg", "env.bak/pyvenv.cfg", "venv.bak/pyvenv.cfg",
        "env/pyvenv.cfg", "env/bin/python", "env/bin/activate", "env/bin/pip",
        "env/Scripts/python.exe", "env/Scripts/activate.bat",
        "env/Scripts/Activate.ps1", "env/Scripts/pip.exe",
        "env/Lib/site-packages/package.py", "env/other.txt",
        ".mypy_cache/cache.json", ".dmypy.json", "dmypy.json", ".pyre/cache",
        ".coverage", ".coverage.test", "coverage.xml", "htmlcov/index.html",
        ".tox/config", ".nox/config", ".hypothesis/examples/entry",
        "logs/run.log", "run.log", ".cache", ".vscode/settings.json",
        ".idea/config.xml", ".DS_Store", "notes.swp", "notes.swo", "notes~",
    ],
)
def test_local_environment_stays_ignored(report_repo, prefix, name):
    root, git = report_repo
    relative = f"docs/reports/modules/alpha/{prefix}{name}"
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    result = git("check-ignore", "--no-index", "--quiet", "--", relative)
    assert result.returncode == 0, relative + "\n" + result.stderr


@pytest.mark.parametrize(
    "relative",
    ["docs/reports/unrelated/report.html", "reports/local/report.html"],
)
def test_other_report_directories_remain_ignored(report_repo, relative):
    root, git = report_repo
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    result = git("check-ignore", "--no-index", "--quiet", "--", relative)
    assert result.returncode == 0, relative + "\n" + result.stderr
