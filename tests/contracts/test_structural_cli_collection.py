# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep structural CLI tests visible through file and directory discovery."""

import json
import os
from pathlib import Path
import subprocess
import sys


TARGET = "tests/specialized/cli/test_structural_analysis_cli.py"
EXPECTED = {
    "TestStressCommand::test_basic_stress_calculation",
    "TestBucklingPlateCommand::test_basic_plate_buckling",
    "TestCLIHelp::test_main_help",
    "TestCLIErrorHandling::test_invalid_material",
    "TestIntegrationScenarios::test_full_stress_analysis_workflow",
    "TestIntegrationScenarios::test_material_comparison",
}

# Only the target module may be imported during recursive discovery. A missing
# internal helper must surface as a collection error, never an optional skip.
PLUGIN = '''
import json
from pathlib import Path
import sys

TARGET = "tests/specialized/cli/test_structural_analysis_cli.py"
reports = {"skipped": [], "failed": []}

class NoLicensedImports:
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] in {"OrcFxAPI", "win32com", "pythoncom", "ansys"}:
            raise AssertionError("Licensed binding import forbidden: " + fullname)

sys.meta_path.insert(0, NoLicensedImports())

def forbid_network(event, args):
    if event in {"socket.connect", "socket.getaddrinfo", "socket.sendto"}:
        raise AssertionError("Network access forbidden during collection")

sys.addaudithook(forbid_network)

def pytest_ignore_collect(collection_path, config):
    if collection_path.is_file() and collection_path.suffix == ".py":
        return collection_path != config.rootpath / TARGET

def pytest_collectreport(report):
    if report.skipped or report.failed:
        reports[report.outcome].append(report.nodeid)

def pytest_sessionfinish(session, exitstatus):
    reports["nodes"] = [item.nodeid for item in session.items]
    reports["exitstatus"] = int(exitstatus)
    Path("collection.json").write_text(json.dumps(reports), encoding="utf-8")
'''


def test_structural_cli_visible_from_file_and_directory(tmp_path):
    """Both discovery routes collect the same runnable suite without skips."""
    root = Path(__file__).resolve().parents[2]
    env = {
        key: value for key, value in os.environ.items()
        if not key.startswith(("GIT_", "PYTEST_"))
    }
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    plugin = tmp_path / "structural_collection_probe.py"
    plugin.write_text(PLUGIN, encoding="utf-8")
    env["PYTHONPATH"] = os.pathsep.join(map(str, (tmp_path, root, root / "src")))
    results = []
    for name, entry in (("file", TARGET), ("directory", "tests/specialized/cli")):
        work = tmp_path / name
        work.mkdir()
        completed = subprocess.run(
            [sys.executable, "-m", "pytest", "-c", str(root / "pytest.ini"),
             str(root / entry), "--collect-only", "-q", "-rA",
             "-p", "no:cacheprovider", "-p", "structural_collection_probe",
             "--basetemp", str(work / "temp")],
            cwd=work, env=env, capture_output=True, text=True, timeout=60,
            creationflags=subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0,
        )
        output = completed.stdout + completed.stderr
        report = work / "collection.json"
        assert report.is_file(), output
        results.append((completed.returncode, json.loads(report.read_text()), output))

    for returncode, report, output in results:
        assert returncode == report["exitstatus"] == 0, output
        assert report["skipped"] == [], output
        assert report["failed"] == [], output
        nodes = report["nodes"]
        assert nodes and all(node.startswith(TARGET + "::") for node in nodes), output
        assert {TARGET + "::" + node for node in EXPECTED} <= set(nodes), output
    assert set(results[0][1]["nodes"]) == set(results[1][1]["nodes"])
