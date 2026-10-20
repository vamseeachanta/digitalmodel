# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline audit cases derived from issue #2096; no adapter is imported.

Counts below are independent enumerations of the synthetic source strings.
They are software inventory checks, with no engineering acceptance limits.
"""

import copy
import hashlib
import importlib.util
import json
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/audit_solver_controls.py"
CATALOG = ROOT / "docs/domains/solvers/control-audit.json"
NEUTRAL_SOURCE = """
raise RuntimeError('must never be imported')
import subprocess as sp
def launch():
    return sp.run(['neutral'])
"""


@pytest.fixture
def audit_module():
    spec = importlib.util.spec_from_file_location("audit_solver_controls", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def put(root, name, text):
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


@pytest.fixture
def repository(tmp_path):
    put(tmp_path, "src/digitalmodel/solvers/neutral/runner.py", NEUTRAL_SOURCE)
    put(tmp_path, "tests/solvers/neutral/test_runner.py", "def test_run(): pass\n")
    put(
        tmp_path,
        "tests/DOMAINS.md",
        """
| Domain | Roots | Purpose |
| solver-smoke | `tests/solvers/neutral/` | Synthetic ownership |
""",
    )
    return tmp_path


def observation():
    return {
        "id": "neutral-timeout",
        "family": "neutral",
        "control": "owned_timeout",
        "status": "STATIC_ONLY",
        "observation": "The source contains a direct subprocess invocation.",
        "evidence": [
            {
                "path": "src/digitalmodel/solvers/neutral/runner.py",
                "symbol": "launch",
                "sha256": hashlib.sha256(NEUTRAL_SOURCE.encode()).hexdigest(),
            }
        ],
        "follow_up": "#2096",
    }


def test_alias_inventory_does_not_execute_source(audit_module):
    """Hand enumeration: run, Popen and Model are three candidate call sites."""
    source = """import subprocess as sp
from subprocess import Popen as start
import OrcFxAPI as api
raise RuntimeError('do not import')
sp.run(['neutral'])
start(['neutral'])
api.Model()
unrelated.run()
# sp.run(['comment'])
text = 'api.Model()'
"""
    assert audit_module.execution_sites(source) == [
        {"line": 5, "api": "subprocess.run"},
        {"line": 6, "api": "subprocess.Popen"},
        {"line": 7, "api": "OrcFxAPI.Model"},
    ]


def test_inventory_ownership_and_unknown_controls(repository, audit_module):
    """One file, one call and one owned test follow directly from the fixture."""
    result = audit_module.audit(repository, [])
    assert result["launch_allowed"] is False
    assert result["qualification"] == "NOT_EVALUATED"
    assert result["inventory"] == [
        {
            "family": "neutral",
            "python_files": 1,
            "execution_sites": [
                {
                    "path": "src/digitalmodel/solvers/neutral/runner.py",
                    "line": 5,
                    "api": "subprocess.run",
                }
            ],
        }
    ]
    assert result["test_ownership"]["unowned"] == []
    assert result["test_ownership"]["owned"] == 1
    assert all(row["status"] == "NOT_EVALUATED" for row in result["controls"])
    assert {row["control"] for row in result["controls"]} == set(audit_module.CONTROLS)


def test_unowned_tests_are_a_gap_not_runtime_qualification(repository, audit_module):
    """The directory prefix must not own the sibling with a longer name."""
    put(repository, "tests/solvers/neutral_extra/test_other.py", "pass\n")
    result = audit_module.audit(repository, [])
    assert result["test_ownership"]["unowned"] == [
        "tests/solvers/neutral_extra/test_other.py"
    ]
    assert result["test_ownership"]["status"] == "GAP"
    assert result["qualification"] == "NOT_EVALUATED"


def test_evidence_contains_digest_and_symbol_extent(repository, audit_module):
    """The fixture's launch function spans lines four and five, inclusively."""
    result = audit_module.audit(repository, [observation()])
    row = next(r for r in result["controls"] if r["control"] == "owned_timeout")
    evidence = row["observations"][0]["evidence"][0]
    assert evidence["start_line"] == 4
    assert evidence["end_line"] == 5
    # The digest is a byte identity only, not an expected engineering value.
    raw = (repository / evidence["path"]).read_bytes()
    assert evidence["sha256"] == hashlib.sha256(raw).hexdigest()
    assert row["status"] == "STATIC_ONLY"
    assert result == audit_module.audit(repository, [observation()])


@pytest.mark.parametrize(
    "mutation",
    [
        {"status": "PASS"},
        {"status": "GAP"},  # A gap requires a test reproducer, not just a source claim.
        {"control": "invented"},
        {"follow_up": ""},
        {"evidence": []},
        {"evidence": [{"path": "../outside.py", "symbol": "launch"}]},
        {
            "evidence": [
                {
                    "path": "src/digitalmodel/solvers/neutral/runner.py",
                    "symbol": "missing",
                }
            ]
        },
    ],
)
def test_invalid_or_unsubstantiated_claims_refuse(repository, audit_module, mutation):
    record = observation()
    record.update(mutation)
    with pytest.raises(ValueError):
        audit_module.audit(repository, [record])


def test_duplicate_observations_refuse(repository, audit_module):
    with pytest.raises(ValueError, match="duplicate"):
        audit_module.audit(repository, [observation(), observation()])


def test_source_change_is_visible_and_stale_symbol_refuses(repository, audit_module):
    before = audit_module.audit(repository, [observation()])
    path = put(
        repository, "src/digitalmodel/solvers/neutral/runner.py", "def launch(): pass\n"
    )
    with pytest.raises(ValueError, match="digest"):
        audit_module.audit(repository, [observation()])
    reviewed = observation()
    reviewed["evidence"][0]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    after = audit_module.audit(repository, [reviewed])

    def first(report):
        return next(r for r in report["controls"] if r["control"] == "owned_timeout")

    assert first(before)["observations"] != first(after)["observations"]
    put(
        repository,
        "src/digitalmodel/solvers/neutral/runner.py",
        "def renamed(): pass\n",
    )
    reviewed["evidence"][0]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match="symbol"):
        audit_module.audit(repository, [reviewed])


def test_symlink_source_refuses(repository, tmp_path, audit_module):
    path = repository / "src/digitalmodel/solvers/neutral/runner.py"
    target = put(tmp_path, "other.py", "def launch(): pass\n")
    path.unlink()
    path.symlink_to(target)
    with pytest.raises(ValueError, match="symlink"):
        audit_module.audit(repository, [])


def test_malformed_source_and_missing_scope_refuse(repository, audit_module):
    put(repository, "src/digitalmodel/solvers/neutral/runner.py", "def broken(\n")
    with pytest.raises(ValueError, match="Python"):
        audit_module.audit(repository, [])
    with pytest.raises(ValueError, match="scope"):
        audit_module.audit(repository / "missing", [])


def test_real_batch_invocation_has_no_explicit_timeout(audit_module):
    """Static reproducer for #2096: inspect the call; never invoke the batch.

    The direct subprocess.run call in the named method supplies no timeout
    keyword and no keyword unpacking. No claim about unseen callers is made.
    """
    import ast

    path = ROOT / "src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py"
    tree = ast.parse(path.read_text())
    cls = next(
        n
        for n in tree.body
        if isinstance(n, ast.ClassDef) and n.name == "OrcaWaveOrchestrator"
    )
    method = next(
        n
        for n in cls.body
        if isinstance(n, ast.FunctionDef) and n.name == "_phase2_orcawave_execution"
    )
    calls = [
        n
        for n in ast.walk(method)
        if isinstance(n, ast.Call) and ast.unparse(n.func) == "subprocess.run"
    ]
    assert len(calls) == 1  # Independent enumeration of the named method.
    assert all(k.arg not in {"timeout", None} for k in calls[0].keywords)


def test_catalog_is_complete_resolvable_and_never_qualifies(audit_module):
    initial_modules = set(sys.modules)
    catalog = json.loads(CATALOG.read_text())
    result = audit_module.audit(ROOT, catalog)
    observed = {r["control"] for r in result["controls"] if r["observations"]}
    assert observed == set(audit_module.CONTROLS)
    assert result["test_ownership"]["unowned"] == []
    assert result["launch_allowed"] is False
    assert result["qualification"] == "NOT_EVALUATED"
    assert not any(
        name.startswith("digitalmodel.solvers")
        for name in set(sys.modules) - initial_modules
    )
    # A catalog cannot claim native approval by changing the status spelling.
    forged = copy.deepcopy(catalog)
    forged[0]["status"] = "QUALIFIED"
    with pytest.raises(ValueError):
        audit_module.audit(ROOT, forged)


def test_cli_is_solver_free_and_errors_do_not_echo_input(
    repository, audit_module, capsys
):
    catalog = put(repository, "catalog.json", "[]")
    assert (
        audit_module.main(["--root", str(repository), "--catalog", str(catalog)]) == 0
    )
    result = json.loads(capsys.readouterr().out)
    assert result["qualification"] == "NOT_EVALUATED"
    catalog.write_text('{"untrusted": "do not echo this value"}')
    assert (
        audit_module.main(["--root", str(repository), "--catalog", str(catalog)]) == 2
    )
    output = capsys.readouterr()
    assert "do not echo" not in output.err
    assert str(repository) not in output.err
