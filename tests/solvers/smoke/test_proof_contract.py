"""Offline proof contracts; no solver import, launch or licence is required."""

import pytest
from pathlib import Path
import runpy
import subprocess
import sys
import os

from digitalmodel.infrastructure.solver_proof import capture_inputs


# NIST, SHA-256 worked example, One Block Message Sample, message "abc".
# https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf
ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"


def test_missing_mandatory_input_cannot_be_filtered(tmp_path):
    """Required = {present, missing}; one readable file cannot cover two names."""
    (tmp_path / "present.txt").write_bytes(b"abc")
    with pytest.raises(ValueError, match="input_unreadable"):
        capture_inputs(tmp_path, {"present.txt": ABC_SHA256, "missing.txt": ABC_SHA256})


def test_exact_inputs_match_published_digest(tmp_path):
    """The NIST one-block example gives the literal digest of three ASCII bytes."""
    (tmp_path / "source.txt").write_bytes(b"abc")
    expected = {"source.txt": ABC_SHA256}
    assert capture_inputs(tmp_path, expected) == expected


@pytest.mark.parametrize("name", ["../source.txt", "/source.txt", "a//b", "./a", "a\\b", "C:a"])
def test_noncanonical_input_paths_are_rejected(tmp_path, name):
    """Mandatory identities are canonical relative POSIX names, never aliases."""
    with pytest.raises(ValueError, match="invalid_input_path"):
        capture_inputs(tmp_path, {name: ABC_SHA256})


@pytest.mark.parametrize("required", [{}, {"source.txt": "bad"}, {"source.txt": None}])
def test_invalid_declaration_is_rejected(tmp_path, required):
    with pytest.raises(ValueError, match="invalid_declaration"):
        capture_inputs(tmp_path, required)


def test_substituted_input_is_rejected(tmp_path):
    (tmp_path / "source.txt").write_bytes(b"changed")
    with pytest.raises(ValueError, match="input_substituted"):
        capture_inputs(tmp_path, {"source.txt": ABC_SHA256})


@pytest.mark.parametrize("directory_link", [False, True])
def test_redirected_input_is_rejected(tmp_path, directory_link):
    target = tmp_path / "original"
    target.mkdir()
    (target / "source.txt").write_bytes(b"abc")
    alias = tmp_path / "alias"
    alias.symlink_to(target if directory_link else target / "source.txt",
                     target_is_directory=directory_link)
    name = "alias/source.txt" if directory_link else "alias"
    with pytest.raises(ValueError, match="input_redirected"):
        capture_inputs(tmp_path, {name: ABC_SHA256})


def test_case_aliases_are_rejected(tmp_path):
    for name in ("source.txt", "SOURCE.txt"):
        (tmp_path / name).write_bytes(b"abc")
    with pytest.raises(ValueError, match="invalid_declaration"):
        capture_inputs(tmp_path, {"source.txt": ABC_SHA256, "SOURCE.txt": ABC_SHA256})


@pytest.fixture
def evidence(tmp_path):
    """Two disjoint files contain the NIST vector; required sets each have one name."""
    for name in ("source.txt", "result.txt"):
        (tmp_path / name).write_bytes(b"abc")
    return dict(
        root=tmp_path,
        required_inputs={"source.txt": ABC_SHA256},
        required_outputs=["result.txt"],
        child={"ok": True, "input_hashes": {"source.txt": ABC_SHA256},
               "output_hashes": {"result.txt": ABC_SHA256}},
        cleanup={"scope": "owned_process_tree", "observation_succeeded": True,
                 "root_exited": True, "active_processes": 0},
    )


def test_complete_proof_requires_exact_coverage_and_observation(evidence):
    """One input + one output + zero observed processes satisfies the declaration."""
    from digitalmodel.infrastructure.solver_proof import audit_proof

    audit = audit_proof(**evidence)
    assert audit == {"complete": True, "failures": [],
                     "input_hashes": evidence["required_inputs"],
                     "output_hashes": {"result.txt": ABC_SHA256}}


@pytest.mark.parametrize("manifest", ["input_hashes", "output_hashes"])
@pytest.mark.parametrize("mutation", ["missing", "extra", "substituted", "nonmapping"])
def test_fake_child_incomplete_evidence_is_rejected(evidence, manifest, mutation):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    hashes = evidence["child"][manifest]
    name = next(iter(hashes))
    if mutation == "missing":
        hashes.pop(name)
    elif mutation == "extra":
        hashes["optional.log"] = ABC_SHA256
    elif mutation == "substituted":
        hashes[name] = "unbound"
    else:
        evidence["child"][manifest] = None
    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert manifest + "_mismatch" in audit["failures"]


@pytest.mark.parametrize("change", [
    {"termination_requested": True, "observation_succeeded": False},
    {"root_exited": False}, {"active_processes": 1},
    {"active_processes": False}, {"active_processes": -1},
    {"scope": "root_process"}, {"root_exited": 1},
])
def test_unconfirmed_cleanup_cannot_complete_proof(evidence, change):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    evidence["cleanup"].update(change)
    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert "cleanup_unconfirmed" in audit["failures"]


def test_failure_retains_diagnostics_without_success(evidence):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    evidence["child"]["ok"] = False
    evidence["cleanup"] = None
    (evidence["root"] / "result.txt").write_bytes(b"changed")
    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert set(audit["failures"]) == {
        "child_not_successful", "output_hashes_mismatch", "cleanup_unconfirmed"}
    assert audit["input_hashes"] == evidence["required_inputs"]


def test_postflight_detects_changed_input_and_preserves_diagnostics(evidence):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    diagnostic = evidence["root"] / "failure.log"
    diagnostic.write_text("retained diagnostic", encoding="utf-8", newline="\n")
    (evidence["root"] / "source.txt").write_bytes(b"changed after launch")
    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert "input_substituted" in audit["failures"]
    assert diagnostic.read_text(encoding="utf-8") == "retained diagnostic"


def test_unreadable_input_is_rejected_even_with_a_reported_hash(tmp_path, monkeypatch):
    import digitalmodel.infrastructure.solver_proof as contract

    (tmp_path / "source.txt").write_bytes(b"abc")

    def denied(*args):
        raise PermissionError("simulated unreadable input")

    monkeypatch.setattr(contract.os, "open", denied)
    with pytest.raises(ValueError, match="input_unreadable"):
        capture_inputs(tmp_path, {"source.txt": ABC_SHA256})


@pytest.mark.parametrize("name", ["source.txt", "result.txt"])
def test_missing_artifact_never_completes_proof(evidence, name):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    # Replace an input/output with a directory without removing fixture data.
    path = evidence["root"] / name
    path.rename(path.with_suffix(".retained"))
    path.mkdir()
    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert any("unreadable" in reason for reason in audit["failures"])


def test_optional_diagnostics_do_not_enlarge_required_manifests(evidence):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    (evidence["root"] / "optional.log").write_text("diagnostics", encoding="utf-8", newline="\n")
    audit = audit_proof(**evidence)
    assert audit["complete"] is True
    assert set(audit["input_hashes"]) == {"source.txt"}
    assert set(audit["output_hashes"]) == {"result.txt"}


def test_changed_during_hashing_is_rejected(tmp_path, monkeypatch):
    import digitalmodel.infrastructure.solver_proof as contract

    source = tmp_path / "source.txt"
    source.write_bytes(b"abc")
    original = contract.hashlib.file_digest

    def changing_digest(stream, algorithm):
        digest = original(stream, algorithm)
        source.write_bytes(b"different bytes")
        return digest

    monkeypatch.setattr(contract.hashlib, "file_digest", changing_digest)
    with pytest.raises(ValueError, match="input_changed_during_read"):
        capture_inputs(tmp_path, {"source.txt": ABC_SHA256})


def test_input_and_output_must_be_disjoint(evidence):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    evidence["required_outputs"] = ["source.txt"]
    evidence["child"]["output_hashes"] = evidence["required_inputs"].copy()
    assert audit_proof(**evidence)["complete"] is False


def test_input_and_output_aliases_must_be_disjoint(evidence):
    """Case aliases cannot provide separate input and output proof on Windows."""
    from digitalmodel.infrastructure.solver_proof import audit_proof

    (evidence["root"] / "SOURCE.txt").write_bytes(b"abc")
    evidence["required_outputs"] = ["SOURCE.txt"]
    evidence["child"]["output_hashes"] = {"SOURCE.txt": ABC_SHA256}
    assert audit_proof(**evidence)["complete"] is False


@pytest.mark.parametrize("linked_input", ["source.txt", "extra.txt"])
def test_input_and_output_hard_links_must_be_disjoint(evidence, linked_input):
    """Every input identity must differ from every output identity."""
    from digitalmodel.infrastructure.solver_proof import audit_proof

    root = evidence["root"]
    (root / "extra.txt").write_bytes(b"abc")
    evidence["required_inputs"]["extra.txt"] = ABC_SHA256
    evidence["child"]["input_hashes"]["extra.txt"] = ABC_SHA256
    alias = root / "zz_alias.txt"
    alias.hardlink_to(root / linked_input)
    assert alias.samefile(root / linked_input)
    evidence["required_outputs"].append("zz_alias.txt")
    evidence["child"]["output_hashes"]["zz_alias.txt"] = ABC_SHA256

    audit = audit_proof(**evidence)
    assert audit["complete"] is False
    assert "output_unreadable_or_invalid" in audit["failures"]


@pytest.mark.parametrize("required", [None, [], {"source.txt": None}, {1: ABC_SHA256}])
def test_malformed_declaration_retains_failure_report(evidence, required):
    from digitalmodel.infrastructure.solver_proof import audit_proof

    evidence["required_inputs"] = required
    result = audit_proof(**evidence)
    assert result["complete"] is False
    assert result["failures"]


def test_offline_import_does_not_require_optional_solver_api(tmp_path):
    """The helper imports when the optional API and solver namespace are absent."""
    source = Path(__file__).resolve().parents[3] / "src"
    code = """
import importlib.abc
import sys
sys.path.insert(0, sys.argv[1])
class AbsentAPI(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, *args):
        if fullname == 'OrcFxAPI' or fullname.startswith('digitalmodel.solvers'):
            raise ModuleNotFoundError('optional solver deliberately absent')
sys.meta_path.insert(0, AbsentAPI())
from digitalmodel.infrastructure.solver_proof import capture_inputs, audit_proof
assert not any(name == 'OrcFxAPI' or name.startswith('digitalmodel.solvers')
               for name in sys.modules)
"""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    completed = subprocess.run([sys.executable, "-B", "-c", code, str(source)],
                               cwd=tmp_path, env=env, capture_output=True, text=True)
    assert completed.returncode == 0, completed.stderr


def test_worked_example_retains_both_dispositions(tmp_path):
    """NIST abc digest; empty observed tree passes, unobserved cleanup fails."""
    import json

    example = Path(__file__).resolve().parents[3] / "examples/solver_proof_completeness.py"
    directory = tmp_path / "example"
    answers = runpy.run_path(str(example))["run_example"](directory)
    assert answers["observed"]["complete"] is True
    assert answers["observed"]["input_hashes"] == {"source.txt": ABC_SHA256}
    assert answers["observed"]["output_hashes"] == {"result.txt": ABC_SHA256}
    assert answers["requested_only"]["complete"] is False
    assert answers["requested_only"]["failures"] == ["cleanup_unconfirmed"]
    assert json.loads((directory / "audit.json").read_text(encoding="utf-8")) == answers
