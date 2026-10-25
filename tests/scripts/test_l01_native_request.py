# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline issuance controls use synthetic decks, never a licensed API."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/benchmark/l01_investigation.py"
REQUEST = ROOT / "docs/domains/orcawave/L01_aqwa_benchmark/native_request.json"
RIGHTS = (
    "Source rights: internal use only. The L01 source model files are held in "
    "the project repositories; runs use the project licences; results are kept "
    "in the private data store; nothing derived from the source model is "
    "published in a public repository or page until its provenance is recorded."
)


@pytest.fixture
def mod():
    spec = importlib.util.spec_from_file_location("l01_native_request", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def candidate(tmp_path):
    # Deliberately synthetic values and mesh; not an engineering model.
    ow = tmp_path / "candidate.yml"
    ow.write_text(
        "WavesReferredToBy: period (s)\nBodyMeshFormat: Aqwa dat\n"
        "PeriodOrFrequency:\n  - 1\n  - 2\n  - 3\n  - 4\n"
        "WaveHeading:\n  - 0\n  - 30\n  - 60\n  - 90\n"
        "QTFMinCrossingAngle: 0\nBodyMeshFileName: candidate.dat\n",
        encoding="utf-8",
        newline="\n",
    )
    aqwa = tmp_path / "candidate.dat"
    aqwa.write_text(
        "JOB AQWA  LINE\nTITLE               Synthetic model\n"
        "NUM_CORES         1\nOPTIONS GOON\n"
        "OPTIONS LHFR SFBM MQTF REST END\nRESTART  1  5\n"
        "          FDR1\n     1HRTZ    1    1        1.\n END\n",
        encoding="utf-8", newline="\n")
    sources = {
        solver: {"file": path.name,
                 "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}
        for solver, path in (("OrcaWave", ow), ("AQWA", aqwa))
    }
    request = {
        "source_rights": RIGHTS,
        "allocation_policy": {
            "stage1_max_licensed_minutes": 60,
            "later_stage_multiplier": 1.25,
            "citation": "Synthetic resource policy, revision 1, section 2",
            "contingency_rationale": "Synthetic contingency for timing variability",
        },
        "sources": sources,
        "reconciliation": [
            {"quantity": quantity, "status": "AGREES", "unit": "synthetic",
             "OrcaWave": {"value": 1, "file_line": "candidate.yml:1"},
             "AQWA": {"value": 1, "file_line": "candidate.dat:1"},
             "note": "Synthetic evidence only."}
            for quantity in ("geometry", "mass", "centre_of_gravity",
                             "reference_point", "water_depth", "restraints")
        ],
        "stage1": {"cap_licensed_minutes": 60,
                   "periods_s": [1, 2, 3, 4], "headings_deg": [0, 30, 60, 90]},
    }
    return request, tmp_path


@pytest.mark.parametrize("status", ["DISAGREES", "NOT_EVALUATED"])
@pytest.mark.parametrize("quantity", ["geometry", "mass", "centre_of_gravity",
                                     "reference_point", "water_depth", "restraints"])
def test_unreconciled_row_prevents_any_native_output(mod, candidate, status, quantity):
    request, root = candidate
    next(row for row in request["reconciliation"]
         if row["quantity"] == quantity)["status"] = status
    output = root.parent / (root.name + "-issued")
    with pytest.raises(ValueError, match="reconciliation"):
        mod.issue_native_inputs(request, root, output)
    assert not output.exists()


@pytest.mark.parametrize("rights", [None, "", "internal", "NOT_EVALUATED"])
def test_missing_or_incomplete_rights_prevents_issuance(mod, candidate, rights):
    request, root = candidate
    if rights is None:
        del request["source_rights"]
    else:
        request["source_rights"] = rights
    with pytest.raises(ValueError, match="rights"):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


@pytest.mark.parametrize("cap", [None, 60.1, 61, 0, -1, True, float("nan")])
def test_absent_or_invalid_stage1_cap_prevents_issuance(mod, candidate, cap):
    request, root = candidate
    if cap is None:
        del request["stage1"]["cap_licensed_minutes"]
    else:
        request["stage1"]["cap_licensed_minutes"] = cap
    with pytest.raises(ValueError, match="cap"):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


@pytest.mark.parametrize("mutation", ["missing_row", "duplicate_row", "value", "unit",
                                      "citation", "source", "grid", "unknown_value",
                                      "empty_value", "nonfinite"])
def test_declarations_cannot_bypass_evidence_checks(mod, candidate, mutation):
    request, root = candidate
    row = request["reconciliation"][0]
    if mutation == "missing_row":
        request["reconciliation"].pop()
    elif mutation == "duplicate_row":
        request["reconciliation"][-1] = copy.deepcopy(row)
    elif mutation == "value":
        row["AQWA"]["value"] = 2
    elif mutation == "unit":
        row["unit"] = ""
    elif mutation == "citation":
        row["AQWA"]["file_line"] = "candidate.dat:9999"
    elif mutation == "source":
        (root / "candidate.dat").write_text("changed\n", encoding="utf-8", newline="\n")
    elif mutation in {"unknown_value", "empty_value", "nonfinite"}:
        value = {"unknown_value": "NOT_EVALUATED", "empty_value": [],
                 "nonfinite": float("inf")}[mutation]
        row["AQWA"]["value"] = value
        row["OrcaWave"]["value"] = copy.deepcopy(value)
    else:
        request["stage1"]["periods_s"][-1] = 5
    with pytest.raises(ValueError):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


def test_all_controls_issue_real_files_and_shared_budget_manifest(mod, candidate):
    request, root = candidate
    output = root.parent / (root.name + "-issued")
    manifest = mod.issue_native_inputs(request, root, output)
    assert manifest["native_inputs"] == "ISSUED"
    assert manifest["stage1"]["cap_licensed_minutes"] == 60
    assert "stop at 60 minutes" in manifest["stage1"]["stop_rule"]
    cases = manifest["stage1"]["cases"]
    assert len(cases) == 16
    assert len({case["id"] for case in cases}) == 16
    assert all(case["solvers"] == ["AQWA", "OrcaWave"] for case in cases)
    assert {(case["period_s"], case["heading_deg"]) for case in cases} == {
        (p, h) for p in (1, 2, 3, 4) for h in (0, 30, 60, 90)
    }
    assert all(case["expected_outputs"]["AQWA"] == ["stage1.LIS", "stage1.AH1"]
               for case in cases)
    assert all(case["expected_outputs"]["OrcaWave"] == ["stage1.owr"]
               for case in cases)
    for item in manifest["files"]:
        data = (output / item["file"]).read_bytes()
        assert hashlib.sha256(data).hexdigest() == item["sha256"]
    ow = (output / "stage1.yml").read_text(encoding="utf-8")
    aqwa = (output / "stage1.dat").read_text(encoding="utf-8")
    assert "BodyMeshFileName: stage1.dat" in ow
    assert aqwa.count("HRTZ") == 4
    assert aqwa.count("DIRN") == 4
    assert json.loads((output / "manifest.json").read_text(encoding="utf-8")) == manifest
    for stage, count in zip(manifest["later_stages"], (90, 180)):
        assert stage["cases"] == count
        assert stage["allocation"] == (
            f"allocation = measured stage-1 minutes per case x {count} x 1.25, "
            "set by the solver manager from the measured time"
        )


def test_existing_output_is_preserved(mod, candidate):
    request, root = candidate
    output = root.parent / (root.name + "-issued")
    output.mkdir()
    (output / "manifest.json").write_text("preserve", encoding="utf-8", newline="\n")
    with pytest.raises(FileExistsError):
        mod.issue_native_inputs(request, root, output)
    assert (output / "manifest.json").read_text(encoding="utf-8") == "preserve"


@pytest.mark.parametrize("mutation", ["convention", "mesh_format", "mesh_reference",
                                      "case_list", "duplicate_grid"])
def test_incompatible_native_mapping_is_refused_before_output(mod, candidate, mutation):
    request, root = candidate
    path = root / "candidate.yml"
    source = path.read_text(encoding="utf-8")
    if mutation == "convention":
        source = source.replace("period (s)", "frequency (Hz)")
    elif mutation == "mesh_format":
        source = source.replace("Aqwa dat", "Wamit gdf")
    elif mutation == "mesh_reference":
        source = source.replace("candidate.dat", "other.dat")
    elif mutation == "case_list":
        request["stage1"]["cases"] = []
    else:
        source += "PeriodOrFrequency:\n  - 1\n  - 2\n  - 3\n  - 4\n"
    path.write_text(source, encoding="utf-8", newline="\n")
    request["sources"]["OrcaWave"]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    with pytest.raises(ValueError):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


def test_native_frequency_mapping_uses_the_emitted_card_precision(mod, candidate):
    request, root = candidate
    sources_before = {p.name: p.read_bytes() for p in root.iterdir()}
    manifest = mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    native = ((root.parent / (root.name + "-issued")) / "stage1.dat").read_text(encoding="utf-8")
    cards = [line for line in native.splitlines() if line[6:10] == "HRTZ"]
    assert [float(line[20:30]) for line in cards] == [0.25, 0.3333333, 0.5, 1]
    mapping = manifest["native_to_request"]
    assert [row["AQWA_frequency_hz"] for row in mapping] == [1, 0.5, 0.3333333, 0.25]
    assert [row["OrcaWave_period_s"] for row in mapping] == [1, 2, 3, 4]
    for name, before in sources_before.items():
        assert (root / name).read_bytes() == before


def test_cli_refusal_has_no_output(mod, tmp_path, capsys):
    output = tmp_path / "issued"
    with pytest.raises(SystemExit) as error:
        mod.main(["issue", str(REQUEST), str(output), "--source-root", str(ROOT)])
    assert error.value.code == 1
    assert "cap" in capsys.readouterr().err
    assert not output.exists()


def test_repository_basis_remains_withheld(mod, tmp_path):
    request = json.loads(REQUEST.read_text(encoding="utf-8"))
    with pytest.raises(ValueError, match="cap"):
        mod.issue_native_inputs(request, ROOT, tmp_path / "issued")
    assert not (tmp_path / "issued").exists()


@pytest.mark.parametrize("already_selected", [False, True])
def test_complete_deck0_selects_ah1_before_end_without_splitting_restart(
        mod, candidate, already_selected):
    request, root = candidate
    path = root / "candidate.dat"
    source = path.read_text(encoding="utf-8")
    if already_selected:
        source = source.replace("OPTIONS GOON\n", "OPTIONS GOON\nOPTIONS AHD1\n")
        path.write_text(source, encoding="utf-8", newline="\n")
        request["sources"]["AQWA"]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    native = ((root.parent / (root.name + "-issued")) / "stage1.dat").read_text(encoding="utf-8")
    lines = native.splitlines()
    assert sum("AHD1" in line.split()[1:] for line in lines
               if line.startswith("OPTIONS ")) == 1
    assert lines.index("OPTIONS AHD1") < lines.index("OPTIONS LHFR SFBM MQTF REST END")
    assert "OPTIONS LHFR SFBM MQTF REST END\nRESTART  1  5\n" in native
    assert source.replace("OPTIONS AHD1\n", "").split("          FDR1")[0] == (
        native.replace("OPTIONS AHD1\n", "").split("          FDR1")[0])


@pytest.mark.parametrize("mutation", ["missing_end", "late_ahd1", "missing_options",
                                      "after_end_token", "split_restart", "repeated_end"])
def test_invalid_deck0_refuses_issuance(mod, candidate, mutation):
    request, root = candidate
    path = root / "candidate.dat"
    source = path.read_text(encoding="utf-8")
    if mutation == "missing_end":
        source = source.replace("REST END", "REST")
    elif mutation == "late_ahd1":
        source = source.replace("RESTART  1  5\n", "RESTART  1  5\nOPTIONS AHD1\n")
    elif mutation == "after_end_token":
        source = source.replace("REST END", "REST END AHD1")
    elif mutation == "split_restart":
        source = source.replace("RESTART  1  5\n", "OPTIONS AHD1\nRESTART  1  5\n")
    elif mutation == "repeated_end":
        source = source.replace("REST END", "REST END AHD1 END")
    else:
        source = "\n".join(line for line in source.splitlines()
                           if not line.startswith("OPTIONS")) + "\n"
    path.write_text(source, encoding="utf-8", newline="\n")
    request["sources"]["AQWA"]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match="Deck 0"):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


@pytest.mark.parametrize("field", ["allocation_policy", "stage1_max_licensed_minutes",
                                   "later_stage_multiplier", "citation",
                                   "contingency_rationale"])
def test_missing_allocation_policy_prevents_issuance(mod, candidate, field):
    request, root = candidate
    if field == "allocation_policy":
        del request[field]
    else:
        del request["allocation_policy"][field]
    with pytest.raises(ValueError, match="allocation policy"):
        mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert not (root.parent / (root.name + "-issued")).exists()


@pytest.mark.parametrize("multiplier", [1.5, 1.0000001])
def test_issuance_uses_and_retains_supplied_resource_policy(mod, candidate, multiplier):
    request, root = candidate
    request["allocation_policy"].update(stage1_max_licensed_minutes=45,
                                         later_stage_multiplier=multiplier)
    request["stage1"]["cap_licensed_minutes"] = 40
    manifest = mod.issue_native_inputs(request, root, root.parent / (root.name + "-issued"))
    assert manifest["allocation_policy"] == request["allocation_policy"]
    assert "stop at 40 minutes" in manifest["stage1"]["stop_rule"]
    assert all(f"x {multiplier}," in stage["allocation"]
               for stage in manifest["later_stages"])


@pytest.mark.parametrize("destination", ["source", "nested", "symlink", "git_dir",
                                          "git_file", "git_ancestor", "git_symlink"])
def test_public_tree_destinations_refuse_before_output(mod, candidate, tmp_path,
                                                      destination):
    request, root = candidate
    if destination == "source":
        output = root
    elif destination == "nested":
        (root / "docs").mkdir()
        output = root / "docs/stage1"
    elif destination == "symlink":
        alias = root.parent / (root.name + "-alias")
        alias.symlink_to(root, target_is_directory=True)
        output = alias / "stage1"
    else:
        tree = root.parent / (root.name + "-tree")
        tree.mkdir()
        if destination == "git_file":
            (tree / ".git").write_text("gitdir: synthetic\n", encoding="utf-8")
        else:
            (tree / ".git").mkdir()
            (tree / ".git/HEAD").write_text("ref: refs/heads/synthetic\n", encoding="utf-8")
        parent = tree / "docs" if destination == "git_ancestor" else tree
        parent.mkdir(exist_ok=True)
        output = parent / "stage1"
        if destination == "git_symlink":
            alias = root.parent / (root.name + "-git-alias")
            alias.symlink_to(tree, target_is_directory=True)
            output = alias / "stage1"
    before = sorted(root.rglob("*"))
    with pytest.raises(ValueError, match="private output"):
        mod.issue_native_inputs(request, root, output)
    assert sorted(root.rglob("*")) == before
    if destination != "source":
        assert not output.exists()


@pytest.mark.parametrize("field", ["citation", "contingency_rationale"])
@pytest.mark.parametrize("value", ["-", "NOT-EVALUATED", "NOT_EVALUATED.",
                                    "none", "TODO", "TBC"])
def test_placeholder_policy_evidence_refuses_issuance(mod, candidate, field, value):
    request, root = candidate
    request["allocation_policy"][field] = value
    output = root.parent / (root.name + "-issued")
    with pytest.raises(ValueError, match="allocation policy"):
        mod.issue_native_inputs(request, root, output)
    assert not output.exists()


@pytest.mark.parametrize("citation", ["approved", "a policy exists", "section 2"])
def test_policy_citation_requires_a_document_reference(mod, candidate, citation):
    request, root = candidate
    request["allocation_policy"]["citation"] = citation
    output = root.parent / (root.name + "-issued")
    with pytest.raises(ValueError, match="document reference"):
        mod.issue_native_inputs(request, root, output)
    assert not output.exists()


def test_restart_before_options_end_refuses_issuance(mod, candidate):
    request, root = candidate
    path = root / "candidate.dat"
    source = path.read_text(encoding="utf-8").replace(
        "OPTIONS LHFR SFBM MQTF REST END\nRESTART  1  5\n",
        "OPTIONS LHFR SFBM MQTF REST\nRESTART  1  5\nOPTIONS END\n")
    path.write_text(source, encoding="utf-8", newline="\n")
    request["sources"]["AQWA"]["sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    output = root.parent / (root.name + "-issued")
    with pytest.raises(ValueError, match="Deck 0"):
        mod.issue_native_inputs(request, root, output)
    assert not output.exists()


def test_stored_allocations_remain_unevaluated_until_cited():
    request = json.loads(REQUEST.read_text(encoding="utf-8"))
    assert set(request["allocation_policy"].values()) == {"NOT_EVALUATED"}
    assert request["stage1"]["cap_licensed_minutes"] == "NOT_EVALUATED"
    assert request["stage1"]["stop_rule"] == "NOT_EVALUATED"
    assert all(stage["allocation"] == "NOT_EVALUATED"
               for stage in request["later_stages"])


@pytest.mark.parametrize("citation", [
    "Synthetic resource policy, revision 1, section 2",
    "policies/synthetic-resource-policy.md:12",
    "https://example.org/synthetic-resource-policy#section-2",
])
def test_document_reference_forms_are_retained(mod, candidate, citation):
    request, root = candidate
    request["allocation_policy"]["citation"] = citation
    output = root.parent / (root.name + "-issued")
    manifest = mod.issue_native_inputs(request, root, output)
    assert manifest["allocation_policy"]["citation"] == citation
