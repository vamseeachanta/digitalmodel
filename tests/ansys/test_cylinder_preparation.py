"""Synthetic filesystem tests for offline inputs; no native/checker execution."""
import copy
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_benchmark import build_case


ORDER = ["ocv-zero-t60-n16", "ocv-t60-p10-n4", "ocv-t60-p10-n8", "ocv-t60-p10-n16"]
PROPOSAL = Path(__file__).resolve().parents[2] / "docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json"


@pytest.fixture
def proposal():
    return json.loads(PROPOSAL.read_text(encoding="utf-8"))


def prepare(path, proposal):
    from digitalmodel.ansys.cylinder_preparation import prepare_inputs
    return prepare_inputs(path, proposal)


def test_exact_four_case_order_and_verified_artifact_roundtrip(tmp_path, proposal):
    root = tmp_path / "prepared"
    result = prepare(root, proposal)
    assert result["case_order"] == ORDER
    assert len(list(root.glob("*.inp"))) == 4
    assert result["status"] == "prepared_not_executed"
    assert result["native_execution"] is False
    assert result["independent_reference_established"] is False
    assert json.loads((root / "preparation.json").read_bytes()) == result
    for artifact in result["files"]:
        data = (root / artifact["path"]).read_bytes()
        assert hashlib.sha256(data).hexdigest() == artifact["sha256"]
        assert len(data) == artifact["bytes"]
        assert not Path(artifact["path"]).is_absolute()
    for case_id in ORDER:
        case = build_case(case_id)
        assert (root / f"{case_id}.inp").read_bytes() == case["deck_bytes"]
        metadata = json.loads((root / f"{case_id}.json").read_bytes())
        assert metadata["stations"] == case["stations"]
        assert metadata["deck_sha256"] == case["deck_sha256"]


def test_output_identity_is_reproducible_and_proposal_is_not_mutated(tmp_path, proposal):
    before = copy.deepcopy(proposal)
    first = prepare(tmp_path / "a", proposal)
    second = prepare(tmp_path / "b", proposal)
    assert first == second and proposal == before
    assert {p.name: p.read_bytes() for p in (tmp_path / "a").iterdir()} == {
        p.name: p.read_bytes() for p in (tmp_path / "b").iterdir()}


@pytest.mark.parametrize("existing", ["empty", "populated"])
def test_existing_output_is_never_overwritten(tmp_path, proposal, existing):
    root = tmp_path / "existing"
    root.mkdir()
    if existing == "populated":
        (root / "keep.txt").write_bytes(b"original")
    before = list(root.iterdir())
    with pytest.raises(FileExistsError):
        prepare(root, proposal)
    assert list(root.iterdir()) == before


def test_partial_filesystem_failure_is_preserved_and_explicit(tmp_path, proposal, monkeypatch):
    from digitalmodel.ansys.cylinder_preparation import PreparationError
    original = Path.open
    def failing_open(path, mode="r", *args, **kwargs):
        if path.name == "ocv-t60-p10-n4.inp" and mode == "xb":
            raise OSError("synthetic disk failure")
        return original(path, mode, *args, **kwargs)
    monkeypatch.setattr(Path, "open", failing_open)
    root = tmp_path / "partial"
    with pytest.raises(PreparationError) as caught:
        prepare(root, proposal)
    assert caught.value.failed_path == "ocv-t60-p10-n4.inp"
    failure = json.loads((root / "preparation-failure.json").read_bytes())
    assert failure["status"] == "partial_preparation"
    assert failure["failed_path"] == caught.value.failed_path
    assert (root / "ocv-zero-t60-n16.inp").is_file()
    assert not (root / "preparation.json").exists()
    assert not (root / "ocv-t60-p10-n8.inp").exists()


@pytest.mark.parametrize("change", ["basis", "criterion", "budget", "case", "order", "float", "extra"])
def test_changed_frozen_proposal_refused_before_creating_output(tmp_path, proposal, change):
    if change == "basis":
        proposal["basis"]["wall_thickness_mm"] = "6"
    elif change == "criterion":
        proposal["criteria"][0]["rule"] = "abs(q-reference) <= max(0.1*abs(reference), floor)"
    elif change == "budget":
        proposal["budget"]["maximum_attempts"] = 5
    elif change == "case":
        proposal["cases"][1]["pressure_mpa"] = "10.001"
    elif change == "order":
        proposal["cases"].reverse()
    elif change == "float":
        proposal["basis"]["wall_thickness_mm"] = 60.0
    else:
        proposal["extra"] = "unapproved"
    root = tmp_path / "refused"
    with pytest.raises(ValueError):
        prepare(root, proposal)
    assert not root.exists()


def test_equivalent_exact_decimals_normalize_without_integer_zero_loss(tmp_path, proposal):
    proposal["basis"]["wall_thickness_mm"] = "6e1"
    proposal["basis"]["inner_radius_mm"] = "750.000"
    proposal["cases"][1]["pressure_mpa"] = "10.000"
    root = tmp_path / "normalized"
    prepare(root, proposal)
    stored = json.loads((root / "proposal.json").read_bytes())
    assert stored["basis"]["wall_thickness_mm"] == "60"
    assert stored["cases"][1]["pressure_mpa"] == "10"
    assert stored["implementation_authorized"] is False  # historical status, not authority
