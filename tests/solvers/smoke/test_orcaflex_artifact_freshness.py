"""Saved-artifact provenance regressions for the issue 2082 smoke proof.

The independent oracle is file identity: pre-existing bytes cannot establish
that the current simulation was saved. Every test uses the injected fake API.
"""

import json

import pytest

from digitalmodel.solvers.smoke.probes import check_orcaflex


@pytest.mark.parametrize("name", ["smoke.dat", "smoke.sim"])
def test_existing_artifact_refuses_before_model_allocation(tmp_path, fake_api, name):
    previous = tmp_path / name
    previous.write_bytes(b"previous smoke result")
    # A save that produces nothing must never borrow a previous simulation.
    fake_api.sim_file = "missing"

    result = check_orcaflex(tmp_path)

    assert result["ok"] is False
    assert result["stage"] == "prepare"
    assert "already exists" in result["error"]
    assert name in result["error"]
    assert fake_api.constructors == []
    assert previous.read_bytes() == b"previous smoke result"
    assert not result.get("saved_sim_reloaded")
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("name", ["smoke.dat", "smoke.sim"])
def test_existing_artifact_directory_is_preserved(tmp_path, fake_api, name):
    previous = tmp_path / name
    previous.mkdir()
    evidence = previous / "evidence.txt"
    evidence.write_text("retained evidence", encoding="utf-8")

    result = check_orcaflex(tmp_path)

    assert result["ok"] is False
    assert result["stage"] == "prepare"
    assert fake_api.constructors == []
    assert evidence.read_text(encoding="utf-8") == "retained evidence"


@pytest.mark.parametrize("name", ["smoke.dat", "smoke.sim"])
@pytest.mark.parametrize("target_exists", [True, False])
def test_artifact_symlink_is_preserved(tmp_path, fake_api, name, target_exists):
    target = tmp_path / "retained-target"
    if target_exists:
        target.write_bytes(b"retained target")
    link = tmp_path / name
    try:
        link.symlink_to(target.name)
    except (OSError, NotImplementedError) as exc:
        pytest.skip(f"symlink creation unavailable: {exc}")

    result = check_orcaflex(tmp_path)

    assert result["ok"] is False
    assert result["stage"] == "prepare"
    assert fake_api.constructors == []
    assert link.is_symlink()
    assert link.readlink().name == target.name
    assert target.exists() is target_exists
    if target_exists:
        assert target.read_bytes() == b"retained target"


def test_completed_output_cannot_be_reused_as_a_new_proof(tmp_path, fake_api):
    assert check_orcaflex(tmp_path)["ok"] is True
    previous = {path.name: path.read_bytes() for path in tmp_path.iterdir()}
    fake_api.constructors.clear()

    result = check_orcaflex(tmp_path)

    assert result["ok"] is False
    assert result["stage"] == "prepare"
    assert fake_api.constructors == []
    assert {path.name: path.read_bytes() for path in tmp_path.iterdir()} == previous


def test_unrelated_output_does_not_prevent_a_fresh_proof(tmp_path, fake_api):
    retained = tmp_path / "notes.txt"
    retained.write_text("retained notes", encoding="utf-8")

    result = check_orcaflex(tmp_path)

    assert result["ok"] is True
    assert retained.read_text(encoding="utf-8") == "retained notes"
