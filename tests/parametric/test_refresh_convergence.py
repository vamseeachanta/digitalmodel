# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Apply must confirm installed freshness, including mixed rebuild outcomes."""

from __future__ import annotations

from types import SimpleNamespace

import pandas as pd
import pytest

from digitalmodel.parametric import build, refresh
from digitalmodel.parametric.atlas import Atlas


@pytest.fixture
def synthetic_refresh(monkeypatch, tmp_path):
    """Build only temporary synthetic atlases through the real install path."""
    root = tmp_path / "atlases"
    original_status = refresh.refresh_status
    calls = []
    built = []

    def save(wid, destination, fingerprint):
        Atlas(wid, "0123456789ab", "linear", "value", [],
              pd.DataFrame({"value": [1.0]}), {"passes": True},
              {"content_fingerprint": fingerprint}).save(destination)

    def configure(outcomes, stale_library=False):
        ids = list(outcomes)
        monkeypatch.setattr(refresh, "REPO_ROOT", tmp_path)
        monkeypatch.setattr(refresh, "DEFAULT_ATLAS_ROOT", root)
        monkeypatch.setattr(refresh, "parametric_workflow_ids", lambda: ids)
        monkeypatch.setattr(refresh, "_row", lambda wid: {"basename": wid})
        monkeypatch.setattr(refresh, "content_fingerprint", lambda wid: "a" * 64)
        monkeypatch.setattr(refresh, "LIBRARY_EXPECTATIONS",
                            {"synthetic_library": {}} if stale_library else {})
        monkeypatch.setattr(refresh, "library_status", lambda basename: {
            "stale": True, "reason": "needs licensed run",
        })
        for wid, outcome in outcomes.items():
            if outcome != "missing":
                save(wid, root, "b" * 64)

        def status(wid):
            calls.append(wid)
            return original_status(wid, atlas_root=root)

        def stub_build(wid, atlas_root):
            built.append(wid)
            outcome = outcomes[wid]
            if outcome == "missing":
                (atlas_root / wid).mkdir(parents=True)
            else:
                save(wid, atlas_root, "a" * 64 if outcome == "current" else "b" * 64)
            return SimpleNamespace(basename=wid, validation={"passes": outcome != "invalid"})

        monkeypatch.setattr(refresh, "refresh_status", status)
        monkeypatch.setattr(build, "build_atlas_from_registry", stub_build)
        return root, calls, built

    return configure


def test_apply_fails_closed_when_builder_leaves_stale_content(synthetic_refresh, capsys):
    _, calls, built = synthetic_refresh({"example": "stale"})
    assert refresh.main(["--apply"]) == 1
    output = capsys.readouterr().out
    assert calls == ["example", "example"]
    assert built == ["example"]
    assert "all atlases current" not in output
    assert "ERROR:" in output
    assert "atlases/example/0123456789ab" in output
    assert "stored=" + "b" * 64 in output
    assert "current=" + "a" * 64 in output


def test_apply_succeeds_only_after_installed_content_is_current(synthetic_refresh, capsys):
    _, calls, built = synthetic_refresh({"example": "current"})
    assert refresh.main(["--apply"]) == 0
    output = capsys.readouterr().out
    assert calls == ["example", "example"]
    assert built == ["example"]
    assert output.splitlines()[-1] == "all atlases current"
    assert "ERROR:" not in output


def test_apply_fails_closed_when_post_build_artifact_is_missing(synthetic_refresh, capsys):
    _, calls, built = synthetic_refresh({"example": "missing"})
    assert refresh.main(["--apply"]) == 1
    output = capsys.readouterr().out
    assert calls == ["example", "example"]
    assert built == ["example"]
    assert "all atlases current" not in output
    assert "ERROR:" in output
    assert "no atlas built" in output
    assert "stored=None" in output
    assert "current=" + "a" * 64 in output


@pytest.mark.parametrize("outcomes", [
    {"first": "stale", "second": "current"},
    {"first": "current", "second": "stale"},
])
def test_apply_mixed_results_retain_stale_failure(synthetic_refresh, capsys, outcomes):
    _, calls, built = synthetic_refresh(outcomes)
    assert refresh.main(["--apply"]) == 1
    output = capsys.readouterr().out
    assert calls == ["first", "second", "first", "second"]
    assert built == ["first", "second"]
    stale = next(wid for wid, outcome in outcomes.items() if outcome == "stale")
    assert f"ERROR: {stale}" in output
    assert output.count("ERROR:") == 1
    assert "all atlases current" not in output


@pytest.mark.parametrize("strict,expected", [(False, 0), (True, 1)])
def test_converged_apply_preserves_library_policy(synthetic_refresh, capsys, strict, expected):
    _, calls, _ = synthetic_refresh({"example": "current"}, stale_library=True)
    arguments = ["--apply", "--strict-libraries"] if strict else ["--apply"]
    assert refresh.main(arguments) == expected
    output = capsys.readouterr().out
    assert calls == ["example", "example"]
    assert "not auto-regenerated" in output
    assert "all atlases current" not in output


@pytest.mark.parametrize("strict", [False, True])
@pytest.mark.parametrize("outcomes", [
    {"first": "invalid", "second": "stale"},
    {"first": "stale", "second": "invalid"},
])
def test_validation_failure_takes_precedence_and_keeps_closing_block(
    synthetic_refresh, capsys, outcomes, strict,
):
    root, calls, built = synthetic_refresh(outcomes, stale_library=True)
    arguments = ["--apply", "--strict-libraries"] if strict else ["--apply"]
    assert refresh.main(arguments) == 2
    lines = capsys.readouterr().out.splitlines()
    invalid = next(wid for wid, outcome in outcomes.items() if outcome == "invalid")
    stale = next(wid for wid, outcome in outcomes.items() if outcome == "stale")
    assert built == ["first", "second"]
    assert calls == ["first", "second", stale]
    assert Atlas.load(root, invalid).provenance["content_fingerprint"] == "b" * 64
    assert lines[-2:] == [
        f"  WARNING: {invalid} regenerated but failed validation gate",
        "1 rebuilt computed atlas(es) failed validation; "
        "previous version of each kept; exit status 2",
    ]
    assert any(f"ERROR: {stale}" in line for line in lines[:-2])
    assert not any("WARNING:" in line for line in lines[:-2])
    assert not any(f"ERROR: {invalid}" in line for line in lines)
    assert "all atlases current" not in lines


def test_post_apply_status_error_propagates(synthetic_refresh, monkeypatch):
    _, calls, _ = synthetic_refresh({"example": "current"})
    status = refresh.refresh_status

    def unreadable_status(wid):
        if calls:
            raise OSError("installed atlas unreadable")
        return status(wid)

    monkeypatch.setattr(refresh, "refresh_status", unreadable_status)
    with pytest.raises(OSError, match="installed atlas unreadable"):
        refresh.main(["--apply"])
