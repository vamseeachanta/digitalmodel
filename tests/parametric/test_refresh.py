"""Atlas refresh / staleness contract (#799, epic #794)."""

from __future__ import annotations

import json
from types import SimpleNamespace

import pytest

from digitalmodel.parametric import build, refresh
from digitalmodel.parametric.build import build_atlas_from_registry
from digitalmodel.parametric.query import _staleness, router


def test_fingerprint_is_deterministic():
    a = refresh.content_fingerprint("mooring-fatigue")
    b = refresh.content_fingerprint("mooring-fatigue")
    assert a == b
    assert len(a) == 64


def test_parametric_workflow_ids_discovered():
    ids = set(refresh.parametric_workflow_ids())
    assert {"mooring-fatigue", "api-rp-2rd-riser", "rao-tabulation"} <= ids


def test_committed_atlases_are_current():
    for wid in ("mooring-fatigue", "api-rp-2rd-riser", "rao-tabulation"):
        status = refresh.refresh_status(wid)
        assert status["stale"] is False, f"{wid} unexpectedly stale: {status}"


def test_fresh_build_is_not_stale(tmp_path):
    atlas = build_atlas_from_registry("rao-tabulation", atlas_root=tmp_path)
    assert _staleness(atlas) is None


def test_tampered_basis_is_stale_and_escalates(tmp_path):
    atlas = build_atlas_from_registry("rao-tabulation", atlas_root=tmp_path)
    # simulate a basis change after build: stored fingerprint no longer matches
    atlas.provenance["content_fingerprint"] = "deadbeef" * 8
    assert _staleness(atlas) is not None
    atlas.save(tmp_path)

    cfg = router({
        "_config_file_path": str(tmp_path / "q.yml"),
        "parametric_query": {
            "atlas": "rao_tabulation",
            "atlas_root": str(tmp_path),
            "output_dir": str(tmp_path),
            "policy": {"on_out_of_range": "escalate"},
            "point": {"period_s": 8.0, "heading_deg": 45.0},
        },
    })
    result = cfg["parametric_query"]["result"]
    assert result["in_range"] is False
    assert result["stale"] is True
    assert "stale" in result["reason"]


# -- CI drift gate (#833) ----------------------------------------------------

def test_ci_gate_is_green_on_the_committed_state():
    # the real committed atlases are all current -> exit 0
    assert refresh.main([]) == 0


def test_ci_gate_fails_on_a_stale_computed_atlas(monkeypatch):
    monkeypatch.setattr(refresh, "parametric_workflow_ids", lambda: ["w"])
    monkeypatch.setattr(refresh, "refresh_status",
                        lambda wid: {"stale": True, "reason": "basis changed"})
    monkeypatch.setattr(refresh, "LIBRARY_EXPECTATIONS", {})
    assert refresh.main([]) == 1


def test_ci_gate_reports_but_does_not_fail_on_a_stale_library(monkeypatch):
    monkeypatch.setattr(refresh, "parametric_workflow_ids", lambda: [])
    monkeypatch.setattr(refresh, "LIBRARY_EXPECTATIONS", {"diffraction_library": {}})
    monkeypatch.setattr(refresh, "library_status",
                        lambda b: {"stale": True, "reason": "needs licensed run"})
    assert refresh.main([]) == 0                       # report-only by default
    assert refresh.main(["--strict-libraries"]) == 1   # enforced under --strict


def test_sn_curve_atlases_track_the_c203_sources():
    """#2165: mooring_fatigue and riser_fatigue compute damage through
    fatigue.sn_curves.get_sn_curve, whose DNV-RP-C203 values live in
    fatigue.c203_sn_tables. A change to either must mark those atlases stale;
    riser_fatigue previously tracked only its workflow module."""
    for basename in ("mooring_fatigue", "riser_fatigue"):
        files = refresh.SOURCE_FILES[basename]
        assert "src/digitalmodel/fatigue/sn_curves.py" in files, basename
        assert "src/digitalmodel/fatigue/c203_sn_tables.py" in files, basename


def _copy_basis(workflow_id: str, dst):
    """Copy the files a workflow's fingerprint reads into a scratch repo root."""
    import shutil

    row = refresh._row(workflow_id)
    rels = [row["input"], *refresh.SOURCE_FILES[row["basename"]]]
    for rel in rels:
        target = dst / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(refresh.REPO_ROOT / rel, target)


def test_riser_fatigue_fingerprint_tracks_touchdown(tmp_path):
    """#2165 review finding 4: the wave contribution runs through
    riser_fatigue.touchdown.assess_touchdown_fatigue, so an edit to
    touchdown.py must change the riser_fatigue fingerprint (stale atlas)."""
    rel = "src/digitalmodel/riser_fatigue/touchdown.py"
    assert rel in refresh.SOURCE_FILES["riser_fatigue"]

    _copy_basis("riser-fatigue", tmp_path)
    before = refresh.content_fingerprint("riser-fatigue", repo_root=tmp_path)
    assert before == refresh.content_fingerprint("riser-fatigue")

    touchdown = tmp_path / rel
    touchdown.write_bytes(touchdown.read_bytes() + b"\n# edited\n")
    after = refresh.content_fingerprint("riser-fatigue", repo_root=tmp_path)
    assert after != before


@pytest.fixture
def stub_refresh_builds(monkeypatch, tmp_path):
    """Replace registry builds with validation records saved in temporary storage."""
    def configure(validations, stale_library=False):
        ids = [f"workflow-{index}" for index in range(len(validations))]
        records = dict(zip(ids, validations))
        built = []
        monkeypatch.setattr(refresh, "DEFAULT_ATLAS_ROOT", tmp_path)
        monkeypatch.setattr(refresh, "parametric_workflow_ids", lambda: ids)
        monkeypatch.setattr(
            refresh, "refresh_status",
            lambda wid: {"stale": True, "reason": "basis changed"},
        )
        monkeypatch.setattr(
            refresh, "LIBRARY_EXPECTATIONS",
            {"stub_library": {}} if stale_library else {},
        )
        monkeypatch.setattr(
            refresh, "library_status",
            lambda basename: {"stale": True, "reason": "needs licensed run"},
        )

        def stub_build(workflow_id):
            built.append(workflow_id)
            destination = tmp_path / workflow_id
            destination.mkdir()
            (destination / "validation.json").write_text(
                json.dumps(records[workflow_id]) + "\n", encoding="utf-8",
            )
            return SimpleNamespace(validation=records[workflow_id])

        monkeypatch.setattr(build, "build_atlas_from_registry", stub_build)
        return ids, built

    return configure


@pytest.mark.parametrize(
    "passes,strict",
    [([False], False), ([False, True], False),
     ([True, False], False), ([False], True)],
    ids=["single-failure", "failure-then-success", "success-then-failure",
         "failure-with-strict-library"],
)
def test_apply_failed_validation_returns_distinct_status(
    stub_refresh_builds, tmp_path, capsys, passes, strict,
):
    records = [{"passes": value} for value in passes]
    ids, built = stub_refresh_builds(records, stale_library=strict)
    arguments = ["--apply", "--strict-libraries"] if strict else ["--apply"]
    result = refresh.main(arguments)
    output = capsys.readouterr().out
    assert result == 2
    assert built == ids
    assert "all atlases current" not in output
    for workflow_id, record in zip(ids, records):
        assert json.loads((tmp_path / workflow_id / "validation.json").read_text()) == record
        if not record["passes"]:
            assert f"WARNING: {workflow_id} regenerated but failed validation gate" in output


@pytest.mark.parametrize("strict,expected", [(False, 0), (True, 1)])
def test_apply_passing_rebuild_library_status_pin(stub_refresh_builds, strict, expected):
    """Regression pin: passing builds preserve report-only and strict library statuses."""
    ids, built = stub_refresh_builds([{"passes": True}], stale_library=True)
    arguments = ["--apply", "--strict-libraries"] if strict else ["--apply"]
    assert refresh.main(arguments) == expected
    assert built == ids


def test_apply_no_stale_atlas_status_pin(stub_refresh_builds, capsys):
    """Regression pin: a current apply run succeeds without calling the builder."""
    _, built = stub_refresh_builds([])
    assert refresh.main(["--apply"]) == 0
    assert built == []
    assert "all atlases current" in capsys.readouterr().out


def test_apply_missing_passes_key_pin(stub_refresh_builds):
    """Regression pin: a missing passes key retains the existing KeyError."""
    ids, built = stub_refresh_builds([{}])
    with pytest.raises(KeyError, match="passes"):
        refresh.main(["--apply"])
    assert built == ids
