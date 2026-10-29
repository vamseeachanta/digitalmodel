# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Atlas refresh / staleness contract (#799, epic #794)."""

from __future__ import annotations

import hashlib
import json
from types import SimpleNamespace

import pandas as pd
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
    for wid in ("mooring-fatigue", "api-rp-2rd-riser", "rao-tabulation",
                "synthetic-rope-mooring-fatigue", "free-span-f105"):
        status = refresh.refresh_status(wid)
        assert status["stale"] is False, f"{wid} unexpectedly stale: {status}"


def test_committed_synthetic_rope_query_returns_screening_estimate(tmp_path):
    """The shipped in-range query must use a current atlas without escalation."""
    import yaml

    row = refresh._row("synthetic-rope-atlas-query")
    cfg = yaml.safe_load((refresh.REPO_ROOT / row["input"]).read_text())
    cfg["parametric_query"]["output_dir"] = str(tmp_path)
    result = router(cfg)["parametric_query"]["result"]
    assert result["in_range"] is True
    assert result.get("stale", False) is False
    assert result["response"] == "fatigue_life_years"
    assert result["value"] > 0.0
    assert result["screening_status"] in {"pass", "fail"}
    lo, hi = result["confidence"]["band"]
    assert lo <= result["value"] <= hi
    assert "screening estimate" in result["disclaimer"].lower()
    assert result["provenance"]["atlas_id"]


def test_fresh_build_is_not_stale(tmp_path):
    atlas = build_atlas_from_registry("rao-tabulation", atlas_root=tmp_path)
    assert _staleness(atlas) is None


def test_committed_free_span_atlas_matches_rebuild(tmp_path):
    """Check native data and parsed artifacts, allowing hook-only whitespace."""
    from digitalmodel.parametric.atlas import Atlas
    import yaml

    rebuilt = build_atlas_from_registry("free-span-f105", atlas_root=tmp_path)
    committed = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "free_span")
    assert committed.atlas_id == rebuilt.atlas_id
    pd.testing.assert_frame_equal(committed.grid, rebuilt.grid)
    relative = "free_span/" + rebuilt.atlas_id
    for name in ("surrogate.json", "validation.json"):
        actual = (refresh.DEFAULT_ATLAS_ROOT / relative / name).read_text()
        expected = (tmp_path / relative / name).read_text()
        assert json.loads(actual) == json.loads(expected)
    actual = yaml.safe_load(
        (refresh.DEFAULT_ATLAS_ROOT / relative / "manifest.yaml").read_text())
    expected = yaml.safe_load((tmp_path / relative / "manifest.yaml").read_text())
    # Unrelated commits do not change the dependency fingerprint or atlas id.
    actual["provenance"].pop("code_version")
    expected["provenance"].pop("code_version")
    assert actual == expected
    assert (refresh.DEFAULT_ATLAS_ROOT / "free_span/default.txt").read_text().strip() == (
        tmp_path / "free_span/default.txt").read_text().strip()


def test_committed_free_span_has_verified_provenance():
    """Retain the current atlas's source basis and all four artifact digests."""
    from digitalmodel.parametric import historical_provenance as history
    from digitalmodel.parametric.atlas import Atlas

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "free_span")
    manifest = f"atlases/free_span/{atlas.atlas_id}/manifest.yaml"
    inventory = json.loads(
        (refresh.DEFAULT_ATLAS_ROOT / "historical-provenance.json").read_text())
    record = next((row for row in inventory["atlases"]
                   if row["manifest"] == manifest), None)
    assert record is not None, f"Missing source and artifact evidence: {manifest}"
    assert record["status"] == "VERIFIED"
    assert history.verify_sources(record, history.archive_reader(refresh.REPO_ROOT)) == (
        atlas.provenance["content_fingerprint"])
    history.verify_artifacts(record, refresh.REPO_ROOT)


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


def test_viv_atlas_tracks_c205_edition():
    assert {"id": "DNV-RP-C205", "edition": "2010-10"} in refresh.STANDARDS["viv_analysis"]


def test_viv_atlas_matches_corrected_legacy_response():
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.generate import RESPONSE_FUNCS

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "viv_analysis")
    point = {"span_length_ft": 60.0, "current_speed_in_s": 36.0}
    prediction = atlas.predict(point)
    # The atlas selects the minimum over empty and flooded cases.
    # Flooded f_n = 1.7375267386 Hz; in-line excitation = 2 * St * U / D.
    expected = 1.7375267386 / (2 * 0.2 * 36 / 12.75)
    assert prediction.in_range
    assert prediction.value == pytest.approx(expected, abs=1e-9)
    assert prediction.value == pytest.approx(RESPONSE_FUNCS["viv_analysis"](point), abs=1e-9)
    assert refresh.refresh_status("viv-analysis")["stale"] is False
    assert atlas.validation["passes"] is True


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

        def stub_build(workflow_id, atlas_root=None):
            built.append(workflow_id)
            destination = tmp_path / workflow_id
            destination.mkdir()
            (destination / "validation.json").write_text(
                json.dumps(records[workflow_id]) + "\n", encoding="utf-8",
            )
            basename = f"atlas-{workflow_id}"
            staged = (atlas_root or tmp_path) / basename / "rebuilt"
            staged.mkdir(parents=True)
            (staged / "validation.json").write_text(
                json.dumps(records[workflow_id]) + "\n", encoding="utf-8",
            )
            (staged.parent / "default.txt").write_text("rebuilt")
            return SimpleNamespace(validation=records[workflow_id],
                                   basename=basename, atlas_id="rebuilt")

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


def test_apply_missing_passes_key_pin(stub_refresh_builds, tmp_path, capsys):
    """A missing passes key fails validation and preserves the previous atlas."""
    ids, built = stub_refresh_builds([{}])
    previous = tmp_path / f"atlas-{ids[0]}"
    previous.mkdir()
    (previous / "default.txt").write_text("previous")
    (previous / "validation.json").write_text('{"passes": true}\n')
    before = _file_digests(previous)
    assert refresh.main(["--apply"]) == 2
    assert built == ids
    output = capsys.readouterr().out
    assert "atlas-workflow-0/rebuilt/validation.json missing key 'passes'" in output
    assert _file_digests(previous) == before


def _file_digests(root):
    return {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in root.rglob("*") if path.is_file()}


@pytest.fixture
def disk_refresh_builds(monkeypatch, tmp_path):
    """Exercise refresh with stub builds using the actual atlas persistence format."""
    from digitalmodel.parametric.atlas import Atlas

    root = tmp_path / "atlases"
    status = refresh.refresh_status

    def configure(validations, same_id=False):
        ids = [f"workflow-{index}" for index in range(len(validations))]
        roots = []
        originals = {}
        monkeypatch.setattr(refresh, "DEFAULT_ATLAS_ROOT", root)
        monkeypatch.setattr(refresh, "parametric_workflow_ids", lambda: ids)
        monkeypatch.setattr(refresh, "LIBRARY_EXPECTATIONS", {})
        monkeypatch.setattr(refresh, "_row", lambda wid: {"basename": wid})
        monkeypatch.setattr(refresh, "content_fingerprint", lambda wid: "current")
        monkeypatch.setattr(refresh, "refresh_status", lambda wid: status(wid, root))
        for wid in ids:
            atlas = Atlas(wid, "old", "linear", "value", [],
                          pd.DataFrame({"value": [0.0]}), {"passes": True},
                          {"content_fingerprint": "previous"})
            atlas.save(root)
            (root / wid / "old" / "notes.txt").write_text("retained\n")
            originals[wid] = _file_digests(root / wid)

        def stub_build(wid, atlas_root=None):
            destination = atlas_root if atlas_root is not None else root
            roots.append(destination)
            destination.mkdir(parents=True, exist_ok=True)
            assert destination.stat().st_dev == root.stat().st_dev
            record = validations[ids.index(wid)]
            atlas = Atlas(wid, "old" if same_id else "new", "linear", "value", [],
                          pd.DataFrame({"value": [1.0]}),
                          {} if isinstance(record, Exception) else record,
                          {"content_fingerprint": "current"})
            atlas.save(destination)
            assert sorted(p.parent.name for p in root.glob("*/default.txt")) == ids
            if isinstance(record, Exception):
                raise record
            return atlas

        monkeypatch.setattr(build, "build_atlas_from_registry", stub_build)
        return root, ids, originals, roots, stub_build

    return configure


def test_failed_rebuild_keeps_previous_and_stale(disk_refresh_builds, capsys):
    root, ids, originals, _, _ = disk_refresh_builds([{"passes": False}])
    assert refresh.main(["--apply"]) == 2
    assert _file_digests(root / ids[0]) == originals[ids[0]]
    assert refresh.main([]) == 1
    assert "[STALE] workflow-0" in capsys.readouterr().out


@pytest.mark.parametrize("passes", [[False, True], [True, False], [False, False]],
                         ids=["failure-then-success", "success-then-failure", "two-failures"])
def test_mixed_rebuilds_replace_only_passing(disk_refresh_builds, capsys, passes):
    from digitalmodel.parametric.atlas import Atlas

    root, ids, originals, _, _ = disk_refresh_builds(
        [{"passes": value} for value in passes])
    assert refresh.main(["--apply"]) == 2
    for wid, passed in zip(ids, passes):
        if passed:
            assert Atlas.load(root, wid).provenance["content_fingerprint"] == "current"
            assert _file_digests(root / wid) != originals[wid]
        else:
            assert _file_digests(root / wid) == originals[wid]
    assert capsys.readouterr().out.splitlines()[-1] == (
        f"{passes.count(False)} rebuilt computed atlas(es) failed validation; "
        "previous version of each kept; exit status 2")


@pytest.mark.parametrize("same_id", [False, True], ids=["new-id", "same-id"])
def test_passing_rebuild_matches_direct_build(disk_refresh_builds, tmp_path, same_id):
    import shutil

    root, _, _, roots, stub_build = disk_refresh_builds([{"passes": True}], same_id)
    direct = tmp_path / "direct"
    shutil.copytree(root, direct)
    stub_build("workflow-0", atlas_root=direct)
    assert refresh.main(["--apply"]) == 0
    assert _file_digests(root) == _file_digests(direct)
    assert roots[-1] != root
    assert not roots[-1].exists()


@pytest.mark.parametrize("record,expected", [({"passes": True}, 0),
                                             ({"passes": False}, 2)],
                         ids=["passing", "failing"])
def test_rebuild_removes_temporary_root(disk_refresh_builds, record, expected):
    root, ids, _, roots, _ = disk_refresh_builds([record])
    assert refresh.main(["--apply"]) == expected
    assert roots[0] != root
    assert not roots[0].exists()
    assert sorted(path.name for path in root.iterdir()) == ids


def test_raising_builder_keeps_previous_and_removes_temporary_root(disk_refresh_builds):
    root, ids, originals, roots, _ = disk_refresh_builds([RuntimeError("build failed")])
    with pytest.raises(RuntimeError, match="build failed"):
        refresh.main(["--apply"])
    assert _file_digests(root / ids[0]) == originals[ids[0]]
    assert roots[0] != root
    assert not roots[0].exists()
    assert sorted(path.name for path in root.iterdir()) == ids


def test_malformed_rebuild_keeps_previous_and_removes_temporary_root(
    disk_refresh_builds, capsys,
):
    root, ids, originals, roots, _ = disk_refresh_builds([{}])
    assert refresh.main(["--apply"]) == 2
    lines = capsys.readouterr().out.splitlines()
    assert lines[-2] == (
        "  WARNING: workflow-0 workflow-0/new/validation.json "
        "missing key 'passes'; failed validation gate")
    assert lines[-1] == (
        "1 rebuilt computed atlas(es) failed validation; "
        "previous version of each kept; exit status 2")
    assert _file_digests(root / ids[0]) == originals[ids[0]]
    assert roots[0] != root
    assert not roots[0].exists()
    assert sorted(path.name for path in root.iterdir()) == ids
