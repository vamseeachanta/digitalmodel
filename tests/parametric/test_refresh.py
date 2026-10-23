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


def test_apply_missing_passes_key_preserves_previous_atlas(stub_refresh_builds, tmp_path, capsys):
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
    assert "WARNING: workflow-0 missing key 'passes'; failed validation gate" in output
    assert "validation.json" not in output
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
        "  WARNING: workflow-0 "
        "missing key 'passes'; failed validation gate")
    assert lines[-1] == (
        "1 rebuilt computed atlas(es) failed validation; "
        "previous version of each kept; exit status 2")
    assert _file_digests(root / ids[0]) == originals[ids[0]]
    assert roots[0] != root
    assert not roots[0].exists()
    assert sorted(path.name for path in root.iterdir()) == ids


@pytest.mark.parametrize("move", ["previous", "replacement"])
@pytest.mark.parametrize("after_move", [False, True], ids=["before-rename", "after-rename"])
def test_install_failure_preserves_an_atlas(tmp_path, monkeypatch, move, after_move):
    """Issue 2451: a failed swap shall leave a complete atlas at the destination.

    Independent oracle: the seeded previous bytes must survive when the
    destination is absent; an already installed replacement must remain intact.
    No engineering calculation or acceptance tolerance is involved.
    """
    from pathlib import Path

    destination = tmp_path / "live"
    staged = tmp_path / "staged"
    temporary = tmp_path / "temporary"
    for path in (destination, staged, temporary):
        path.mkdir()
    (destination / "default.txt").write_text("previous")
    (staged / "default.txt").write_text("replacement")
    original_rename = Path.rename

    def failing_rename(source, target):
        selected = (source == destination if move == "previous"
                    else source == temporary / "replacement")
        if selected:
            if after_move:
                original_rename(source, target)
            raise OSError("install interrupted")
        return original_rename(source, target)

    monkeypatch.setattr(Path, "rename", failing_rename)
    with pytest.raises(OSError, match="install interrupted"):
        refresh._swap_atlas(staged, destination, temporary)
    expected = "replacement" if move == "replacement" and after_move else "previous"
    assert (destination / "default.txt").read_text() == expected


@pytest.mark.parametrize("record", [None, False, 7, [], ["passes"], "passes"])
def test_non_mapping_validation_preserves_previous_atlas(
    stub_refresh_builds, tmp_path, capsys, record,
):
    """Issue 2451: malformed records yield status 2, with seeded bytes unchanged."""
    ids, _ = stub_refresh_builds([record])
    previous = tmp_path / f"atlas-{ids[0]}"
    previous.mkdir()
    (previous / "default.txt").write_text("previous")
    before = _file_digests(previous)
    assert refresh.main(["--apply"]) == 2
    assert _file_digests(previous) == before
    assert not list(tmp_path.glob(".refresh-*"))
    assert "WARNING: workflow-0 validation record is not a mapping" in capsys.readouterr().out


def test_two_failures_are_named_in_the_closing_block(stub_refresh_builds, capsys):
    """Issue 2451: two failed inputs require two closing diagnostics, after all builds."""
    ids, built = stub_refresh_builds([{}, {"passes": False}])
    assert refresh.main(["--apply"]) == 2
    lines = capsys.readouterr().out.splitlines()
    closing = lines[lines.index(f"refreshing {ids[-1]} ...") + 1:]
    assert built == ids
    assert closing[:-1] == [
        "  WARNING: workflow-0 missing key 'passes'; failed validation gate",
        "  WARNING: workflow-1 regenerated but failed validation gate",
    ]
    assert "validation.json" not in "\n".join(closing)


def test_leftover_refresh_directory_is_reported_before_status(
    disk_refresh_builds, capsys,
):
    """Issue 2451: an interrupted run's evidence is reported and never removed."""
    root, _, _, _, _ = disk_refresh_builds([])
    leftover = root / ".refresh-interrupted"
    leftover.mkdir(parents=True)
    (leftover / "evidence.txt").write_text("retained")
    before = _file_digests(leftover)
    assert refresh.main([]) == 0
    assert capsys.readouterr().out.splitlines()[0] == (
        "WARNING: leftover refresh directory .refresh-interrupted; inspect before removal")
    assert _file_digests(leftover) == before


def test_refresh_scratch_is_ignored():
    """Issue 2451: only temporary refresh trees under the default atlas root are ignored."""
    import os
    import subprocess

    environment = {k: v for k, v in os.environ.items()
                   if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--stdin"],
        cwd=refresh.REPO_ROOT, env=environment, text=True, capture_output=True,
        input="atlases/.refresh-interrupted/evidence.txt\natlases/real_atlas/default.txt\n",
        check=False,
    )
    assert result.returncode == 0
    assert result.stdout.splitlines() == ["atlases/.refresh-interrupted/evidence.txt"]


def test_failed_rollback_retains_previous_tree(disk_refresh_builds, monkeypatch, capsys):
    """Issue 2451: if both install and rollback fail, the original bytes remain recoverable."""
    from pathlib import Path

    root, ids, originals, _, _ = disk_refresh_builds([{"passes": True}])
    original_rename = Path.rename

    def failing_rename(source, target):
        if source.name in {"replacement", "previous"}:
            raise OSError("rename unavailable")
        return original_rename(source, target)

    monkeypatch.setattr(Path, "rename", failing_rename)
    with pytest.raises(OSError, match="rename unavailable"):
        refresh.main(["--apply"])
    retained = list(root.glob(".refresh-*/previous"))
    assert len(retained) == 1
    assert _file_digests(retained[0]) == originals[ids[0]]
    assert "WARNING: refresh recovery directory" in capsys.readouterr().out
