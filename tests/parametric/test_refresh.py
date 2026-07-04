"""Atlas refresh / staleness contract (#799, epic #794)."""

from __future__ import annotations

from digitalmodel.parametric import refresh
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
