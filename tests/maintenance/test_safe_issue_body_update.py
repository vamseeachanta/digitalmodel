"""Tests for the #690 non-destructive body-overwrite guard."""
import importlib.util
import sys
from pathlib import Path

import pytest

_MOD = (Path(__file__).resolve().parents[2]
        / "scripts" / "maintenance" / "safe_issue_body_update.py")
_spec = importlib.util.spec_from_file_location("safe_issue_body_update", _MOD)
sib = importlib.util.module_from_spec(_spec)
sys.modules["safe_issue_body_update"] = sib
_spec.loader.exec_module(sib)


HUMAN_BODY = (
    "- [ ] Add Analytical calculation for structures. Refer to MP's calculation\n"
    "- [ ] Automate Orcaflex results for each mode\n"
)
WRK_1149_TEMPLATE = (
    "## WRK-1149: Method assessment and selection — propeller-rudder\n\n"
    "**Status:** done | **Priority:** medium | **Category:** engineering\n"
)
WRK_036_TEMPLATE = (
    "## WRK-036: OrcaFlex structure deployment analysis\n\n"
    "**Status:** done | **Priority:** medium\n"
)


def test_blocks_overwrite_of_human_body():
    """The exact #690 failure: stamping WRK template over a human body."""
    with pytest.raises(sib.UnsafeBodyOverwrite):
        sib.assert_safe_to_overwrite(HUMAN_BODY, "WRK-1149")


def test_blocks_wrong_target_wrk_template():
    """A DIFFERENT wrk's template already there = wrong-target mapping -> block."""
    with pytest.raises(sib.UnsafeBodyOverwrite):
        sib.assert_safe_to_overwrite(WRK_1149_TEMPLATE, "WRK-036")


def test_allows_empty_body():
    # null/empty body has nothing to lose (e.g. issue #17).
    sib.assert_safe_to_overwrite("", "WRK-099")
    sib.assert_safe_to_overwrite(None, "WRK-099")


def test_allows_idempotent_restamp():
    # Same wrk template already present -> safe to refresh.
    sib.assert_safe_to_overwrite(WRK_1149_TEMPLATE, "WRK-1149")
    # normalization: WRK-036 vs WRK036
    sib.assert_safe_to_overwrite(WRK_036_TEMPLATE, "WRK036")


def test_dry_run_is_default(monkeypatch):
    """gh_edit_body_safely must not call gh issue edit unless apply=True."""
    monkeypatch.setattr(sib, "current_body", lambda repo, n: "")

    calls = []

    def fake_run(cmd, *a, **k):
        calls.append(cmd)
        class R:
            returncode = 0
            stdout = ""
            stderr = ""
        return R()

    monkeypatch.setattr(sib.subprocess, "run", fake_run)
    msg = sib.gh_edit_body_safely("o/r", 13, "WRK-1149", "body", apply=False)
    assert "DRY-RUN" in msg
    assert calls == []  # no gh issue edit performed


def test_apply_blocked_does_not_write(monkeypatch):
    """Even with apply=True, a clobbering write must be blocked before gh runs."""
    monkeypatch.setattr(sib, "current_body", lambda repo, n: HUMAN_BODY)
    calls = []
    monkeypatch.setattr(sib.subprocess, "run",
                        lambda cmd, *a, **k: calls.append(cmd))
    with pytest.raises(sib.UnsafeBodyOverwrite):
        sib.gh_edit_body_safely("o/r", 13, "WRK-1149", "body", apply=True)
    assert calls == []  # blocked before any gh call
