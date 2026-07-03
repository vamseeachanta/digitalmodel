"""#1246: resolve_dff() DFF precedence for the riser-fatigue workflow.

Three behaviors, all fail-closed:
  * an explicit ``dff`` in settings wins (every existing config unchanged);
  * an absent ``dff`` resolves the cited DNV-OS-F201 default in-context (10.0);
  * an absent ``dff`` with no resolvable wiki RAISES — a pass/fail-driving
    input never silently defaults.
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.riser_fatigue.workflow import resolve_dff


def _fixture_repo_root() -> Path:
    """Vendored wiki fixture root (shared with the getter citation tests)."""
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


def test_explicit_dff_wins_and_skips_the_wiki(tmp_path):
    # tmp_path has no wiki page; an explicit dff must never touch resolution.
    assert resolve_dff({"dff": 3.0}, repo_root=tmp_path) == 3.0


def test_explicit_dff_is_validated_positive():
    with pytest.raises(ValueError):
        resolve_dff({"dff": 0.0})
    with pytest.raises(ValueError):
        resolve_dff({"dff": -1.0})


def test_absent_dff_resolves_cited_default_in_context():
    assert resolve_dff({}, repo_root=_fixture_repo_root()) == 10.0


def test_absent_dff_standalone_raises(tmp_path):
    # No wiki page under repo_root => fail closed, not a silent default.
    with pytest.raises(CitationResolutionError) as exc:
        resolve_dff({}, repo_root=tmp_path)
    assert exc.value.code_id == "dnv-os-f201"
