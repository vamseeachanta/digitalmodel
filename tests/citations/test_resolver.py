"""Tests for the citation resolver (#617).

Covers the 6-level precedence chain:
  1. explicit override kwarg
  2. LLM_WIKI_PATH env var
  3. DIGITALMODEL_REPO_ROOT (legacy alias with DeprecationWarning)
  4. bounded parent walk (cap=8) for workspace-hub overlay
  5. known local clones (Path.home() / 'workspace-hub' / 'llm-wiki',
     /mnt/local-analysis/llm-wiki)
  6. fail-closed with actionable message naming LLM_WIKI_PATH, the
     private repo URL, and the routing-rule pointer.

Also covers layout-detection: standalone llm-wiki repos use `wikis/<domain>/...`
without the `knowledge/` prefix; workspace-hub overlay uses `knowledge/wikis/...`.
The resolver detects which layout a given base uses and joins accordingly.
"""
from __future__ import annotations

import logging
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.resolver import (
    WIKI_REPO_URL,
    ROUTING_RULE_URL,
    resolve_wiki_base,
    resolve_wiki_path,
    _validate_clone,
    _RESOLUTION_CACHE,
)


# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------


def _make_standalone_clone(root: Path) -> Path:
    """Create a fake standalone-layout llm-wiki clone at root."""
    (root / "wikis" / "engineering" / "wiki" / "standards").mkdir(parents=True)
    (root / "wikis" / "engineering" / "wiki" / "standards" / "dnv-os-e301.md").write_text(
        "---\ncode_id: DNV-OS-E301\npublisher: DNV\nrevision: 2021-07\n---\n",
        encoding="utf-8",
    )
    return root


def _make_workspace_hub_overlay(root: Path) -> Path:
    """Create a fake workspace-hub-layout overlay (with `knowledge/wikis/`) at root."""
    base = root / "knowledge" / "wikis" / "engineering" / "wiki" / "standards"
    base.mkdir(parents=True)
    (base / "dnv-os-e301.md").write_text(
        "---\ncode_id: DNV-OS-E301\npublisher: DNV\nrevision: 2021-07\n---\n",
        encoding="utf-8",
    )
    return root


@pytest.fixture(autouse=True)
def _clean_env(monkeypatch):
    """Each test starts with both env vars unset."""
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)


@pytest.fixture(autouse=True)
def _clean_cache():
    """Each test starts with an empty one-shot warn cache."""
    _RESOLUTION_CACHE.clear()
    yield
    _RESOLUTION_CACHE.clear()


@pytest.fixture
def isolated_known_clones(monkeypatch):
    """Disable real-filesystem fallbacks for tests that exercise fail-closed paths."""
    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "_KNOWN_LOCAL_CLONES", ())


# ---------------------------------------------------------------------------
# 1. Explicit override precedence
# ---------------------------------------------------------------------------


def test_resolver_explicit_override_wins(tmp_path, monkeypatch):
    """Explicit override kwarg beats all env vars."""
    standalone = _make_standalone_clone(tmp_path / "explicit")
    monkeypatch.setenv("LLM_WIKI_PATH", "/this/is/garbage")
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", "/also/garbage")

    resolved = resolve_wiki_base(override=standalone)
    assert resolved == standalone


# ---------------------------------------------------------------------------
# 2. LLM_WIKI_PATH env var — valid + invalid + layout cases
# ---------------------------------------------------------------------------


def test_resolver_llm_wiki_path_env_valid_standalone_layout(tmp_path, monkeypatch):
    """LLM_WIKI_PATH pointing to a standalone llm-wiki clone (no knowledge/ prefix)."""
    standalone = _make_standalone_clone(tmp_path / "standalone")
    monkeypatch.setenv("LLM_WIKI_PATH", str(standalone))

    resolved = resolve_wiki_base()
    assert resolved == standalone


def test_resolver_llm_wiki_path_env_valid_workspace_hub_layout(tmp_path, monkeypatch):
    """LLM_WIKI_PATH pointing to a workspace-hub root with knowledge/wikis/ overlay."""
    overlay = _make_workspace_hub_overlay(tmp_path / "workspace-hub")
    monkeypatch.setenv("LLM_WIKI_PATH", str(overlay))

    resolved = resolve_wiki_base()
    assert resolved == overlay


def test_resolver_llm_wiki_path_env_invalid_path(tmp_path, monkeypatch, isolated_known_clones):
    """LLM_WIKI_PATH set but path doesn't exist → fail-closed."""
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path / "nonexistent"))

    with pytest.raises(CitationResolutionError) as exc:
        resolve_wiki_base()
    assert "llm_wiki_path_invalid" in exc.value.reason or "resolver_unconfigured" in exc.value.reason
    assert "LLM_WIKI_PATH" in str(exc.value)


def test_resolver_llm_wiki_path_env_stale_clone_detection(tmp_path, monkeypatch, isolated_known_clones):
    """LLM_WIKI_PATH path exists but is missing the expected wiki structure."""
    bare = tmp_path / "stale"
    bare.mkdir()
    monkeypatch.setenv("LLM_WIKI_PATH", str(bare))

    with pytest.raises(CitationResolutionError) as exc:
        resolve_wiki_base()
    reason = exc.value.reason
    assert "stale_clone" in reason or "llm_wiki_path_invalid" in reason or "resolver_unconfigured" in reason


# ---------------------------------------------------------------------------
# 3. DIGITALMODEL_REPO_ROOT legacy alias with DeprecationWarning
# ---------------------------------------------------------------------------


def test_resolver_digitalmodel_repo_root_legacy_alias(tmp_path, monkeypatch):
    """Legacy env var still works and emits a DeprecationWarning."""
    overlay = _make_workspace_hub_overlay(tmp_path / "wsh")
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(overlay))

    with pytest.warns(DeprecationWarning, match="DIGITALMODEL_REPO_ROOT"):
        resolved = resolve_wiki_base()
    assert resolved == overlay


def test_resolver_llm_wiki_path_takes_precedence_over_legacy(tmp_path, monkeypatch):
    """When both env vars are set, LLM_WIKI_PATH wins."""
    primary = _make_standalone_clone(tmp_path / "primary")
    legacy = _make_workspace_hub_overlay(tmp_path / "legacy")
    monkeypatch.setenv("LLM_WIKI_PATH", str(primary))
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(legacy))

    resolved = resolve_wiki_base()
    assert resolved == primary


# ---------------------------------------------------------------------------
# 4. Parent-walk for workspace-hub overlay (with sentinel)
# ---------------------------------------------------------------------------


def test_resolver_parent_walk_finds_overlay(tmp_path, monkeypatch):
    """When invoked from within a workspace-hub checkout, parent walk locates root."""
    overlay = _make_workspace_hub_overlay(tmp_path / "wsh")
    deep = overlay / "src" / "pkg" / "module"
    deep.mkdir(parents=True)
    fake_file = deep / "caller.py"
    fake_file.write_text("# fake caller")

    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    resolved = resolve_wiki_base()
    assert resolved == overlay


def test_resolver_parent_walk_has_sentinel(tmp_path, monkeypatch, isolated_known_clones):
    """Walk halts at hard cap; does NOT infinite-loop at /."""
    import time

    deep = tmp_path
    for i in range(20):
        deep = deep / f"d{i}"
    deep.mkdir(parents=True)
    fake_file = deep / "fake.py"
    fake_file.write_text("# fake")

    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    start = time.monotonic()
    with pytest.raises(CitationResolutionError):
        resolve_wiki_base()
    elapsed = time.monotonic() - start
    assert elapsed < 0.5, f"walk took {elapsed:.3f}s — expected bounded"


# ---------------------------------------------------------------------------
# 5. Known local-clone fallback
# ---------------------------------------------------------------------------


def test_resolver_known_local_clone_fallback(tmp_path, monkeypatch):
    """Falls back to a known local-clone path when env vars unset and walk fails."""
    standalone = _make_standalone_clone(tmp_path / "fake-local-clone")

    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "_KNOWN_LOCAL_CLONES", (standalone,))
    # disable parent walk by pointing __file__ outside any clone
    fake_file = tmp_path / "outside" / "no_clone_here.py"
    fake_file.parent.mkdir(parents=True)
    fake_file.write_text("# fake")
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    resolved = resolve_wiki_base()
    assert resolved == standalone


# ---------------------------------------------------------------------------
# 6. Fail-closed with actionable message
# ---------------------------------------------------------------------------


def test_resolver_fail_closed_no_env_no_clone(tmp_path, monkeypatch, isolated_known_clones):
    """All resolution paths exhausted → fail-closed with actionable message."""
    fake_file = tmp_path / "outside" / "no_clone.py"
    fake_file.parent.mkdir(parents=True)
    fake_file.write_text("# fake")
    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    with pytest.raises(CitationResolutionError) as exc:
        resolve_wiki_base()
    assert "resolver_unconfigured" in exc.value.reason


def test_resolver_fail_closed_message_is_actionable(tmp_path, monkeypatch, isolated_known_clones):
    """Fail-closed error message must name LLM_WIKI_PATH, the repo URL, the routing rule, and `export`."""
    fake_file = tmp_path / "outside" / "no_clone.py"
    fake_file.parent.mkdir(parents=True)
    fake_file.write_text("# fake")
    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    with pytest.raises(CitationResolutionError) as exc:
        resolve_wiki_base()
    msg = str(exc.value)
    assert "LLM_WIKI_PATH" in msg
    assert "vamseeachanta/llm-wiki" in msg or WIKI_REPO_URL in msg
    assert "codes-standards-data-routing" in msg or ROUTING_RULE_URL in msg
    assert "export LLM_WIKI_PATH=" in msg


# ---------------------------------------------------------------------------
# 7. Logging behavior
# ---------------------------------------------------------------------------


def test_resolver_info_log_on_success(tmp_path, monkeypatch, caplog):
    """Successful resolution emits an INFO log naming the resolved base."""
    standalone = _make_standalone_clone(tmp_path / "ok")
    monkeypatch.setenv("LLM_WIKI_PATH", str(standalone))

    with caplog.at_level(logging.INFO, logger="digitalmodel.citations.resolver"):
        resolve_wiki_base()
    info_records = [r for r in caplog.records if r.levelno == logging.INFO]
    assert any(str(standalone) in r.getMessage() for r in info_records)


def test_resolver_error_log_on_failure(tmp_path, monkeypatch, caplog, isolated_known_clones):
    """Failed resolution emits an ERROR log."""
    fake_file = tmp_path / "outside" / "no_clone.py"
    fake_file.parent.mkdir(parents=True)
    fake_file.write_text("# fake")
    from digitalmodel.citations import resolver
    monkeypatch.setattr(resolver, "__file__", str(fake_file))

    with caplog.at_level(logging.ERROR, logger="digitalmodel.citations.resolver"):
        with pytest.raises(CitationResolutionError):
            resolve_wiki_base()
    error_records = [r for r in caplog.records if r.levelno == logging.ERROR]
    assert len(error_records) >= 1


# ---------------------------------------------------------------------------
# 8. Layout join semantics
# ---------------------------------------------------------------------------


def test_resolver_layout_join_workspace_hub(tmp_path, monkeypatch):
    """resolve_wiki_path() finds the file under knowledge/wikis/ when base is overlay."""
    overlay = _make_workspace_hub_overlay(tmp_path / "wsh")
    monkeypatch.setenv("LLM_WIKI_PATH", str(overlay))

    resolved = resolve_wiki_path("wikis/engineering/wiki/standards/dnv-os-e301.md")
    assert resolved.is_file()
    assert resolved == overlay / "knowledge" / "wikis" / "engineering" / "wiki" / "standards" / "dnv-os-e301.md"


def test_resolver_layout_join_standalone(tmp_path, monkeypatch):
    """resolve_wiki_path() finds the file under wikis/ when base is standalone."""
    standalone = _make_standalone_clone(tmp_path / "sa")
    monkeypatch.setenv("LLM_WIKI_PATH", str(standalone))

    resolved = resolve_wiki_path("wikis/engineering/wiki/standards/dnv-os-e301.md")
    assert resolved.is_file()
    assert resolved == standalone / "wikis" / "engineering" / "wiki" / "standards" / "dnv-os-e301.md"


# ---------------------------------------------------------------------------
# 9. One-shot warning cache (multiple calls don't multi-warn)
# ---------------------------------------------------------------------------


def test_resolver_one_shot_warning_cached(tmp_path, monkeypatch, isolated_known_clones, recwarn):
    """Multiple deprecation-warn paths emit at most one DeprecationWarning."""
    overlay = _make_workspace_hub_overlay(tmp_path / "wsh")
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(overlay))

    resolve_wiki_base()
    resolve_wiki_base()
    resolve_wiki_base()

    deprecation_warnings = [w for w in recwarn.list if issubclass(w.category, DeprecationWarning)]
    assert len(deprecation_warnings) == 1, (
        f"expected exactly 1 DeprecationWarning, got {len(deprecation_warnings)}"
    )


# ---------------------------------------------------------------------------
# 10. _validate_clone helper
# ---------------------------------------------------------------------------


def test_validate_clone_accepts_standalone(tmp_path):
    """_validate_clone() returns base for a valid standalone clone."""
    standalone = _make_standalone_clone(tmp_path / "ok")
    assert _validate_clone(standalone) == standalone


def test_validate_clone_accepts_overlay(tmp_path):
    """_validate_clone() returns base for a valid workspace-hub overlay."""
    overlay = _make_workspace_hub_overlay(tmp_path / "ok")
    assert _validate_clone(overlay) == overlay


def test_validate_clone_rejects_empty_dir(tmp_path):
    """_validate_clone() returns None for an empty directory (no wiki structure)."""
    empty = tmp_path / "empty"
    empty.mkdir()
    assert _validate_clone(empty) is None


def test_validate_clone_rejects_nonexistent(tmp_path):
    """_validate_clone() returns None for a path that doesn't exist."""
    assert _validate_clone(tmp_path / "ghost") is None
