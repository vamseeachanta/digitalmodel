"""Wiki base-path resolver for the citation contract (#617).

Implements the 6-level precedence chain:
  1. explicit override kwarg
  2. LLM_WIKI_PATH env var
  3. DIGITALMODEL_REPO_ROOT env var (legacy alias; DeprecationWarning)
  4. bounded parent walk from __file__ (cap=8) for workspace-hub overlay
  5. known local-clone fallbacks (~/workspace-hub/llm-wiki, /mnt/local-analysis/llm-wiki)
  6. fail-closed with actionable message

Handles both layouts:
  - standalone llm-wiki clone:  <base>/wikis/<domain>/...
  - workspace-hub overlay:      <base>/knowledge/wikis/<domain>/...

The resolver detects which layout a given base uses and joins accordingly.
Canonical citation `wiki_path` form is `wikis/<domain>/...` (no `knowledge/`
prefix) post-2026-05-20 privacy flip per the codes-standards-data-routing rule.

Per `feedback_path_parent_infinite_loop`: parent walk is hard-capped at 8 levels.
"""
from __future__ import annotations

import logging
import os
import warnings
from pathlib import Path
from typing import Optional

from digitalmodel.citations.schema import CitationResolutionError


WIKI_REPO_URL = "https://github.com/vamseeachanta/llm-wiki"
ROUTING_RULE_URL = (
    "https://github.com/vamseeachanta/workspace-hub/blob/main/"
    ".claude/rules/codes-standards-data-routing.md"
)

_WALK_HARD_CAP = 8

# Default cross-platform local-clone search paths. Cross-platform via Path.home().
_KNOWN_LOCAL_CLONES: tuple[Path, ...] = (
    Path("/mnt/local-analysis/llm-wiki"),
    Path.home() / "workspace-hub" / "llm-wiki",
)

# One-shot cache so repeated calls don't multi-warn for the same condition.
_RESOLUTION_CACHE: dict[str, bool] = {}

_LOGGER = logging.getLogger("digitalmodel.citations.resolver")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def resolve_wiki_base(*, override: Optional[Path] = None) -> Path:
    """Resolve the wiki base directory using the 6-level precedence chain.

    Returns:
        Path to a directory that contains either `wikis/` or `knowledge/wikis/`
        as a subdirectory.

    Raises:
        CitationResolutionError with reason `resolver_unconfigured:` and an
        actionable message naming LLM_WIKI_PATH, the private repo URL, and
        the routing-rule pointer.
    """
    # 1. Explicit override
    if override is not None:
        validated = _validate_clone(Path(override))
        if validated is not None:
            _LOGGER.info("citation-resolver: resolved via explicit override → %s", validated)
            return validated
        raise CitationResolutionError(
            code_id="<resolver>",
            wiki_path="<base>",
            reason=f"override_invalid:{override!s} does not contain wikis/ or knowledge/wikis/",
        )

    # 2. LLM_WIKI_PATH env var
    llm_wiki_env = os.environ.get("LLM_WIKI_PATH")
    if llm_wiki_env:
        env_path = Path(llm_wiki_env)
        if not env_path.exists():
            _emit_error_log("LLM_WIKI_PATH points to nonexistent path: %s", env_path)
            raise CitationResolutionError(
                code_id="<resolver>",
                wiki_path="<base>",
                reason=(
                    f"llm_wiki_path_invalid: LLM_WIKI_PATH={env_path!s} does not exist"
                ),
            )
        validated = _validate_clone(env_path)
        if validated is not None:
            _LOGGER.info("citation-resolver: resolved via LLM_WIKI_PATH → %s", validated)
            return validated
        _emit_error_log("LLM_WIKI_PATH set but path is not a valid wiki clone: %s", env_path)
        raise CitationResolutionError(
            code_id="<resolver>",
            wiki_path="<base>",
            reason=(
                f"llm_wiki_path_stale_clone: LLM_WIKI_PATH={env_path!s} exists "
                "but does not contain wikis/ or knowledge/wikis/ subdirectory"
            ),
        )

    # 3. DIGITALMODEL_REPO_ROOT legacy alias
    legacy_env = os.environ.get("DIGITALMODEL_REPO_ROOT")
    if legacy_env:
        legacy_path = Path(legacy_env)
        validated = _validate_clone(legacy_path)
        if validated is not None:
            if "deprecation_warned" not in _RESOLUTION_CACHE:
                warnings.warn(
                    "DIGITALMODEL_REPO_ROOT is deprecated; use LLM_WIKI_PATH instead. "
                    "See codes-standards-data-routing rule §4.",
                    DeprecationWarning,
                    stacklevel=3,
                )
                _RESOLUTION_CACHE["deprecation_warned"] = True
            _LOGGER.info(
                "citation-resolver: resolved via DIGITALMODEL_REPO_ROOT (legacy) → %s",
                validated,
            )
            return validated
        # legacy env var set but invalid — fall through to remaining steps rather than raise
        _LOGGER.warning(
            "DIGITALMODEL_REPO_ROOT set but path %s is not a valid wiki clone; "
            "falling through to remaining precedence steps",
            legacy_path,
        )

    # 4. Bounded parent walk from __file__
    here = Path(__file__).resolve()
    for parent in [here, *here.parents][:_WALK_HARD_CAP]:
        validated = _validate_clone(parent)
        if validated is not None:
            _LOGGER.info("citation-resolver: resolved via parent-walk → %s", validated)
            return validated

    # 5. Known local-clone fallbacks
    for candidate in _KNOWN_LOCAL_CLONES:
        validated = _validate_clone(candidate)
        if validated is not None:
            _LOGGER.info(
                "citation-resolver: resolved via known-local-clone fallback → %s",
                validated,
            )
            return validated

    # 6. Fail-closed
    _emit_error_log("citation-resolver: all 5 resolution paths exhausted; fail-closed")
    raise CitationResolutionError(
        code_id="<resolver>",
        wiki_path="<base>",
        reason=(
            "resolver_unconfigured: cannot find an llm-wiki clone.\n\n"
            "The vamseeachanta/llm-wiki repo is PRIVATE as of 2026-05-20 "
            "(codes-standards-data-routing rule §4).\n\n"
            "To fix: clone the private repo (requires GitHub auth) and set:\n"
            "    git clone " + WIKI_REPO_URL + ".git\n"
            "    export LLM_WIKI_PATH=$(pwd)/llm-wiki\n\n"
            "Or point the env var at an existing local clone:\n"
            "    export LLM_WIKI_PATH=/path/to/your/llm-wiki\n\n"
            "See: " + ROUTING_RULE_URL + " §4"
        ),
    )


def resolve_wiki_path(citation_wiki_path: str, *, override: Optional[Path] = None) -> Path:
    """Resolve a citation `wiki_path` (canonical form: `wikis/<domain>/...`) to
    an absolute file path on disk, handling both layouts.

    Canonical form is `wikis/<domain>/...` (no `knowledge/` prefix). When the
    resolved base uses the workspace-hub overlay layout, this helper prepends
    `knowledge/` so the file is actually found at `<base>/knowledge/wikis/...`.
    """
    base = resolve_wiki_base(override=override)
    # Try standalone layout first
    standalone_join = base / citation_wiki_path
    if standalone_join.is_file():
        return standalone_join
    # Try workspace-hub overlay layout
    overlay_join = base / "knowledge" / citation_wiki_path
    if overlay_join.is_file():
        return overlay_join
    # Neither found — caller (validate_citation) handles page_missing
    return standalone_join  # return the canonical-form path so callers can report it


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _validate_clone(base: Path) -> Optional[Path]:
    """Return base if it looks like a real wiki clone; else None.

    A valid clone contains either `<base>/wikis/` (standalone llm-wiki layout)
    or `<base>/knowledge/wikis/` (workspace-hub overlay layout).
    """
    if not base.is_dir():
        return None
    if (base / "wikis").is_dir():
        return base
    if (base / "knowledge" / "wikis").is_dir():
        return base
    return None


def _emit_error_log(fmt: str, *args) -> None:
    """ERROR-level log helper. Wrapper for symmetric INFO/ERROR coverage."""
    _LOGGER.error(fmt, *args)
