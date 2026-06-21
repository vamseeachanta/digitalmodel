"""Wiki base-path resolver for the citation contract (#617).

Implements the 6-level precedence chain:
  1. explicit override kwarg
  2. LLM_WIKI_PATH env var
  3. DIGITALMODEL_REPO_ROOT env var (legacy alias; DeprecationWarning)
  4. bounded parent walk from __file__ (cap=8) for workspace-hub overlay
  5. known local-clone fallbacks (home workspace clone, local-analysis clone)
  6. fail-closed with actionable message

Handles both layouts:
  - standalone llm-wiki clone:  <base>/wikis/<domain>/...
  - workspace-hub overlay:      <base>/knowledge/wikis/<domain>/...

The resolver detects which layout a given base uses and joins accordingly.
Canonical citation `wiki_path` form is `wikis/<domain>/...` (no `knowledge/`
prefix) post-2026-05-20 privacy flip per the codes-standards-data-routing rule.

Per `feedback_path_parent_infinite_loop`: parent walk is hard-capped at 8 levels.

Trust boundary (#618):
  Env-var-supplied paths (LLM_WIKI_PATH, DIGITALMODEL_REPO_ROOT) and explicit
  overrides are treated as untrusted input. `_validate_clone` resolves them with
  `Path.resolve(strict=True)` so that symlinks and `..` segments are collapsed to a
  real on-disk path BEFORE the `wikis/` / `knowledge/wikis/` containment check. The
  containment subdirectory is itself resolved and required to live under the resolved
  base, so a `wikis/` symlink that escapes the base is rejected. This closes the
  TOCTOU/symlink-escape gap where a base passing validation could later read a
  citation file through a symlink pointing outside the intended tree.

Log hygiene (#618):
  Resolved/base paths in INFO/ERROR logs are redacted by default (home-directory
  prefix collapsed to `~`) so resolver logs shipped to centralized aggregators do
  not leak local filesystem layout. Set `DIGITALMODEL_QUIET_RESOLVER=1` to suppress
  resolver INFO/ERROR logging entirely (opt-out for noise-sensitive environments).
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
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
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
            _emit_info_log("citation-resolver: resolved via explicit override → %s", validated)
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
            _emit_info_log("citation-resolver: resolved via LLM_WIKI_PATH → %s", validated)
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
            _emit_info_log(
                "citation-resolver: resolved via DIGITALMODEL_REPO_ROOT (legacy) → %s",
                validated,
            )
            return validated
        # legacy env var set but invalid — raise hard (symmetric with LLM_WIKI_PATH).
        # If a user has gone out of their way to set this env var, an invalid value
        # is almost certainly a misconfiguration they want to know about, not silent
        # fall-through to whatever else happens to be lying around.
        _emit_error_log(
            "DIGITALMODEL_REPO_ROOT set but path is not a valid wiki clone: %s",
            legacy_path,
        )
        raise CitationResolutionError(
            code_id="<resolver>",
            wiki_path="<base>",
            reason=(
                f"digitalmodel_repo_root_invalid: DIGITALMODEL_REPO_ROOT={legacy_path!s} "
                "does not exist or does not contain wikis/ or knowledge/wikis/. "
                "Consider migrating to LLM_WIKI_PATH (the legacy alias is deprecated)."
            ),
        )

    # 4. Bounded parent walk from __file__
    here = Path(__file__).resolve()
    for parent in [here, *here.parents][:_WALK_HARD_CAP]:
        validated = _validate_clone(parent)
        if validated is not None:
            _emit_info_log("citation-resolver: resolved via parent-walk → %s", validated)
            return validated

    # 5. Known local-clone fallbacks
    for candidate in _KNOWN_LOCAL_CLONES:
        validated = _validate_clone(candidate)
        if validated is not None:
            _emit_info_log(
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


def _is_contained(child: Path, base: Path) -> bool:
    """True if `child` (already resolved) is `base` or lives under `base`."""
    try:
        child.relative_to(base)
        return True
    except ValueError:
        return False


def _validate_clone(base: Path) -> Optional[Path]:
    """Return the resolved base if it looks like a real wiki clone; else None.

    A valid clone contains either `<base>/wikis/` (standalone llm-wiki layout)
    or `<base>/knowledge/wikis/` (workspace-hub overlay layout).

    Hardening (#618): the base is resolved with `Path.resolve(strict=True)` so a
    symlinked or `..`-laden input is collapsed to its real on-disk path before the
    structure check. The matched `wikis/` subdirectory is itself resolved and must
    remain contained under the resolved base — a `wikis/` symlink whose target
    escapes the base is rejected (returns None) rather than silently followed.
    Returns the *resolved* path so downstream joins operate on the canonical tree.
    """
    try:
        resolved_base = base.resolve(strict=True)
    except (OSError, RuntimeError):
        # strict=True raises FileNotFoundError for missing paths; RuntimeError on
        # symlink loops. Either way the base is not a usable clone.
        return None
    if not resolved_base.is_dir():
        return None
    for sub in (resolved_base / "wikis", resolved_base / "knowledge" / "wikis"):
        try:
            resolved_sub = sub.resolve(strict=True)
        except (OSError, RuntimeError):
            continue
        if resolved_sub.is_dir() and _is_contained(resolved_sub, resolved_base):
            return resolved_base
    return None


def _quiet() -> bool:
    """True when resolver logging is opted out via DIGITALMODEL_QUIET_RESOLVER=1."""
    return os.environ.get("DIGITALMODEL_QUIET_RESOLVER") == "1"


def _redact(value: object) -> str:
    """Redact a path/value for logging: collapse the home-dir prefix to `~`.

    Keeps the trailing path segments (useful for diagnosis) while not shipping the
    user's absolute home-directory layout to centralized log aggregators (#618).
    """
    text = str(value)
    try:
        home = str(Path.home())
    except (RuntimeError, OSError):
        return text
    if home and text.startswith(home):
        return "~" + text[len(home):]
    return text


def _emit_info_log(fmt: str, *args) -> None:
    """INFO-level log helper with default path redaction + opt-out."""
    if _quiet():
        return
    _LOGGER.info(fmt, *(_redact(a) for a in args))


def _emit_error_log(fmt: str, *args) -> None:
    """ERROR-level log helper with default path redaction + opt-out."""
    if _quiet():
        return
    _LOGGER.error(fmt, *(_redact(a) for a in args))
