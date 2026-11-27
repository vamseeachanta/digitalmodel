"""Wiki base-path resolver for the citation contract (#617).

Resolution order: explicit override, LLM_WIKI_PATH, private configuration,
deprecated DIGITALMODEL_REPO_ROOT, a bounded parent walk, then the per-user
data directory. An invalid explicit override or selected environment path
raises CitationResolutionError.

The optional UTF-8 JSON file `$XDG_CONFIG_HOME/digitalmodel/paths.json`
(default `~/.config/digitalmodel/paths.json`) can supply `wiki_path` (absolute
checkout path) and `wiki_sibling` (one directory name beside this repository).
The absolute path is tried first. A missing or stale checkout allows the
configured sibling to be tried next. The source parent walk is capped at 8.

Handles both layouts:
  - standalone knowledge-base clone:  <base>/wikis/<domain>/...
  - workspace overlay:      <base>/knowledge/wikis/<domain>/...

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

import json
import logging
import os
import warnings
from pathlib import Path, PurePath, PureWindowsPath
from typing import Iterator, Optional

from digitalmodel.citations.schema import CitationResolutionError


WIKI_REPO_URL = os.environ.get("DIGITALMODEL_WIKI_URL") or None
ROUTING_RULE_URL = (
    "https://github.com/vamseeachanta/workspace-hub/blob/main/"
    ".claude/rules/codes-standards-data-routing.md"
)

RESOLVER_UNCONFIGURED_PREFIX = "resolver_unconfigured:"

_WALK_HARD_CAP = 8

# Default cross-platform local-clone search paths. Cross-platform via Path.home().
_KNOWN_LOCAL_CLONES: tuple[Path, ...] = (
    Path.home() / ".local" / "share" / "digitalmodel" / "knowledge",
)

# One-shot cache so repeated calls don't multi-warn for the same condition.
_RESOLUTION_CACHE: dict[str, bool] = {}

_LOGGER = logging.getLogger("digitalmodel.citations.resolver")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def resolve_wiki_base(*, override: Optional[Path] = None) -> Path:
    """Resolve the wiki base directory in the order documented by this module.

    Returns:
        Path to a directory that contains either `wikis/` or `knowledge/wikis/`
        as a subdirectory.

    Raises:
        CitationResolutionError with reason `resolver_unconfigured:` and
        setup instructions naming LLM_WIKI_PATH and DIGITALMODEL_WIKI_URL.
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
            reason=(
                f"override_invalid:{Path(override).as_posix()} "
                "does not contain wikis/ or knowledge/wikis/"
            ),
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
                    f"llm_wiki_path_invalid: LLM_WIKI_PATH={env_path.as_posix()} "
                    "does not exist"
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
                f"llm_wiki_path_stale_clone: LLM_WIKI_PATH={env_path.as_posix()} exists "
                "but does not contain wikis/ or knowledge/wikis/ subdirectory"
            ),
        )

    for candidate in _configured_clones():
        validated = _validate_clone(candidate)
        if validated is not None:
            return validated

    # Legacy environment alias for callers without checkout configuration
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
                "digitalmodel_repo_root_invalid: "
                f"DIGITALMODEL_REPO_ROOT={legacy_path.as_posix()} "
                "does not exist or does not contain wikis/ or knowledge/wikis/. "
                "Consider migrating to LLM_WIKI_PATH (the legacy alias is deprecated)."
            ),
        )

    # Bounded parent walk from __file__
    here = Path(__file__).resolve()
    for parent in [here, *here.parents][:_WALK_HARD_CAP]:
        validated = _validate_clone(parent)
        if validated is not None:
            _emit_info_log("citation-resolver: resolved via parent-walk → %s", validated)
            return validated

    # Per-user data directory
    for candidate in _KNOWN_LOCAL_CLONES:
        validated = _validate_clone(candidate)
        if validated is not None:
            _emit_info_log(
                "citation-resolver: resolved via known-local-clone fallback → %s",
                validated,
            )
            return validated

    # 6. Fail-closed
    _emit_error_log("citation-resolver: all resolution paths exhausted; fail-closed")
    raise CitationResolutionError(
        code_id="<resolver>",
        wiki_path="<base>",
        reason=(
            f"{RESOLVER_UNCONFIGURED_PREFIX} cannot find a knowledge-base checkout.\n\n"
            "Set DIGITALMODEL_WIKI_URL to the authorized Git remote, then run:\n"
            '    git clone "$DIGITALMODEL_WIKI_URL" knowledge-base\n'
            "    export LLM_WIKI_PATH=$(pwd)/knowledge-base\n\n"
            "For an existing checkout, set LLM_WIKI_PATH to its directory.\n\n"
            "See: " + ROUTING_RULE_URL + " §4"
        ),
    )


def resolve_wiki_path(citation_wiki_path: str, *, override: Optional[Path] = None) -> Path:
    """Resolve a citation `wiki_path` (canonical form: `wikis/<domain>/...`) to
    an absolute file path on disk, handling both layouts.

    Canonical form is `wikis/<domain>/...` (no `knowledge/` prefix). When the
    resolved base uses the workspace overlay layout, this helper prepends
    `knowledge/` so the file is actually found at `<base>/knowledge/wikis/...`.
    """
    base = resolve_wiki_base(override=override)
    # Try standalone layout first
    standalone_join = base / citation_wiki_path
    if standalone_join.is_file():
        return standalone_join
    # Try workspace overlay layout
    overlay_join = base / "knowledge" / citation_wiki_path
    if overlay_join.is_file():
        return overlay_join
    # Neither found — caller (validate_citation) handles page_missing
    return standalone_join  # return the canonical-form path so callers can report it


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def read_checkout_config() -> dict:
    """Read optional host-local paths without logging their values."""
    config_home = os.environ.get("XDG_CONFIG_HOME")
    directory = Path(config_home) if config_home else Path.home() / ".config"
    path = directory / "digitalmodel" / "paths.json"
    try:
        values = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        return {}
    except (OSError, ValueError) as error:
        raise _config_error("could not read paths.json as UTF-8 JSON") from error
    if not isinstance(values, dict):
        raise _config_error("expected a JSON object in paths.json")
    for key in ("wiki_path", "workspace_root"):
        if key in values:
            value = values[key]
            if not isinstance(value, str) or "\x00" in value or not Path(value).is_absolute():
                raise _config_error(f"{key} must be an absolute path")
    return values


def _config_error(message: str) -> CitationResolutionError:
    return CitationResolutionError(
        code_id="<resolver>", wiki_path="<base>", reason=f"wiki_config_invalid: {message}"
    )


def _configured_clones() -> Iterator[Path]:
    settings = read_checkout_config()
    if "wiki_path" in settings:
        yield Path(settings["wiki_path"])

    if "wiki_sibling" in settings:
        name = settings["wiki_sibling"]
        if (
            not isinstance(name, str) or name in ("", ".", "..") or "\x00" in name
            or PureWindowsPath(name).name != name or Path(name).name != name
        ):
            raise _config_error("wiki_sibling must be a single directory name")
        for parent in Path(__file__).resolve().parents[:_WALK_HARD_CAP]:
            if (parent / ".git").exists():
                yield parent.parent / name
                break


def find_wiki_content_root() -> Optional[Path]:
    """Return the directory containing wikis/, or None for standalone mode."""
    try:
        base = resolve_wiki_base()
    except CitationResolutionError as error:
        if error.reason.startswith(RESOLVER_UNCONFIGURED_PREFIX):
            return None
        raise
    return _content_root(base)


def _is_contained(child: Path, base: Path) -> bool:
    """True if `child` (already resolved) is `base` or lives under `base`."""
    try:
        child.relative_to(base)
        return True
    except ValueError:
        return False


def _validate_clone(base: Path) -> Optional[Path]:
    """Return the resolved base if it looks like a real wiki clone; else None.

    A valid clone contains either `<base>/wikis/` (standalone knowledge base layout)
    or `<base>/knowledge/wikis/` (workspace overlay layout).

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
    return resolved_base if _content_root(resolved_base) is not None else None


def _content_root(resolved_base: Path) -> Optional[Path]:
    for root in (resolved_base, resolved_base / "knowledge"):
        sub = root / "wikis"
        try:
            resolved_sub = sub.resolve(strict=True)
        except (OSError, RuntimeError):
            continue
        if resolved_sub.is_dir() and _is_contained(resolved_sub, resolved_base):
            return root
    return None


def _quiet() -> bool:
    """True when resolver logging is opted out via DIGITALMODEL_QUIET_RESOLVER=1."""
    return os.environ.get("DIGITALMODEL_QUIET_RESOLVER") == "1"


def _redact(value: object) -> str:
    """Redact a path/value for logging: collapse the home-dir prefix to `~`.

    Keeps the trailing path segments (useful for diagnosis) while not shipping the
    user's absolute home-directory layout to centralized log aggregators (#618).
    """
    text = value.as_posix() if isinstance(value, PurePath) else str(value)
    try:
        home = Path.home().as_posix()
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
