"""Citation schema + fail-closed validator.

Per #2481 decisions:
- D2: fail-closed at calc time; CitationResolutionError carries code_id
- D3: direct file read for v1; migrate to MCP #2400 later without schema change

Contract: docs/standards/calc-output-citation.md (workspace-hub).
"""
from __future__ import annotations

import re
import unicodedata
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Any, Mapping, Optional
from urllib.parse import urlsplit

_WIKI_PATH_PREFIX = "wikis/"
_FORBIDDEN_SEGMENTS = ("..", "")


class CitationResolutionError(RuntimeError):
    """Raised when a citation cannot be resolved against the live wiki.

    Message always carries code_id so operators can retarget a cited constant
    rather than disabling the citation when a wiki page is moved or archived.
    """

    def __init__(self, *, code_id: str, wiki_path: str, reason: str) -> None:
        self.code_id = code_id
        self.wiki_path = wiki_path
        self.reason = reason
        super().__init__(
            f"CitationResolutionError: code_id={code_id!r} wiki_path={wiki_path!r} reason={reason}"
        )


class CitationValidationError(ValueError):
    """Raised at schema-construction time for structurally invalid citations."""


@dataclass(frozen=True)
class Citation:
    code_id: str
    publisher: str
    revision: str
    section: str
    wiki_path: str = ""
    note: str = ""
    # Wiki sibling routing fields (workspace-hub#2778, calc-citation-contract.md Rule 8).
    # source_sibling: required — "generic" (sourced from vamseeachanta/llm-wiki) or a
    #                 client slug (e.g., "acma" when sourced from llm-wiki-acma).
    #                 Default "generic" preserves backcompat for existing call sites
    #                 during the rollout; new code should set explicitly.
    # source_project: optional — populated when the citation is project-scoped
    #                 (e.g., "example-project" for content under llm-wiki-acma/projects/example-project/);
    #                 None for client-level or generic citations.
    source_sibling: str = "generic"
    source_project: Optional[str] = None
    # Report references are metadata only; wiki routing applies only to wiki_path.
    source_url: str = ""
    repository_path: str = ""
    # Calendar validity only: historical metadata is checked without reading a clock.
    retrieved_on: str = ""

    def __post_init__(self) -> None:
        for f in ("code_id", "publisher", "revision", "section"):
            v = getattr(self, f)
            if not isinstance(v, str) or not v.strip():
                raise CitationValidationError(f"Citation.{f} must be a non-empty string")
        # source_sibling: must be non-empty string (live registry validation is
        # the resolver's job at calc time, per workspace-hub#2778 Layer 4).
        if not isinstance(self.source_sibling, str) or not self.source_sibling.strip():
            raise CitationValidationError(
                "Citation.source_sibling must be a non-empty string "
                "('generic' or a client slug per .claude/rules/wiki-sibling-routing.md)"
            )
        # source_project: optional, but when set must be non-empty string.
        if self.source_project is not None:
            if not isinstance(self.source_project, str) or not self.source_project.strip():
                raise CitationValidationError(
                    "Citation.source_project, when set, must be a non-empty string"
                )
        locators = (self.wiki_path, self.source_url, self.repository_path)
        if any(not isinstance(value, str) for value in locators):
            raise CitationValidationError("Citation locators must be strings")
        if sum(bool(value) for value in locators) != 1:
            raise CitationValidationError("Citation requires exactly one locator")
        if self.wiki_path:
            _validate_wiki_path(self.wiki_path)
        else:
            if self.source_sibling != "generic" or self.source_project is not None:
                raise CitationValidationError("Public references cannot use wiki routing")
            if self.source_url:
                _validate_public_url(self.source_url)
            else:
                _validate_repository_path(self.repository_path)
        if not isinstance(self.retrieved_on, str):
            raise CitationValidationError("retrieved_on must be a YYYY-MM-DD calendar date")
        if not self.wiki_path or self.retrieved_on:
            _validate_reference_date(self.retrieved_on)


@dataclass(frozen=True)
class CitedValue:
    value: float
    citation: Citation
    units: str = ""

    def __post_init__(self) -> None:
        # Missing/malformed payloads remain the downstream validator's concern.
        if isinstance(self.citation, Citation) and not self.citation.wiki_path:
            raise CitationValidationError(
                "CitedValue cannot use a reference-only citation"
            )


def _validate_reference_date(value: str) -> None:
    if not isinstance(value, str) or not re.fullmatch(r"[0-9]{4}-[0-9]{2}-[0-9]{2}", value):
        raise CitationValidationError("retrieved_on must be a YYYY-MM-DD calendar date")
    try:
        date.fromisoformat(value)
    except ValueError as exc:
        raise CitationValidationError("retrieved_on must be a real calendar date") from exc


def _validate_public_url(value: str) -> None:
    message = "source_url must be an HTTPS URL without credentials or unsafe characters"
    # Local report policy caps URLs at 2048 characters; this is not an RFC limit.
    if (
        not value.startswith("https://") or len(value) > 2048
        or any(
            c.isspace() or unicodedata.category(c) in {"Cc", "Cf"}
            or c in "\\<>\"'`" for c in value
        )
    ):
        raise CitationValidationError(message)
    try:
        parts = urlsplit(value)
        host = parts.hostname
        port = parts.port
        if (
            not host or not host.isascii() or parts.username is not None
            or parts.password is not None or port == 0
        ):
            raise ValueError(message)
        # RFC 1035 section 2.3.4: 63-octet labels and 255-octet wire names.
        # https://www.rfc-editor.org/rfc/rfc1035#section-2.3.4
        # ASCII presentation omits wire length/root octets: at most 253 characters.
        # The same local cap applies when an optional trailing dot is supplied.
        label = r"[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?"
        if len(host) > 253 or not re.fullmatch(rf"{label}(?:\.{label})*\.?", host):
            raise ValueError(message)
    except ValueError as exc:
        raise CitationValidationError(message) from exc


def _validate_repository_path(value: str) -> None:
    if (
        not value.startswith("docs/") or not re.fullmatch(r"[A-Za-z0-9._/-]+", value)
        or any(segment in ("", ".", "..") for segment in value.split("/"))
    ):
        raise CitationValidationError("repository_path must be a relative docs/ file locator")


def _validate_wiki_path(wiki_path: str) -> None:
    if not wiki_path.startswith(_WIKI_PATH_PREFIX):
        raise CitationValidationError(
            f"wiki_path must be under {_WIKI_PATH_PREFIX!r}: got {wiki_path!r}"
        )
    if any(seg in _FORBIDDEN_SEGMENTS for seg in wiki_path.split("/")):
        raise CitationValidationError(
            f"wiki_path may not contain empty or '..' segments: got {wiki_path!r}"
        )
    if "\\" in wiki_path:
        raise CitationValidationError(
            f"wiki_path must use forward slashes: got {wiki_path!r}"
        )


_FRONTMATTER_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)


def _read_frontmatter(path: Path) -> Mapping[str, Any]:
    text = path.read_text(encoding="utf-8")
    m = _FRONTMATTER_RE.match(text)
    if not m:
        return {}
    # Minimal YAML-frontmatter parser covering the fields this validator needs.
    # Avoids adding a yaml dependency for the pilot; the three fields are flat scalars.
    fm: dict[str, str] = {}
    for line in m.group(1).splitlines():
        if ":" not in line or line.lstrip().startswith("#"):
            continue
        key, _, rest = line.partition(":")
        key = key.strip()
        value = rest.strip()
        if value.startswith('"') and value.endswith('"'):
            value = value[1:-1]
        fm[key] = value
    return fm


def _join_with_layout_detection(base: Path, wiki_path: str) -> Path:
    """Join base + wiki_path handling both standalone and workspace-hub layouts.

    Canonical wiki_path form is `wikis/<domain>/...` (no knowledge/ prefix).
    Tries the standalone layout first, then the workspace-hub overlay layout.
    Returns the standalone-form path (for error reporting) when neither hits.
    """
    standalone = base / wiki_path
    if standalone.is_file():
        return standalone
    overlay = base / "knowledge" / wiki_path
    if overlay.is_file():
        return overlay
    return standalone


def validate_citation(citation: Citation, *, repo_root: Optional[Path] = None) -> None:
    """Fail-closed resolution check.

    Public URL/repository references are report metadata only and raise
    ``CitationResolutionError(reason="reference_only")`` before any file access.

    Raises CitationResolutionError if:
    - the cited wiki page does not exist
    - the page frontmatter lacks code_id / publisher / revision
    - any of the three fields disagree with the citation

    When repo_root is None, defers to the resolver (LLM_WIKI_PATH precedence chain).
    When repo_root is provided, joins directly but tries both standalone and
    workspace-hub-overlay layouts to find the file.

    Does NOT validate the `section` locator — that's a human-readable field.
    """
    if not citation.wiki_path:
        raise CitationResolutionError(
            code_id=citation.code_id, wiki_path="", reason="reference_only"
        )
    if repo_root is None:
        # Defer to resolver for path resolution
        from digitalmodel.citations.resolver import resolve_wiki_path
        path = resolve_wiki_path(citation.wiki_path)
    else:
        path = _join_with_layout_detection(Path(repo_root), citation.wiki_path)
    if not path.is_file():
        raise CitationResolutionError(
            code_id=citation.code_id, wiki_path=citation.wiki_path, reason="page_missing"
        )
    fm = _read_frontmatter(path)
    for field_name in ("code_id", "publisher", "revision"):
        expected = getattr(citation, field_name)
        actual = fm.get(field_name)
        if actual is None:
            raise CitationResolutionError(
                code_id=citation.code_id,
                wiki_path=citation.wiki_path,
                reason=f"frontmatter_missing:{field_name}",
            )
        if actual != expected:
            raise CitationResolutionError(
                code_id=citation.code_id,
                wiki_path=citation.wiki_path,
                reason=f"frontmatter_mismatch:{field_name}:{actual!r}!={expected!r}",
            )
