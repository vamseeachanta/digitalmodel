"""Citation schema + fail-closed validator.

Per #2481 decisions:
- D2: fail-closed at calc time; CitationResolutionError carries code_id
- D3: direct file read for v1; migrate to MCP #2400 later without schema change

Contract: docs/standards/calc-output-citation.md (workspace-hub).
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Mapping

_WIKI_PATH_PREFIX = "knowledge/wikis/"
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
    wiki_path: str
    note: str = ""

    def __post_init__(self) -> None:
        for f in ("code_id", "publisher", "revision", "section", "wiki_path"):
            v = getattr(self, f)
            if not isinstance(v, str) or not v.strip():
                raise CitationValidationError(f"Citation.{f} must be a non-empty string")
        _validate_wiki_path(self.wiki_path)


@dataclass(frozen=True)
class CitedValue:
    value: float
    citation: Citation
    units: str = ""


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


def validate_citation(citation: Citation, *, repo_root: Path) -> None:
    """Fail-closed resolution check.

    Raises CitationResolutionError if:
    - the cited wiki page does not exist
    - the page frontmatter lacks code_id / publisher / revision
    - any of the three fields disagree with the citation

    Does NOT validate the `section` locator — that's a human-readable field.
    """
    path = repo_root / citation.wiki_path
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
