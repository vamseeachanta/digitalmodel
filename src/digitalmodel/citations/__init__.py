"""Citation schema and registry for standards-derived calc constants.

Contract: docs/standards/calc-output-citation.md (workspace-hub)
Governing issue: #2481
"""
from __future__ import annotations

from digitalmodel.citations.schema import (
    Citation,
    CitedValue,
    CitationResolutionError,
    validate_citation,
)
from digitalmodel.citations.resolver import (
    WIKI_REPO_URL,
    ROUTING_RULE_URL,
    resolve_wiki_base,
    resolve_wiki_path,
)

__all__ = [
    "Citation",
    "CitedValue",
    "CitationResolutionError",
    "validate_citation",
    "resolve_wiki_base",
    "resolve_wiki_path",
    "WIKI_REPO_URL",
    "ROUTING_RULE_URL",
]
