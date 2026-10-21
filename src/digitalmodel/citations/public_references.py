"""Dated public report references, separate from calculation constants."""
from __future__ import annotations

from typing import Final

from digitalmodel.citations.schema import Citation


_PUBLIC_REFERENCES: Final[dict[str, dict[str, str]]] = {
    "NASA-DRAG-EQUATION": {
        "publisher": "NASA",
        "revision": "Public web edition, no numbered edition stated",
        "section": "Drag Equation: drag and dynamic pressure",
        "source_url": "https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/",
        "retrieved_on": "2026-10-20",
        "note": "Dynamic-pressure relation only; no coefficient values adopted.",
    },
    "ORCINA-LINKS-DATA": {
        "publisher": "Orcina",
        "revision": "Public web edition, no numbered edition stated",
        "section": "Links: Data; Tethers and Spring/dampers",
        "source_url": "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Data.htm",
        "retrieved_on": "2026-10-20",
        "note": (
            "Tethers are tension-only; spring/dampers can carry compression. "
            "No stiffness or capacity values adopted."
        ),
    },
    "PASSING-SHIP-METHOD": {
        "publisher": "digitalmodel",
        "revision": "01fc9c59728739b7c961a0a7fc77bb62846ef054",
        "section": "Passing-ship force method note: closed-form kernels and finite-depth images",
        "repository_path": "docs/domains/hydrodynamics/wang-passing-ship-forces.md",
        "retrieved_on": "2026-10-20",
        "note": (
            "Repository note accessed on the stated date; original papers were not newly retrieved "
            "or verified in this change. Reference metadata does not establish hydrodynamic qualification."
        ),
    },
}


def get_public_reference(source_id: str) -> Citation:
    """Return registered report metadata, never a validated calculation source.

    Retrieval/access dates describe the inspected web pages or repository note;
    no network request or wiki resolution is performed by this metadata API.
    Calculation registry getters retain their independent fail-closed wiki validation.
    """
    if not isinstance(source_id, str) or source_id not in _PUBLIC_REFERENCES:
        raise ValueError(f"Unknown public reference: {source_id!r}")
    return Citation(code_id=source_id, **_PUBLIC_REFERENCES[source_id])
