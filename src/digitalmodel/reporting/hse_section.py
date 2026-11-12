#!/usr/bin/env python3
"""Standard "HSE and safety" report appendix.

ABOUTME: Every engineering report carries an HSE and safety appendix: the public
incident history of the equipment it analyses, the recorded failure modes and
causes, and the lessons for the design. This module adds that appendix to a
report skeleton as six required slots and fills them from the files written by
the worldenergydata equipment failure table
(``python -m worldenergydata.safety_analysis.equipment section``).

The appendix is generated, not written by hand. This module adds no words of its
own to the incident text: it checks the generated section and places it. A
cause appears only where the source data codes it or the cited investigation
states it; that rule is enforced where the section is generated, and
:func:`check_sidecar` refuses a section that still has open items or a number
without a dated source.
"""

from __future__ import annotations

import html
import json
import re
from pathlib import Path
from typing import Dict, List, Optional

from digitalmodel.reporting.skeleton import BlockSpec, ReportSkeleton, SectionSpec

HSE_SECTION_KEY = "hse_safety"

# Slot key -> (subsection number, heading). The headings are fixed.
HSE_BLOCKS: Dict[str, tuple] = {
    "hse_data_used": (1, "Data used"),
    "hse_incident_history": (2, "Incident history"),
    "hse_failure_modes": (3, "Failure modes and stated causes"),
    "hse_public_cases": (4, "Illustrative public cases"),
    "hse_lessons": (5, "Lessons for this design"),
    "hse_references": (6, "References for this appendix"),
}


def hse_section_spec(letter: str = "F") -> SectionSpec:
    """The appendix as a skeleton section; every slot is required."""
    return SectionSpec(
        key=HSE_SECTION_KEY,
        label=f"Appendix {letter} HSE and safety",
        blocks=[
            BlockSpec(key=key, label=f"{letter}.{n} {heading}", required=True)
            for key, (n, heading) in HSE_BLOCKS.items()
        ],
    )


def with_hse_section(skeleton: ReportSkeleton, letter: str = "F") -> ReportSkeleton:
    """Return ``skeleton`` with the HSE appendix as its last section."""
    if any(s.key == HSE_SECTION_KEY for s in skeleton.sections):
        return skeleton
    return skeleton.model_copy(
        update={"sections": [*skeleton.sections, hse_section_spec(letter)]}
    )


def check_sidecar(sidecar: dict, *, allow_open: bool = False) -> List[str]:
    """Problems that stop the appendix going into a report (empty when ready)."""
    problems: List[str] = []
    if not sidecar.get("equipment_class"):
        problems.append("no equipment class")
    sources = sidecar.get("sources") or []
    if not sources:
        problems.append("no data source listed")
    for src in sources:
        name = src.get("source_id", "?")
        if not str(src.get("url", "")).startswith("https://"):
            problems.append(f"source {name}: no public link")
        if not re.fullmatch(r"\d{4}-\d{2}-\d{2}", str(src.get("retrieved", ""))):
            problems.append(f"source {name}: no date read")
    if not sidecar.get("references"):
        problems.append("no references")
    for lesson in sidecar.get("lessons") or []:
        if not str(lesson.get("addressed_in", "")).strip():
            problems.append(f"lesson {lesson.get('key', '?')}: no design response")
    if not allow_open:
        problems.extend(f"open item: {x}" for x in sidecar.get("open_items") or [])
    return problems


_BOLD = re.compile(r"\*\*(.+?)\*\*")
_LINK = re.compile(r"(https://[^\s<)]+)")


def _inline(text: str) -> str:
    out = html.escape(text, quote=False)
    out = _BOLD.sub(r"<strong>\1</strong>", out)
    return _LINK.sub(r'<a href="\1">\1</a>', out)


def _cells(line: str) -> List[str]:
    return [c.strip() for c in line.strip().strip("|").split("|")]


def markdown_to_html(text: str) -> str:
    """Convert the small Markdown subset the generator writes.

    Paragraphs, ``-`` bullets, pipe tables and ``**bold**``. All text is
    escaped; nothing is passed through as raw HTML.
    """
    out: List[str] = []
    lines = text.strip().split("\n")
    i = 0
    while i < len(lines):
        line = lines[i].rstrip()
        if not line:
            i += 1
        elif line.startswith("|"):
            rows = []
            while i < len(lines) and lines[i].startswith("|"):
                rows.append(lines[i])
                i += 1
            head, body = rows[0], [r for r in rows[1:] if not set(r) <= set("|-: ")]
            out.append("<table><thead><tr>")
            out += [f"<th>{_inline(c)}</th>" for c in _cells(head)]
            out.append("</tr></thead><tbody>")
            for row in body:
                out.append(
                    "<tr>"
                    + "".join(f"<td>{_inline(c)}</td>" for c in _cells(row))
                    + "</tr>"
                )
            out.append("</tbody></table>")
        elif line.startswith("- "):
            out.append("<ul>")
            while i < len(lines) and lines[i].startswith("- "):
                out.append(f"<li>{_inline(lines[i][2:].strip())}</li>")
                i += 1
            out.append("</ul>")
        else:
            out.append(f"<p>{_inline(line)}</p>")
            i += 1
    return "".join(out)


_HEADING = re.compile(r"^### ([A-Z])\.(\d) (.+)$", re.MULTILINE)


def hse_content(markdown: str) -> Dict[str, str]:
    """Split the generated appendix into the six slots, as HTML.

    Raises ``ValueError`` if a subsection is missing or its heading differs
    from the fixed one.
    """
    marks = list(_HEADING.finditer(markdown))
    found = {
        int(m.group(2)): (m.group(3).strip(), m.end(), i, m.group(1))
        for i, m in enumerate(marks)
    }
    content: Dict[str, str] = {}
    for key, (n, heading) in HSE_BLOCKS.items():
        if n not in found:
            raise ValueError(f"HSE appendix: subsection {n} ({heading}) is missing")
        got, start, idx, letter = found[n]
        if got != heading:
            raise ValueError(
                f"HSE appendix: subsection {n} is {got!r}, not {heading!r}"
            )
        end = marks[idx + 1].start() if idx + 1 < len(marks) else len(markdown)
        body = markdown[start:end].strip()
        if not body:
            raise ValueError(f"HSE appendix: subsection {n} ({heading}) is empty")
        content[key] = f"<h3>{letter}.{n} {html.escape(got)}</h3>" + markdown_to_html(
            body
        )
    return content


def load_hse_appendix(
    markdown_path: str | Path,
    sidecar_path: Optional[str | Path] = None,
    *,
    allow_open: bool = False,
) -> Dict[str, str]:
    """Read the generated appendix and return slot content for a skeleton.

    ``sidecar_path`` defaults to the ``.json`` file beside the Markdown file.
    """
    markdown_path = Path(markdown_path)
    sidecar_path = (
        Path(sidecar_path) if sidecar_path else markdown_path.with_suffix(".json")
    )
    sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
    problems = check_sidecar(sidecar, allow_open=allow_open)
    if problems:
        raise ValueError("HSE appendix is not ready: " + "; ".join(problems))
    return hse_content(markdown_path.read_text(encoding="utf-8"))


__all__ = [
    "HSE_BLOCKS",
    "HSE_SECTION_KEY",
    "check_sidecar",
    "hse_content",
    "hse_section_spec",
    "load_hse_appendix",
    "markdown_to_html",
    "with_hse_section",
]
