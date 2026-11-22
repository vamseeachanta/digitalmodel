# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare explicit UTF-8 HTML with the declared census, entirely offline.

The structural contract is section IDs with descendant headings (the first
heading belonging to each section supplies its title). Navigation is optional;
only local fragment links inside nav elements are counted. This is not a browser
or a general HTML validator. No compatibility with a live page is asserted.
The CLI always exits successfully for a report, including drift and unknown
shapes; input/schema errors propagate. It performs no fetching or file writes.
"""

from __future__ import annotations

import argparse
from collections import Counter
import hashlib
from html import unescape
from html.parser import HTMLParser
import importlib.util
import json
from pathlib import Path
from typing import Any

import yaml

_spec = importlib.util.spec_from_file_location(
    "capabilities_inventory", Path(__file__).with_name("build_capabilities_inventory.py")
)
if _spec is None or _spec.loader is None:
    raise ImportError("The declared census loader is unavailable")
_inventory = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_inventory)
_HEADINGS = {f"h{i}" for i in range(1, 7)}
_INERT = {"template", "script", "style"}
_TRACKED = _HEADINGS | _INERT | {"section", "nav"}


def _title(text: str) -> str:
    return " ".join(unescape(text).split())


class SurfaceParser(HTMLParser):
    """Collect scoped sections; tolerate markup outside the structural contract."""

    def __init__(self) -> None:
        """Initialize counters and document-scoped parser state."""
        super().__init__(convert_charrefs=True)
        self.stack: list[tuple[str, Any]] = []
        self.sections: list[dict[str, Any]] = []
        self.tags: Counter[str] = Counter()
        self.nav_count = 0
        self.nav_fragments = 0
        self.nav_malformed = False
        self.malformed = False
        self.redirect = False

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        """Collect active sections and navigation without interpreting scripts."""
        attributes = {name: value or "" for name, value in attrs}
        self.tags[tag] += 1
        inert = any(t in _INERT for t, _ in self.stack)
        if inert:
            if tag in _INERT:
                self.stack.append((tag, None))
            return
        if tag == "meta" and attributes.get("http-equiv", "").lower() == "refresh":
            self.redirect = True
        if tag == "nav":
            self.nav_count += 1
        if tag == "a" and any(t == "nav" for t, _ in self.stack):
            self.nav_fragments += int(attributes.get("href", "").startswith("#")
                                      and len(attributes.get("href", "")) > 1)
        record: dict[str, Any] | None = None
        if tag == "section":
            record = {"id": attributes.get("id", ""), "headings": []}
            self.sections.append(record)
        elif tag in _HEADINGS:
            section = next((r for t, r in reversed(self.stack) if t == "section"), None)
            record = {"section": section, "text": []}
        if tag in _TRACKED:
            self.stack.append((tag, record))

    def handle_startendtag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        """Apply the same structural rules to explicitly self-closing elements."""
        self.handle_starttag(tag, attrs)
        self.handle_endtag(tag)

    def handle_data(self, data: str) -> None:
        """Accumulate only heading text outside inert elements."""
        if not any(t in _INERT for t, _ in self.stack):
            heading = next((r for t, r in reversed(self.stack) if t in _HEADINGS), None)
            if heading is not None:
                heading["text"].append(data)

    def handle_endtag(self, tag: str) -> None:
        """Complete headings and expose mismatched structural boundaries."""
        if any(t in _INERT for t, _ in self.stack) and tag not in _INERT:
            return
        if tag not in _TRACKED:
            return
        tags = [t for t, _ in self.stack]
        if tag not in tags:
            self.malformed = True
            self.nav_malformed |= tag == "nav"
            return
        index = len(tags) - 1 - tags[::-1].index(tag)
        self.malformed |= index != len(tags) - 1
        if index != len(tags) - 1:
            self.nav_malformed |= "nav" in tags[index:]
        _, record = self.stack[index]
        if tag in _HEADINGS and record["section"] is not None:
            record["section"]["headings"].append(_title("".join(record["text"])))
        del self.stack[index:]


def compare_surface(html: str, repo: Path | str = _inventory.REPO_DEFAULT) -> dict:
    """Return a deterministic report; an empty or unusable census cannot match."""
    config = yaml.safe_load((Path(repo) / _inventory.SECTIONS_YML).read_text(encoding="utf-8"))
    sections = [{"id": s["id"], "title": s.get("title", ""),
                 "hrefs": list(s.get("hrefs") or []), "card_id": s.get("card_id")}
                for s in (config.get("sections") or [])]
    expected = {s["id"]: _title(s["title"]) for s in _inventory.census(sections)}
    parser = SurfaceParser()
    parser.feed(html)
    parser.close()
    observed: dict[str, list[str]] = {}
    for section in parser.sections:
        observed.setdefault(section["id"], []).append(
            section["headings"][0] if section["headings"] else "")
    reasons = []
    if parser.redirect:
        reasons.append("redirect")
    if parser.malformed or parser.stack:
        reasons.append("malformed_structure")
    if not any(any(titles) for titles in observed.values()):
        reasons.append("no_titled_sections")
    if any(not s["id"] or not s["headings"] or not s["headings"][0]
           for s in parser.sections):
        reasons.append("untitled_section")
    mismatches = {
        "missing": sorted(expected.keys() - observed.keys()),
        "extra": sorted(observed.keys() - expected.keys()),
        "duplicate": sorted(sid for sid, titles in observed.items() if len(titles) > 1),
        "changed_title": [
            {"id": sid, "expected": expected[sid], "observed": sorted(set(observed[sid]))}
            for sid in sorted(expected.keys() & observed.keys())
            if any(title != expected[sid] for title in observed[sid])
        ],
    }
    return {
        "schema_version": "1.0",
        "canonical_surface": config["canonical_surface"],
        "source_sha256": hashlib.sha256(html.encode("utf-8")).hexdigest(),
        "expected_count": len(expected),
        "observed_count": len(parser.sections),
        "status": ("unrecognized-shape" if reasons else
                   "drift" if any(mismatches.values()) else "match"),
        "mismatches": mismatches,
        "diagnostics": {
            "reasons": reasons,
            "tag_counts": {tag[:120]: count for tag, count in sorted(parser.tags.items())[:40]},
            "section_ids": [sid[:120] for sid in sorted(observed)[:40]],
            "nav_fragment_count": parser.nav_fragments,
            "navigation": ("malformed" if parser.nav_malformed
                           or any(t == "nav" for t, _ in parser.stack) else
                           "absent" if not parser.nav_count else
                           "present" if parser.nav_fragments else "empty"),
        },
    }


def main() -> int:
    """Emit JSON from a caller-supplied file without changing inventory inputs."""
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("html", type=Path)
    ap.add_argument("--repo", type=Path, default=_inventory.REPO_DEFAULT)
    args = ap.parse_args()
    print(json.dumps(compare_surface(args.html.read_bytes().decode("utf-8"), args.repo),
                     indent=2, sort_keys=True, ensure_ascii=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
