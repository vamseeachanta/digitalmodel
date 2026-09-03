"""Drawing sheet assembly: frame, title block, notes column, and rendering.

A P&ID sheet is not just a drawing — it is a controlled document. The border
grid lets a continuation cite a zone, the title block carries the revision and
the issue-status stamp, and the numbered notes column is where the engineering
that does not fit on the drawing lives. This module builds that scaffold and
renders the whole sheet as a standalone SVG fragment.

Sheets carry a ``NOT FOR CONSTRUCTION`` stamp by default. A drawing that has
not been through a hazard review and a relief study should say so on its face.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from .geometry import _n, escape
from .tags import Tag, TagError, parse_tag

ZONE_LETTERS = "HGFEDCBA"


@dataclass
class TitleBlock:
    """Title-block content. ``scale`` is always NONE — P&IDs are schematic."""

    drawing_number: str
    title: str
    project: str = "TYPICAL FACILITY"
    originator: str = ""
    location: str = ""
    revision: str = "A"
    sheet: str = "1 OF 1"
    date: str = ""
    status: str = "NOT FOR CONSTRUCTION"
    scale: str = "NONE"


@dataclass
class Sheet:
    """An assembled P&ID sheet.

    Elements are appended as raw SVG fragments (typically from
    :mod:`~digitalmodel.process_diagrams.symbols`) and rendered inside a
    themed group, so the whole sheet follows the host page's foreground colour.
    """

    width: float
    height: float
    title_block: TitleBlock
    aria_label: str = ""
    zone_columns: int = 8
    zone_rows: int = 4
    _elements: list[str] = field(default_factory=list)
    _notes: list[str] = field(default_factory=list)
    _tags: list[Tag] = field(default_factory=list)
    _valves_without_fail: list[str] = field(default_factory=list)

    # ---------------------------------------------------------------- content
    def add(self, *fragments: str) -> "Sheet":
        """Append raw SVG fragments. Returns self so calls can be chained."""
        self._elements.extend(fragments)
        return self

    def add_instrument(self, cx: float, cy: float, tag: str, **kwargs) -> "Sheet":
        """Add an instrument bubble, validating the tag against ISA-5.1.

        Raises :class:`~digitalmodel.process_diagrams.tags.TagError` on a
        malformed tag, which catches typos at build time rather than at
        drawing review.
        """
        from .symbols import bubble

        parsed = parse_tag(tag)
        self._tags.append(parsed)
        letters, loop = parsed.raw.split("-", 1)
        return self.add(bubble(cx, cy, letters, loop, **kwargs))

    def add_control_valve(self, cx: float, cy: float, tag: str,
                          fail: str | None = "FC", **kwargs) -> "Sheet":
        """Add a control valve and record whether it carries a fail action."""
        from .symbols import control_valve

        parsed = parse_tag(tag)
        self._tags.append(parsed)
        if not fail:
            self._valves_without_fail.append(parsed.raw)
        return self.add(control_valve(cx, cy, fail=fail, **kwargs))

    def note(self, text: str) -> int:
        """Append a numbered note and return its number, for flagging."""
        self._notes.append(text)
        return len(self._notes)

    # ------------------------------------------------------------------- lint
    def lint(self) -> list[str]:
        """Return drafting defects found on this sheet.

        Checks that are cheap and catch real errors:

        - a control valve with no fail action annotated
        - two devices sharing a tag
        - a safety-instrumented tag drawn in a non-SIS bubble is *not* checked
          here, because the bubble kind is not recorded against the tag
        """
        problems = [f"control valve {tag} has no fail action annotated"
                    for tag in self._valves_without_fail]
        seen: dict[str, int] = {}
        for tag in self._tags:
            seen[tag.raw] = seen.get(tag.raw, 0) + 1
        problems.extend(f"tag {raw} is used {count} times"
                        for raw, count in sorted(seen.items()) if count > 1)
        return problems

    def loops(self) -> dict[str, list[str]]:
        """Group the sheet's tags by loop number."""
        grouped: dict[str, list[str]] = {}
        for tag in self._tags:
            grouped.setdefault(tag.loop, []).append(tag.raw)
        return grouped

    # ----------------------------------------------------------------- render
    def render(self, marker_id: str = "arrow") -> str:
        """Render the sheet as a standalone ``<svg>`` fragment."""
        parts = [
            f'<svg viewBox="0 0 {_n(self.width)} {_n(self.height)}" role="img" '
            f'width="{_n(self.width)}" aria-label="{escape(self.aria_label)}">',
            f'<defs><marker id="{marker_id}" viewBox="0 0 10 10" refX="9" refY="5" '
            f'markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,1 L9,5 L0,9 Z" fill="currentColor" stroke="none"/></marker></defs>',
            '<g fill="none" stroke="currentColor" stroke-width="2.2" '
            'stroke-linecap="round" stroke-linejoin="round" '
            'font-family="IBM Plex Mono, ui-monospace, monospace">',
            self._frame(),
            *self._elements,
            self._notes_column(),
            self._title_block(),
            "</g></svg>",
        ]
        return "\n".join(p for p in parts if p)

    def _frame(self) -> str:
        w, h = self.width, self.height
        out = (f'<rect x="10" y="10" width="{_n(w - 20)}" height="{_n(h - 20)}" '
               f'stroke-width="2.4" fill="none"/>'
               f'<rect x="26" y="26" width="{_n(w - 52)}" height="{_n(h - 52)}" '
               f'stroke-width="1" fill="none"/>')
        for i in range(self.zone_columns):
            x = 26 + (w - 52) * (i + 0.5) / self.zone_columns
            label = str(self.zone_columns - i)
            out += (f'<text x="{_n(x)}" y="22" font-size="9" text-anchor="middle" '
                    f'stroke="none" fill="currentColor" opacity=".5">{label}</text>'
                    f'<text x="{_n(x)}" y="{_n(h - 13)}" font-size="9" text-anchor="middle" '
                    f'stroke="none" fill="currentColor" opacity=".5">{label}</text>')
        letters = ZONE_LETTERS[-self.zone_rows:]
        for j in range(self.zone_rows):
            y = 26 + (h - 52) * (j + 0.5) / self.zone_rows
            out += (f'<text x="18" y="{_n(y + 3)}" font-size="9" text-anchor="middle" '
                    f'stroke="none" fill="currentColor" opacity=".5">{letters[j]}</text>'
                    f'<text x="{_n(w - 18)}" y="{_n(y + 3)}" font-size="9" '
                    f'text-anchor="middle" stroke="none" fill="currentColor" '
                    f'opacity=".5">{letters[j]}</text>')
        return out

    def _notes_column(self) -> str:
        if not self._notes:
            return ""
        x, y = self.width - 320, self.height - 260
        out = (f'<text x="{_n(x)}" y="{_n(y)}" font-size="11" letter-spacing="1.6" '
               f'stroke="none" fill="currentColor" opacity=".85">NOTES</text>'
               f'<path d="M{_n(x)},{_n(y + 6)} H{_n(self.width - 26)}" '
               f'stroke-width="1.2" opacity=".5"/>')
        for i, text in enumerate(self._notes):
            out += (f'<text x="{_n(x)}" y="{_n(y + 28 + i * 17)}" font-size="10" '
                    f'stroke="none" fill="currentColor" opacity=".88">'
                    f'{i + 1}. {escape(text)}</text>')
        return out

    def _title_block(self) -> str:
        tb = self.title_block
        x2, y2 = self.width - 26, self.height - 26
        x1, y1 = x2 - 460, y2 - 104
        xs = x1 + 162
        cx = (x1 + xs) / 2

        def text(tx, ty, s, size=10.5, anchor="start", opacity=None):
            op = f' opacity="{opacity}"' if opacity else ""
            return (f'<text x="{_n(tx)}" y="{_n(ty)}" font-size="{_n(size)}" '
                    f'text-anchor="{anchor}" stroke="none" fill="currentColor"{op}>{escape(str(s))}</text>')

        out = (f'<rect x="{_n(x1)}" y="{_n(y1)}" width="{_n(x2 - x1)}" '
               f'height="{_n(y2 - y1)}" stroke-width="1.6" fill="none"/>'
               f'<path d="M{_n(xs)},{_n(y1)} V{_n(y2)} M{_n(xs)},{_n(y1 + 34)} H{_n(x2)} '
               f'M{_n(xs)},{_n(y1 + 69)} H{_n(x2)} M{_n(x2 - 118)},{_n(y1 + 69)} V{_n(y2)}" '
               f'stroke-width="1.3" fill="none"/>')
        if tb.originator:
            out += text(cx, y1 + 22, tb.originator, 18, "middle")
        if tb.location:
            out += text(cx, y1 + 38, tb.location, 8, "middle", ".65")
        out += text(cx, y1 + 62, tb.status, 9, "middle")
        out += text(cx, y1 + 90, f"SCALE: {tb.scale}  ·  {tb.date}", 8, "middle", ".65")
        out += (text(xs + 10, y1 + 14, "PROJECT", 7.5, opacity=".6")
                + text(xs + 10, y1 + 27, tb.project)
                + text(xs + 10, y1 + 49, "TITLE", 7.5, opacity=".6")
                + text(xs + 10, y1 + 62, tb.title)
                + text(xs + 10, y1 + 84, "DRAWING NO.", 7.5, opacity=".6")
                + text(xs + 10, y1 + 97, tb.drawing_number)
                + text(x2 - 110, y1 + 84, "REV / SHEET", 7.5, opacity=".6")
                + text(x2 - 110, y1 + 97, f"{tb.revision} · {tb.sheet}"))
        return out


def off_page_connector(x: float, y: float, lines: list[str],
                       direction: str = "right", width: float = 150.0) -> str:
    """Home-plate continuation flag.

    Carries the continuing line number, the destination drawing number, and the
    grid coordinate on that drawing. Incoming flags belong on the left edge and
    outgoing on the right, so the sheet reads left to right in the direction of
    process flow.
    """
    h = 15.0
    if direction == "right":
        d = (f"M{_n(x)},{_n(y - h)} H{_n(x + width - 16)} L{_n(x + width)},{_n(y)} "
             f"L{_n(x + width - 16)},{_n(y + h)} H{_n(x)} Z")
        tx = x + 8
    else:
        d = (f"M{_n(x + width)},{_n(y - h)} H{_n(x + 16)} L{_n(x)},{_n(y)} "
             f"L{_n(x + 16)},{_n(y + h)} H{_n(x + width)} Z")
        tx = x + 22
    out = f'<path d="{d}" stroke-width="1.6" fill="none"/>'
    if len(lines) == 1:
        out += (f'<text x="{_n(tx)}" y="{_n(y + 4)}" font-size="10.5" stroke="none" '
                f'fill="currentColor">{escape(lines[0])}</text>')
    else:
        for i, line in enumerate(lines[:2]):
            out += (f'<text x="{_n(tx)}" y="{_n(y - 1 + i * 11)}" font-size="10" '
                    f'stroke="none" fill="currentColor">{escape(line)}</text>')
    return out


def note_flag(x: float, y: float, number: int) -> str:
    """Numbered triangular flag pointing at what a note governs."""
    return (f'<path d="M{_n(x)},{_n(y - 12)} L{_n(x + 12)},{_n(y + 8)} '
            f'L{_n(x - 12)},{_n(y + 8)} Z" stroke-width="1.6" fill="none"/>'
            f'<text x="{_n(x)}" y="{_n(y + 6)}" font-size="10.5" text-anchor="middle" '
            f'stroke="none" fill="currentColor">{number}</text>')


def hold_flag(x: float, y: float, number: int) -> str:
    """Hold flag — an unresolved item, conventionally clouded on a real sheet."""
    return note_flag(x, y, number)


__all__ = ["Sheet", "TitleBlock", "off_page_connector", "note_flag", "hold_flag",
           "TagError"]
