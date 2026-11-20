"""Well-life schematic: natural flow, reservoir pressure decline, artificial lift.

A clean line drawing, written as inline SVG with no data and no network use, for reuse
in articles on artificial lift. Pass ``highlight`` to mark the lift method an article
deals with; the other methods stay plain.

Usage:
    python examples/domains/artificial_lift/well_life_schematic.py [--highlight "Rod pump"] [out.svg]
"""
from __future__ import annotations

import argparse
import html
import sys
from pathlib import Path

LIFT_METHODS = (
    "Rod pump",
    "Electric submersible pump",
    "Gas lift",
    "Progressing cavity pump",
    "Plunger lift",
    "Hydraulic pump",
)

INK, LINE, MUTED, FLUID, ACCENT = "#0b0b0b", "#52514e", "#a3a29b", "#e3eef8", "#1b8a8f"
FONT = "font-family:system-ui,-apple-system,'Segoe UI',sans-serif"


def _text(x, y, text, *, size=13, anchor="start", fill=LINE, weight=None) -> str:
    w = f' font-weight="{weight}"' if weight else ""
    return (f'<text x="{x}" y="{y}" font-size="{size}" text-anchor="{anchor}" fill="{fill}"{w} '
            f'style="{FONT}">{html.escape(text)}</text>')


def _well(xc: float, *, fluid_top: float, arrow: bool, pump: str | None) -> list[str]:
    """One well: surface, wellhead, casing, reservoir, fluid column and optional pump."""
    surface, bottom, half = 92, 222, 9
    out = [
        f'<line x1="{xc - 50}" y1="{surface}" x2="{xc + 50}" y2="{surface}" stroke="{LINE}" stroke-width="1.5"/>',
        f'<rect x="{xc + 1 - half}" y="{fluid_top}" width="{2 * half - 2}" height="{bottom - fluid_top}" fill="{FLUID}"/>',
        f'<line x1="{xc - half}" y1="{surface - 14}" x2="{xc - half}" y2="{bottom}" stroke="{LINE}" stroke-width="1.5"/>',
        f'<line x1="{xc + half}" y1="{surface - 14}" x2="{xc + half}" y2="{bottom}" stroke="{LINE}" stroke-width="1.5"/>',
        f'<rect x="{xc - 14}" y="{surface - 22}" width="28" height="8" fill="none" stroke="{LINE}" stroke-width="1.5"/>',
        f'<rect x="{xc - 56}" y="{bottom}" width="112" height="26" fill="none" stroke="{MUTED}" '
        'stroke-width="1.2" stroke-dasharray="4 3"/>',
        _text(xc, bottom + 17, "Reservoir", size=11, anchor="middle", fill=LINE),
    ]
    if arrow:
        start = bottom - 38 if pump else bottom - 8
        out.append(f'<line x1="{xc}" y1="{start}" x2="{xc}" y2="{surface - 4}" stroke="{INK}" '
                   'stroke-width="1.5" marker-end="url(#wl-arrow)"/>')
    if pump:
        out.append(f'<rect x="{xc - 6}" y="{bottom - 34}" width="12" height="24" fill="#fff" '
                   f'stroke="{ACCENT}" stroke-width="2"/>')
    return out


def well_life_svg(highlight: str | None = "Rod pump", *, title: str = "Life of a well, schematic") -> str:
    """Return the schematic as one SVG element (viewBox 800 x 330)."""
    if highlight is not None and highlight not in LIFT_METHODS:
        raise ValueError(f"highlight must be one of {LIFT_METHODS}")
    out = [f'<svg viewBox="0 0 800 330" role="img" aria-label="{html.escape(title)}" '
           'xmlns="http://www.w3.org/2000/svg" style="width:100%;height:auto;display:block">',
           '<defs><marker id="wl-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" '
           f'orient="auto-start-reverse"><path d="M0 0L10 5L0 10z" fill="{INK}"/></marker></defs>']
    # Stage headings and dividers.
    for x, heading in ((20, "1  Natural flow"), (250, "2  Pressure declines"), (480, "3  Artificial lift")):
        out.append(_text(x, 22, heading, size=14, fill=INK, weight="600"))
    for x in (240, 470):
        out.append(f'<line x1="{x}" y1="8" x2="{x}" y2="258" stroke="{MUTED}" stroke-width="1" stroke-dasharray="2 4"/>')
    # Stage 1: the reservoir pressure lifts the fluid to surface.
    out += _well(120, fluid_top=78, arrow=True, pump=None)
    out.append(_text(20, 46, "Reservoir pressure lifts", size=12))
    out.append(_text(20, 61, "the fluid to the surface.", size=12))
    # Stage 2: the fluid column no longer reaches the surface.
    out += _well(350, fluid_top=160, arrow=False, pump=None)
    out.append(f'<line x1="{350 + 12}" y1="160" x2="{350 + 40}" y2="160" stroke="{LINE}" stroke-width="1"/>')
    out.append(_text(350 + 44, 164, "Fluid level", size=11))
    out.append(_text(250, 46, "The fluid no longer", size=12))
    out.append(_text(250, 61, "reaches the surface.", size=12))
    # Stage 3: a pump or injected gas adds the energy; the methods, one highlighted.
    out += _well(540, fluid_top=78, arrow=True, pump="pump")
    out.append(_text(480, 46, "A pump or injected gas", size=12))
    out.append(_text(480, 61, "adds the energy.", size=12))
    y = 92
    for method in LIFT_METHODS:
        if method == highlight:
            out.append(f'<rect x="604" y="{y - 15}" width="184" height="22" rx="3" fill="none" '
                       f'stroke="{ACCENT}" stroke-width="2"/>')
            out.append(_text(612, y, method, size=12, fill=INK, weight="600"))
            out.append(_text(612, y + 22, "This article", size=11, fill=ACCENT))
            y += 22
        else:
            out.append(_text(612, y, method, size=12))
        y += 26
    # Reservoir pressure over the life of the well: falling, crossing the pressure the well needs to flow.
    out.append(_text(20, 276, "Reservoir pressure over the life of the well", size=11))
    out.append(f'<path d="M20 284 C 200 288, 300 298, 470 306 S 720 316, 780 318" fill="none" '
               f'stroke="{INK}" stroke-width="1.5" marker-end="url(#wl-arrow)"/>')
    out.append(f'<line x1="20" y1="300" x2="780" y2="300" stroke="{MUTED}" stroke-width="1.2" stroke-dasharray="5 4"/>')
    out.append(_text(780, 294, "Pressure the well needs to flow", size=11, anchor="end", fill=LINE))
    out.append("</svg>")
    return "".join(out)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("out", nargs="?", type=Path)
    parser.add_argument("--highlight", default="Rod pump")
    args = parser.parse_args(argv)
    svg = well_life_svg(args.highlight or None)
    if args.out:
        args.out.write_text(svg + "\n", encoding="utf-8", newline="\n")
    else:
        sys.stdout.write(svg + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
