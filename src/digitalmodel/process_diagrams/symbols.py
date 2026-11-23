"""ISA-5.1 symbol primitives as SVG fragments.

Each function returns a self-contained SVG fragment positioned at absolute
coordinates. Fragments inherit ``stroke``/``fill`` from the enclosing group, so
a whole sheet can be themed by setting ``stroke="currentColor"`` once.

The bubble taxonomy in :func:`bubble` is the post-2009 one. ISA-5.1-2009
retired the pre-2009 reading of circle-in-square as "DCS" and diamond-in-square
as "PLC": the enclosing square now means *shared display, shared control*, and
the inner shape selects the system — a circle for the basic process control
system, a diamond for the safety instrumented system.
"""

from __future__ import annotations

from typing import Literal

from .geometry import _n, escape

# Signal line dash patterns, keyed by ISA-5.1 signal type.
SIGNAL_DASH: dict[str, str | None] = {
    "electric": "7 4",
    "pneumatic": None,      # solid, with double-slash tick marks
    "data": "12 10",        # internal system link, drawn with small circles
    "capillary": None,      # solid, with X tick marks
    "process": None,
}

BubbleKind = Literal["field", "bpcs", "sis", "computer", "interlock", "auxiliary"]
Actuator = Literal["diaphragm", "piston", "motor", "solenoid", "manual"]


def _t(x: float, y: float, text: str, size: float = 11.5,
       anchor: str = "middle") -> str:
    return (f'<text x="{_n(x)}" y="{_n(y)}" font-size="{_n(size)}" '
            f'text-anchor="{anchor}" stroke="none" fill="currentColor">{escape(text)}</text>')


# --------------------------------------------------------------------------- #
# Instrument bubbles
# --------------------------------------------------------------------------- #
def bubble(cx: float, cy: float, top: str, bottom: str = "",
           kind: BubbleKind = "field", radius: float = 21.0) -> str:
    """An instrument bubble.

    ``kind`` selects where the function lives, which is the whole point of the
    symbol:

    - ``field`` — discrete, field-mounted instrument (plain circle)
    - ``bpcs`` — shared display / shared control, basic process control system
      (circle inside a square)
    - ``sis`` — shared display / shared control, safety instrumented system
      (diamond inside a square)
    - ``computer`` — computer or high-level control function (hexagon)
    - ``interlock`` — simple interlock logic, AND/OR only (plain diamond)
    - ``auxiliary`` — auxiliary or local panel location (circle, double line)
    """
    r = radius
    out = ""
    if kind in ("bpcs", "sis"):
        out += (f'<rect x="{_n(cx - r)}" y="{_n(cy - r)}" width="{_n(2 * r)}" '
                f'height="{_n(2 * r)}" fill="none"/>')
    if kind == "sis" or kind == "interlock":
        out += (f'<path d="M{_n(cx)},{_n(cy - r)} L{_n(cx + r)},{_n(cy)} '
                f'L{_n(cx)},{_n(cy + r)} L{_n(cx - r)},{_n(cy)} Z" fill="none"/>')
    elif kind == "computer":
        h = r * 0.87
        out += (f'<path d="M{_n(cx - r)},{_n(cy)} L{_n(cx - r / 2)},{_n(cy - h)} '
                f'L{_n(cx + r / 2)},{_n(cy - h)} L{_n(cx + r)},{_n(cy)} '
                f'L{_n(cx + r / 2)},{_n(cy + h)} L{_n(cx - r / 2)},{_n(cy + h)} Z" fill="none"/>')
    else:
        out += f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="{_n(r)}" fill="none"/>'
    if kind == "auxiliary":
        out += (f'<path d="M{_n(cx - r)},{_n(cy - 2.5)} H{_n(cx + r)} '
                f'M{_n(cx - r)},{_n(cy + 2.5)} H{_n(cx + r)}"/>')
    if bottom:
        out += _t(cx, cy - 2, top) + _t(cx, cy + 11, bottom)
    else:
        out += _t(cx, cy + 4, top)
    return out


# --------------------------------------------------------------------------- #
# Valve bodies and actuators
# --------------------------------------------------------------------------- #
def gate(cx: float, cy: float, vertical_run: bool = False) -> str:
    """Gate valve — the hollow bowtie that every other body builds on."""
    if vertical_run:
        d = (f"M{_n(cx - 9)},{_n(cy - 12)} L{_n(cx + 9)},{_n(cy - 12)} "
             f"L{_n(cx - 9)},{_n(cy + 12)} L{_n(cx + 9)},{_n(cy + 12)} Z")
    else:
        d = (f"M{_n(cx - 12)},{_n(cy - 9)} L{_n(cx - 12)},{_n(cy + 9)} "
             f"L{_n(cx + 12)},{_n(cy - 9)} L{_n(cx + 12)},{_n(cy + 9)} Z")
    return f'<path d="{d}" fill="none"/>'


def globe(cx: float, cy: float, vertical_run: bool = False) -> str:
    """Globe valve — bowtie with a filled centre circle."""
    return gate(cx, cy, vertical_run) + (
        f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="4" fill="currentColor" stroke="none"/>')


def ball(cx: float, cy: float, vertical_run: bool = False) -> str:
    """Ball valve — bowtie with a hollow centre circle."""
    return gate(cx, cy, vertical_run) + (
        f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="4.5" fill="none"/>')


def check(cx: float, cy: float) -> str:
    """Check valve. Flow direction must be unambiguous, so the bar is drawn."""
    return (f'<path d="M{_n(cx - 11)},{_n(cy - 9)} L{_n(cx + 9)},{_n(cy)} '
            f'L{_n(cx - 11)},{_n(cy + 9)} Z" fill="none"/>'
            f'<path d="M{_n(cx + 9)},{_n(cy - 9)} V{_n(cy + 9)}"/>')


def control_valve(cx: float, cy: float, actuator: Actuator = "diaphragm",
                  fail: str | None = "FC", vertical_run: bool = False) -> str:
    """Control valve: a body, a stem, an actuator, and a fail-action code.

    ``fail`` is annotated in text rather than left to the stem arrow, because
    that is how it is read in practice. Passing ``fail=None`` omits it, which
    :func:`~digitalmodel.process_diagrams.sheet.Sheet.lint` reports — a missing
    fail action is a real drafting defect, not a cosmetic one. On a compressor
    sheet the anti-surge recycle valve is typically the only ``FO`` among
    otherwise ``FC`` valves, and reversing that wrecks the machine.
    """
    out = gate(cx, cy, vertical_run)
    if vertical_run:
        ax, ay = cx + 22, cy
        out += f'<path d="M{_n(cx)},{_n(cy)} H{_n(ax)}"/>'
    else:
        ax, ay = cx, cy - 20
        out += f'<path d="M{_n(cx)},{_n(cy)} V{_n(ay)}"/>'
    out += _actuator(ax, ay, actuator)
    if fail:
        out += _t(ax, ay - 13 if not vertical_run else ay - 15, fail, 10)
    return out


def _actuator(ax: float, ay: float, kind: Actuator) -> str:
    if kind == "diaphragm":
        return (f'<path d="M{_n(ax - 9)},{_n(ay)} A9,8 0 0 1 {_n(ax + 9)},{_n(ay)} Z" '
                f'fill="none"/>')
    if kind == "piston":
        return (f'<rect x="{_n(ax - 10)}" y="{_n(ay - 10)}" width="20" height="10" '
                f'fill="none"/>')
    if kind == "motor":
        return (f'<circle cx="{_n(ax)}" cy="{_n(ay - 7)}" r="9" fill="none"/>'
                + _t(ax, ay - 3, "M", 11))
    if kind == "solenoid":
        return (f'<rect x="{_n(ax - 9)}" y="{_n(ay - 13)}" width="18" height="13" '
                f'fill="none"/>' + _t(ax, ay - 3, "S", 11))
    return f'<path d="M{_n(ax - 9)},{_n(ay)} H{_n(ax + 9)}"/>'


def relief_valve(cx: float, cy: float) -> str:
    """Pressure safety valve: angle body, spring bonnet, side outlet."""
    return (f'<path d="M{_n(cx - 10)},{_n(cy + 16)} L{_n(cx + 10)},{_n(cy + 16)} '
            f'L{_n(cx - 10)},{_n(cy + 1)} L{_n(cx + 10)},{_n(cy + 1)} Z" fill="none"/>'
            f'<path d="M{_n(cx)},{_n(cy + 1)} V{_n(cy - 5)}"/>'
            f'<path d="M{_n(cx - 8)},{_n(cy - 5)} L{_n(cx + 8)},{_n(cy - 9)} '
            f'L{_n(cx - 8)},{_n(cy - 13)} L{_n(cx + 8)},{_n(cy - 17)} '
            f'L{_n(cx - 8)},{_n(cy - 21)} L{_n(cx + 8)},{_n(cy - 25)}" fill="none"/>'
            f'<path d="M{_n(cx + 7)},{_n(cy + 1)} H{_n(cx + 26)}"/>')


def orifice(cx: float, cy: float) -> str:
    """Orifice plate primary element, with flange-tap marks."""
    return (f'<path d="M{_n(cx)},{_n(cy - 13)} V{_n(cy + 13)}" stroke-width="2.4"/>'
            f'<path d="M{_n(cx - 6)},{_n(cy - 9)} V{_n(cy + 9)} '
            f'M{_n(cx + 6)},{_n(cy - 9)} V{_n(cy + 9)}" stroke-width="1" opacity=".5"/>')


# --------------------------------------------------------------------------- #
# Equipment
# --------------------------------------------------------------------------- #
def vessel_vertical(cx: float, cy: float, half_width: float, half_height: float,
                    head_depth: float | None = None) -> str:
    """Vertical vessel with dished heads. Draw to rough proportion."""
    hd = head_depth if head_depth is not None else half_width * 0.5
    return (f'<path d="M{_n(cx - half_width)},{_n(cy - half_height)} '
            f'A{_n(half_width)},{_n(hd)} 0 0 1 {_n(cx + half_width)},{_n(cy - half_height)} '
            f'L{_n(cx + half_width)},{_n(cy + half_height)} '
            f'A{_n(half_width)},{_n(hd)} 0 0 1 {_n(cx - half_width)},{_n(cy + half_height)} Z" '
            f'fill="none"/>')


def vessel_horizontal(cx: float, cy: float, half_width: float, half_height: float,
                      head_depth: float | None = None) -> str:
    """Horizontal vessel. A three-phase separator is identified by *two* level
    bridles, not by the shell — draw both if that is what it is."""
    hd = head_depth if head_depth is not None else half_height * 0.5
    return (f'<path d="M{_n(cx - half_width)},{_n(cy - half_height)} '
            f'L{_n(cx + half_width)},{_n(cy - half_height)} '
            f'A{_n(hd)},{_n(half_height)} 0 0 1 {_n(cx + half_width)},{_n(cy + half_height)} '
            f'L{_n(cx - half_width)},{_n(cy + half_height)} '
            f'A{_n(hd)},{_n(half_height)} 0 0 1 {_n(cx - half_width)},{_n(cy - half_height)} Z" '
            f'fill="none"/>')


def column(cx: float, cy: float, half_width: float, half_height: float,
           packed_beds: list[tuple[float, float]] | None = None) -> str:
    """Trayed or packed column. ``packed_beds`` are absolute (y_top, y_bottom) pairs."""
    out = vessel_vertical(cx, cy, half_width, half_height, half_width * 0.5)
    for top, bottom in packed_beds or []:
        out += (f'<rect x="{_n(cx - half_width + 3)}" y="{_n(top)}" '
                f'width="{_n(2 * half_width - 6)}" height="{_n(bottom - top)}" fill="none"/>')
        y = top + 6
        while y < bottom:
            out += (f'<path d="M{_n(cx - half_width + 3)},{_n(y)} '
                    f'L{_n(cx + half_width - 3)},{_n(y - 5)}" stroke-width=".8" opacity=".5"/>')
            y += 9
    return out


def exchanger_shell_tube(cx: float, cy: float, r: float) -> str:
    """Shell-and-tube exchanger."""
    return (f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="{_n(r)}" fill="none"/>'
            f'<path d="M{_n(cx - r)},{_n(cy - r * 0.42)} H{_n(cx + r * 0.28)} '
            f'A{_n(r * 0.42)},{_n(r * 0.42)} 0 0 1 {_n(cx + r * 0.28)},{_n(cy + r * 0.42)} '
            f'H{_n(cx - r)}" fill="none"/>')


def exchanger_plate_fin(cx: float, cy: float, half_width: float,
                        half_height: float) -> str:
    """Brazed-aluminium plate-fin exchanger — the cold-box workhorse."""
    return (f'<rect x="{_n(cx - half_width)}" y="{_n(cy - half_height)}" '
            f'width="{_n(2 * half_width)}" height="{_n(2 * half_height)}" fill="none"/>'
            f'<path d="M{_n(cx - half_width)},{_n(cy + half_height)} '
            f'L{_n(cx + half_width)},{_n(cy - half_height)}" stroke-width="1" opacity=".55"/>')


def air_cooler(cx: float, cy: float, half_width: float) -> str:
    """Air cooler (fin-fan): tube bundle over a fan."""
    return (f'<rect x="{_n(cx - half_width)}" y="{_n(cy - 18)}" '
            f'width="{_n(2 * half_width)}" height="16" fill="none"/>'
            f'<path d="M{_n(cx - 14)},{_n(cy + 6)} A14,9 0 0 1 {_n(cx + 14)},{_n(cy + 6)}" '
            f'fill="none"/>'
            f'<path d="M{_n(cx - 14)},{_n(cy + 14)} A14,9 0 0 1 {_n(cx + 14)},{_n(cy + 14)}" '
            f'fill="none"/>')


def turbomachine(cx: float, cy: float, half_width: float, half_height: float,
                 expanding: bool = False) -> str:
    """Trapezoid for a compressor (narrowing) or expander (widening)."""
    if expanding:
        d = (f"M{_n(cx - half_width)},{_n(cy - half_height * 0.45)} "
             f"L{_n(cx + half_width)},{_n(cy - half_height)} "
             f"L{_n(cx + half_width)},{_n(cy + half_height)} "
             f"L{_n(cx - half_width)},{_n(cy + half_height * 0.45)} Z")
    else:
        d = (f"M{_n(cx - half_width)},{_n(cy - half_height)} "
             f"L{_n(cx + half_width)},{_n(cy - half_height * 0.45)} "
             f"L{_n(cx + half_width)},{_n(cy + half_height * 0.45)} "
             f"L{_n(cx - half_width)},{_n(cy + half_height)} Z")
    return f'<path d="{d}" fill="none"/>'


def pump(cx: float, cy: float, r: float = 16.0, flip: bool = False) -> str:
    """Centrifugal pump. ``flip`` points the impeller marker left for a
    right-to-left run, so the symbol agrees with the flow arrow."""
    d = -1 if flip else 1
    return (f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="{_n(r)}" fill="none"/>'
            f'<path d="M{_n(cx - d * r * 0.5)},{_n(cy - r * 0.62)} '
            f'L{_n(cx + d * r * 0.7)},{_n(cy)} '
            f'L{_n(cx - d * r * 0.5)},{_n(cy + r * 0.62)} Z" fill="none"/>')


def generator(cx: float, cy: float, r: float = 16.0, label: str = "G") -> str:
    """Rotating machine circle — generator, motor, or driver."""
    return (f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="{_n(r)}" fill="none"/>'
            + _t(cx, cy + 5, label, 13))


def transformer(cx: float, cy: float, r: float = 13.0) -> str:
    """Two-winding transformer, for the electrical one-line inset."""
    return (f'<circle cx="{_n(cx)}" cy="{_n(cy - r * 0.62)}" r="{_n(r)}" fill="none"/>'
            f'<circle cx="{_n(cx)}" cy="{_n(cy + r * 0.62)}" r="{_n(r)}" fill="none"/>')


def breaker(cx: float, cy: float, half: float = 9.0) -> str:
    """Circuit breaker."""
    return (f'<rect x="{_n(cx - half)}" y="{_n(cy - half)}" width="{_n(2 * half)}" '
            f'height="{_n(2 * half)}" fill="none"/>')


def busbar(x1: float, x2: float, y: float) -> str:
    """Switchgear bus."""
    return (f'<path d="M{_n(x1)},{_n(y)} H{_n(x2)}" stroke-width="5" '
            f'stroke-linecap="butt"/>')
