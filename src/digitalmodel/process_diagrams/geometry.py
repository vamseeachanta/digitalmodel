"""Pipe-run and signal-line geometry for P&ID sheets.

Process lines on a P&ID cross constantly, and the drafting convention is that
one direction consistently hops the other across a whole sheet — a small
semicircular jump that tells the reader the two lines are not connected. This
module builds those runs as SVG path data.

Instrument signal lines are different: they may cross process lines and each
other freely without a hop, because their line style already distinguishes
them. :func:`signal` therefore never inserts a jump.

Coordinates are SVG user units with y increasing downward.
"""

from __future__ import annotations

from collections.abc import Iterable, Sequence

HOP_RADIUS = 7.0
"""Radius of a line-crossing jump, in user units."""


def _hops_between(crossings: Iterable[float], start: float, end: float) -> list[float]:
    """Crossings strictly inside the run, ordered along the direction of travel.

    A crossing at or beyond an endpoint is not a crossing — the run terminates
    there, which is a connection rather than a jump.
    """
    inside = [c for c in crossings if min(start, end) < c < max(start, end)]
    return sorted(inside, reverse=end < start)


def horizontal(x1: float, x2: float, y: float,
               crossings: Iterable[float] = ()) -> str:
    """Path data for a horizontal run at ``y`` from ``x1`` to ``x2``.

    ``crossings`` are x positions of lines this run jumps over. The arc always
    bulges away from the reading direction's right-hand side (upward for a
    left-to-right run), which keeps a sheet visually consistent.

    >>> horizontal(0, 100, 50)
    'M0,50 L100,50'
    >>> "A7" in horizontal(0, 100, 50, crossings=[40])
    True
    >>> horizontal(0, 100, 50, crossings=[100])   # endpoint is a connection
    'M0,50 L100,50'
    """
    step = HOP_RADIUS if x2 > x1 else -HOP_RADIUS
    d = f"M{_n(x1)},{_n(y)}"
    for hop in _hops_between(crossings, x1, x2):
        d += (f" L{_n(hop - step)},{_n(y)}"
              f" A{_n(HOP_RADIUS)},{_n(HOP_RADIUS)} 0 0 1 {_n(hop + step)},{_n(y)}")
    return d + f" L{_n(x2)},{_n(y)}"


def vertical(y1: float, y2: float, x: float,
             crossings: Iterable[float] = ()) -> str:
    """Path data for a vertical run at ``x`` from ``y1`` to ``y2``.

    ``crossings`` are y positions of lines this run jumps over.

    >>> vertical(0, 100, 50)
    'M50,0 L50,100'
    >>> "A7" in vertical(0, 100, 50, crossings=[40])
    True
    """
    step = HOP_RADIUS if y2 > y1 else -HOP_RADIUS
    d = f"M{_n(x)},{_n(y1)}"
    for hop in _hops_between(crossings, y1, y2):
        d += (f" L{_n(x)},{_n(hop - step)}"
              f" A{_n(HOP_RADIUS)},{_n(HOP_RADIUS)} 0 0 1 {_n(x)},{_n(hop + step)}")
    return d + f" L{_n(x)},{_n(y2)}"


def polyline(points: Sequence[tuple[float, float]]) -> str:
    """Path data through a sequence of points, for runs with corners.

    >>> polyline([(0, 0), (10, 0), (10, 20)])
    'M0,0 L10,0 L10,20'
    """
    if len(points) < 2:
        raise ValueError("a run needs at least two points")
    head, *tail = points
    return f"M{_n(head[0])},{_n(head[1])}" + "".join(
        f" L{_n(x)},{_n(y)}" for x, y in tail)


def signal(points: Sequence[tuple[float, float]]) -> str:
    """Path data for an instrument signal line — never hops.

    Signal lines are distinguished by dash pattern (see
    :data:`~digitalmodel.process_diagrams.symbols.SIGNAL_DASH`), so they cross
    process lines and each other without a jump.
    """
    return polyline(points)


def run_length(points: Sequence[tuple[float, float]]) -> float:
    """Total length of a polyline run, for line-list or takeoff estimates."""
    total = 0.0
    for (x1, y1), (x2, y2) in zip(points, points[1:]):
        total += ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** 0.5
    return total


def escape(text: str) -> str:
    """Escape text for inclusion in SVG markup.

    Labels routinely contain ``&`` ("SHELL & TUBE") and quote marks (a 12-inch
    line number), both of which make the document non-parseable if passed
    through raw.

    >>> escape('EXCHANGER, SHELL & TUBE')
    'EXCHANGER, SHELL &amp; TUBE'
    """
    return (text.replace("&", "&amp;").replace("<", "&lt;")
                .replace(">", "&gt;").replace('"', "&quot;"))


def _n(value: float) -> str:
    """Format a coordinate without a trailing ``.0``."""
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return f"{value:g}"
