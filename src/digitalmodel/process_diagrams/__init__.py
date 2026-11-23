"""Piping and instrumentation diagram (P&ID) authoring to ISA-5.1.

Builds P&ID sheets as standalone SVG: the ISA-5.1 symbol set, pipe runs with
line-crossing jumps, instrument-tag and line-number parsing, and the sheet
scaffold (zone-gridded border, numbered notes column, title block).

Pure standard library, no plotting or CAD dependency, so a sheet can be
generated in a report pipeline or embedded in an HTML page.

    >>> from digitalmodel.process_diagrams import Sheet, TitleBlock, symbols
    >>> sheet = Sheet(width=800, height=600,
    ...               title_block=TitleBlock(drawing_number="PID-1001",
    ...                                      title="INLET SEPARATION"))
    >>> _ = sheet.add(symbols.vessel_vertical(200, 300, 30, 70))
    >>> _ = sheet.add_instrument(320, 260, "LIC-101", kind="bpcs")
    >>> sheet.lint()
    []
    >>> sheet.render().startswith("<svg")
    True

The bubble taxonomy follows ISA-5.1-2009, which retired the pre-2009 reading of
circle-in-square as "DCS" and diamond-in-square as "PLC". See
:mod:`~digitalmodel.process_diagrams.symbols`.
"""

from __future__ import annotations

from . import geometry, symbols, tags
from .geometry import horizontal, polyline, signal, vertical
from .sheet import Sheet, TitleBlock, hold_flag, note_flag, off_page_connector
from .tags import LineNumber, Tag, TagError, parse_line_number, parse_tag

__all__ = [
    "Sheet", "TitleBlock", "Tag", "LineNumber", "TagError",
    "parse_tag", "parse_line_number",
    "horizontal", "vertical", "polyline", "signal",
    "off_page_connector", "note_flag", "hold_flag",
    "geometry", "symbols", "tags",
]
