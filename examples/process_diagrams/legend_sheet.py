#!/usr/bin/env python3
"""Generate Sheet 1 of a P&ID set: the legend and symbol sheet.

Every P&ID set opens with a legend, because house variations on ISA-5.1 are
normal and the legend is what actually governs a given set. This script builds
one from the symbol library and writes it as a standalone SVG.

Run:
    PYTHONPATH=src python examples/process_diagrams/legend_sheet.py [out.svg]
"""

from __future__ import annotations

import sys
from pathlib import Path

from digitalmodel.process_diagrams import Sheet, TitleBlock, symbols
from digitalmodel.process_diagrams.geometry import _n, escape

COLUMNS = [
    ("EQUIPMENT", [
        (lambda x, y: symbols.vessel_vertical(x, y, 13, 16, 7), "VESSEL, VERTICAL"),
        (lambda x, y: symbols.vessel_horizontal(x, y, 16, 13, 7), "VESSEL, HORIZONTAL"),
        (lambda x, y: symbols.column(x, y, 12, 24, [(y - 16, y + 4)]), "TOWER / COLUMN"),
        (lambda x, y: symbols.exchanger_shell_tube(x, y, 17), "EXCHANGER, SHELL & TUBE"),
        (lambda x, y: symbols.exchanger_plate_fin(x, y, 17, 14), "EXCHANGER, PLATE-FIN"),
        (lambda x, y: symbols.air_cooler(x, y, 18), "AIR COOLER (FIN-FAN)"),
        (lambda x, y: symbols.turbomachine(x, y, 22, 13), "COMPRESSOR"),
        (lambda x, y: symbols.turbomachine(x, y, 22, 13, True), "EXPANDER"),
        (lambda x, y: symbols.pump(x, y, 13), "PUMP, CENTRIFUGAL"),
    ]),
    ("VALVES", [
        (symbols.gate, "GATE VALVE"),
        (symbols.globe, "GLOBE VALVE"),
        (symbols.ball, "BALL VALVE"),
        (symbols.check, "CHECK VALVE (NRV)"),
        (lambda x, y: symbols.control_valve(x, y + 12, "diaphragm", "FC"),
         "CONTROL VALVE, DIAPHRAGM"),
        (lambda x, y: symbols.control_valve(x, y + 12, "piston", "FO"),
         "CONTROL VALVE, PISTON"),
        (lambda x, y: symbols.control_valve(x, y + 12, "motor", None), "MOTOR OPERATED"),
        (lambda x, y: symbols.control_valve(x, y + 12, "solenoid", "FC"), "ESD / SHUTDOWN"),
        (lambda x, y: symbols.relief_valve(x, y - 4), "PRESSURE SAFETY VALVE"),
    ]),
    ("INSTRUMENTS", [
        (lambda x, y: symbols.bubble(x, y, "PT", "101", "field"), "FIELD MOUNTED"),
        (lambda x, y: symbols.bubble(x, y, "PIC", "101", "bpcs"), "BPCS (THE DCS)"),
        (lambda x, y: symbols.bubble(x, y, "PZHH", "101", "sis"), "SIS - SAFETY SYSTEM"),
        (lambda x, y: symbols.bubble(x, y, "AIC", "101", "computer"), "COMPUTER FUNCTION"),
        (lambda x, y: symbols.bubble(x, y, "I", "", "interlock"), "INTERLOCK LOGIC"),
        (lambda x, y: symbols.bubble(x, y, "PI", "101", "auxiliary"), "LOCAL PANEL"),
        (lambda x, y: symbols.orifice(x, y), "ORIFICE PLATE (FE)"),
        (lambda x, y: symbols.transformer(x, y), "TRANSFORMER"),
        (lambda x, y: symbols.breaker(x, y), "CIRCUIT BREAKER"),
    ]),
]


def build() -> Sheet:
    sheet = Sheet(
        width=1120, height=720, zone_columns=6, zone_rows=4,
        title_block=TitleBlock(
            drawing_number="AE-0000-PID-1001",
            title="LEGEND, SYMBOLS & LINE DESIGNATION",
            project="TYPICAL FACILITY - REFERENCE SET",
            originator="ACEENGINEER", location="HOUSTON, TEXAS",
            revision="A", sheet="1 OF 3", date="2026-09-03",
        ),
        aria_label=(
            "Legend sheet showing ISA-5.1 equipment, valve and instrument "
            "symbols with their meanings."
        ),
    )
    for col, (heading, rows) in enumerate(COLUMNS):
        x0 = 50 + col * 340
        sheet.add(
            f'<text x="{_n(x0)}" y="60" font-size="11" letter-spacing="1.6" '
            f'stroke="none" fill="currentColor" font-weight="600">{escape(heading)}</text>',
            f'<path d="M{_n(x0)},70 H{_n(x0 + 300)}" stroke-width="1" opacity=".45"/>',
        )
        for i, (draw, label) in enumerate(rows):
            cy = 106 + i * 54
            sheet.add(
                draw(x0 + 32, cy),
                f'<text x="{_n(x0 + 82)}" y="{_n(cy + 4)}" font-size="10.5" '
                f'stroke="none" fill="currentColor">{escape(label)}</text>',
            )
    sheet.note("ISA-5.1-2009: the square means shared display / shared control. "
               "The inner shape selects BPCS or SIS - it no longer means DCS or PLC.")
    sheet.note("Fail action (FC / FO / FL) is annotated in text on every actuated "
               "valve, not inferred from the stem arrow.")
    sheet.note("Line number reads SIZE-SERVICE-SEQUENCE-CLASS-INSULATION, e.g. "
               '12"-PG-1001-A1A-IH. A new number is assigned whenever size, '
               "service or piping class changes.")
    return sheet


def main() -> int:
    sheet = build()
    problems = sheet.lint()
    for problem in problems:
        print(f"lint: {problem}", file=sys.stderr)
    out = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("legend-sheet.svg")
    out.write_text(sheet.render(), encoding="utf-8")
    print(f"wrote {out} ({out.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
