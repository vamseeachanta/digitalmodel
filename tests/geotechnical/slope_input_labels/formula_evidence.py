"""Extract formula evidence and referenced inputs; never save a source workbook.

Run from the repository root:
    python tests/geotechnical/slope_input_labels/formula_evidence.py

openpyxl is an extraction/test dependency only. Whole-column lookup inputs are
stored through the sheet's last nonblank row; omitted later rows are blank.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from openpyxl import load_workbook
from openpyxl.formula import Tokenizer
from openpyxl.utils.cell import coordinate_from_string, range_boundaries


REPOSITORY = Path(__file__).resolve().parents[3]
SOURCES = (
    "tests/fixtures/geotechnical/slope/xslope_simple1.xlsx",
    "tests/fixtures/geotechnical/slope/xslope_clay_blanket.xlsx",
    "src/digitalmodel/data/crosby/26_shackles_bolt_type_anchor_g2130.xlsx",
)


def extract_workbook(path: Path, repository: Path = REPOSITORY) -> dict:
    """Read formulas and caches separately, including referenced blank inputs.

    Formula literals remain in formula text. Constants include every referenced
    cell within the used bounds, plus visible unit labels. Whole-column inputs
    end at the last nonblank sheet row; omitted later rows are blank. Unsupported
    named or external references fail rather than silently omitting input evidence.
    """
    source = path.resolve().relative_to(repository.resolve()).as_posix()
    formulas_book = load_workbook(path, data_only=False, keep_links=True)
    cached_book = load_workbook(path, data_only=True, keep_links=True)
    try:
        constants: dict[str, dict] = {s.title: {} for s in formulas_book}
        sheets = []
        formulas = []
        for sheet in formulas_book:
            cells = [c for row in sheet for c in row if c.data_type == "f"]
            sheets.append({
                "sheet": sheet.title,
                "formula_count": len(cells),
                "cached_count": sum(
                    cached_book[sheet.title][c.coordinate].value is not None
                    for c in cells
                ),
            })
            for cell in cells:
                for token in Tokenizer(cell.value).items:
                    if token.type != "OPERAND" or token.subtype != "RANGE":
                        continue
                    target = sheet
                    address = token.value
                    if "!" in address:
                        title, address = address.rsplit("!", 1)
                        target = formulas_book[title.strip("'")]
                    min_col, min_row, max_col, max_row = range_boundaries(address)
                    if max_row is None:
                        max_row = max(
                            (c.row for row in target for c in row if c.value is not None),
                            default=1,
                        )
                    for row in target.iter_rows(
                        min_col=min_col, max_col=max_col,
                        min_row=min_row or 1, max_row=max_row,
                    ):
                        for dependency in row:
                            if dependency.data_type != "f":
                                constants[target.title][dependency.coordinate] = dependency.value
                formulas.append({
                    "sheet": sheet.title,
                    "cell": cell.coordinate,
                    "formula": cell.value,
                    "cached_value": cached_book[sheet.title][cell.coordinate].value,
                })
        if "main" in constants:
            constants["main"]["D8"] = formulas_book["main"]["D8"].value
        else:
            constants["Sheet1"]["A1"] = formulas_book["Sheet1"]["A1"].value
        return {
            "source": source,
            "sheets": sheets,
            "defined_names": {k: v.attr_text for k, v in formulas_book.defined_names.items()},
            "external_links_count": len(formulas_book._external_links),
            "vba_present": formulas_book.vba_archive is not None,
            "constants": constants,
            "formulas": formulas,
        }
    finally:
        formulas_book.close()
        cached_book.close()


def format_evidence(evidence: dict) -> str:
    """Group each constant row and each formula record on one JSON line."""
    constants = evidence["constants"]
    lines = ["{"]
    for sheet, cells in constants.items():
        lines.append(f"    {json.dumps(sheet)}: {{")
        rows = {}
        for address, value in cells.items():
            row = coordinate_from_string(address)[1]
            rows.setdefault(row, {})[address] = value
        lines.extend(
            "      " + json.dumps(cells, ensure_ascii=False)[1:-1] + ","
            for _, cells in sorted(rows.items())
        )
        if rows:
            lines[-1] = lines[-1].removesuffix(",")
        lines.append("    },")
    if constants:
        lines[-1] = lines[-1].removesuffix(",")
    lines.append("  }")
    expanded = json.dumps(constants, ensure_ascii=False, indent=2)
    expanded = expanded.replace("\n", "\n  ")
    formatted = json.dumps(evidence, ensure_ascii=False, indent=2).replace(
        '"constants": ' + expanded, '"constants": ' + "\n".join(lines), 1,
    )
    records = evidence["formulas"]
    compact = "[\n" + ",\n".join(
        "    " + json.dumps(record, ensure_ascii=False, separators=(", ", ": "))
        for record in records
    ) + "\n  ]" if records else "[]"
    expanded = json.dumps(records, ensure_ascii=False, indent=2)
    expanded = expanded.replace("\n", "\n  ")
    return formatted.replace('"formulas": ' + expanded, '"formulas": ' + compact, 1) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir", type=Path,
        help="Override the destination directory for all fixture files",
    )
    args = parser.parse_args()
    for source in SOURCES:
        evidence = extract_workbook(REPOSITORY / source)
        if "geotechnical" in source:
            output_dir = REPOSITORY / "tests/geotechnical/slope_input_labels/fixtures"
            filename = Path(source).stem + ".json"
        else:
            output_dir = REPOSITORY / "tests/specialized/rigging/fractional_inch/fixtures"
            filename = "anchor_shackle_sizes.json"
        output_dir = args.output_dir if args.output_dir is not None else output_dir
        output_dir.resolve().relative_to(REPOSITORY)
        output_dir.mkdir(parents=True, exist_ok=True)
        destination = output_dir / filename
        destination.write_text(
            format_evidence(evidence), encoding="utf-8",
        )
        print(f"{destination.relative_to(REPOSITORY)}: {len(evidence['formulas'])} formulas")


if __name__ == "__main__":
    main()
