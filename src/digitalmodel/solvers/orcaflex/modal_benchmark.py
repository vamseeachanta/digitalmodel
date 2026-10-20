# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare stored modal results without importing a solver or parent package.

Run as a script (not ``python -m``)::

    python src/digitalmodel/solvers/orcaflex/modal_benchmark.py targets.yaml result.json

CSV goes to stdout, or to a new file with ``--output comparison.csv``.
For Python callers, load this file with importlib.util.spec_from_file_location.
The parent package is not part of this standalone interface.

For an unfiltered stored ``_modes_summary.csv`` add ``--summary-csv --model mj01
--period-unit s --synthetic false``. Confirm extraction started at the first mode.
The CSV adapter leaves directions unknown; annotated JSON is required to compare
classified targets. No solver is imported or executed by either input route.

Result JSON: model (target-table key), synthetic (boolean), modes (list of
mode, frequency_hz, optional direction). Frequencies are finite and positive;
mode identifiers are positive integers. Directions are in-plane/out-of-plane.
Export a complete ascending spectrum from the first mode, even where published
frequencies are missing. This is rank pairing, not a mode-shape correlation.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import sys
from pathlib import Path
from typing import Any, TextIO

import yaml

PAIRING_RULE = (
    "Ascending frequency within the same direction class where recorded; "
    "reserve classified targets first, then pair remaining slots by ascending "
    "frequency. Missing published slots consume ranks; no direction is guessed. "
    "Unclassified pairing is withheld if any classified target lacks a match. "
    "Equal computed frequencies use mode number as tie-break. No pass/fail limit."
)
MISSING = "not held in the repository"
DIRECTIONS = (None, "in-plane", "out-of-plane")
FIELDS = (
    "model",
    "synthetic",
    "published_mode",
    "published_frequency_hz",
    "published_direction",
    "computed_mode",
    "computed_frequency_hz",
    "computed_direction",
    "difference_percent",
    "target_status",
    "status",
    "citation",
    "sources",
    "pairing_rule",
)


def _validate_modes(rows: Any, *, published: bool) -> list[dict[str, Any]]:
    if not isinstance(rows, list):
        raise ValueError("modes must be a list")
    seen = set()
    for row in rows:
        if not isinstance(row, dict):
            raise ValueError("each mode must be a mapping")
        number, frequency = row.get("mode"), row.get("frequency_hz")
        if type(number) is not int or number < 1 or number in seen:
            raise ValueError("mode numbers must be unique positive integers")
        seen.add(number)
        if not (published and frequency is None):
            if isinstance(frequency, bool) or not isinstance(frequency, (int, float)):
                raise ValueError("frequency_hz must be finite numeric data")
            if not math.isfinite(frequency):
                raise ValueError("frequency_hz must be finite numeric data")
            if frequency <= 0:
                raise ValueError("frequency_hz must be positive")
        if row.get("direction") not in DIRECTIONS:
            raise ValueError("direction must be in-plane, out-of-plane, or null")
    if seen != set(range(1, len(rows) + 1)):
        raise ValueError("modes must include every rank slot from one")
    if published:
        known = [
            r["frequency_hz"]
            for r in sorted(rows, key=lambda r: r["mode"])
            if r["frequency_hz"] is not None
        ]
        if known != sorted(known):
            raise ValueError("published mode ranks must follow ascending frequency")
    return rows


def load_targets(path: str | Path) -> dict[str, Any]:
    """Read repository targets with explicit gaps and per-mode provenance."""
    table = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
    if not isinstance(table, dict) or not isinstance(table.get("models"), dict):
        raise ValueError("target table must contain a models mapping")
    for model in table["models"].values():
        for row in _validate_modes(model["modes"], published=True):
            if not row.get("citation") or not row.get("sources"):
                raise ValueError("every target slot needs citation and sources")
            if row["frequency_hz"] is None and row.get("availability") != MISSING:
                raise ValueError("missing published frequency needs an explicit gap")
            for source in row["sources"]:
                file = source.get("file")
                start, end = source.get("line_start"), source.get("line_end")
                if not isinstance(file, str) or not file or ":" in file:
                    raise ValueError("source file must be repository-relative")
                if Path(file).is_absolute() or ".." in Path(file).parts:
                    raise ValueError("source file must stay within the repository")
                if (
                    type(start) is not int
                    or type(end) is not int
                    or not 1 <= start <= end
                ):
                    raise ValueError("source lines must be positive ordered integers")
    return dict(table)


def load_result(path: str | Path) -> dict[str, Any]:
    """Read stored hertz data; never infer direction from solver DOF labels."""
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(data, dict) or not isinstance(data.get("model"), str):
        raise ValueError("result must identify its target-table model")
    if type(data.get("synthetic")) is not bool:
        raise ValueError("result must explicitly declare synthetic true or false")
    _validate_modes(data.get("modes"), published=False)
    return dict(data)


def read_summary_csv(
    stream: TextIO, *, model: str, synthetic: bool, period_unit: str
) -> dict[str, Any]:
    """Convert stored, unfiltered modal summary rows into the JSON interface.

    Column basis: OrcModalAnalysis.modes_summary_df_columns and save_summary_files
    in orcaflex_modal_analysis.py. Repeated object rows share a global mode index.
    Period basis: Orcina Python reference, Modes.modeDetails (accessed 2026-10-20):
    https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference,Modes.htm
    f = 1 / T by definition of period; callers must confirm T is in seconds.
    DOF labels cannot establish the jumper plane; directions remain unknown.
    """
    if period_unit != "s" or type(synthetic) is not bool:
        raise ValueError("confirm period_unit='s' and a boolean synthetic origin")
    if not isinstance(model, str) or not model:
        raise ValueError("model must be a nonempty target-table key")
    reader = csv.DictReader(stream)
    if not {"modeIndex", "period"}.issubset(reader.fieldnames or []):
        raise ValueError("summary CSV needs modeIndex and period columns")
    periods = {}
    for row in reader:
        index_text = row.get("modeIndex", "")
        if not isinstance(index_text, str) or not index_text.isdecimal():
            raise ValueError("modeIndex must be a zero-based integer")
        index = int(index_text)
        try:
            period = float(row["period"])
        except (ValueError, TypeError) as error:
            raise ValueError("period must be finite and positive") from error
        if not math.isfinite(period) or period <= 0:
            raise ValueError("period must be finite and positive")
        if index in periods and periods[index] != period:
            raise ValueError("conflicting periods for one global modeIndex")
        periods[index] = period
    if not periods:
        raise ValueError("summary CSV must contain modes")
    # +1 converts the producer's zero-based index to the benchmark's rank slots.
    modes = [
        {"mode": index + 1, "frequency_hz": 1 / period, "direction": None}
        for index, period in sorted(periods.items())
    ]
    _validate_modes(modes, published=False)
    return {"model": model, "synthetic": synthetic, "modes": modes}


def _pair_modes(
    targets: list[dict[str, Any]],
    modes: list[dict[str, Any]],
) -> dict[int, dict[str, Any]]:
    remaining = sorted(modes, key=lambda row: (row["frequency_hz"], row["mode"]))
    paired = {}
    # Classified targets reserve matching modes before unrestricted rows.
    for published in targets:
        direction = published.get("direction")
        if direction is None:
            continue
        match = next(
            (row for row in remaining if row.get("direction") == direction), None
        )
        if match is not None:
            paired[published["mode"]] = match
            remaining.remove(match)
    if any(
        row.get("direction") is not None and row["mode"] not in paired
        for row in targets
    ):
        return paired
    unclassified = [row for row in targets if row.get("direction") is None]
    for published, computed in zip(unclassified, remaining):
        paired[published["mode"]] = computed
    return paired


def compare_modes(
    targets: list[dict[str, Any]],
    modes: list[dict[str, Any]],
) -> list[dict[str, Any]]:
    """Return signed percentage differences, with no acceptance threshold.

    Formula: (computed - published) / published * 100, per the benchmark plan.
    A null reference frequency preserves its rank and never yields a difference.
    Inputs are not mutated. Extra computed modes are outside the target table.
    """
    targets = sorted(
        _validate_modes(targets, published=True), key=lambda row: row["mode"]
    )
    _validate_modes(modes, published=False)
    pairs = _pair_modes(targets, modes)
    rows = []
    for published in targets:
        computed = pairs.get(published["mode"], {})
        reference, actual = published["frequency_hz"], computed.get("frequency_hz")
        difference = None
        status = "not computed"
        if actual is not None:
            status = MISSING if reference is None else "compared"
            if reference is not None:
                difference = (actual - reference) / reference * 100
        rows.append(
            {
                "published_mode": published["mode"],
                "published_frequency_hz": reference,
                "published_direction": published.get("direction"),
                "computed_mode": computed.get("mode"),
                "computed_frequency_hz": actual,
                "computed_direction": computed.get("direction"),
                "difference_percent": difference,
                "target_status": MISSING if reference is None else "held",
                "status": status,
                "citation": published.get("citation", ""),
                "sources": json.dumps(published.get("sources", [])),
                "pairing_rule": PAIRING_RULE,
            }
        )
    return rows


def write_comparison(
    rows: list[dict[str, Any]],
    stream: TextIO,
    *,
    model: str,
    synthetic: bool,
) -> None:
    """Write UTF-8-compatible CSV rows, including the rule and data origin."""
    writer = csv.DictWriter(stream, fieldnames=FIELDS, lineterminator="\n")
    writer.writeheader()
    for row in rows:
        output = dict(row, model=model, synthetic=synthetic)
        difference = output["difference_percent"]
        if difference is not None:
            output["difference_percent"] = f"{difference:.1f}"
        writer.writerow(output)


def main(argv: list[str] | None = None) -> None:
    """Write a comparison CSV from stored targets and one model's JSON result."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("targets", type=Path)
    parser.add_argument("result", type=Path)
    parser.add_argument("--output", type=Path, help="New CSV file; defaults to stdout")
    parser.add_argument("--summary-csv", action="store_true")
    parser.add_argument("--model", help="Required target-table key for summary CSV")
    parser.add_argument("--period-unit", choices=["s"])
    parser.add_argument("--synthetic", choices=["true", "false"])
    args = parser.parse_args(argv)
    table = load_targets(args.targets)
    if args.summary_csv:
        if not args.model or not args.period_unit or args.synthetic is None:
            parser.error("summary CSV requires --model, --period-unit and --synthetic")
        with args.result.open(encoding="utf-8-sig", newline="") as stream:
            result = read_summary_csv(
                stream,
                model=args.model,
                synthetic=args.synthetic == "true",
                period_unit=args.period_unit,
            )
    else:
        if args.model or args.period_unit or args.synthetic is not None:
            parser.error("CSV metadata options require --summary-csv")
        result = load_result(args.result)
    if result["model"] not in table["models"]:
        raise ValueError("result model is absent from target table")
    rows = compare_modes(table["models"][result["model"]]["modes"], result["modes"])
    print(PAIRING_RULE, file=sys.stderr)
    if args.output:
        with args.output.open("x", encoding="utf-8", newline="") as stream:
            write_comparison(
                rows, stream, model=result["model"], synthetic=result["synthetic"]
            )
    else:
        write_comparison(
            rows, sys.stdout, model=result["model"], synthetic=result["synthetic"]
        )


if __name__ == "__main__":
    main()
