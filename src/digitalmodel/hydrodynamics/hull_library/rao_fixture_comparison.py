"""Compare the two box exports at fixture-derived numeric export precision."""

from decimal import Decimal
from pathlib import Path
from xml.etree import ElementTree
from zipfile import ZipFile

import numpy as np

FIELDS = ("frequencies", "directions", "amplitudes", "phases")


def xlsx_significant_digits(path: Path) -> int:
    """Read the maximum numeric significand length from source worksheet XML.

    The pipeline writer uses openpyxl's significant-digit numeric formatter;
    shorter cell strings can omit trailing zeros and do not imply coarse data.
    """
    namespace = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
    lengths = []
    with ZipFile(path) as workbook:
        for name in workbook.namelist():
            if not name.startswith("xl/worksheets/") or not name.endswith(".xml"):
                continue
            for cell in ElementTree.fromstring(workbook.read(name)).iter(namespace + "c"):
                value = cell.find(namespace + "v")
                if cell.get("t", "n") == "n" and value is not None and value.text:
                    lengths.append(len(Decimal(value.text).as_tuple().digits))
    if not lengths:
        raise ValueError("No numeric worksheet values establish export precision")
    return max(lengths)


def _export_bounds(values: np.ndarray, significant_digits: int) -> np.ndarray:
    """Half a decimal export unit plus binary parsing spacing, per source value.

    Decimal rounding unit = 10**(order - significant_digits + 1).
    This derives numerical bounds from source values, not a physical tolerance.
    """
    decimal_bounds = [
        float(Decimal(10) ** (Decimal(str(abs(float(value)))).adjusted()
                             - significant_digits + 1) / 2) if value else 0.0
        for value in values.flat
    ]
    return np.array(decimal_bounds).reshape(values.shape) + np.abs(np.spacing(values))


def compare_box_exports(left, right, left_path: Path, right_path: Path) -> dict:
    """Report equivalence of this named pair under source export precision.

    Circular phase difference follows comparison_framework.py::compare_raos.
    Direction equality is exact. Engineering coordinate qualification is separate.
    """
    if any(not np.isfinite(getattr(rao, field)).all()
           for rao in (left, right) for field in FIELDS):
        raise ValueError("RAO arrays must be finite")
    digits = [xlsx_significant_digits(path) for path in (left_path, right_path)]
    directions_exact = np.array_equal(left.directions, right.directions)
    report = {"equivalent": directions_exact, "directions_exact": directions_exact,
              "source_significant_digits": digits, "components": {}}
    # Full/half cycle follow the repository's degree/radian phase convention.
    turn = float(np.degrees(2 * np.pi))
    units = {"frequencies": "rad/s", "amplitudes": "m/m (translation), rad/m (rotation)",
             "phases": "deg"}
    for field in ("frequencies", "amplitudes", "phases"):
        a, b = getattr(left, field), getattr(right, field)
        if a.shape != b.shape:
            report["equivalent"] = False
            report["components"][field] = {"within_bounds": False, "reason": "shape mismatch"}
            continue
        raw_difference = a - b
        bounds = _export_bounds(a, digits[0]) + _export_bounds(b, digits[1])
        difference = raw_difference
        if field == "phases":
            difference = np.mod(raw_difference + turn / 2, turn) - turn / 2
            bounds += np.spacing(turn)
        within = bool(np.all(np.abs(difference) <= bounds))
        report["components"][field] = {
            "units": units[field], "within_bounds": within,
            "max_raw_abs_difference": float(np.max(np.abs(raw_difference))),
            "max_comparison_abs_difference": float(np.max(np.abs(difference))),
            "max_export_bound": float(np.max(bounds)),
        }
        report["equivalent"] = report["equivalent"] and within
    return report
