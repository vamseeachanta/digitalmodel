"""Read reviewed representative elements with explicit millimetre columns."""
import csv
import hashlib
from io import StringIO
from pathlib import Path
import re

from .screen import number

_COMMON = {"id", "as_built_mm", "measured_mm", "uncertainty_mm"}
_FAMILY = {"csr": {"corrosion_mm", "voluntary_mm"}, "percentage": {"renewal_mm"}}
_DECIMAL = re.compile(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?")


def read_elements_csv(path, family, *, base_dir=None):
    """Return elements and provenance for exact UTF-8 source bytes.

    One CSV row is one representative element; no averaging, unit conversion,
    zone assignment or acceptance-limit inference is performed. Relative paths
    use base_dir when supplied. URLs and network-share syntax are unsupported.
    """
    if not isinstance(family, str) or family not in _FAMILY:
        raise ValueError("rule_family must be csr or percentage")
    if not isinstance(path, (str, Path)) or not str(path).strip():
        raise ValueError("elements_csv must select a local file")
    if "://" in str(path) or str(path).startswith(("//", "\\\\")):
        raise ValueError("elements_csv must select a local file without network-share syntax")
    path = Path(path)
    if not path.is_absolute() and base_dir is not None:
        path = Path(base_dir) / path
    raw = path.read_bytes()
    try:
        text = raw.decode("utf-8-sig")
    except UnicodeDecodeError as error:
        raise ValueError("CSV row 1 field encoding: UTF-8 required") from error
    reader = csv.reader(StringIO(text, newline=""), strict=True)
    elements, seen = [], set()
    try:
        header = next(reader, [])
        if len(header) != len(set(header)) or set(header) != _COMMON | _FAMILY[family]:
            raise ValueError(f"CSV row 1 field header: exact {family} column names required")
        for row in reader:
            line = reader.line_num
            if len(row) != len(header):
                raise ValueError(f"CSV row {line} field columns: row width differs from header")
            item = dict(zip(header, row))
            key = item["id"]
            if not key.strip() or key in seen:
                raise ValueError(f"CSV row {line} field id: nonempty unique identifier required")
            seen.add(key)
            for field in header:
                if field != "id":
                    try:
                        if not _DECIMAL.fullmatch(item[field].strip()):
                            raise ValueError("finite decimal number required; dot decimal separator only")
                        item[field] = float(item[field])
                        item[field] = number(item, field, positive=field in {"as_built_mm", "renewal_mm"})
                    except ValueError as error:
                        raise ValueError(f"CSV row {line} field {field}: {error}") from error
            elements.append(item)
    except csv.Error as error:
        raise ValueError(f"CSV row {max(1, reader.line_num)} field csv: malformed record") from error
    if not elements:
        raise ValueError("CSV row 2 field records: at least one representative element required")
    return {"elements": elements, "provenance": {
        "schema": "hull-wastage-elements/v1", "units": "mm",
        "sha256": hashlib.sha256(raw).hexdigest(), "record_count": len(elements),
    }}
