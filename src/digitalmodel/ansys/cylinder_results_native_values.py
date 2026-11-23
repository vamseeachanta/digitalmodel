"""Observed native summary VALUE records; no whitespace-token fallback."""
from digitalmodel.ansys.cylinder_results import EvidenceError, parse_e24


def summary_fields(row, count):
    """Preserve each E24.16 field, including adjacent negative values."""
    if (not isinstance(row, bytes) or type(count) is not int or count not in (2, 4, 6)
            or not row.startswith(b' VALUE  ') or len(row) != 8 + 24 * count):
        raise EvidenceError('Native summary VALUE prefix or fixed width differs')
    fields = [row[8 + 24 * i:32 + 24 * i] for i in range(count)]
    for field in fields:
        parse_e24(field, 'dimensionless')
    return fields
