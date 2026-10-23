# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exact grid completeness contracts; no numerical tolerance is applied."""

import math


class GridCompletenessError(ValueError):
    """Machine-readable refusal to expose incomplete hydrodynamic data."""

    def __init__(self, table, reason, *, expected=None, actual=None):
        self.table = table
        self.reason = reason
        self.expected = expected
        self.actual = actual
        super().__init__(f"{table}: {reason} (expected={expected}, actual={actual})")


def require_count(table, actual, expected):
    """Compare integer cardinalities, not a solver-accuracy acceptance limit."""
    if expected is not None and actual != expected:
        raise GridCompletenessError(table, "count_mismatch",
                                    expected=expected, actual=actual)


def deck_frequency_count(text):
    """Count explicit indexed HRTZ cards emitted by the repository backend.

    Source: aqwa_backend.py, build_deck6, Deck 6 FDR1.
    Fields are fixed card grammar, not character windows over result tables.
    Ranged/other frequency cards require an explicit count from the caller;
    their interpolation rules are deliberately not guessed here.
    """
    indices = set()
    for line in text.splitlines():
        if line[6:10] not in {"HRTZ", "FREQ", "PERD"}:
            continue
        if line[6:10] != "HRTZ":
            raise GridCompletenessError("deck", "unsupported_frequency_card")
        try:
            first, last = int(line[10:15]), int(line[15:20])
            value = float(line[20:30].replace("D", "E"))
        except ValueError as exc:
            raise GridCompletenessError("deck", "invalid_frequency_card") from exc
        if first != last:
            raise GridCompletenessError("deck", "unsupported_frequency_range")
        if first <= 0 or first in indices or not math.isfinite(value) or not value > 0:
            raise GridCompletenessError("deck", "invalid_frequency_index")
        indices.add(first)
    if not indices:
        raise GridCompletenessError("deck", "missing_frequency_declaration")
    if indices != set(range(1, len(indices) + 1)):
        raise GridCompletenessError("deck", "noncontiguous_frequency_indices")
    return len(indices)


def validate_parsed_grid(data):
    """Protect converters from stale/external parsed dictionaries before padding."""
    frequencies = data["frequencies"]
    declared = data.get("declared_frequency_count")
    require_count("frequencies", len(frequencies), declared)
    require_count("unique_frequencies", len(set(frequencies)), len(frequencies))
    for table in ("added_mass", "damping"):
        require_count(table, len(data[table]), len(frequencies))
        if set(data[table]) != set(frequencies):
            raise GridCompletenessError(table, "frequency_keys_mismatch")
    if "raos" in data:
        expected_keys = {(f, h) for f in frequencies for h in data["headings"]}
        if set(data["raos"]) != expected_keys:
            raise GridCompletenessError("raos", "frequency_heading_keys_mismatch",
                                        expected=len(expected_keys), actual=len(data["raos"]))


def validate_result_grid(result):
    """Reject an inconsistent declared grid at both comparison entry points.

    FrequencyData.count is the schema declaration. Matrix and RAO sizes must
    agree with it before any interpolation, alignment or statistical scoring.
    """
    declared = getattr(result, "declared_frequency_count", None)
    if declared is None:
        return
    for name in ("raos", "added_mass", "damping"):
        data = getattr(result, name, None)
        if data is None:
            continue
        if name == "raos":
            components = [getattr(data, dof) for dof in
                          ("surge", "sway", "heave", "roll", "pitch", "yaw")]
            for component in components:
                frequencies = component.frequencies
                require_count(name, len(frequencies.values), frequencies.count)
                require_count(name, len(frequencies.values), declared)
                require_count(name, len(component.magnitude), frequencies.count)
                require_count(name, len(component.phase), frequencies.count)
        else:
            frequencies = data.frequencies
            require_count(name, len(frequencies.values), frequencies.count)
            require_count(name, len(frequencies.values), declared)
            require_count(name, len(data.matrices), frequencies.count)
