"""Cited ABS Guidance Notes on Cathodic Protection of Ships (2017) values."""

from __future__ import annotations

from typing import Final

from digitalmodel.citations import Citation, CitedValue

ABS_WIKI_PATH: Final = (
    "wikis/engineering-standards/wiki/standards/"
    "abs-gn-ships-cathodic-protection-2017.md"
)
_BASE: Final = {
    "code_id": "abs-gn-ships",
    "publisher": "ABS",
    "revision": "2017-12",
    "wiki_path": ABS_WIKI_PATH,
    "source_sibling": "generic",
}


def _value(value: float, section: str, units: str, note: str) -> CitedValue:
    return CitedValue(
        value=value,
        units=units,
        citation=Citation(section=section, note=note, **_BASE),
    )


_ALUMINIUM: Final = {
    "A1": (-1.09, 2500.0),
    "A2": (-1.09, 2500.0),
    "A3": (-1.09, 2500.0),
    "A4": (-0.83, 1500.0),
}
_ZINC: Final = {
    "Z1": (-1.03, 780.0),
    "Z2": (-1.00, 760.0),
    "Z3": (-1.03, 780.0),
    "Z4": (-1.03, 780.0),
}
_COATING_RATES: Final = {
    "low": (3.0, 3.0),
    "medium": (1.5, 1.5),
    "high": (0.5, 1.0),
}
_DESIGN_CURRENTS: Final = {
    ("up_to_1_m_s_no_tide", "bare"): (100.0, 200.0),
    ("up_to_1_m_s_no_tide", "coated"): (5.0, 15.0),
    ("up_to_1_m_s_with_tide", "bare"): (150.0, 250.0),
    ("up_to_1_m_s_with_tide", "coated"): (7.0, 20.0),
    ("1_to_10_m_s", "bare"): (220.0, 350.0),
    ("1_to_10_m_s", "coated"): (11.0, 28.0),
    ("at_least_10_m_s", "bare"): (350.0, 500.0),
    ("at_least_10_m_s", "coated"): (18.0, 40.0),
    ("ice", "bare"): (500.0, 750.0),
    ("ice", "coated"): (35.0, 90.0),
}
_AVERAGE_COATED: Final = {
    "up_to_18_months": (15.0, 25.0),
    "19_to_36_months": (26.0, 45.0),
    "37_to_60_months": (46.0, 75.0),
}


def aluminium_properties(alloy: str) -> tuple[CitedValue, CitedValue]:
    """Closed-circuit potential and practical capacity, Section 3 Table 4."""
    key = alloy.upper()
    if key not in _ALUMINIUM:
        raise ValueError(f"unsupported aluminium alloy {alloy!r}; expected A1-A4")
    potential, capacity = _ALUMINIUM[key]
    return (
        _value(potential, "Section 3, Table 4", "V", f"{key} closed-circuit potential"),
        _value(capacity, "Section 3, Table 4", "Ah/kg", f"{key} practical capacity"),
    )


def zinc_properties(
    alloy: str, *, temperature_c: float = 25.0
) -> tuple[CitedValue, CitedValue]:
    """Closed-circuit potential and practical capacity, Section 3 Table 2."""
    key = alloy.upper()
    if key not in _ZINC:
        raise ValueError(f"unsupported zinc alloy {alloy!r}; expected Z1-Z4")
    if key == "Z4" and 60.0 <= temperature_c <= 80.0:
        potential, capacity = (-0.97, 690.0)
    elif 5.0 <= temperature_c <= 25.0:
        potential, capacity = _ZINC[key]
    else:
        raise ValueError("zinc temperature must match a Section 3 Table 2 row")
    return (
        _value(potential, "Section 3, Table 2", "V", f"{key} closed-circuit potential"),
        _value(capacity, "Section 3, Table 2", "Ah/kg", f"{key} practical capacity"),
    )


def coating_breakdown_range(durability: str) -> tuple[CitedValue, CitedValue]:
    """Annual coating deterioration range in percentage points per year."""
    key = durability.lower()
    if key not in _COATING_RATES:
        raise ValueError(f"unsupported coating durability {durability!r}")
    low, high = _COATING_RATES[key]
    section = "Section 2, Table 4"
    return (
        _value(low, section, "%/yr", f"{key} durability lower rate"),
        _value(high, section, "%/yr", f"{key} durability upper rate"),
    )


def design_current_density(
    situation: str, condition: str
) -> tuple[CitedValue, CitedValue]:
    """Section 2 Table 3 design current-density range in mA/m2."""
    key = (situation, condition)
    if key not in _DESIGN_CURRENTS:
        raise ValueError(f"unsupported Table 3 current-density row {key!r}")
    low, high = _DESIGN_CURRENTS[key]
    note = f"{situation} {condition} steel"
    return (
        _value(low, "Section 2, Table 3", "mA/m2", f"{note} lower bound"),
        _value(high, "Section 2, Table 3", "mA/m2", f"{note} upper bound"),
    )


def average_coated_current_density(period: str) -> tuple[CitedValue, CitedValue]:
    """Section 2 Table 5 average coated-hull current-density range."""
    if period not in _AVERAGE_COATED:
        raise ValueError(f"unsupported Table 5 docking period {period!r}")
    low, high = _AVERAGE_COATED[period]
    return (
        _value(low, "Section 2, Table 5", "mA/m2", f"{period} lower bound"),
        _value(high, "Section 2, Table 5", "mA/m2", f"{period} upper bound"),
    )


def equation_reference(section: str, note: str) -> CitedValue:
    """Citation sidecar for a governing equation or layout criterion."""
    return _value(1.0, section, "reference", note)


def protection_potential(anaerobic: bool = False) -> CitedValue:
    """Carbon/low-alloy steel protection criterion versus Ag/AgCl/seawater."""
    value = -0.90 if anaerobic else -0.80
    return _value(value, "Section 2, Table 1", "V", "steel protection potential")


def citation_label(value: CitedValue) -> str:
    """Stable compact label retained in result sidecars."""
    c = value.citation
    return f"{c.code_id} {c.revision} {c.section}"


__all__ = [
    "ABS_WIKI_PATH",
    "aluminium_properties",
    "average_coated_current_density",
    "citation_label",
    "coating_breakdown_range",
    "design_current_density",
    "equation_reference",
    "protection_potential",
    "zinc_properties",
]
